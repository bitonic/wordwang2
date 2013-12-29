module WordWang
    ( module WordWang.Messages
    , module WordWang.Monad
    , module WordWang.Objects

    , RoomEnv(..)
    , roomEnvRoom
    , roomEnvConnections
    , roomEnvPGPool
    , RootEnv(..)
    , rootEnvRooms
    , rootEnvPGPool
    , restoreRoom
    , addRoom
    , serverWW
    , wordwang
    ) where

import           Control.Concurrent.MVar (MVar, modifyMVar_, readMVar, newEmptyMVar, putMVar)
import           Control.Exception (try)
import           Control.Monad (filterM, void, unless)
import           Data.Foldable (forM_, toList)
import           Data.Functor ((<$>))
import           Data.IORef (newIORef, atomicModifyIORef')
import           Data.Int (Int64)

import           Control.Monad.State (execStateT, StateT)
import           Control.Monad.Trans (liftIO)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           System.Random (randomIO)

import           Control.Lens
import           Crypto.Random (genBytes, newGenIO)
import           Crypto.Random.DRBG (HashDRBG)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64.URL as Base64.URL
import           Data.Pool (Pool, withResource)
import qualified Database.PostgreSQL.Simple as PG
import qualified Network.WebSockets as WS
import           Snap (Snap)
import qualified Snap as Snap

import           WordWang.Utils
import           WordWang.Objects
import           WordWang.Messages
import           WordWang.Monad
import qualified WordWang.PostgreSQL as WWPG


data RoomEnv = RoomEnv
    { _roomEnvRoom        :: !Room
    , _roomEnvConnections :: ![TaggedConn]
    , _roomEnvPGPool      :: !(Pool PG.Connection)
    }

data RootEnv = RootEnv
    { _rootEnvRooms  :: MVar (HashMap RoomId (MVar RoomEnv))
    , _rootEnvPGPool :: !(Pool PG.Connection)
    }

makeLenses ''RoomEnv
makeLenses ''RootEnv


internalError :: String -> WW a
internalError = terminate . InternalError

makeSecret :: WW UserSecret
makeSecret = do
    gen <- liftIO (newGenIO :: IO HashDRBG)
    case genBytes 15 gen of
      Left err      -> internalError $ show err
      Right (bs, _) -> return $ Base64.URL.encode bs

-- data IncreWorkerMsg = Done | Bump

-- increWorker :: MVar Story -> Worker.Ref RespWorkerMsg
--             -> IO (Worker.Send IncreWorkerMsg -> Worker IncreWorkerMsg)
-- increWorker storyMv respWRef = do
--     sid <- _roomId <$> readMVar storyMv
--     return $ \send -> Worker
--         { workerName = Text.pack (show sid ++ "-incre")
--         , workerStart = return Nothing
--         , workerRestart = \_ _ _ -> return (Left "increWorker: shouldn't terminate")
--         , workerReceive = \increM msg -> (Just <$> process send increM msg)
--         }
--   where
--     startT = 10000000

--     halveMaybe n | n >= 500000 = Just (n - (n `div` 3))
--     halveMaybe _ = Nothing

--     process send Nothing Bump = do
--         incre <- Incre.start startT halveMaybe
--         -- TODO are we sure this terminates?
--         supervise $ do
--             Incre.wait incre
--             send Done
--         return (Just incre)
--     process _ (Just incre) Bump = Just incre <$ Incre.bump incre
--     process _ increM Done = do
--         maybe (return ()) Incre.stop increM
--         modifyMVar_ storyMv $ \story -> do
--             case HashMap.elems (story^.storyCandidates) of
--                 [] -> error "increWorker: incremental terminated with no candidates!"
--                 cands@(_:_) -> do
--                     let cand  = maximumBy (comparing (HashSet.size . _candVotes)) cands
--                         block = cand^.candBlock
--                     Worker.send respWRef
--                         (SendResp (respToAll (RespVotingClosed block)))
--                     let story' = story & storyCandidates .~ HashMap.empty
--                     return (story' & storyBlocks %~ (++ [block]))
--         return increM

authenticated :: WW UserId
authenticated = do
    mbAuth <- use (wwReq . reqAuth)
    case mbAuth of
      Nothing -> do
        terminate NoCredentials
      Just auth -> do
        users <- use (wwRoom . rUsers)
        let userId = auth ^. reqAuthUserId
        case users ^. at userId of
          Just user | auth ^. reqAuthSecret == user ^. uSecret ->
            return userId
          _ ->
            terminate InvalidCredentials

authenticated_ :: WW ()
authenticated_ = void authenticated

restoreRoom :: RoomId -> Room -> RootEnv -> IO ()
restoreRoom roomId room rootEnv =
    modifyMVar_ (rootEnv ^. rootEnvRooms) $ \rooms -> do
      roomMv <- newEmptyMVar
      putMVar roomMv RoomEnv{ _roomEnvRoom        = room
                            , _roomEnvConnections = []
                            , _roomEnvPGPool      = rootEnv ^. rootEnvPGPool
                            }
      return $ HashMap.insert roomId roomMv rooms

addRoom :: RootEnv -> Snap ()
addRoom rootEnv = do
    roomId <- liftIO randomIO
    let room = emptyRoom
    -- Add the room to the DB
    liftIO $ withResource (rootEnv ^. rootEnvPGPool) $ \conn ->
      WWPG.addRoom conn roomId room
    -- Add the room the the mvar
    liftIO $ restoreRoom roomId room rootEnv

    -- Send response
    Snap.modifyResponse $
        Snap.setResponseCode 200 . Snap.setContentType "text/json"
    Snap.writeLBS (Aeson.encode roomId)

serverWW :: RootEnv -> WW () -> WS.ServerApp
serverWW rootEnv m pending = do
    countRef <- newIORef (0 :: Int64)
    go countRef
  where
    go countRef = do
        conn <- WS.acceptRequest pending
        connId <- atomicModifyIORef' countRef (\c -> (c, c + 1))
        decodeReq (TaggedConn conn connId) False

    decodeReq tagConn isReg = do
        reqm <- Aeson.eitherDecode <$> WS.receiveData (tagConn ^. tcConn)
        case reqm of
          Left err -> do
            sendErr tagConn $ ErrorDecodingReq err
            decodeReq tagConn isReg
          Right req -> do
            debugMsg "[{}] received request `{}'" (tagConn ^. tcTag, JSONed req)
            handleReq tagConn isReg req

    handleReq tagConn isReg req = do
        let roomId = req ^. reqRoomId
        rooms <- readMVar $ rootEnv ^. rootEnvRooms
        case rooms ^. at roomId of
          Nothing -> do
            sendErr tagConn $ RoomNotPresent roomId
            decodeReq tagConn isReg
          -- TODO right now we lock everything with the state.  We
          -- should really do this asynchronously.
          Just envMv -> do
            modifyMVar_ envMv $ execStateT (processEnv tagConn isReg req)
            decodeReq tagConn True

    processEnv :: TaggedConn -> Bool -> Req -> StateT RoomEnv IO ()
    processEnv tagConn isReg req = do
        -- Add the current connection to the list, if not registered.
        unless isReg $ roomEnvConnections %= (tagConn :)
        -- Run the monadic action.
        room <- use roomEnvRoom
        result <- liftIO $ runWW req room m
        case result of
          -- If the monadic action went wrong, just send a response with
          -- the error, without writing anything or modifying the state.
          Left err -> do
            sendJSON tagConn $ RespError err

          -- Otherwise...
          Right ((), wwst) -> do
            -- Store the patches in the database.
            liftIO $ withResource (rootEnv ^. rootEnvPGPool) $ \pgConn ->
              WWPG.patchRoom pgConn (req ^. reqRoomId) (toList $ wwst ^. wwPatches)

            -- Send the messages.
            forM_ (wwst ^. wwResps) $ \(recipient, resp) ->
              case recipient of
                This -> sendJSON tagConn resp
                All  -> do
                  conns <- use roomEnvConnections
                  conns' <- flip filterM conns $ \conn -> do
                    mbIOEx <- liftIO $ try $ sendJSON conn resp
                    return $ case mbIOEx of
                      Left (_ :: WS.ConnectionException) -> False
                      Right ()                           -> True
                  roomEnvConnections .= conns'

            -- Store the new room in the state.
            roomEnvRoom .= wwst ^. wwRoom

    sendErr tagConn = sendJSON tagConn . RespError

canApplyPatch :: PatchStory -> WW ()
canApplyPatch patchStory = do
    authenticated_
    case patchStory of
      -- TODO this should throw an error when we have automatic story closing.
      PSVotingClosed _ -> return ()
      _                -> return ()

wordwang :: WW ()
wordwang = do
    req <- use wwReq
    case req ^. reqBody of
      ReqPatch patchStory -> do
        canApplyPatch patchStory
        patchStoryAndRespond patchStory
      ReqStory -> do
        story <- use (wwRoom . rStory)
        respond This $ RespStory story
      ReqJoin -> do
        userId <- liftIO randomIO
        user   <- User <$> makeSecret
        patchRoom $ PNewUser userId user
        respond This $ RespJoin userId user
