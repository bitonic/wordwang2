module WordWang
    ( module WordWang.Messages
    , module WordWang.Monad
    , module WordWang.Objects
    , module WordWang.Config

    , RoomEnv(..)
    , roomEnvRoom
    , roomEnvConnections
    , roomEnvPGPool
    , roomEnvCountdown
    , roomEnvCloser
    , RootEnv(..)
    , rootEnvRooms
    , rootEnvPGPool
    , restoreRoom
    , loadAndRestoreRooms
    , addRoom
    , webSocketWW
    , wordwang
    ) where

import           Control.Concurrent                   (ThreadId, threadDelay)
import           Control.Concurrent.MVar              (MVar, modifyMVar_, readMVar, newEmptyMVar, putMVar)
import           Control.Exception                    (catches, Handler(Handler))
import           Control.Lens                         (makeLenses, view, use, (^.), at, (.=), (%=))
import           Control.Monad                        (filterM, unless, when, forever, void)
import           Control.Monad.Reader                 (ReaderT(runReaderT))
import           Control.Monad.State                  (execStateT, StateT)
import           Control.Monad.Trans                  (MonadIO(liftIO))
import           Crypto.Random                        (genBytes, newGenIO)
import           Crypto.Random.DRBG                   (HashDRBG)
import qualified Data.Aeson                           as Aeson
import qualified Data.ByteString.Base64.URL           as Base64.URL
import           Data.Foldable                        (forM_, toList)
import           Data.Functor                         ((<$>), (<$))
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as HashMap
import qualified Data.HashSet                         as HashSet
import           Data.IORef                           (newIORef, atomicModifyIORef')
import           Data.Int                             (Int64)
import           Data.List                            (maximumBy)
import           Data.Ord                             (comparing)
import           Data.Pool                            (Pool, withResource)
import qualified Database.PostgreSQL.Simple           as PG
import qualified Network.WebSockets                   as WS
import           Snap                                 (Snap)
import qualified Snap                                 as Snap
import           System.Random                        (randomIO)

import           WordWang.Concurrent
import           WordWang.Config
import           WordWang.Countdown                   (Countdown)
import qualified WordWang.Countdown                   as WWCD
import           WordWang.JSON
import           WordWang.Log
import           WordWang.Messages
import           WordWang.Monad
import           WordWang.Objects
import qualified WordWang.PostgreSQL                  as WWPG

------------------------------------------------------------------------
-- Tagged connections

data TaggedConn = TaggedConn
    { _tcConn :: WS.Connection
    , _tcTag  :: Int64
    }

makeLenses ''TaggedConn

sendJSON :: (Aeson.ToJSON a, MonadIO m) => TaggedConn -> a -> m ()
sendJSON taggedConn req = liftIO $ do
    debugMsg "[{}] sending response `{}'" (taggedConn ^. tcTag, JSONed req)
    WS.sendTextData (taggedConn ^. tcConn) (Aeson.encode req)

------------------------------------------------------------------------

data RoomEnv = RoomEnv
    { _roomEnvRoom        :: !Room
    , _roomEnvConnections :: ![TaggedConn]
    , _roomEnvPGPool      :: !(Pool PG.Connection)
    , _roomEnvCountdown   :: !(Maybe Countdown)
    , _roomEnvCloser      :: !ThreadId
    }

data RootEnv = RootEnv
    { _rootEnvRooms  :: MVar (HashMap RoomId (MVar RoomEnv))
    , _rootEnvPGPool :: !(Pool PG.Connection)
    }

makeLenses ''RoomEnv
makeLenses ''RootEnv


internalError :: MonadWW m => String -> m a
internalError = terminate . InternalError

makeSecret :: (MonadIO m, MonadWW m) => m UserSecret
makeSecret = do
    gen <- liftIO (newGenIO :: IO HashDRBG)
    case genBytes 15 gen of
      Left err      -> internalError $ show err
      Right (bs, _) -> return $ Base64.URL.encode bs

authenticated :: ReaderT Req WW UserId
authenticated = do
    mbAuth <- view reqAuth
    case mbAuth of
      Nothing -> do
        terminate NoCredentials
      Just auth -> do
        users <- viewRoom rUsers
        let userId = auth ^. reqAuthUserId
        case users ^. at userId of
          Just user | auth ^. reqAuthSecret == user ^. uSecret ->
            return userId
          _ ->
            terminate InvalidCredentials

loadAndRestoreRooms :: RootEnv -> IO ()
loadAndRestoreRooms rootEnv =
    mapM_ (\(roomId, room) -> restoreRoom roomId room rootEnv) =<<
      withResource (rootEnv ^. rootEnvPGPool) WWPG.loadRooms

restoreRoom :: RoomId -> Room -> RootEnv -> IO ()
restoreRoom roomId room rootEnv =
    modifyMVar_ (rootEnv ^. rootEnvRooms) $ \rooms -> do
      roomEnvMv <- newEmptyMVar

      -- TODO kill this when closing the room.
      closer <- supervise $ forever $ do
        -- Wait for a tenth of a second.
        threadDelay 100000
        mbCountDown <- _roomEnvCountdown <$> readMVar roomEnvMv
        case mbCountDown of
          Nothing -> do
            -- If the countdown is not there, don't do anything.
            return ()
          Just countdown -> do
            -- Otherwise wait on it, and then elect a winning block.
            debugMsg "[{}] Waiting on countdown." (Only (Shown roomId))
            WWCD.wait countdown
            debugMsg "[{}] Countdown expired, closing voting." (Only (Shown roomId))
            let sendThisResp resp =
                  error $ "WordWang.restoreRoom: " ++
                          "trying to send This response " ++ show resp
            -- Close the voting and remove the countdown
            modifyMVar_ roomEnvMv $ \roomEnv -> flip execStateT roomEnv $ do
              void $ executeWW rootEnv sendThisResp roomId $ closeVoting
              roomEnvCountdown .= Nothing

      putMVar roomEnvMv RoomEnv{ _roomEnvRoom        = room
                               , _roomEnvConnections = []
                               , _roomEnvPGPool      = rootEnv ^. rootEnvPGPool
                               , _roomEnvCountdown   = Nothing
                               , _roomEnvCloser      = closer
                               }
      return $ HashMap.insert roomId roomEnvMv rooms

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
    Snap.writeLBS $ Aeson.encode roomId

executeWW :: RootEnv -> (Resp -> IO ()) -> RoomId -> WW ()
          -> StateT RoomEnv IO Bool
executeWW rootEnv sendThisResp roomId m = do
    -- Run the monadic action.
    room <- use roomEnvRoom
    result <- liftIO $ runWW room m
    case result of
      -- If the monadic action went wrong, just send a response with
      -- the error, without writing anything or modifying the state.
      Left err -> do
        liftIO $ sendThisResp $ RespError err
        return False

      -- Otherwise...
      Right ((), wwst) -> do
        -- Store the patches in the database.
        liftIO $ withResource (rootEnv ^. rootEnvPGPool) $ \pgConn ->
          WWPG.patchRoom pgConn roomId (toList $ wwst ^. wwPatches)

        -- Send the messages.
        forM_ (wwst ^. wwResps) $ \(recipient, resp) ->
          case recipient of
            This -> do
              liftIO $ sendThisResp resp
            All  -> do
              conns <- use roomEnvConnections
              conns' <- flip filterM conns $ \conn -> do
                liftIO $ (True <$ sendJSON conn resp) `catches`
                  [ Handler $ \(_ :: WS.ConnectionException) -> do
                      return False
                  , Handler $ \(e :: IOError) -> do
                      debugMsg
                        "Closing connection {} due to IO error {}"
                        (conn ^. tcTag, show e)
                      return False
                  ]
              roomEnvConnections .= conns'

        -- Store the new room in the state.
        roomEnvRoom .= wwst ^. wwRoom

        return $ wwst ^. wwHasStoryChanged

webSocketWW :: RootEnv -> ReaderT Req WW () -> WS.ServerApp
webSocketWW rootEnv m pending = do
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

          -- Send an error if the room is not present.
          Nothing -> do
            sendErr tagConn $ RoomNotPresent roomId
            decodeReq tagConn isReg

          -- TODO right now we lock everything with the state.  We
          -- should really do this asynchronously.
          Just envMv -> do
            modifyMVar_ envMv $ \env -> flip execStateT env $ do
              -- Add the current connection to the list, if not registered.
              unless isReg $ roomEnvConnections %= (tagConn :)

              -- Do what you're told
              hasStoryChanged <-
                executeWW rootEnv (sendJSON tagConn) roomId $ runReaderT m req

              -- Bump/start countdown if necessary
              mbCountDown <- use roomEnvCountdown
              when hasStoryChanged $ case mbCountDown of
                Nothing -> do
                  countdown <- liftIO $ WWCD.start cdStart cdHalve
                  roomEnvCountdown .= Just countdown
                Just countdown -> do
                  liftIO $ WWCD.bump countdown

            -- Loop
            decodeReq tagConn True

    -- Start with a 10 seconds wait
    cdStart = 10000000
    -- Each time a bump is received, add half of the last increment.
    cdHalve n = if n < 100000 then Nothing else Just (n `div` 2)

    sendErr tagConn = sendJSON tagConn . RespError

wordwang :: ReaderT Req WW ()
wordwang = do
    body <- view reqBody
    case body of
      ReqCandidate block -> do
        userId <- authenticated
        patchStoryAndRespond $ PSCandidate userId $ candidate userId block
      -- TODO make it so that only "administrators" can do this.
      ReqCloseVoting -> do
        error "WordWang.wordwang: ReqCloseVoting not supported"
      ReqVote candidateId -> do
        userId <- authenticated
        patchStoryAndRespond $ PSVote candidateId userId
      ReqStory -> do
        story <- viewRoom rStory
        respond This $ RespStory story
      ReqJoin -> do
        userId <- liftIO randomIO
        user   <- User <$> makeSecret
        -- TODO should we record an error if the patch fails?
        wasPatched <- patchRoom $ PNewUser userId user
        when wasPatched $ respond This $ RespJoin userId user

closeVoting :: MonadWW m => m ()
closeVoting = do
    story <- viewRoom rStory
    case HashMap.elems (story ^. sCandidates) of
      [] -> do
        -- TODO should we return an error here?
        return ()
      cands@(_:_) -> do
        let cand  = maximumBy (comparing (HashSet.size . _cVotes)) cands
            block = cand ^. cBlock
        patchStoryAndRespond $ PSVotingClosed block
