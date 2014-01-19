module WordWang
    ( -- * Room environment
      RoomEnv(..)
    , roomEnvRoomId
    , roomEnvConnections
    , roomEnvPGPool

      -- * Root environment
    , RootEnv(..)
    , rootEnvRooms
    , rootEnvPGPool

      -- * Logic
    , wordwang

      -- * Snap\/WS\/IO actions
    , webSocketWW
    , snapWW
    , createRoom

      -- * Re-exports
    , module WordWang.Config
    , module WordWang.Objects
    , module WordWang.Messages
    , module WordWang.Monad
    ) where

import           Control.Concurrent.MVar              (MVar, modifyMVar, modifyMVar_, readMVar, newMVar)
import           Control.Exception                    (catches, Handler(Handler))
import           Control.Lens                         (makeLenses, view, use, (^.), at, (.=), (%=))
import           Control.Monad                        (filterM, unless, void)
import           Control.Monad.Reader                 (ReaderT, runReaderT)
import           Control.Monad.State                  (execStateT, runStateT, StateT)
import           Control.Monad.Trans                  (MonadIO, liftIO)
import qualified Data.Aeson                           as Aeson
import           Data.Foldable                        (forM_, toList)
import           Data.Functor                         ((<$>), (<$))
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as HashMap
import qualified Data.HashSet                         as HashSet
import           Data.IORef                           (newIORef, atomicModifyIORef', readIORef)
import           Data.Int                             (Int64)
import           Data.List                            (maximumBy)
import           Data.Ord                             (comparing)
import           Data.Pool                            (Pool, withResource)
import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple           as PG
import qualified Network.WebSockets                   as WS
import           Snap                                 (Snap)
import qualified Snap                                 as Snap

import           WordWang.Bwd                         (Bwd((:<)))
import qualified WordWang.Bwd                         as Bwd
import           WordWang.Config
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

writeJSON :: (Aeson.ToJSON a) => a -> Snap ()
writeJSON req = do
    debugMsg "[{}] sending response `{}'" ("SNAP" :: T.Text, JSONed req)
    Snap.writeLBS $ Aeson.encode req

------------------------------------------------------------------------
-- Room environment

data RoomEnv = RoomEnv
    { _roomEnvRoomId      :: !RoomId
    , _roomEnvConnections :: ![TaggedConn]
    , _roomEnvPGPool      :: !(Pool PG.Connection)
    }

data RootEnv = RootEnv
    { _rootEnvRooms  :: MVar (HashMap RoomId (MVar RoomEnv))
    , _rootEnvPGPool :: !(Pool PG.Connection)
    }

makeLenses ''RoomEnv
makeLenses ''RootEnv

------------------------------------------------------------------------
-- WW actions

authenticated :: ReaderT Req WW UserId
authenticated = do
    mbAuth <- view reqAuth
    case mbAuth of
      Nothing -> do
        terminate NoCredentials
      Just auth -> do
        let userId = auth ^. reqAuthUserId
        mbUser <- lookupUser userId
        case mbUser of
          Just user | auth ^. reqAuthSecret == user ^. uSecret ->
            return userId
          _ ->
            terminate InvalidCredentials

closeVoting :: MonadWW m => m ()
closeVoting = do
    story <- viewStory vObj
    case HashMap.elems (story ^. sCandidates) of
      [] -> do
        -- TODO should we return an error here?
        return ()
      cands@(_:_) -> do
        let cand  = maximumBy (comparing (HashSet.size . _cVotes)) cands
            block = cand ^. cBlock
        void $ patchStory [PSVotingClosed block]

-- | Main logic.
wordwang :: ReaderT Req WW ()
wordwang = do
    body <- view reqBody
    case body of
      ReqCandidate block -> do
        userId <- authenticated
        void $ patchStory [PSCandidate userId $ candidate userId block]
      -- TODO make it so that only "administrators" can do this.
      ReqCloseVoting -> do
        closeVoting
      ReqVote candidateId -> do
        userId <- authenticated
        void $ patchStory [PSVote candidateId userId]
      ReqStory -> do
        story <- viewStory id
        respond This $ RespStory story
      ReqJoin -> do
        (userId, user) <- createUser
        respond This $ RespJoin userId user

------------------------------------------------------------------------
-- Room setup

addRoomEnv :: RoomId -> RootEnv -> IO (MVar (RoomEnv))
addRoomEnv roomId rootEnv = modifyMVar (rootEnv ^. rootEnvRooms) $ \rooms ->
    case rooms ^. at roomId of
      Just roomEnvMv -> do
        debugMsg "Room {} already present, not adding to RootEnv."
          (Only (JSONed roomId))
        return (rooms, roomEnvMv)
      Nothing -> do
        debugMsg "Adding room {} to RootEnv." (Only (JSONed roomId))
        let roomEnv = RoomEnv
              { _roomEnvRoomId      = roomId
              , _roomEnvConnections = []
              , _roomEnvPGPool      = rootEnv ^. rootEnvPGPool
              }
        roomEnvMv <- newMVar roomEnv
        return (HashMap.insert roomId roomEnvMv rooms, roomEnvMv)

createRoom :: RootEnv -> Snap ()
createRoom rootEnv = do
    -- Add the room and story to the DB
    roomId <- liftIO $ withResource (rootEnv ^. rootEnvPGPool) $ \conn ->
      WWPG.createRoom conn

    -- Add the room the the mvar
    void $ liftIO $ addRoomEnv roomId rootEnv

    -- Send response
    Snap.modifyResponse $
        Snap.setResponseCode 200 . Snap.setContentType "text/json"
    Snap.writeLBS $ Aeson.encode roomId

------------------------------------------------------------------------
-- Executing WW actions.

executeWW :: (Resp -> IO ())
          -- ^ Actions that sends a response to the "current" client.
          -> WW ()
          -> StateT RoomEnv IO ()
executeWW sendThisResp m = do
    -- Run the monadic action.
    roomId <- use roomEnvRoomId
    pgPool <- use roomEnvPGPool
    result <- liftIO $ runWW roomId pgPool m
    case result of
      -- If the monadic action went wrong, just send a response with
      -- the error, without writing anything or modifying the state.
      Left err -> do
        liftIO $ sendThisResp $ RespError err

      -- Otherwise...
      Right ((), wwst) -> do
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

        return ()

lookupRoomEnv :: RoomId -> RootEnv -> IO (Maybe (MVar RoomEnv))
lookupRoomEnv roomId rootEnv = do
    rooms <- readMVar $ rootEnv ^. rootEnvRooms
    case HashMap.lookup roomId rooms of
      Just roomEnvMv -> do
        return $ Just roomEnvMv
      Nothing -> do
        roomExists <- withResource (rootEnv ^. rootEnvPGPool) $ \pgConn ->
          WWPG.exists pgConn roomId
        if roomExists
          then Just <$> addRoomEnv roomId rootEnv
          else return Nothing

snapWW :: RootEnv -> ReaderT Req WW () -> Snap ()
snapWW rootEnv m = do
    mbReq <- Aeson.eitherDecode <$> Snap.readRequestBody (1 * 1024 * 1024)
    case mbReq of
      Left err -> do
        finishWithErr $ ErrorDecodingReq err
      Right req -> do
        debugMsg "[{}] received request `{}'" ("SNAP" :: T.Text, JSONed req)
        let roomId = req ^. reqRoomId
        mbRoomEnvMv <- liftIO $ lookupRoomEnv roomId rootEnv
        case mbRoomEnvMv of
          -- Send an error if the room is not present.
          Nothing -> do
            finishWithErr $ RoomNotPresent roomId

          Just envMv -> do
            resps <- liftIO $ modifyMVar envMv $ \env -> do
              (resps, env') <- flip runStateT env $ do
                respsRef <- liftIO $ newIORef Bwd.B0
                let addResp resp = atomicModifyIORef' respsRef $ \resps ->
                      ((resps :< resp), ())
                executeWW addResp $ runReaderT m req
                liftIO $ readIORef respsRef
              return (env', resps)
            writeJSON $ toList resps
  where
    finishWithErr = writeJSON . (:[]) . RespError

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

          Just envMv -> do
            modifyMVar_ envMv $ \env -> flip execStateT env $ do
              -- Add the current connection to the list, if not registered.
              unless isReg $ roomEnvConnections %= (tagConn :)

              -- Do what you're told
              executeWW (sendJSON tagConn) $ runReaderT m req

              return ()

            -- Loop
            decodeReq tagConn True

    sendErr tagConn = sendJSON tagConn . RespError
