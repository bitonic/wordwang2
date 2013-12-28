-- TODO track WS connections requests in the logging
-- TODO check the various forkIOs and make sure that those threads won't
--      be left dangling in bad ways
module WordWang
    ( module WordWang.Messages
    , module WordWang.Monad
    , module WordWang.Objects

    , Stories
    , createStory'
    , createStory
    , serverWW
--    , wordwang
    ) where

import           Control.Applicative ((<*>))
import           Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, readMVar)
import           Control.Exception (catch)
import           Control.Monad (filterM, when, void, unless)
import           Data.Foldable (toList)
import           Data.Functor ((<$>), (<$))
import           Data.IORef (newIORef, modifyIORef', atomicModifyIORef')
import           Data.Int (Int64)
import           Data.List (maximumBy)
import           Data.Ord (comparing)

import           Control.Monad.Reader (ask)
import           Control.Monad.Trans (liftIO)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import           System.Random (randomIO)

import           Control.Lens
import           Crypto.Random (genBytes, newGenIO)
import           Crypto.Random.DRBG (HashDRBG)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64.URL as Base64.URL
import qualified Network.WebSockets as WS
import           Snap (Snap)
import qualified Snap as Snap

import           WordWang.Bwd
import           WordWang.Config
import           WordWang.Incremental (Incremental)
import qualified WordWang.Incremental as Incre
import           WordWang.Messages
import           WordWang.Monad
import           WordWang.Objects
import           WordWang.State
import           WordWang.Utils

data StoryEnv = StoryEnv
    { _seStoryState  :: StoryState
    , _seConnections :: [TaggedConn]
    , _seIncremental :: Incremental
    }

makeLenses ''StoryEnv

type Stories = MVar (HashMap StoryId (MVar StoryEnv))

internalError :: String -> WW req a
internalError = terminate . InternalError

makeSecret :: WW req UserSecret
makeSecret = do
    gen <- liftIO (newGenIO :: IO HashDRBG)
    case genBytes 15 gen of
      Left err      -> internalError $ show err
      Right (bs, _) -> return $ Base64.URL.encode bs

-- data IncreWorkerMsg = Done | Bump

-- increWorker :: MVar Story -> Worker.Ref RespWorkerMsg
--             -> IO (Worker.Send IncreWorkerMsg -> Worker IncreWorkerMsg)
-- increWorker storyMv respWRef = do
--     sid <- _storyId <$> readMVar storyMv
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

authenticated :: WW req UserId
authenticated = do
    mbAuth <- use (wwReq . reqAuth)
    case mbAuth of
      Nothing -> do
        terminate NoCredentials
      Just auth -> do
        users <- use (wwStoryState . ssUsers)
        let uid = auth ^. reqAuthUser
        case users ^. at uid of
          Just user | auth ^. reqAuthSecret == user ^. uSecret ->
            return uid
          _ ->
            terminate InvalidCredentials

createStory' :: StoryId -> Story -> Stories -> IO ()
createStory' storyId story storiesMv = modifyMVar_ storiesMv $ \stories -> do
    storyMv <- newMVar story
    return undefined

createStory :: Stories -> Snap ()
createStory storiesMv = do
    storyId <- liftIO randomIO
    liftIO $ createStory' storyId emptyStory storiesMv
    Snap.modifyResponse $
        Snap.setResponseCode 200 . Snap.setContentType "text/json"
    Snap.writeLBS (Aeson.encode storyId)

serverWW :: forall req. (Aeson.FromJSON req, Aeson.ToJSON req)
         => Stories -> WW req () -> WS.ServerApp
serverWW storiesMv m pending = do
    countRef <- newIORef (0 :: Int64)
    go countRef
  where
    go countRef = do
        conn <- WS.acceptRequest pending
        connId <- atomicModifyIORef' countRef (\c -> (c, c + 1))
        decodeReq (TaggedConn conn connId) False

    decodeReq (tagConn :: TaggedConn) isReg = do
        reqm <- Aeson.eitherDecode <$> WS.receiveData (tagConn ^. tcConn)
        case reqm of
          Left err -> do
            sendErr tagConn $ ErrorDecodingReq err
            decodeReq tagConn isReg
          Right (req :: Req req) -> do
            debugMsg "[{}] received request `{}'" (tagConn ^. tcTag, JSONP req)
            handleReq tagConn isReg req

    handleReq (tagConn :: TaggedConn) isReg req = do
        let sid = req^.reqStory
        stories <- readMVar storiesMv
        case stories ^. at sid of
          Nothing -> do
            sendErr tagConn $ StoryNotPresent sid
            decodeReq tagConn isReg
          -- TODO right now we lock everything with the state.  We
          -- should really do this asynchronously.
          Just storyEnvMv -> do
            modifyMVar_ storyEnvMv $ \storyEnv0 -> do
              -- Add the current connection to the list, if not registered.
              let storyEnv1 = if isReg
                              then storyEnv0
                              else storyEnv0 & seConnections %~ (tagConn :)
              -- Run the monadic action.
              result <- runWW req (storyEnv1 ^. seStoryState) m
              case result of
                -- If the monadic action went wrong, just send a
                -- response with the error, without writing anything or
                -- modifying the state.
                Left err -> do
                  sendJSON tagConn (RespError err :: Resp ())
                  return $ storyEnv1
                -- Otherwise...
                Right ((), wwst) -> do
                  let storyState = wwst ^. wwStoryState
                      storyEnv2  = storyEnv1 & seStoryState .~ storyState
                  -- Send the story messages to everybody.

                  -- Send the user messages to the user only.
                  
                  -- Story the story messages in the database.
                  
                  -- Return the updated story environment.
                  return storyEnv2
            -- Loop.
            decodeReq tagConn True

    sendErr tagConn err = sendJSON tagConn (RespError err :: Resp ())

-- wordwang :: WW ()
-- wordwang = do
--     req <- ask
--     case req^.reqBody of
--         ReqStory -> respond . respToThis . respStory =<< use wwStory
--         ReqJoin -> do
--             -- TODO should we check if the user is already authenticated?
--             user <- User <$> liftIO randomIO <*> makeSecret
--             wwStory %= (storyUsers.at (user^.userId) ?~ user)
--             respond (respToAll (RespUser (user^.userId)))
--             respond (respToThis (RespJoined (user^.userId) (user^.userSecret)))
--         ReqCandidate body -> do
--             uid <- authenticated
--             let cand = candidate uid body
--             candsM <- use (wwStory . storyCandidates . at uid)
--             case candsM of
--                 Just _ -> return ()
--                 Nothing -> do
--                     wwBump .= True
--                     respond (respToAll (RespCandidate cand))
--                     wwStory %= (storyCandidates.at uid ?~ cand)
--         ReqVote candUid -> do
--             voteUid <- authenticated
--             candM <- use (wwStory . storyCandidates . at candUid)
--             case candM of
--                 Just cand | not (HashSet.member voteUid (cand^.candVotes)) -> do
--                     wwBump .= True
--                     respond (respToAll (RespVote candUid voteUid))
--                     let cand' = cand & candVotes %~ HashSet.insert voteUid
--                     wwStory %= (storyCandidates.at candUid ?~ cand')
--                 _ -> return ()
--         -- TODO for debugging, remove soon
--         ReqCloseVoting -> do
--             authenticated
--             story <- use wwStory
--             case HashMap.elems (story^.storyCandidates) of
--                 [] -> return () -- TODO should we return an error?
--                 cands@(_:_) -> do
--                     let cand  = maximumBy (comparing (HashSet.size . _candVotes))
--                                           cands
--                         block = cand^.candBlock
--                     respond (respToAll (RespVotingClosed block))
--                     wwStory %= (storyCandidates .~ HashMap.empty)
--                     wwStory %= (storyBlocks %~ (++ [block]))
