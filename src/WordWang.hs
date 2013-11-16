-- TODO track WS connections requests in the logging
-- TODO check the various forkIOs and make sure that those threads won't
--      be left dangling in bad ways
module WordWang
    ( module WordWang.Messages
    , module WordWang.Monad
    , module WordWang.Objects

    , Stories
    , createStory
    , serverWW
    , wordwang
    ) where

import           Control.Applicative ((<*>))
import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import           Control.Exception (catch)
import           Control.Monad (filterM, unless, when)
import           Data.Foldable (toList)
import           Data.Functor ((<$>), (<$))
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

import           WordWang.Messages
import           WordWang.Monad
import           WordWang.Objects
import           WordWang.Utils
import           WordWang.Bwd
import           WordWang.Incremental (Incremental)
import qualified WordWang.Incremental as Incre
import           WordWang.Worker (Worker(..))
import qualified WordWang.Worker as Worker

type StoryThings = ( MVar Story
                   , MVar [WS.Connection]
                   , Worker.Ref (Resp WS.Connection)
                   , Worker.Ref IncreWorkerMsg
                   )

type Stories = MVar (HashMap StoryId StoryThings)

internalError :: Text -> WW a
internalError = terminate . InternalError

makeSecret :: WW UserSecret
makeSecret = do
    gen <- liftIO (newGenIO :: IO HashDRBG)
    case genBytes 15 gen of
        Left err -> internalError (Text.pack (show err))
        Right (bs, _) -> return (Base64.URL.encode bs)

data IncreWorkerMsg = Done | Bump

increWorker :: MVar Story -> Worker.Ref (Resp WS.Connection)
            -> IO (Worker.Send IncreWorkerMsg -> Worker (Maybe Incremental) IncreWorkerMsg)
increWorker storyMv respWorker = do
    sid <- _storyId <$> readMVar storyMv
    return $ \send -> Worker
        { workerName    = Text.pack (show sid ++ "-incre")
        , workerStart   = return Nothing
        , workerRestart = \_ _ -> return (Left "increWorker: shouldn't terminate")
        , workerProcess = process send
        }
  where
    startT = 10000000

    halveMaybe n | n >= 500000 = Just (n - (n `div` 3))
    halveMaybe _ = Nothing

    process _ increM [] = return increM
    process send Nothing (Bump : msgs) = do
        incre <- Incre.start startT halveMaybe
        -- TODO are we sure this terminates?
        forkIO $ do
            Incre.wait incre
            send Done
        process send (Just incre) msgs
    process send (Just incre) (Bump : msgs) = do
        Incre.bump incre
        process send (Just incre) msgs
    process send increM (Done : msgs) = do
        maybe (return ()) Incre.stop increM
        modifyMVar_ storyMv $ \story -> do
            case HashMap.elems (story^.storyCandidates) of
                [] -> error "increWorker: incremental terminated with no candidates!"
                cands@(_:_) -> do
                    let cand  = maximumBy (comparing (HashSet.size . _candVotes)) cands
                        block = cand^.candBlock
                    Worker.send respWorker (respToAll (RespVotingClosed block))
                    let story' = story & storyCandidates .~ HashMap.empty
                    return (story' & storyBlocks %~ (++ [block]))
        process send Nothing msgs

authenticated :: WW UserId
authenticated = do
    authM <- view reqAuth
    case authM of
        Nothing -> terminate NoCredentials
        Just auth -> do
            story <- use wwStory
            let uid = auth^.reqAuthUser
            maybe (terminate InvalidCredentials) (const (return uid))
                  (story^.storyUsers.at uid)

createStory :: Stories -> Snap ()
createStory storiesMv = do
    sid <- liftIO go
    Snap.modifyResponse $
        Snap.setResponseCode 200 . Snap.setContentType "text/json"
    Snap.writeLBS (Aeson.encode sid)
  where
    go = modifyMVar storiesMv $ \stories -> do
        sid <- randomIO
        storyMv <- newMVar (emptyStory sid)
        connsMv <- newMVar []
        respsWRef <- Worker.run $ \_ -> Worker
            { workerName    = Text.pack (show sid ++ "-resps")
            , workerStart   = return ()
            , workerRestart = \() _ -> return (Right ())
            , workerProcess = \() -> process connsMv
            }
        increWRef <- Worker.run =<< increWorker storyMv respsWRef
        return (HashMap.insert sid (storyMv, connsMv, respsWRef, increWRef) stories, sid)

    process connsMv resps = do
        let (forAll, forThis) =
                flip eitherUnzip (toList resps) $ \resp ->
                case resp^.respRecipients of
                    All       -> Left  (resp^.respBody)
                    This conn -> Right (conn, resp^.respBody)
        modifyMVar_ connsMv $ \conns -> flip filterM conns $ \conn' ->
            ignoreClosed (mapM_ (sendJSON conn') forAll)
        mapM_ (ignoreClosed . uncurry sendJSON) forThis

    ignoreClosed m =
        (True <$ m) `catch` \(_ :: WS.ConnectionException) -> return False

serverWW :: Stories -> WW () -> WS.ServerApp
serverWW storiesMv m pending = do
    conn <- WS.acceptRequest pending
    go conn False
  where
    go conn isReg = do
        reqm <- Aeson.eitherDecode <$> WS.receiveData conn
        case reqm of
            Left err -> do
                sendErr conn (ErrorDecodingReq (Text.pack err))
                go conn isReg
            Right (req :: Req) -> do
                debugMsg "received request `{}'" (Only (JSONP req))
                handleReq conn isReg req

    handleReq conn isReg req = do
        let sid = req^.reqStory
        stories <- readMVar storiesMv
        case stories ^. at sid of
            Nothing -> do
                sendErr conn NoStory
                go conn isReg
            Just (storyMv, connsMv, respsWorker, incrWorker) -> do
                unless isReg (modifyMVar_ connsMv (return . (conn :)))
                modifyMVar_ storyMv $ \story -> do
                    (res, wwst) <- runWW req story m
                    let resps0 = wwst^.wwResps
                        resps1 = either ((resps0 :<) . respToThis . RespError)
                                        (const resps0) res
                        resps2 = (respRecipients %~ fmap (const conn)) <$> resps1
                    mapM_ (Worker.send respsWorker) (toList resps2)
                    when (wwst^.wwBump) (Worker.send incrWorker Bump)
                    return (wwst^.wwStory)
                go conn True

    sendErr conn err = sendJSON conn (RespError err)

wordwang :: WW ()
wordwang = do
    req <- ask
    case req^.reqBody of
        ReqStory -> respond . respToThis . respStory =<< use wwStory
        ReqJoin -> do
            -- TODO should we check if the user is already authenticated?
            user <- User <$> liftIO randomIO <*> makeSecret
            wwStory %= (storyUsers.at (user^.userId) ?~ user)
            respond (respToAll (RespUser (user^.userId)))
            respond (respToThis (RespJoined (user^.userId) (user^.userSecret)))
        ReqCandidate body -> do
            uid <- authenticated
            let cand = candidate uid body
            candsM <- use (wwStory . storyCandidates . at uid)
            case candsM of
                Just _ -> return ()
                Nothing -> do
                    wwBump .= True
                    respond (respToAll (RespCandidate cand))
                    wwStory %= (storyCandidates.at uid ?~ cand)
        ReqVote candUid -> do
            voteUid <- authenticated
            candM <- use (wwStory . storyCandidates . at candUid)
            case candM of
                Just cand | not (HashSet.member voteUid (cand^.candVotes)) -> do
                    wwBump .= True
                    respond (respToAll (RespVote candUid voteUid))
                    let cand' = cand & candVotes %~ HashSet.insert voteUid
                    wwStory %= (storyCandidates.at candUid ?~ cand')
                _ -> return ()
        -- TODO for debugging, remove soon
        ReqCloseVoting -> do
            authenticated
            story <- use wwStory
            case HashMap.elems (story^.storyCandidates) of
                [] -> return () -- TODO should we return an error?
                cands@(_:_) -> do
                    let cand  = maximumBy (comparing (HashSet.size . _candVotes))
                                          cands
                        block = cand^.candBlock
                    respond (respToAll (RespVotingClosed block))
                    wwStory %= (storyCandidates .~ HashMap.empty)
                    wwStory %= (storyBlocks %~ (++ [block]))
