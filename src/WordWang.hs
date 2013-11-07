-- TODO track WS connections requests in the logging
module WordWang
    ( module WordWang.Messages
    , module WordWang.Monad
    , module WordWang.Objects
    , Stories
    , Connections

    , serverWWT
    , wordwang
    ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception (catch)
import           Control.Monad (forever, filterM, void)
import           Data.Functor ((<$>), (<$))
import           Data.List (maximumBy)
import           Data.Ord (comparing)

import           Control.Monad.Trans (liftIO)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text

import           Control.Lens
import           Crypto.Random (newGenIO, genBytes)
import           Crypto.Random.DRBG (HashDRBG)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64.URL as Base64.URL
import qualified Network.WebSockets as WS

import           WordWang.Messages
import           WordWang.Monad
import           WordWang.Objects
import           WordWang.Utils
import           WordWang.Queue (Queue)
import qualified WordWang.Queue as Queue
import           WordWang.Incremental (Incremental)
import qualified WordWang.Incremental as Incre

type Stories =
    MVar (HashMap StoryId (MVar Story, Queue RespBody, MVar Incremental))
type Connections = MVar [WS.Connection]

internalError :: Text -> WWT IO a
internalError = terminate . RespError . InternalError

makeSecret :: WWT IO UserSecret
makeSecret = do
    gen <- liftIO (newGenIO :: IO HashDRBG)
    case genBytes 15 gen of
        Left err -> internalError (Text.pack (show err))
        Right (bs, _) -> return (Base64.URL.encode bs)

modifyStory :: (Story -> IO (Story, a)) -> WWT IO a
modifyStory f = do
    storyMv <- view wwStory
    liftIO $ modifyMVar storyMv f

modifyStory_ :: (Story -> IO Story) -> WWT IO ()
modifyStory_ f = modifyStory $ \story -> (, ()) <$> f story

modifyStory' :: (Story -> (Story, a)) -> WWT IO a
modifyStory' f = modifyStory (return . f)

-- modifyStory'_ :: (Story -> Story) -> WWT IO ()
-- modifyStory'_ f = modifyStory_ (return . f)

viewStory :: WWT IO Story
viewStory = liftIO . readMVar =<< view wwStory

-- TODO the Incremental stuff relies on the interplay between the
-- 'putMVar' and 'takeMVar'.  It would be better for this game to be
-- self-contained in some module to make it less fragile.

startIncre :: WWT IO (IO ())
startIncre = do
    increMv <- view wwIncre
    storyMv <- view wwStory
    resp <- respondInIO
    return $ do
        incre <- Incre.start startT halveMaybe
        putMVar increMv incre
        void $ forkIO $
            Incre.wait incre >> takeMVar increMv >> closeVoting storyMv resp
  where
    startT = 10000000

    halveMaybe n | n >= startT `div` 2 = Just (n `div` 2)
    halveMaybe _ = Nothing

    closeVoting storyMv resp = modifyMVar_ storyMv $ \story ->
        case HashMap.elems (story^.storyCandidates) of
            [] -> return story -- TODO should we return an error?
            cands@(_:_) -> do
                let cand  = maximumBy (comparing (HashSet.size . _candVotes))
                                      cands
                    block = cand^.candBlock
                resp (respToAll (RespVotingClosed block))
                let story' = story & storyCandidates .~ HashMap.empty
                return (story' & storyBlocks %~ (++ [block]))


bumpIncre :: WWT IO (IO ())
bumpIncre = do
    increMv <- view wwIncre
    return (Incre.bump =<< readMVar increMv)

authenticated :: WWT IO UserId
authenticated = do
    authM <- view (wwReq . reqAuth)
    case authM of
        Nothing -> terminate (RespError NoCredentials)
        Just auth -> do
            story <- viewStory
            let uid = auth^.reqAuthUser
            maybe (terminate (RespError InvalidCredentials))
                  (const (return uid))
                  (story^.storyUsers.at uid)

queueWorker :: Connections -> Queue RespBody -> IO a
queueWorker connsMv queue = forever $ do
    msgs <- Queue.flush queue
    modifyMVar_ connsMv $ \conns -> flip filterM conns $ \conn ->
        (True <$ mapM_ (sendJSON conn) msgs) `catch`
        \(_ :: WS.ConnectionException) -> return False

serverWWT :: Connections -> Stories -> WWT IO ()
          -> WS.ServerApp
serverWWT connsMv storiesMv m pending = do
    conn <- WS.acceptRequest pending
    modifyMVar_ connsMv (return . (conn :))
    forever (go conn)
  where
    go conn = do
        reqm <- Aeson.eitherDecode <$> WS.receiveData conn
        case reqm of
            Left err ->
                sendErr conn (ErrorDecodingReq (Text.pack err))
            Right (req :: Req) -> do
                debugMsg "received request `{}'" (Only (Shown req))
                handleReq conn req

    handleReq conn (_reqBody -> ReqCreate) = do
        sid <- modifyMVar storiesMv $ \stories -> do
            story <- emptyStory
            storyMv <- newMVar story
            let sid = story^.storyId
            queue <- Queue.new
            -- TODO do something with this
            queueTid <- forkIO (queueWorker connsMv queue)
            increMv <- newEmptyMVar
            return (stories & at sid ?~ (storyMv, queue, increMv), sid)
        sendJSON conn (RespCreated sid)
    handleReq conn req = case req^.reqStory of
        Nothing -> sendErr conn NoStory
        Just sid -> do
            stories <- readMVar storiesMv
            case stories ^. at sid of
                Nothing -> sendErr conn NoStory
                Just (story, queue, increMv) -> do
                    let wwState = WWState{ _wwReq   = req
                                         , _wwStory = story
                                         , _wwQueue = queue
                                         , _wwIncre = increMv
                                         , _wwConn  = conn
                                         }
                    res <- runWWT wwState m
                    case res of
                        Left err -> sendJSON conn err
                        Right _  -> return ()

    sendErr conn err = sendJSON conn (RespError err)

wordwang :: WWT IO ()
wordwang = do
    req <- view wwReq
    case req^.reqBody of
        ReqCreate ->
            internalError "wordwang: received ReqCreate"
        ReqStory -> respond . respToThis . respStory =<< viewStory
        ReqJoin -> do
            -- TODO should we check if the user is already authenticated?
            user <- liftIO . newUser =<< makeSecret
            modifyStory' $ \story ->
                let story' = story & storyUsers.at (user^.userId) ?~ user
                in  (story', story')
            respond (respToAll (RespUser (user^.userId)))
            respond (respToThis (RespJoined (user^.userId) (user^.userSecret)))
        ReqCandidate body -> do
            uid <- authenticated
            let cand = candidate uid body
            resp <- respondInIO
            startIncre_ <- startIncre
            bumpIncre_ <- bumpIncre
            modifyStory_ $ \story -> do
                case story^.storyCandidates^.at uid of
                    Just _ -> return story
                    Nothing -> do
                        resp (respToAll (RespCandidate cand))
                        if (HashMap.size (story^.storyCandidates) == 0)
                            then startIncre_ else bumpIncre_
                        return (story & storyCandidates.at uid ?~ cand)
        ReqVote candUid -> do
            voteUid <- authenticated
            resp <- respondInIO
            modifyStory_ $ \story -> case story^.storyCandidates^.at candUid of
                Just cand | not (HashSet.member voteUid (cand^.candVotes)) -> do
                    resp (respToAll (RespVote candUid voteUid))
                    let cand' = cand & candVotes %~ HashSet.insert voteUid
                    return (story & storyCandidates.at candUid ?~ cand')
                _ -> return story
