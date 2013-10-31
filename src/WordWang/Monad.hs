{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WordWang.Monad
    ( -- * State
      Stories
    , Connections
    , Queue
    , WWState(..)
    , wwReq
    , wwStory
    , wwConn

      -- * The monad
    , WWT
    , runWWT
    , serverWWT

      -- * Operations
    , terminate
    , respond
    , respondInIO
    ) where

import           Control.Applicative (Applicative)
import           Control.Concurrent (forkIO)
import           Control.Concurrent (modifyMVar, modifyMVar_, newMVar, readMVar)
import           Control.Concurrent.MVar (MVar)
import           Control.Exception (catch)
import           Control.Monad (forever, filterM)
import           Data.Functor ((<$>), (<$))
import           Data.Monoid ((<>))

import           Control.Monad.Reader (ReaderT(..), MonadReader)
import           Control.Monad.Trans (liftIO, MonadIO)
import           Control.Monad.Trans.Either (EitherT(..))
import           Data.HashMap.Strict (HashMap)
import qualified Data.Text as T

import           Control.Lens hiding (both)
import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS

import           WordWang.Messages
import           WordWang.Objects
import           WordWang.Queue
import           WordWang.Utils

type Stories     = MVar (HashMap StoryId (MVar Story, Queue RespBody))
type Connections = MVar [WS.Connection]

data WWState = WWState
    { _wwReq   :: Req
    , _wwStory :: MVar Story
    , _wwQueue :: Queue RespBody
    , _wwConn  :: WS.Connection
    }

makeLenses ''WWState

newtype WWT m a =
    WWT {unWWT :: EitherT RespBody (ReaderT WWState m) a}
    deriving (Functor, Applicative, Monad, MonadReader WWState, MonadIO)

runWWT :: Monad m => WWState -> WWT m a -> m (Either RespBody a)
runWWT state = flip runReaderT state . runEitherT . unWWT

queueWorker :: Connections -> Queue RespBody -> IO a
queueWorker connsMv queue = forever $ do
    msgs <- flushQueue queue
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
                sendErr conn ("error decoding request: " <> T.pack err)
            Right (req :: Req) -> do
                debugMsg "received request `{}'" (Only (Shown req))
                handleReq conn req

    handleReq conn (_reqBody -> ReqCreate) = do
        sid <- modifyMVar storiesMv $ \stories -> do
            story <- emptyStory
            storyMv <- newMVar story
            let sid = story^.storyId
            queue <- newQueue
            -- TODO do something with this
            queueTid <- forkIO (queueWorker connsMv queue)
            return (stories & at sid ?~ (storyMv, queue), sid)
        sendJSON conn (RespCreated sid)
    handleReq conn req = case req^.reqStory of
        Nothing -> sendErr conn "no story in request"
        Just sid -> do
            stories <- readMVar storiesMv
            case stories ^. at sid of
                Nothing -> sendErr conn "story not found"
                Just (story, queue) -> do
                    let wwState = WWState { _wwReq   = req
                                          , _wwStory = story
                                          , _wwQueue = queue
                                          , _wwConn  = conn
                                          }
                    res <- runWWT wwState m
                    case res of
                        Left err -> sendJSON conn err
                        Right _  -> return ()

    sendErr conn err = sendJSON conn (RespError err)

terminate :: Monad m => RespBody -> WWT m a
terminate = WWT . EitherT . return . Left

respond :: MonadIO m => Resp -> WWT m ()
respond resp = liftIO . ($ resp) =<< respondInIO

respondInIO :: MonadIO m => WWT m (Resp -> IO ())
respondInIO = do
    queue <- view wwQueue
    conn <- view wwConn
    return $ \resp -> case resp^.respRecipients of
        All -> writeQueue queue (resp^.respBody)
        -- TODO make this sending more async, since we do it while
        -- taking a MVar
        This -> sendJSON conn (resp^.respBody)
