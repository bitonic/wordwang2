{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WordWang.Monad
    ( -- * State
      WWState(..)
    , wwReq
    , wwStory
    , wwConn
    , wwIncre

      -- * The monad
    , WWT
    , runWWT

      -- * Operations
    , terminate
    , respond
    , respondInIO
    ) where

import           Control.Applicative (Applicative)
import           Control.Concurrent.MVar (MVar)

import           Control.Monad.Reader (ReaderT(..), MonadReader)
import           Control.Monad.Trans (liftIO, MonadIO)
import           Control.Monad.Trans.Either (EitherT(..))

import           Control.Lens hiding (both)
import qualified Network.WebSockets as WS

import           WordWang.Messages
import           WordWang.Objects
import           WordWang.Utils
import           WordWang.Queue (Queue)
import qualified WordWang.Queue as Queue
import           WordWang.Incremental (Incremental)

data WWState = WWState
    { _wwReq   :: !Req
    , _wwStory :: !(MVar Story)
    , _wwQueue :: !(Queue RespBody)
    , _wwIncre :: !(MVar Incremental)
      -- We put the 'Incremental' in a MVar because we don't have it
      -- with empty candidates
    , _wwConn  :: !WS.Connection
    }

makeLenses ''WWState

newtype WWT m a =
    WWT {unWWT :: EitherT RespBody (ReaderT WWState m) a}
    deriving (Functor, Applicative, Monad, MonadReader WWState, MonadIO)

runWWT :: Monad m => WWState -> WWT m a -> m (Either RespBody a)
runWWT state = flip runReaderT state . runEitherT . unWWT

terminate :: Monad m => RespBody -> WWT m a
terminate = WWT . EitherT . return . Left

respond :: MonadIO m => Resp -> WWT m ()
respond resp = liftIO . ($ resp) =<< respondInIO

respondInIO :: MonadIO m => WWT m (Resp -> IO ())
respondInIO = do
    queue <- view wwQueue
    conn <- view wwConn
    return $ \resp -> case resp^.respRecipients of
        All -> Queue.write queue (resp^.respBody)
        -- TODO make this sending more async, since we do it while
        -- taking a MVar
        This -> sendJSON conn (resp^.respBody)
