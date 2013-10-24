{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WordWang.Monad
    ( WWState
    , wwConnection
    , wwStory
    , WW
    , runWW
    , terminate
    ) where

import           Control.Applicative (Applicative)
import           Control.Concurrent (MVar)

import           Control.Monad.Reader (ReaderT(..), MonadReader)
import           Control.Monad.Trans (MonadIO)
import           Control.Monad.Trans.Either (EitherT(..))

import           Control.Lens (makeLenses)
import qualified Network.WebSockets as WS

import           WordWang.Messages
import           WordWang.Objects

data WWState = WWState
    { _wwConnection :: WS.Connection
    , _wwStory      :: MVar Story
    }

makeLenses ''WWState

newtype WW a = WW {unWW :: EitherT Resp (ReaderT WWState IO) a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader WWState)

runWW :: WWState
      -> (Resp -> ReaderT WWState IO a)
      -> WW a
      -> IO a
runWW state handle ww = flip runReaderT state $ do
    res <- runEitherT (unWW ww)
    case res of
        Left err -> handle err
        Right x  -> return x

terminate :: Resp -> WW a
terminate = WW . EitherT . return . Left
