{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WordWang.Monad
    ( -- * State
      WWState(..)
    , wwStory
    , wwResps

      -- * The monad
    , WW
    , runWW

      -- * Operations
    , terminate
    , respond
    ) where

import           Control.Applicative (Applicative)

import           Control.Monad.Reader (ReaderT(..), MonadReader)
import           Control.Monad.State (StateT, runStateT, MonadState)
import           Control.Monad.Trans (MonadIO)
import           Control.Monad.Trans.Either (EitherT(..))

import           Control.Lens

import           WordWang.Messages
import           WordWang.Objects
import           WordWang.Bwd

data WWState = WWState
    { _wwStory :: Story
    , _wwResps :: Bwd (Resp ())
    }

makeLenses ''WWState

newtype WW a =
    WW {unWW :: EitherT RespError (ReaderT Req (StateT WWState IO)) a}
    deriving (Functor, Applicative, Monad, MonadState WWState, MonadReader Req, MonadIO)

runWW :: forall a. Req -> Story -> WW a -> IO (Either RespError a, WWState)
runWW req story m = do
    let wwst = WWState{ _wwStory = story
                      , _wwResps = B0
                      }
    flip runStateT wwst . flip runReaderT req . runEitherT . unWW $ m

terminate :: RespError -> WW a
terminate = WW . EitherT . return . Left

respond :: Resp () -> WW ()
respond resp = wwResps %= (:< resp)
