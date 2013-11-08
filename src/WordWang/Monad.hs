{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WordWang.Monad
    ( -- * State
      WWState(..)
    , wwStory
    , wwResps
    , wwRG
    , wwDRBG

      -- * The monad
    , WW
    , runWW

      -- * Operations
    , terminate
    , respond
    ) where

import           Control.Applicative (Applicative)

import           Control.Monad.Reader (ReaderT(..), MonadReader)
import           Control.Monad.State (State, runState, MonadState)
import           Control.Monad.Trans.Either (EitherT(..))
import           System.Random (StdGen, newStdGen)

import           Control.Lens
import           Crypto.Random (newGenIO)
import           Crypto.Random.DRBG (HashDRBG)

import           WordWang.Messages
import           WordWang.Objects
import           WordWang.Bwd

data WWState = WWState
    { _wwStory :: Story
    , _wwResps :: Bwd Resp
    , _wwRG    :: StdGen
    , _wwDRBG  :: HashDRBG
    }

makeLenses ''WWState

newtype WW a = WW {unWW :: EitherT RespBody (ReaderT Req (State WWState)) a}
    deriving (Functor, Applicative, Monad, MonadState WWState, MonadReader Req)

runWW :: Req -> Story -> WW a -> IO (Either RespBody a, WWState)
runWW req story m = do
    rg <- newStdGen
    drbg <- newGenIO
    let wwst = WWState story B0 rg drbg
    return . flip runState wwst . flip runReaderT req . runEitherT . unWW $ m

terminate :: RespBody -> WW a
terminate = WW . EitherT . return . Left

respond :: Resp -> WW ()
respond resp = wwResps %= (:< resp)
