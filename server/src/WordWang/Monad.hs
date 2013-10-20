{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WordWang.Monad
    ( WW
    , runWW
    , terminate
    ) where

import           Control.Applicative (Applicative)

import           Control.Monad.Reader (ReaderT(..), MonadReader)
import           Control.Monad.State (StateT(..), MonadState)
import           Control.Monad.Trans (MonadIO)
import           Control.Monad.Trans.Either (EitherT(..))

import           WordWang.Messages
import           WordWang.Objects

newtype WW a = WW
    {unWW :: EitherT Resp (ReaderT Req (StateT Story IO)) a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Req, MonadState Story)

runWW :: Req -> Story
      -> (Resp -> ReaderT Req (StateT Story IO) a)
      -> WW a
      -> IO (a, Story)
runWW req story handle ww = flip runStateT story $ flip runReaderT req $ do
    res <- runEitherT (unWW ww)
    case res of
        Left err -> handle err
        Right x  -> return x

terminate :: Resp -> WW a
terminate = WW . EitherT . return . Left
