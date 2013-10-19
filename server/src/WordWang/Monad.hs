module WordWang.Monad
    ( WW
    , runWW
    , terminate
    ) where

import           Control.Monad.Trans.Either (EitherT(..))
import           Control.Monad.Trans.State (StateT, runStateT)

import           WordWang.Messages
import           WordWang.Objects

newtype WW a = WW {unWW :: EitherT ServerMsg (StateT Story IO) a}

runWW :: Story -> (ServerMsg -> StateT Story IO a) -> WW a -> IO (a, Story)
runWW story handle ww = flip runStateT story $ do
    res <- runEitherT (unWW ww)
    case res of
        Left err -> handle err
        Right x  -> return x

terminate :: ServerMsg -> WW a
terminate = WW . EitherT . return . Left
