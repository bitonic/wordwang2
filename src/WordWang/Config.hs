module WordWang.Config
    ( LogLevel(..)
    , Config(..)
    , cPersist
    , cLogLevel
    , defaultConfig
    , Persist(..)

    , initConfig
    , getConfig
    ) where

import           System.IO.Unsafe (unsafePerformIO)
import           Control.Concurrent (MVar, newEmptyMVar, readMVar, isEmptyMVar, putMVar)

import           Control.Monad.Trans (MonadIO(..))

import           Control.Lens (makeLenses)
import qualified Database.PostgreSQL.Simple as PG

data LogLevel = DEBUG | INFO | ERROR
    deriving (Show, Eq, Ord)

data Config = Config
    { _cPersist  :: Persist
    , _cLogLevel :: LogLevel
    }

defaultConfig :: Config
defaultConfig = Config NoPersist DEBUG

data Persist
    = NoPersist
    | PGPersist PG.ConnectInfo

{-# NOINLINE configVar #-}
configVar :: MVar Config
configVar = unsafePerformIO newEmptyMVar

initConfig :: MonadIO m => Config -> m ()
initConfig = liftIO . putMVar configVar

getConfig :: MonadIO m => m Config
getConfig = liftIO $ do
    b <- isEmptyMVar configVar
    if b then error "WordWang.Config.getConfig: not initialised"
         else readMVar configVar

makeLenses ''Config
