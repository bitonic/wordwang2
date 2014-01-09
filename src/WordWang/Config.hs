{-# LANGUAGE Trustworthy #-}
module WordWang.Config
    ( LogLevel(..)
    , Config(..)
    , confPersist
    , confLogLevel
    , confLogFunction
    , defaultConfig
    , Persist(..)

    , initConfig
    , getConfig
    ) where

import           Control.Concurrent.MVar               (MVar, newEmptyMVar, readMVar, isEmptyMVar, tryPutMVar)
import           Control.Lens                          (makeLenses)
import           Control.Monad                         (unless)
import           Control.Monad.Trans                   (MonadIO(..))
import qualified Data.Text.Lazy                        as TL
import qualified Data.Text.Lazy.IO                     as TL
import qualified Database.PostgreSQL.Simple            as PG
import           System.IO                             (stderr)
import           System.IO.Unsafe                      (unsafePerformIO)

data LogLevel = DEBUG | INFO | ERROR
    deriving (Show, Eq, Ord)

data Config = Config
    { _confPersist     :: Persist
    , _confLogLevel    :: LogLevel
    , _confLogFunction :: TL.Text -> IO ()
    }

defaultConfig :: Config
defaultConfig = Config NoPersist DEBUG (TL.hPutStrLn stderr)

data Persist
    = NoPersist
    | PGPersist PG.ConnectInfo

{-# NOINLINE configVar #-}
configVar :: MVar Config
configVar = unsafePerformIO newEmptyMVar

initConfig :: MonadIO m => Config -> m ()
initConfig config = liftIO $ do
    wasEmpty <- tryPutMVar configVar config
    unless wasEmpty $ error "WordWang.Config.initConfig: already initialised"

getConfig :: MonadIO m => m Config
getConfig = liftIO $ do
    initialised <- isEmptyMVar configVar
    if initialised
      then error "WordWang.Config.getConfig: not initialised"
      else readMVar configVar

makeLenses ''Config
