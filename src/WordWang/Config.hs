{-# LANGUAGE Trustworthy #-}
module WordWang.Config
    ( LogLevel(..)
    , Config(..)
    , confLogLevel
    , confLogFunction
    , defaultConfig

    , initConfig
    , getConfig
    ) where

import           Control.Concurrent.MVar               (MVar, newEmptyMVar, readMVar, isEmptyMVar, tryPutMVar)
import           Control.Lens                          (makeLenses)
import           Control.Monad                         (unless)
import           Control.Monad.Trans                   (MonadIO(..))
import qualified Data.Text.Lazy                        as TL
import qualified Data.Text.Lazy.IO                     as TL
import           System.IO                             (stderr)
import           System.IO.Unsafe                      (unsafePerformIO)

#include "../../impossible.h"

data LogLevel = DEBUG | INFO | ERROR
    deriving (Show, Eq, Ord)

data Config = Config
    { _confLogLevel    :: LogLevel
    , _confLogFunction :: TL.Text -> IO ()
    }

defaultConfig :: Config
defaultConfig = Config DEBUG (TL.hPutStrLn stderr)

{-# NOINLINE configVar #-}
configVar :: MVar Config
configVar = unsafePerformIO newEmptyMVar

initConfig :: MonadIO m => Config -> m ()
initConfig config = liftIO $ do
    wasEmpty <- tryPutMVar configVar config
    unless wasEmpty $ ERROR("already initialised")

getConfig :: MonadIO m => m Config
getConfig = liftIO $ do
    initialised <- isEmptyMVar configVar
    if initialised
      then ERROR("not initialised")
      else readMVar configVar

makeLenses ''Config
