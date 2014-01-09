{-# LANGUAGE Trustworthy #-}
module WordWang.Log
    ( logMsg
    , debugMsg
    , infoMsg
    , errorMsg

      -- * Re-exports
    , Only(..)
    , Shown(..)
    ) where

import           Control.Concurrent.MVar               (MVar, newMVar, withMVar)
import           Control.Lens                          ((^.))
import           Control.Monad                         (when)
import           Control.Monad.Trans                   (MonadIO(..))
import           Data.Monoid                           ((<>))
import           Data.Text.Format                      (Format, Only(Only), Shown(Shown), format)
import           Data.Text.Format.Params               (Params)
import qualified Data.Text.Lazy                        as TL
import           System.IO.Unsafe                      (unsafePerformIO)

import           WordWang.Config

{-# NOINLINE logLock #-}
logLock :: MVar ()
logLock = unsafePerformIO $ newMVar ()

logMsg :: (MonadIO m, Params ps) => LogLevel -> Format -> ps -> m ()
logMsg pri fmt ps = do
    conf <- getConfig
    let minPri = conf ^. confLogLevel
        logf   = conf ^. confLogFunction
    when (pri >= minPri) $ liftIO $ withMVar logLock $ \_ -> logf $
      ("[" <> TL.pack (show pri) <> "] ") <> format fmt ps

debugMsg :: (MonadIO m, Params ps) => Format -> ps -> m ()
debugMsg = logMsg DEBUG

infoMsg :: (MonadIO m, Params ps) => Format -> ps -> m ()
infoMsg = logMsg INFO

errorMsg :: (MonadIO m, Params ps) => Format -> ps -> m ()
errorMsg = logMsg ERROR
