module WordWang.Concurrent
    ( supervise
    ) where

import           Control.Concurrent                    (ThreadId, forkIO, throwTo, myThreadId)
import           Control.Exception                     (mask, catches, SomeException, Handler(Handler), AsyncException, throwIO)

supervise :: IO () -> IO ThreadId
supervise m = mask $ \restore -> do
    tid <- myThreadId
    forkIO $
      restore m `catches` [ Handler $ \(e :: AsyncException) -> throwIO e
                          , Handler $ \(e :: SomeException)  -> throwTo tid e
                          ]
