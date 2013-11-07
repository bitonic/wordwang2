module WordWang.Queue
    ( Queue
    , new
    , write
    , flush
    ) where

import           Control.Applicative ((<*>))
import           Control.Monad (void)
import           Data.Functor ((<$>))

import           Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, modifyMVar_, tryPutMVar, withMVar, takeMVar, modifyMVar)

data Queue a = Queue (MVar [a]) (MVar ()) (MVar ())

-- | Build and returns a new instance of 'Queue'
new :: IO (Queue a)
new = Queue <$> newMVar [] <*> newEmptyMVar <*> newMVar ()

-- | Write a value to a 'Queue'.
write :: Queue a -> a -> IO ()
write (Queue queue signal _lock) x = do
    modifyMVar_ queue (return . (x :))
    void (tryPutMVar signal ())

flush :: Queue a -> IO [a]
flush (Queue queue signal lock) = withMVar lock $ \_ -> do
    takeMVar signal
    modifyMVar queue (\msgs -> return ([], reverse msgs))
