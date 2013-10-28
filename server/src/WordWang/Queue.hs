module WordWang.Queue where

import           Control.Applicative ((<*>))
import           Control.Monad (void)
import           Data.Functor ((<$>))

import           Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, modifyMVar_, tryPutMVar, withMVar, takeMVar, modifyMVar)

data Queue a = Queue
    { queueQueue  :: MVar [a]
    , queueSignal :: MVar ()
    , queueLock   :: MVar ()
    }

-- | Build and returns a new instance of 'Queue'
newQueue :: IO (Queue a)
newQueue = Queue <$> newMVar [] <*> newEmptyMVar <*> newMVar ()

-- | Write a value to a 'Queue'.
writeQueue :: Queue a -> a -> IO ()
writeQueue (Queue queue signal _lock) x = do
    modifyMVar_ queue (return . (x :))
    void (tryPutMVar signal ())

flushQueue :: Queue a -> IO [a]
flushQueue (Queue queue signal lock) = withMVar lock $ \_ -> do
    takeMVar signal
    modifyMVar queue (\msgs -> return ([], reverse msgs))
