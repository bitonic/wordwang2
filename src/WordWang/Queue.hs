module WordWang.Queue
    ( Queue
    , new
    , write
    , flush
    ) where

import           Control.Applicative ((<*>))
import           Control.Monad (void)
import           Data.Foldable (toList)
import           Data.Functor ((<$>))

import           Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, modifyMVar_, tryPutMVar, withMVar, takeMVar, modifyMVar)

import           WordWang.Bwd

data Queue a = Queue (MVar (Bwd a)) (MVar ()) (MVar ())

-- | Build and returns a new instance of 'Queue'
new :: IO (Queue a)
new = Queue <$> newMVar B0 <*> newEmptyMVar <*> newMVar ()

-- | Write a value to a 'Queue'.
write :: Queue a -> a -> IO ()
write (Queue queue signal _lock) x = do
    modifyMVar_ queue (return . (:< x))
    void (tryPutMVar signal ())

flush :: Queue a -> IO [a]
flush (Queue queue signal lock) = withMVar lock $ \_ -> do
    takeMVar signal
    modifyMVar queue (\msgs -> return (B0, toList msgs))
