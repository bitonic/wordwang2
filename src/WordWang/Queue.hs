-- TODO write something faster than a Chan
module WordWang.Queue (Queue, newQueue, writeQueue, flushQueue) where

import           Data.Functor ((<$>))
import           Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)


newtype Queue a = Queue (Chan a)

-- | Build and returns a new instance of 'Queue'
newQueue :: IO (Queue a)
newQueue = Queue <$> newChan

-- | Write a value to a 'Queue'.
writeQueue :: Queue a -> a -> IO ()
writeQueue (Queue chan) x = writeChan chan x

flushQueue :: Queue a -> IO [a]
flushQueue (Queue chan) = return <$> readChan chan
