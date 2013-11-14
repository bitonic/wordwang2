module WordWang.Worker
     ( Name
     , Worker(..)
     , WorkerNotRestarting
     , Ref
     , Send
     , run
     , send
     , kill
     ) where

import           Control.Concurrent (ThreadId, killThread, forkIO)
import           Control.Exception (Exception, throwIO, catches, Handler(..), AsyncException, SomeException)
import           Data.Typeable (Typeable)

import           Data.Text (Text)

import           WordWang.Utils (infoMsg, Only(..))
import           WordWang.Queue (Queue)
import qualified WordWang.Queue as Queue

type Name = Text

data Worker st a = Worker
    { workerName    :: Name
    , workerStart   :: IO st
    , workerRestart :: st -> SomeException -> IO (Either Text st)
    , workerProcess :: st -> [a] -> IO st
    }

data WorkerNotRestarting = WorkerNotRestarting Name Text
    deriving (Show, Typeable)

instance Exception WorkerNotRestarting

data Ref a = Ref
    { refThreadId :: ThreadId
    , refQueue    :: Queue a
    }

type Send a = a -> IO ()

run :: (Send a -> Worker st a) -> IO (Ref a)
run f = do
    queue <- Queue.new
    let Worker name start restart process = f (Queue.write queue)
    st <- start
    tid <- forkIO (go name restart process queue st)
    return (Ref tid queue)
  where
    go name restart process queue st = do
        xs <- Queue.flush queue
        st' <- catches (process st xs)
                   [ Handler $ \(e :: AsyncException) -> throwIO e
                   , Handler $ \(e :: SomeException) -> do
                          infoMsg "Trying to restart worker {}" (Only name)
                          either (throwIO . WorkerNotRestarting name) return =<<
                              restart st e
                   ]
        go name restart process queue st'

send :: Ref a -> Send a
send ref = Queue.write (refQueue ref)

kill :: Ref a -> IO ()
kill = killThread . refThreadId
