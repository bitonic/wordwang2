module WordWang.Worker
     ( Name
     , Worker(..)
     , WorkerNotRestarting
     , Ref
     , Send
     , run
     , send

     , sink
     ) where

import           Control.Exception (Exception, throwIO, catch, SomeException)
import           Data.Typeable (Typeable)

import           Data.Text (Text)

import           WordWang.Utils (infoMsg, Only(..), supervise)
import           WordWang.Queue (Queue)
import qualified WordWang.Queue as Queue

type Name = Text

data Worker a = forall st. Worker
    { workerName    :: Name
    , workerStart   :: IO st
    , workerRestart :: st -> a -> SomeException -> IO (Either Text st)
    , workerReceive :: st -> a -> IO (Maybe st)
    }

data WorkerNotRestarting = WorkerNotRestarting Name Text
    deriving (Show, Typeable)

instance Exception WorkerNotRestarting

data Ref a = Ref {unRef :: Queue a}

type Send a = a -> IO ()

run :: (Send a -> Worker a) -> IO (Ref a)
run f = do
    queue <- Queue.new
    case f (Queue.write queue) of
        Worker name start restart process -> do
            st <- start
            supervise (go name restart process queue st)
            return (Ref queue)
  where
    go name restart  process queue st0 = goFlush st0
      where
        goFlush st = do
            xs <- Queue.flush queue
            goProcess st xs

        goProcess st [] = goFlush st
        goProcess st (x : xs) = do
            stm <- process st x `catch` \(e :: SomeException) -> do
                infoMsg "Trying to restart worker {}" (Only name)
                either (throwIO . WorkerNotRestarting name) (return . Just) =<<
                    restart st x e
            case stm of
                Nothing -> infoMsg "Worker {} quit" (Only name)
                Just st' -> goProcess st' xs

send :: Ref a -> Send a
send ref = Queue.write (unRef ref)

sink :: Worker a
sink = Worker
    { workerName    = ""
    , workerStart   = return ()
    , workerRestart = \() _ _ -> return (Right ())
    , workerReceive = \() _ -> return (Just ())
    }
