module WordWang.Countdown
    ( Countdown
    , start
    , bump
    , wait
    , stop
    ) where

import           Control.Concurrent                    (threadDelay, killThread, ThreadId)
import           Control.Concurrent.MVar               (MVar, newMVar, newEmptyMVar, modifyMVar_, modifyMVar, readMVar, putMVar, tryPutMVar)
import           Control.Monad                         (void)
import           Data.List.NonEmpty                    (NonEmpty(..))

import           WordWang.Concurrent

data ShouldContinue
    = Yes !(NonEmpty Int)
      -- ^ The worker should continue waiting.
    | No !Int
      -- ^ The worker should signal that we're done.

data Countdown = Countdown
    { incrContinue :: MVar ShouldContinue
    , incrDone     :: MVar ()
    , incrNext     :: Int -> Maybe Int
    , incrTid      :: ThreadId
    }

start :: Int -> (Int -> Maybe Int) -> IO Countdown
start x f = do
    shouldContinueMV <- newMVar $ Yes (x :| [])
    doneMV <- newEmptyMVar
    tid <- supervise $ worker shouldContinueMV doneMV
    return Countdown{ incrContinue = shouldContinueMV
                    , incrDone     = doneMV
                    , incrNext     = f
                    , incrTid      = tid
                    }
  where
    worker shouldContinueMV doneMV = do
        mbWaitingTime <- modifyMVar shouldContinueMV $ \shouldContinue ->
          return $ case shouldContinue of
            Yes (t :| ts) -> (No t, Just (sum (t : ts)))
            No _          -> (error "the impossible happened", Nothing)
        case mbWaitingTime of
          Nothing -> do
            void $ putMVar doneMV ()
          Just waitingTime  -> do
            threadDelay waitingTime
            worker shouldContinueMV doneMV

bump :: Countdown -> IO ()
bump Countdown{incrContinue = cont, incrNext = f} =
    modifyMVar_ cont $ \shouldContinue -> return $
      case shouldContinue of
        Yes (t :| ts) | Just t' <- f t -> Yes (t' :| t : ts)
        _                              -> shouldContinue

wait :: Countdown -> IO ()
wait Countdown{incrDone = done} = readMVar done

stop :: Countdown -> IO ()
stop Countdown{incrDone = done, incrTid = tid} = do
    void $ tryPutMVar done ()
    void $ killThread tid
