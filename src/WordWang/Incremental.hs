module WordWang.Incremental
    ( Incremental
    , start
    , bump
    , wait
    , stop
    ) where

import           Control.Concurrent (forkIO, threadDelay, killThread, ThreadId)
import           Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, modifyMVar_, modifyMVar, readMVar, putMVar, tryPutMVar)
import           Control.Monad (void)

import           Data.List.NonEmpty (NonEmpty(..))

data Continue = Yes !(NonEmpty Int) | No !Int

data Incremental = Incremental
    { incrContinue :: MVar Continue
    , incrDone     :: MVar ()
    , incrFirst    :: Int
    , incrNext     :: Int -> Maybe Int
    , incrTid      :: ThreadId
    }

start :: Int -> (Int -> Maybe Int) -> IO Incremental
start x f = do
    cont <- newMVar (Yes (x :| []))
    done <- newEmptyMVar
    -- TODO do something with this
    tid <- forkIO (worker cont done)
    return Incremental{ incrContinue = cont
                      , incrDone     = done
                      , incrFirst    = x
                      , incrNext     = f
                      , incrTid      = tid
                      }
  where
    worker cont done = do
        isDone <- modifyMVar cont $ \yn -> return $
            case yn of
                Yes (t :| ts) -> (No t, Just (sum (t : ts)))
                No _          -> (error "the impossible happened", Nothing)
        case isDone of
            Nothing -> putMVar done () >> return ()
            Just t  -> threadDelay t >> worker cont done

bump :: Incremental -> IO ()
bump Incremental{incrContinue = cont, incrNext = f} =
    modifyMVar_ cont $ \yn -> return $
        case yn of
            Yes (t :| ts) | Just t' <- f t -> Yes (t' :| t : ts)
            _ -> yn

wait :: Incremental -> IO ()
wait Incremental{incrDone = done} = readMVar done

stop :: Incremental -> IO ()
stop Incremental{incrDone = done, incrTid = tid} = do
    tryPutMVar done ()
    void $ killThread tid
