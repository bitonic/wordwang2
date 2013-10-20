import           Control.Concurrent (forkIO, killThread)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)

import qualified Data.HashMap.Strict as HashMap

import           Snap (Snap)
import qualified Snap as Snap

import           WordWang

newtype Processor = Processor
    {unProcessor :: Req -> IO (Resp, Processor)}

storyProcessor :: IO (IO (), Processor)
storyProcessor = do
    id_ <- newId
    let story = Story{ _storyId         = id_
                     , _storyUsers      = HashMap.empty
                     , _storySoFar      = []
                     , _storyCandidates = HashMap.empty
                     }
    mvReq <- newEmptyMVar
    mvResp <- newEmptyMVar
    tid <- forkIO $ go story mvReq mvResp
    return (killThread tid, Processor (send mvReq mvResp))
  where
    go :: Story -> MVar Req -> MVar Resp -> IO a
    go story mvReq mvResp = do
        req <- takeMVar mvReq
        (resp, story') <- runWW req story return receive
        putMVar mvResp resp
        go story' mvReq mvResp

    send :: MVar Req -> MVar Resp -> Req -> IO (Resp, Processor)
    send mvReq mvResp req = do
        putMVar mvReq req
        resp <- takeMVar mvResp
        return (resp, Processor (send mvReq mvResp))

run :: Snap ()
run = do
    Snap.path "/ws" undefined

main :: IO ()
main = Snap.quickHttpServe run
