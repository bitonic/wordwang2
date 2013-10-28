import           Control.Concurrent.MVar (newMVar)

import qualified Data.HashMap.Strict as HashMap

import qualified Network.WebSockets.Snap as WS
import           Snap (Snap)
import qualified Snap as Snap

import           WordWang

run :: Connections -> Stories -> Snap ()
run conns stories = do
    Snap.path "ws" (WS.runWebSocketsSnap (serverWWT conns stories wordwang))

main :: IO ()
main = do
    conns <- newMVar []
    stories <- newMVar HashMap.empty
    Snap.quickHttpServe (run conns stories)
