-- import           Control.Concurrent (forkIO, killThread)
-- import           Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, putMVar, takeMVar)


-- import           Data.HashMap.Strict (HashMap)
-- import qualified Data.HashMap.Strict as HashMap
-- import qualified Data.HashSet as HashSet

-- import qualified Data.Aeson as Aeson
-- import qualified Network.WebSockets as WS
-- import qualified Network.WebSockets.Snap as WS
-- import           Snap (Snap)
-- import qualified Snap as Snap

-- import           WordWang

-- accept :: Connections -> Stories -> WS.Connection -> IO ()
-- accept conns stories conn = do
--     reqm <- Aeson.decode <$> WS.receiveData conn
--     case reqm of
--         Nothing -> do
--             WS.sendTextData conn
--                             (Aeson.encode (RespError "error decoding request"))
--             accept conn
--         Just req -> do
--             either (WS.sendTextData conn . Aeson.encode) return =<<
--                 join (initWWT conn conns stories wordwang)
--             accept conn

-- runWS :: Connections -> Stories -> WS.ServerApp
-- runWS conns stories pending = do
--     conn <- WS.acceptRequest pending
--     accept conn stories

-- run :: Connections -> Stories -> Snap ()
-- run conns stories = do
--     Snap.path "/ws" (WS.runWebSocketsSnap (runWS conns stories))

-- main :: IO ()
-- main = do
--     conns <- newMVar []
--     stories <- newMVar HashMap.empty
--     Snap.quickHttpServe (run rooms stories)

main :: IO ()
main = undefined