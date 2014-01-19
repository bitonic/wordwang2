{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative                   ((<|>))
import           Control.Concurrent.MVar               (newMVar)
import           Control.Lens                          ((&), (.~))
import qualified Data.HashMap.Strict                   as HashMap
import           Data.Pool                             (Pool, createPool)
import qualified Database.PostgreSQL.Simple            as PG
import qualified Network.WebSockets.Snap               as WS
import           Snap                                  (Snap)
import qualified Snap                                  as Snap
import qualified Snap.Util.FileServe                   as Snap
import           System.FilePath                       ((</>))

import           Paths_wordwang (getDataDir)
import           WordWang

run :: RootEnv -> FilePath -> Snap ()
run rootEnv dataDir =
        Snap.path "ws" (WS.runWebSocketsSnap (webSocketWW rootEnv wordwang))
    <|> Snap.path "req" (snapWW rootEnv wordwang)
    <|> Snap.path "create" (createRoom rootEnv)
    <|> Snap.serveDirectory (dataDir </> "www")

createPGPool :: PG.ConnectInfo -> IO (Pool PG.Connection)
createPGPool connInfo = createPool (PG.connect connInfo) PG.close 1 100 10

main :: IO ()
main = do
    let connInfo = PG.defaultConnectInfo
          { PG.connectDatabase = "wordwang"
          , PG.connectUser     = "wordwang"
          , PG.connectPassword = "mitchell"
          }
    initConfig $ defaultConfig & confPersist .~ PGPersist connInfo

    -- Create root env
    roomsMv <- newMVar HashMap.empty
    pgPool <- createPGPool connInfo
    let rootEnv = RootEnv{ _rootEnvRooms  = roomsMv
                         , _rootEnvPGPool = pgPool
                         }

    -- Run the server
    dataDir <- getDataDir
    Snap.quickHttpServe $ run rootEnv dataDir
