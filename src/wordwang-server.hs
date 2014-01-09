{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<|>))
import           Control.Concurrent.MVar (newMVar)

import qualified Data.HashMap.Strict as HashMap
import           System.FilePath ((</>))

import           Control.Lens ((&), (.~))
import           Data.Pool (Pool, createPool, withResource)
import qualified Database.PostgreSQL.Simple as PG
import qualified Network.WebSockets.Snap as WS
import           Snap (Snap)
import qualified Snap as Snap
import qualified Snap.Util.FileServe as Snap

import           WordWang
import           WordWang.Config
import qualified WordWang.PostgreSQL as WWPG
import           Paths_wordwang (getDataDir)

run :: RootEnv -> FilePath -> Snap ()
run rootEnv dataDir =
        Snap.path "ws" (WS.runWebSocketsSnap (webSocketWW rootEnv wordwang))
    <|> Snap.path "create" (addRoom rootEnv)
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

    -- Load existing stories
    mapM_ (\(roomId, room) -> restoreRoom roomId room rootEnv) =<<
      withResource pgPool WWPG.loadRooms

    -- Run the server
    dataDir <- getDataDir
    Snap.quickHttpServe $ run rootEnv dataDir
