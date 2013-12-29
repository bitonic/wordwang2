{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<|>))
import           Control.Concurrent.MVar (newMVar)

import qualified Data.HashMap.Strict as HashMap
import           System.FilePath ((</>))

import           Control.Lens ((&), (.~))
import qualified Database.PostgreSQL.Simple as PG
import qualified Network.WebSockets.Snap as WS
import           Snap (Snap)
import qualified Snap as Snap
import qualified Snap.Util.FileServe as Snap

import           WordWang
import           WordWang.Config
import qualified WordWang.PostgreSQL as WWPG
import           Paths_wordwang (getDataDir)

run :: Rooms -> FilePath -> Snap ()
run roomsMv dataDir =
        Snap.path "ws" (WS.runWebSocketsSnap (serverWW roomsMv wordwang))
    <|> Snap.path "create" (addRoom roomsMv)
    <|> Snap.serveDirectory (dataDir </> "www")

main :: IO ()
main = do
    let connInfo = PG.defaultConnectInfo
          { PG.connectDatabase = "wordwang"
          , PG.connectUser     = "wordwang"
          , PG.connectPassword = "mitchell"
          }
    initConfig $ defaultConfig & confPersist .~ PGPersist connInfo
    roomsMv <- newMVar HashMap.empty
    conn <- PG.connect connInfo
    mapM_ (\(roomId, room) -> restoreRoom roomId room roomsMv) =<<
      WWPG.loadRooms conn
    dataDir <- getDataDir
    Snap.quickHttpServe (run roomsMv dataDir)
