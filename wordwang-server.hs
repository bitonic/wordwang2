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
import           WordWang.PostgreSQL
import           Paths_wordwang (getDataDir)

run :: Stories -> FilePath -> Snap ()
run storiesMv dataDir =
        Snap.path "ws" (WS.runWebSocketsSnap (serverWW storiesMv wordwang))
    <|> Snap.path "create" (createStory storiesMv)
    <|> Snap.serveDirectory (dataDir </> "www")

main :: IO ()
main = do
    let pgCI = PG.defaultConnectInfo{ PG.connectDatabase = "wordwang"
                                    , PG.connectUser     = "wordwang"
                                    , PG.connectPassword = "mitchell" }
    initConfig (defaultConfig & cPersist .~ PGPersist pgCI)
    storiesMv <- newMVar HashMap.empty
    mapM_ (createStory' storiesMv) =<< loadStories pgCI
    dataDir <- getDataDir
    Snap.quickHttpServe (run storiesMv dataDir)
