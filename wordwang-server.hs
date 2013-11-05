{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<|>))
import           Control.Concurrent.MVar (newMVar)

import qualified Data.HashMap.Strict as HashMap
import           System.FilePath ((</>))

import qualified Network.WebSockets.Snap as WS
import           Snap (Snap)
import qualified Snap as Snap
import qualified Snap.Util.FileServe as Snap

import           WordWang
import           Paths_wordwang (getDataDir)

run :: Connections -> Stories -> FilePath -> Snap ()
run conns stories dataDir =
        Snap.path "ws" (WS.runWebSocketsSnap (serverWWT conns stories wordwang))
    <|> Snap.serveDirectory (dataDir </> "www")

main :: IO ()
main = do
    conns <- newMVar []
    stories <- newMVar HashMap.empty
    dataDir <- getDataDir
    Snap.quickHttpServe (run conns stories dataDir)
