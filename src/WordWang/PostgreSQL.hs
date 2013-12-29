module WordWang.PostgreSQL
    ( Revision
    , addRoom
    , patchRoom
    , loadRooms
    ) where

import           Control.Monad (void, forM)
import           Data.Functor ((<$>))
import           Data.Int (Int64)

import           Control.Monad.Trans.Maybe (MaybeT(runMaybeT))

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Transaction as PG
import           Database.PostgreSQL.Simple.SqlQQ (sql)

import           WordWang.Utils
import           WordWang.Objects

type Revision = Int64

addRoom :: PG.Connection -> RoomId -> Room -> IO ()
addRoom conn roomId room =
    void $ PG.execute conn
      [sql| INSERT INTO rooms VALUES (?, ?, 0) |]
      (roomId, JSONed room)

patchRoom :: PG.Connection -> RoomId -> Patch -> IO (Maybe Revision)
patchRoom conn roomId patch = withRetryingTransaction conn $ do
    revisions <- map PG.fromOnly <$> PG.query conn
      [sql| SELECT revision
              FROM patches
              WHERE roomId = ?
              ORDER BY revision DESC
              LIMIT 1
      |] (PG.Only roomId)
    case revisions of
      [] -> do
        return Nothing
      [revision] -> do
        let revision' = revision + 1
        void $ PG.execute conn
          [sql| INSERT INTO patches VALUES (?, ?, ?) |]
          (roomId, revision', JSONed patch)
        return $ Just revision'
      (_ : _) -> do
        error "WordWang.PostgreSQL.patchRoom: the impossible happened"

loadRooms :: PG.Connection -> IO [(RoomId, Room)]
loadRooms conn = withRetryingTransaction conn $ do
    revisionedRooms <- PG.query_ conn
      [sql| SELECT (roomId, room, revision)
              FROM rooms
      |]
    forM revisionedRooms $ \(roomId, (unJSONed -> room), revision) -> do
      patches <- map (unJSONed . PG.fromOnly) <$> PG.query conn
        [sql| SELECT (patch)
                FROM patches
                WHERE (roomId = ?)
                  AND (revision > ?)
                ORDER BY revision ASC
        |] (roomId :: RoomId, revision :: Revision)
      case runMaybeT (applyPatches patches room) of
        Left  err          -> error $ "WordWang.PostgreSQL.loadRooms: " ++ err
        Right Nothing      -> return (roomId, room)
        Right (Just room') -> return (roomId, room')

withRetryingTransaction :: PG.Connection -> IO a -> IO a
withRetryingTransaction =
    PG.withTransactionModeRetry PG.defaultTransactionMode (const True)
