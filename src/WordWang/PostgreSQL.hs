-- TODO better exceptions instead of 'error'.
module WordWang.PostgreSQL
    ( Revision
    , addRoom
    , patchRoom
    , loadRooms
    ) where

import           Control.Monad (void, forM, forM_)
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

patchRoom :: PG.Connection -> RoomId -> [Patch] -> IO ()
patchRoom conn roomId patches = withRetryingTransaction conn $ do
    revisions <- map PG.fromOnly <$> PG.query conn
      [sql| SELECT revision
              FROM patches
              WHERE roomId = ?
              ORDER BY revision DESC
              LIMIT 1
      |] (PG.Only roomId)
    revision :: Revision <- case revisions of
      [] -> do
        roomsRevisions <- map PG.fromOnly <$> PG.query conn
          [sql| SELECT revision
                  FROM rooms
                  WHERE roomId = ?
                  LIMIT 1
          |] (PG.Only roomId)
        case roomsRevisions of
          [] ->
            patchRoomError $ "no revision found for room " ++ show roomId
          [revision] ->
            return revision
          (_ : _) ->
            patchRoomError $ "rooms query returned multiple rows"
      [revision] -> do
        return revision
      (_ : _) -> do
        patchRoomError $ "patches query returned multiple rows"

    forM_ (zip [revision+1..] patches) $ \(patchRevision, patch) ->
      PG.execute conn
        [sql| INSERT INTO patches VALUES (?, ?, ?) |]
        (roomId, patchRevision, JSONed patch)
  where
    patchRoomError s = error $ "WordWang.PostgreSQL.patchRoom: " ++ s

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
