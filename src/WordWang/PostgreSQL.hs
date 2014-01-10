-- TODO better exceptions instead of 'error'.
module WordWang.PostgreSQL
    ( addRoom
    , patchRoom
    , loadRooms
    ) where

import           Control.Monad                         (void, forM, forM_)
import           Control.Monad.Trans.Maybe             (MaybeT(runMaybeT))
import           Data.Functor                          ((<$>))
import           Data.Int                              (Int64)
import qualified Database.PostgreSQL.Simple            as PG
import           Database.PostgreSQL.Simple.SqlQQ      (sql)

import           WordWang.JSON
import           WordWang.Objects

type Revision = Int64

addRoom :: PG.Connection -> RoomId -> Room -> IO ()
addRoom conn roomId room =
    void $ PG.execute conn
      [sql| INSERT INTO rooms VALUES (?, ?, 0) |]
      (roomId, JSONed room)

patchRoom :: PG.Connection -> RoomId -> [Patch] -> IO ()
patchRoom conn roomId patches = do
    -- Get the latest revision.
    revisions :: [Revision] <- map PG.fromOnly <$> PG.query conn
      [sql| SELECT revision
              FROM patches
              WHERE room_id = ?
              ORDER BY revision DESC
              LIMIT 1
      |] (PG.Only roomId)
    revision <- case revisions of
      -- If no revisions are present, get it from the room table
      -- directly.
      [] -> do
        roomsRevisions <- map PG.fromOnly <$> PG.query conn
          [sql| SELECT revision
                  FROM rooms
                  WHERE room_id = ?
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

    -- Insert all the patches on top of the latest revision.
    forM_ (zip [revision+1..] patches) $ \(patchRevision, patch) ->
      PG.execute conn
        [sql| INSERT INTO patches VALUES (?, ?, ?) |]
        (roomId, patchRevision, JSONed patch)
  where
    patchRoomError s = error $ "WordWang.PostgreSQL.patchRoom: " ++ s

loadRooms :: PG.Connection -> IO [(RoomId, Room)]
loadRooms conn = do
    revisionedRooms <- PG.query_ conn
      [sql| SELECT room_id, room, revision
              FROM rooms
      |]
    forM revisionedRooms $
      \(roomId, JSONed room, revision :: Revision) -> do
         patches <- map (unJSONed . PG.fromOnly) <$> PG.query conn
           [sql| SELECT patch
                   FROM patches
                   WHERE (room_id = ?) AND (revision > ?)
                   ORDER BY revision ASC
           |] (roomId, revision)
         case runMaybeT (applyPatches patches room) of
           Left  err          -> error $ "WordWang.PostgreSQL.loadRooms: " ++ err
           Right Nothing      -> return (roomId, room)
           Right (Just room') -> return (roomId, room')

------------------------------------------------------------------------
