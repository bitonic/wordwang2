-- TODO match type when querying after an id
module WordWang.PostgreSQL
    ( patch
    , lookup
    , exists

    , createRoom
    , lookupRoomStory
    , createUser
    , lookupUser
    ) where

import           Control.Applicative                   (empty)
import           Control.Lens                          ((^.))
import           Control.Monad                         (void, forM)
import           Control.Monad.Trans                   (lift)
import           Control.Monad.Trans.Maybe             (MaybeT(MaybeT), runMaybeT)
import           Data.Functor                          ((<$>))
import           Database.PostgreSQL.Simple            ((:.)(..))
import qualified Database.PostgreSQL.Simple            as PG
import           Database.PostgreSQL.Simple.SqlQQ      (sql)
import           Prelude                               hiding (lookup)
import           System.Random                         (randomIO)

import           WordWang.JSON
import           WordWang.Log
import           WordWang.Objects

#include "../impossible.h"

----------------------------------------------------------------------

create :: (Patchable obj) => PG.Connection -> obj -> IO Id
create conn obj = do
    objId <- randomIO
    void $ PG.execute conn
      [sql| INSERT INTO objects VALUES (?, ?, 0) |]
      (objId, objTag obj)
    void $ PG.execute conn
      [sql| INSERT INTO snapshots VALUES (?, 0, ?) |]
      (objId, JSONed obj)
    return objId

querySingle :: (PG.ToRow q, PG.FromRow r)
            => String -> PG.Connection -> PG.Query -> q -> MaybeT IO r
querySingle caller conn q pars = do
    results <- lift $ PG.query conn q pars
    case results of
        []       -> empty
        [result] -> return result
        (_ : _)  -> ERROR(caller ++ ": query returned multiple results.")

patch :: forall obj. (Patchable obj) => PG.Connection -> Id -> [Patch obj]
      -> IO (Maybe [Versioned (Patch obj)])
patch conn objId patches = runMaybeT $ do
    verObj :: Versioned obj <- MaybeT $ lookup conn objId

    -- Test that all the patches are fine
    case runMaybeT (applyPatches patches (verObj ^. vObj)) of
      -- We had an error
      Left err -> do
        ERROR("error applying patch: " ++ err)

      -- The applied patches do nothing
      Right Nothing -> do
        return []

      -- The applied patches do something, write them
      Right _ -> do
        -- Insert all the patches on top of the latest revision.
        lift $ forM (zipWith Versioned [(verObj ^. vRev)..] patches) $
          \verPatch -> do
            void $ PG.execute conn
              [sql| INSERT INTO patches VALUES (?, ?, ?) |]
              (PG.Only objId :. verPatch)
            return verPatch

exists :: PG.Connection -> Id -> IO Bool
exists conn objId = do
    res <- map PG.fromOnly <$> PG.query conn
      [sql| SELECT EXISTS (SELECT 1 FROM objects WHERE obj_id = ?) |]
      (PG.Only objId)
    case res of
      [True] -> return True
      _      -> return False

lookup :: Patchable obj => PG.Connection -> Id -> IO (Maybe (Versioned obj))
lookup conn objId = runMaybeT $ do
    verObj <- querySingle "WordWang.PostgreSQL.lookup" conn
      [sql| SELECT revision, obj
              FROM snapshots
              WHERE obj_id = ?
              ORDER BY revision DESC
              LIMIT 1
      |] (PG.Only objId)

    verPatches <- lift $ PG.query conn
      [sql| SELECT revision, patch
              FROM patches
              WHERE (obj_id = ?) AND (revision >= ?)
              ORDER BY revision ASC
      |] (objId, verObj ^. vRev)

    case runMaybeT (applyPatchesVersioned verPatches verObj) of
      Left err ->
        ERROR("error while applying patch: " ++ err)
      Right Nothing ->
        return verObj
      Right (Just verObj') ->
        return verObj'

------------------------------------------------------------------------

createRoom :: PG.Connection -> IO RoomId
createRoom conn = do
    storyId <- create conn emptyStory
    roomId <- create conn $ emptyRoom storyId
    void $ PG.execute conn
      [sql| INSERT INTO room_stories VALUES (?, ?) |]
      (roomId, storyId)
    return roomId

lookupRoomStory :: PG.Connection -> RoomId -> IO (Maybe StoryId)
lookupRoomStory conn roomId = runMaybeT $
    PG.fromOnly <$> querySingle "WordWang.PostgreSQL.roomStory" conn
      [sql| SELECT story_id
            FROM room_stories
            WHERE room_id = ?
      |] (PG.Only roomId)

createUser :: PG.Connection -> User -> RoomId -> IO (Maybe UserId)
createUser conn user roomId = do
    -- TODO do this in a transaction and remove the leftover user if the
    -- room is not there.
    userId <- create conn user
    mbPatches <- patch conn roomId [PRNewUser userId]
    case mbPatches of
      Nothing -> do
        return Nothing
      Just _ -> do
        void $ PG.execute conn
          [sql| INSERT INTO room_users VALUES (?, ?) |]
          (roomId, userId)
        return $ Just userId

lookupUser :: PG.Connection -> UserId -> RoomId -> IO (Maybe User)
lookupUser conn userId roomId = do
    isRoomUser <- runMaybeT $
      PG.fromOnly <$> querySingle "viewUser" conn
        [sql| SELECT EXISTS (
                SELECT 1
                FROM room_users
                WHERE room_id = ?
                  AND user_id = ?
              )
        |] (roomId, userId)
    case isRoomUser of
      Just True -> fmap _vObj <$> lookup conn userId
      _         -> return Nothing
