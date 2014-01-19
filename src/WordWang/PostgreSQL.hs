-- TODO match type when querying after an id
module WordWang.PostgreSQL
    ( Relation(..)
    , Object(..)

    , patch
    , lookup
    , exists

    , createRoom
    , lookupRoomStory
    , createUser
    , lookupUser
    ) where

import           Control.Applicative                   (empty)
import           Control.Lens                          ((^.))
import           Control.Monad                         (void, forM, forM_, unless)
import           Control.Monad.Trans                   (lift)
import           Control.Monad.Trans.Maybe             (MaybeT(MaybeT), runMaybeT)
import           Data.Functor                          ((<$>), (<$))
import qualified Data.HashSet                          as HashSet
import           Data.List                             (intersperse)
import           Data.Monoid                           ((<>), mconcat)
import           Database.PostgreSQL.Simple            ((:.)(..))
import qualified Database.PostgreSQL.Simple            as PG
import           Database.PostgreSQL.Simple.SqlQQ      (sql)
import qualified Database.PostgreSQL.Simple.ToRow      as PG
import           Prelude                               hiding (lookup)
import           System.Random                         (randomIO)

import           WordWang.JSON
import           WordWang.Objects

#include "../impossible.h"

----------------------------------------------------------------------

data Relation = forall r. PG.ToRow r => Relation
    { relTableName :: PG.Query
    , relObjIdCol  :: PG.Query
    , relRows      :: [r]
    }

class Patchable obj => Object obj where
    objTag       :: obj -> String
    objRelations :: obj -> [Relation]

instance Object Story where
    objTag _     = "story"
    objRelations _ = []

instance Object Room where
    objTag _ = "room"

    objRelations room =
        [ Relation "room_story" "room_id"
            [PG.Only (room ^. rStoryId)]
        , Relation "room_users" "room_id"
            (map PG.Only (HashSet.toList (room ^. rUsers)))
        ]

instance Object User where
    objTag _ = "user"
    objRelations _ = []

----------------------------------------------------------------------

relations :: (Object obj) => PG.Connection -> Id -> obj -> IO ()
relations conn objId obj = do
    let rels = objRelations obj
    forM_ rels $ \(Relation tableName objIdCol rows) -> unless (null rows) $ do
      void $ PG.execute conn
        ("DELETE FROM " <> tableName <> " WHERE " <> objIdCol <> " = ?")
        (PG.Only objId)
      forM_ rows $ \row0 -> do
        -- TODO we shouldn't invoke `toRow` twice.
        let row  = PG.Only objId :. row0
            pars = length $ PG.toRow row
            q = "INSERT INTO " <> tableName <> " VALUES (" <>
                mconcat (intersperse ", " $ replicate pars "?") <> ")"
        void $ PG.execute conn q row

create :: (Object obj) => PG.Connection -> obj -> IO Id
create conn obj = do
    objId <- randomIO
    void $ PG.execute conn
      [sql| INSERT INTO objects VALUES (?, ?, 0) |]
      (objId, objTag obj)
    void $ PG.execute conn
      [sql| INSERT INTO snapshots VALUES (?, 0, ?) |]
      (objId, JSONed obj)
    relations conn objId obj
    return objId

querySingle :: (PG.ToRow q, PG.FromRow r)
            => String -> PG.Connection -> PG.Query -> q -> MaybeT IO r
querySingle caller conn q pars = do
    results <- lift $ PG.query conn q pars
    case results of
        []       -> empty
        [result] -> return result
        (_ : _)  -> ERROR(caller ++ ": query returned multiple results.")

patch :: forall obj. (Object obj) => PG.Connection -> Id -> [Patch obj]
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

      -- The applied patches do something, write them and update the
      -- relations.
      Right (Just obj') -> do
        lift $ relations conn objId obj'
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
    verObj <- querySingle "lookup" conn
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
    return roomId

lookupRoomStory :: PG.Connection -> RoomId -> IO (Maybe StoryId)
lookupRoomStory conn roomId = runMaybeT $
    PG.fromOnly <$> querySingle "lookupRoomStory" conn
      [sql| SELECT story_id
            FROM room_story
            WHERE room_id = ?
      |] (PG.Only roomId)

createUser :: PG.Connection -> User -> RoomId -> IO (Maybe UserId)
createUser conn user roomId = do
    -- TODO do this in a transaction and remove the leftover user if the
    -- room is not there.
    userId <- create conn user
    (userId <$) <$> patch conn roomId [PRNewUser userId]

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
