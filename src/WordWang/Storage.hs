module WordWang.Storage
    ( -- * Versioned objects
      Revision
    , Versioned(..)
    , vRev
    , vObj
    , applyPatchesVersioned

      -- * Relations
    , Relation(..)
    , Object(..)
    ) where

import           Control.Lens                          ((^.), makeLenses)
import           Control.Monad                         (when)
import           Control.Monad.Trans.Maybe             (MaybeT(MaybeT))
import           Data.Aeson                            (FromJSON, ToJSON)
import           Data.Aeson.TH                         as Aeson
import           Data.Foldable                         (Foldable)
import           Data.Functor                          ((<$>))
import qualified Data.HashSet                          as HashSet
import           Data.Int                              (Int64)
import           Data.Traversable                      (Traversable)
import           Data.Typeable                         (Typeable)
import qualified Database.PostgreSQL.Simple            as PG
import qualified Database.PostgreSQL.Simple.FromRow    as PG
import qualified Database.PostgreSQL.Simple.ToRow      as PG
import qualified Database.PostgreSQL.Simple.ToField    as PG
import           Prelude                               hiding (lookup)

import           WordWang.JSON
import           WordWang.Objects

#include "../impossible.h"

----------------------------------------------------------------------

type Revision = Int64

data Versioned obj = Versioned
    { _vRev :: Revision
    , _vObj :: obj
    } deriving (Eq, Show, Typeable, Functor, Traversable, Foldable)

makeLenses ''Versioned

applyPatchesVersioned :: Patchable obj
                      => [Versioned (Patch obj)] -> Versioned obj
                      -> MaybeT (Either String) (Versioned obj)
applyPatchesVersioned [] verObj = do
    return $ verObj
applyPatchesVersioned (verPatch : verPatches) verObj = do
    when (verPatch^.vRev /= verObj^.vRev) $ MaybeT $ return Nothing
    obj' <- applyPatch (verPatch^.vObj) (verObj^.vObj)
    applyPatchesVersioned verPatches $ Versioned (verObj^.vRev + 1) obj'

instance (FromJSON a, Typeable a) => PG.FromRow (Versioned a) where
    fromRow = do
        rev <- PG.field
        obj <- unJSONed <$> PG.field
        return $ Versioned rev obj

instance (ToJSON a, Typeable a) => PG.ToRow (Versioned a) where
    toRow (Versioned rev obj) = [PG.toField rev, PG.toField (JSONed obj)]

Aeson.deriveJSON (wwJSON $ delPrefix "_v") ''Versioned

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
