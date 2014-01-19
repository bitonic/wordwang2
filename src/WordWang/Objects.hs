{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WordWang.Objects
    ( -- * Objects
      -- ** 'Id'
      Id

      -- ** 'User'
    , UserId
    , UserSecret
    , User(..)
    , uSecret

      -- ** 'Candidate'
    , CandidateId
    , Candidate(..)
    , cBlock
    , cVotes
    , candidate

      -- ** 'Story'
    , Block
    , StoryId
    , Story(..)
    , sCandidates
    , sBlocks
    , emptyStory

      -- ** 'Room'
    , RoomId
    , Room(..)
    , rStoryId
    , rUsers
    , emptyRoom

      -- * Patching
    , Patchable(..)
    , Patch(..)
    , applyPatches

      -- * Versioned objects
    , Revision
    , Versioned(..)
    , vRev
    , vObj
    , applyPatchesVersioned
    ) where

import           Control.Lens                          (makeLenses, at, (.~), (?~), (%~), (^.), (&))
import           Control.Monad                         (when)
import           Control.Monad.Trans                   (lift)
import           Control.Monad.Trans.Maybe             (MaybeT(MaybeT))
import           Data.Aeson                            ((.=), FromJSON, ToJSON)
import qualified Data.Aeson                            as Aeson
import qualified Data.Aeson.TH                         as Aeson
import           Data.ByteString                       (ByteString)
import           Data.Foldable                         (Foldable)
import           Data.Functor                          ((<$>))
import           Data.HashMap.Strict                   (HashMap)
import qualified Data.HashMap.Strict                   as HashMap
import           Data.HashSet                          (HashSet)
import qualified Data.HashSet                          as HashSet
import           Data.Hashable                         (Hashable)
import           Data.Int                              (Int64)
import           Data.Text                             (Text)
import qualified Data.Text.Encoding                    as T
import           Data.Traversable                      (Traversable)
import           Data.Traversable                      (traverse)
import           Data.Typeable                         (Typeable, Typeable1)
import qualified Data.UUID                             as UUID
import qualified Database.PostgreSQL.Simple.FromField  as PG
import qualified Database.PostgreSQL.Simple.FromRow    as PG
import qualified Database.PostgreSQL.Simple.ToField    as PG
import qualified Database.PostgreSQL.Simple.ToRow      as PG
import           System.Random                         (Random(..))

import           WordWang.JSON

#include "../impossible.h"

newtype Id = Id {unId :: UUID.UUID}
    deriving (Eq, Typeable, PG.ToField, PG.FromField, Hashable, Random)

instance Show Id where
    showsPrec n = showsPrec n . unId

type UserId = Id
type UserSecret = ByteString
data User = User
    { _uSecret     :: !UserSecret -- TODO Hash the secret
    } deriving (Eq, Show, Typeable)

type CandidateId = UserId
data Candidate = Candidate
    { _cBlock :: !Block
    , _cVotes :: !(HashSet UserId)
    } deriving (Eq, Show, Typeable)

candidate :: UserId -> Block -> Candidate
candidate uid block =
    Candidate{ _cBlock = block
             , _cVotes = HashSet.singleton uid
             }

type Block = Text
type StoryId = Id
data Story = Story
    { _sBlocks     :: ![Block]
    , _sCandidates :: !(HashMap CandidateId Candidate)
    } deriving (Eq, Show, Typeable)

emptyStory :: Story
emptyStory = Story{ _sBlocks     = []
                  , _sCandidates = HashMap.empty
                  }

type RoomId = Id
data Room = Room
    { _rStoryId :: StoryId
    , _rUsers   :: !(HashSet UserId)
    } deriving (Eq, Show, Typeable)

emptyRoom :: StoryId -> Room
emptyRoom storyId = Room{ _rStoryId = storyId
                        , _rUsers   = HashSet.empty
                        }

----------------------------------------------------------------------

makeLenses ''User
makeLenses ''Candidate
makeLenses ''Story
makeLenses ''Room

----------------------------------------------------------------------

class
  ( Aeson.FromJSON obj
  , Aeson.ToJSON obj
  , Aeson.FromJSON (Patch obj)
  , Aeson.ToJSON (Patch obj)
  , Typeable obj
  ) => Patchable obj
  where
    data Patch obj :: *
    objTag     :: obj -> String
    applyPatch :: Patch obj -> obj -> MaybeT (Either String) obj

deriving instance Typeable1 Patch

applyPatches :: Patchable obj => [Patch obj] -> obj -> MaybeT (Either String) obj
applyPatches []                obj = return obj
applyPatches (patch : patches) obj = applyPatches patches =<< applyPatch patch obj

instance Patchable Room where
    data Patch Room
        = PRNewUser !UserId

    applyPatch (PRNewUser userId) root = do
        -- TODO should we check that the user doesn't exist?
        return $ root & rUsers %~ HashSet.insert userId

    objTag _ = "room"

deriving instance Eq (Patch Room)
deriving instance Show (Patch Room)

instance Patchable Story where
    data Patch Story
        = PSVotingClosed !Block
        | PSCandidate !CandidateId !Candidate
        | PSVote !CandidateId !UserId

    applyPatch (PSVotingClosed block) story = do
        return $ story & (sCandidates .~ HashMap.empty) . (sBlocks %~ (++ [block]))
    applyPatch (PSCandidate candId cand) story = do
        case story ^. sCandidates ^. at candId of
          Just _  -> nothing -- The user has already proposed a candidate.
          Nothing -> return $ story & sCandidates . at candId ?~ cand
    applyPatch (PSVote candId userId) story = do
        case story ^. sCandidates ^. at candId of
          Nothing -> do
            lift $ Left $ "candidate " ++ show candId ++ " not present"
          Just cand | HashSet.member userId (cand ^. cVotes) -> do
            -- The user has already voted
            nothing
          Just cand -> do
            let cand' = cand & cVotes %~ HashSet.insert userId
            return $ story & sCandidates . at candId ?~ cand'

    objTag _ = "story"

deriving instance Eq (Patch Story)
deriving instance Show (Patch Story)

nothing :: Monad m => MaybeT m a
nothing = MaybeT $ return Nothing

instance Patchable User where
    data Patch User

    objTag _ = "user"

    applyPatch _ _user = IMPOSSIBLE

deriving instance Eq (Patch User)
deriving instance Show (Patch User)

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

----------------------------------------------------------------------

instance (FromJSON a, Typeable a) => PG.FromRow (Versioned a) where
    fromRow = do
        rev <- PG.field
        obj <- unJSONed <$> PG.field
        return $ Versioned rev obj

instance (ToJSON a, Typeable a) => PG.ToRow (Versioned a) where
    toRow (Versioned rev obj) = [PG.toField rev, PG.toField (JSONed obj)]

Aeson.deriveJSON (wwJSON $ delPrefix "_v") ''Versioned

----------------------------------------------------------------------

instance Aeson.ToJSON Id where
    toJSON = Aeson.toJSON . UUID.toASCIIBytes . unId

instance Aeson.FromJSON Id where
    parseJSON json = do
        bytes <- Aeson.parseJSON json
        case UUID.fromASCIIBytes bytes of
          Nothing   -> fail "Error decoding ASCII bytes to UUID"
          Just uuid -> return (Id uuid)

instance Aeson.ToJSON v => Aeson.ToJSON (HashMap Id v) where
    toJSON = Aeson.Object
           . mapKeyVal (T.decodeUtf8 . UUID.toASCIIBytes . unId) Aeson.toJSON
      where
        mapKeyVal fk kv =
          HashMap.foldrWithKey (\k v -> HashMap.insert (fk k) (kv v)) HashMap.empty

instance Aeson.FromJSON v => Aeson.FromJSON (HashMap Id v) where
    parseJSON json = do
        hm <- Aeson.parseJSON json
        case f hm of
          Nothing  -> fail "Error decoding ASCII bytes keys to UUID"
          Just kvs -> return (HashMap.fromList kvs)
      where
        f = traverse (\(k, v) -> (, v) . Id <$> UUID.fromASCIIBytes k)
          . HashMap.toList

Aeson.deriveJSON (wwJSON $ delPrefix "_u") ''User
Aeson.deriveJSON (wwJSON $ delPrefix "_c") ''Candidate
Aeson.deriveJSON (wwJSON $ delPrefix "_s") ''Story
Aeson.deriveJSON (wwJSON $ delPrefix "_r") ''Room

instance Aeson.ToJSON (Patch Room) where
    toJSON (PRNewUser userId) =
        tagJSON ("newUser", ["userId" .= userId])

instance Aeson.FromJSON (Patch Room) where
    parseJSON val =
        parseTagged [("newUser", parseUnary PRNewUser "userId")] val

instance Aeson.ToJSON (Patch Story) where
    toJSON = toTaggedJSON $ \case
        PSVotingClosed block ->
          ("votingClosed", ["block" .= block])
        PSCandidate candId cand ->
          ("candidate",    ["candidateId" .= candId, "candidate" .= cand])
        PSVote candId userId ->
          ("vote",         ["candidateId" .= candId , "userId" .= userId])

instance Aeson.FromJSON (Patch Story) where
    parseJSON = parseTagged
        [ ("votingClosed", parseUnary  PSVotingClosed  "block")
        , ("candidate",    parseBinary PSCandidate "candidateId" "candidate")
        , ("vote",         parseBinary PSVote "candidateId" "userId")
        ]


instance Aeson.ToJSON (Patch User) where
    toJSON _ = IMPOSSIBLE

instance Aeson.FromJSON (Patch User) where
    parseJSON _ = fail "Patch User has no members."

----------------------------------------------------------------------
