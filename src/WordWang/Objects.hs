{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WordWang.Objects
    ( Text

    , Id

    , UserId
    , UserSecret
    , User(..)
    , uSecret

    , CandidateId
    , Candidate(..)
    , cBlock
    , cVotes
    , candidate

    , Block
    , Story(..)
    , sCandidates
    , sBlocks
    , emptyStory

    , RoomId
    , Room(..)
    , rStory
    , rUsers
    , emptyRoom

    , PatchStory(..)
    , Patch(..)
    , applyPatch
    , applyPatches
    ) where

import           Control.Applicative ((<|>))
import           Control.Arrow (first)
import           Data.Functor ((<$>))
import           Data.Traversable (traverse)
import           Data.Typeable (Typeable)

import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Maybe (MaybeT(MaybeT))
import           Data.ByteString (ByteString)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Hashable (Hashable(hashWithSalt))
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           System.Random (Random(..))

import           Control.Lens (makeLenses, at, (.~), (?~), (%~), (^.), (&))
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.UUID as UUID
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField as PG

import           WordWang.Utils

newtype Id = Id {unId :: UUID.UUID}
    deriving (Eq, Typeable, PG.ToField, PG.FromField)

instance Show Id where
    showsPrec n = showsPrec n . unId

instance Hashable Id where
    hashWithSalt salt = hashWithSalt salt . UUID.toASCIIBytes . unId

instance Random Id where
    randomR (Id lo, Id hi) g = first Id (randomR (lo, hi) g)
    random g = first Id (random g)

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
    { _rStory :: Story
    , _rUsers :: !(HashMap UserId User)
    } deriving (Eq, Show, Typeable)

emptyRoom :: Room
emptyRoom = Room{ _rStory = emptyStory
                , _rUsers = HashMap.empty
                }

----------------------------------------------------------------------

makeLenses ''User
makeLenses ''Candidate
makeLenses ''Story
makeLenses ''Room

----------------------------------------------------------------------

data PatchStory
    = PSVotingClosed !Block
    | PSCandidate !CandidateId !Candidate
    | PSVote !CandidateId !UserId
    deriving (Eq, Show, Typeable)

data Patch
    = PStory !PatchStory
    | PNewUser !UserId !User
    deriving (Eq, Show, Typeable)

applyPatch :: Patch -> Room -> MaybeT (Either String) Room
applyPatch (PStory patchStory) root = do
    story <- applyPatchStory patchStory (root ^. rStory)
    return $ root & rStory .~ story
applyPatch (PNewUser userId user) root = do
    -- TODO should we check that the user doesn't exist?
    return $ root & rUsers . at userId ?~ user

applyPatches :: [Patch] -> Room -> MaybeT (Either String) Room
applyPatches []                room = return room
applyPatches (patch : patches) room = applyPatches patches =<< applyPatch patch room

nothing :: Monad m => MaybeT m a
nothing = MaybeT $ return Nothing

applyPatchStory :: PatchStory -> Story -> MaybeT (Either String) Story
applyPatchStory (PSVotingClosed block) story = do
    return $ story & (sCandidates .~ HashMap.empty) . (sBlocks %~ (++ [block]))
applyPatchStory (PSCandidate candId cand) story = do
    case story ^. sCandidates ^. at candId of
      Just _  -> nothing -- The user has already proposed a candidate.
      Nothing -> return $ story & sCandidates . at candId ?~ cand
applyPatchStory (PSVote candId userId) story = do
    case story ^. sCandidates ^. at candId of
      Nothing -> do
        lift $ Left $ "candidate " ++ show candId ++ " not present"
      Just cand | HashSet.member userId (cand ^. cVotes) -> do
        -- The user has already voted
        nothing
      Just cand -> do
        let cand' = cand & cVotes %~ HashSet.insert userId
        return $ story & sCandidates . at candId ?~ cand'

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

instance Aeson.ToJSON Patch where
    toJSON (PStory patchStory) =
        Aeson.toJSON patchStory
    toJSON (PNewUser userId user) =
        tagJSON ("newUser", ["userId" .= userId, "user" .= user])

instance Aeson.FromJSON Patch where
    parseJSON val =
        (PStory <$> Aeson.parseJSON val) <|>
        parseTagged [("newUser", parseBinary PNewUser "userId" "user")] val

instance Aeson.ToJSON PatchStory where
    toJSON = toTaggedJSON $ \case
        PSVotingClosed block ->
          ("votingClosed", ["block" .= block])
        PSCandidate candId cand ->
          ("candidate",    ["candidateId" .= candId, "candidate" .= cand])
        PSVote candId userId ->
          ("vote",         ["candidateId" .= candId , "userId" .= userId])

instance Aeson.FromJSON PatchStory where
    parseJSON = parseTagged
        [ ("votingClosed", parseUnary  PSVotingClosed  "block")
        , ("candidate",    parseBinary PSCandidate "candidateId" "candidate")
        , ("vote",         parseBinary PSVote "candidateId" "userId")
        ]

----------------------------------------------------------------------
