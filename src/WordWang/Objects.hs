{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WordWang.Objects
    ( Text

    , Id

    , UserId
    , UserNick
    , UserSecret
    , User(..)
    , userId
    , userSecret

    , Candidate(..)
    , candBlock
    , candUser
    , candVotes
    , candidate

    , StoryId
    , Block
    , Story(..)
    , storyId
    , storyUsers
    , storyCandidates
    , storyBlocks
    , emptyStory
    ) where

import           Control.Arrow (first)
import           Data.Functor ((<$>))
import           Data.Traversable (traverse)

import           Data.ByteString (ByteString)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Hashable (Hashable(hashWithSalt))
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           System.Random (Random(..))

import           Control.Lens (makeLenses)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.UUID as UUID
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField as PG

import           WordWang.Utils

newtype Id = Id {unId :: UUID.UUID}
    deriving (Eq, Ord, PG.ToField, PG.FromField)

instance Show Id where
    showsPrec n = showsPrec n . unId

instance Hashable Id where
    hashWithSalt salt = hashWithSalt salt . UUID.toASCIIBytes . unId

instance Random Id where
    randomR (Id lo, Id hi) g = first Id (randomR (lo, hi) g)
    random g = first Id (random g)

type UserId = Id
type UserSecret = ByteString
type UserNick = Text
data User = User
    { _userId         :: !UserId
    , _userSecret     :: !UserSecret -- TODO Hash the secret
    } deriving (Eq, Show)

data Candidate = Candidate
    { _candUser  :: !UserId
    , _candBlock :: !Block
    , _candVotes :: !(HashSet UserId)
    } deriving (Eq, Show)

candidate :: UserId -> Block -> Candidate
candidate uid block =
    Candidate{ _candUser  = uid
             , _candBlock = block
             , _candVotes = HashSet.singleton uid
             }

type StoryId = Id
type Block = Text
data Story = Story
    { _storyId         :: !StoryId
    , _storyUsers      :: !(HashMap UserId User)
    , _storyBlocks     :: ![Block]
    , _storyCandidates :: !(HashMap UserId Candidate)
    } deriving (Eq, Show)

emptyStory :: StoryId -> Story
emptyStory sid = Story{ _storyId         = sid
                      , _storyUsers      = HashMap.empty
                      , _storyBlocks     = []
                      , _storyCandidates = HashMap.empty
                      }

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

Aeson.deriveJSON (wwJSON $ delPrefix "_user") ''User
Aeson.deriveJSON (wwJSON $ delPrefix "_cand") ''Candidate

----------------------------------------------------------------------

makeLenses ''User
makeLenses ''Candidate
makeLenses ''Story
