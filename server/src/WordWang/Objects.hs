{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module WordWang.Objects
    ( Text

    , Id

    , UserId
    , UserNick
    , UserSecret
    , User(..)
    , userId
    , userNick
    , userSecret

    , CandidateBody
    , Candidate(..)
    , candidateBody
    , candidateUser
    , candidateVotes

    , StoryId
    , Block
    , Story(..)
    , storyId
    , storyUsers
    , storyCandidates
    , storySoFar
    ) where

import           Data.Functor ((<$>))
import           Data.Traversable (traverse)

import           Data.ByteString (ByteString)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (HashSet)
import           Data.Hashable (Hashable(hashWithSalt))
import           Data.Text (Text)
import qualified Data.Text.Encoding as T

import           Control.Lens (makeLenses)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.UUID as UUID

import           WordWang.Utils

newtype Id = Id { unId :: UUID.UUID }
    deriving (Eq, Ord, Show, Read)

type UserId = Id
type UserSecret = ByteString
type UserNick = Text
data User = User
    { _userId     :: UserId
    , _userNick   :: UserNick
    , _userSecret :: UserSecret
    }

type CandidateBody = Text
data Candidate = Candidate
    { _candidateUser  :: Id
    , _candidateBody  :: CandidateBody
    , _candidateVotes :: HashSet UserId
    }

type StoryId = Id
type Block = Text
data Story = Story
    { _storyId         :: StoryId
    , _storyUsers      :: HashMap UserId User
    , _storySoFar      :: [Block]
    , _storyCandidates :: HashMap Id Candidate
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

instance Hashable Id where
    hashWithSalt salt = hashWithSalt salt . UUID.toASCIIBytes . unId

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

Aeson.deriveJSON (wwJSON $ delPrefix "_user")      ''User
Aeson.deriveJSON (wwJSON $ delPrefix "_candidate") ''Candidate
Aeson.deriveJSON (wwJSON $ delPrefix "_story")     ''Story

----------------------------------------------------------------------

makeLenses ''User
makeLenses ''Candidate
makeLenses ''Story
