{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module WordWang.Objects
    ( Id
    , User(..)
    , Candidate(..)
    , Story(..)
    ) where

import           Data.Functor ((<$>))
import           Data.Traversable (traverse)

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable (Hashable(hashWithSalt))
import           Data.Text (Text)
import qualified Data.Text.Encoding as T

import qualified Data.UUID as UUID
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson

import           WordWang.Utils

newtype Id = Id { unId :: UUID.UUID }
    deriving (Eq, Ord, Show, Read)

data User = User
    { userID   :: Id
    , userNick :: Text
    }

data Candidate = Candidate
    { candidateUser  :: Id
    , candidateText  :: Text
    , candidateVotes :: Int
    }

data Story = Story
    { storySoFar      :: [Text]
    , storyCandidates :: HashMap Id Candidate
    , storyUsers      :: HashMap Id User
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

Aeson.deriveJSON (wwJSON $ delPrefix "user")      ''User
Aeson.deriveJSON (wwJSON $ delPrefix "candidate") ''Candidate
Aeson.deriveJSON (wwJSON $ delPrefix "story")     ''Story
