{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module WordWang.Messages
    ( ClientMsg(..)
    , clientMsgStory
    , clientMsgAuth
    , clientMsgBody

    , ClientAuth(..)
    , clientAuthId
    , clientAuthSecret

    , ClientMsgBody(..)

    , ServerError
    , ServerMsg(..)
    ) where

import           Control.Lens (makeLenses)
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson

import           WordWang.Objects
import           WordWang.Utils

data ClientMsg = ClientMsg
    { _clientMsgStory :: StoryId
    , _clientMsgAuth  :: Maybe ClientAuth
    , _clientMsgBody  :: ClientMsgBody
    }

data ClientAuth = ClientAuth
    { _clientAuthId     :: UserId
    , _clientAuthSecret :: UserSecret
    }

data ClientMsgBody
    = CMJoin
    | CMCandidate CandidateBody
    | CMVote UserId
    | CMBlock Block

type ServerError = Text
data ServerMsg
    = SMOk Story
    | SMError ServerError

----------------------------------------------------------------------

Aeson.deriveJSON (wwJSON $ delPrefix "_clientMsg")  ''ClientMsg
Aeson.deriveJSON (wwJSON $ delPrefix "_clientAuth") ''ClientAuth

instance Aeson.ToJSON ClientMsgBody where
    toJSON CMJoin = tagObj "join" []
    toJSON (CMCandidate body) = tagObj "candidate" ["body" .= body]
    toJSON (CMVote uid) = tagObj "vote" ["userId" .= uid]
    toJSON (CMBlock block) = tagObj "block" ["body" .= block]

instance Aeson.FromJSON ClientMsgBody where
    parseJSON = parseTagged
        [ ("join",      parseNullary CMJoin)
        , ("candidate", parseUnary   CMCandidate "body")
        , ("vote",      parseUnary   CMVote      "userId")
        , ("block",     parseUnary   CMBlock     "body")
        ]

instance Aeson.ToJSON ServerMsg where
    toJSON (SMOk story)  = tagObj "ok" ["story" .= Aeson.toJSON story]
    toJSON (SMError err) = tagObj "error" ["message" .= Aeson.toJSON err]

instance Aeson.FromJSON ServerMsg where
    parseJSON = parseTagged
        [ ("ok",    parseUnary SMOk    "story")
        , ("error", parseUnary SMError "message")
        ]

----------------------------------------------------------------------

makeLenses ''ClientMsg
makeLenses ''ClientAuth
