{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module WordWang.Messages
    ( Req(..)
    , reqStory
    , reqAuth
    , reqBody

    , ReqAuth(..)
    , reqAuthId
    , reqAuthSecret

    , ReqBody(..)

    , RespError
    , Resp(..)
    ) where

import           Control.Lens (makeLenses)
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson

import           WordWang.Objects
import           WordWang.Utils

data Req = Req
    { _reqStory :: StoryId
    , _reqAuth  :: Maybe ReqAuth
    , _reqBody  :: ReqBody
    }

data ReqAuth = ReqAuth
    { _reqAuthId     :: UserId
    , _reqAuthSecret :: UserSecret
    }

data ReqBody
    = ReqJoin
    | ReqCandidate CandidateBody
    | ReqVote UserId
    | ReqBlock Block

type RespError = Text
data Resp
    = RespOk Story
    | RespError RespError

----------------------------------------------------------------------

Aeson.deriveJSON (wwJSON $ delPrefix "_req")     ''Req
Aeson.deriveJSON (wwJSON $ delPrefix "_reqAuth") ''ReqAuth

instance Aeson.ToJSON ReqBody where
    toJSON ReqJoin = tagObj "join" []
    toJSON (ReqCandidate body) = tagObj "candidate" ["body" .= body]
    toJSON (ReqVote uid) = tagObj "vote" ["userId" .= uid]
    toJSON (ReqBlock block) = tagObj "block" ["body" .= block]

instance Aeson.FromJSON ReqBody where
    parseJSON = parseTagged
        [ ("join",      parseNullary ReqJoin)
        , ("candidate", parseUnary   ReqCandidate "body")
        , ("vote",      parseUnary   ReqVote      "userId")
        , ("block",     parseUnary   ReqBlock     "body")
        ]

instance Aeson.ToJSON Resp where
    toJSON (RespOk story)  = tagObj "ok" ["story" .= Aeson.toJSON story]
    toJSON (RespError err) = tagObj "error" ["message" .= Aeson.toJSON err]

instance Aeson.FromJSON Resp where
    parseJSON = parseTagged
        [ ("ok",    parseUnary RespOk    "story")
        , ("error", parseUnary RespError "message")
        ]

----------------------------------------------------------------------

makeLenses ''Req
makeLenses ''ReqAuth
