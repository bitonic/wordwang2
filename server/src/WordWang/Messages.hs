module WordWang.Messages
    ( Req(..)
    , reqStory
    , reqAuth
    , reqBody

    , ReqAuth(..)
    , reqAuthId
    , reqAuthSecret

    , ReqBody(..)

    , Resp(..)
    , respStory
    , respRecipients
    , respBody
    , RespError
    , RespRecipients(..)
    , RespBody(..)
    ) where

import           Control.Lens (makeLenses)
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson

import           WordWang.Objects
import           WordWang.Utils

data Req = Req
    { _reqStory :: Maybe StoryId
    , _reqAuth  :: Maybe ReqAuth
    , _reqBody  :: ReqBody
    }

data ReqStory = ReqStory
    { _reqStoryStory :: Story
    , _reqStoryAuth  :: Maybe ReqAuth
    }

data ReqAuth = ReqAuth
    { _reqAuthId     :: UserId
    , _reqAuthSecret :: UserSecret
    }

data ReqBody
    = ReqCreate
    | ReqJoin UserSecret
    | ReqCandidate CandidateBody
    | ReqVote UserId
    | ReqBlock Block

data RespRecipients = All | One UserId

data Resp = Resp
    { _respStory      :: StoryId
    , _respRecipients :: RespRecipients
    , _respBody       :: RespBody
    }

type RespError = Text
data RespBody
    = RespStory Story
    | RespError RespError
    | RespJoined UserId
    | RespCreated StoryId
    | RespOk

----------------------------------------------------------------------

Aeson.deriveJSON (wwJSON $ delPrefix "_req")      ''Req
Aeson.deriveJSON (wwJSON $ delPrefix "_reqStory") ''ReqStory
Aeson.deriveJSON (wwJSON $ delPrefix "_reqAuth")  ''ReqAuth

instance Aeson.ToJSON ReqBody where
    toJSON ReqCreate = tagObj "create" []
    toJSON (ReqJoin secret) = tagObj "join" ["secret" .= secret]
    toJSON (ReqCandidate body) = tagObj "candidate" ["body" .= body]
    toJSON (ReqVote uid) = tagObj "vote" ["userId" .= uid]
    toJSON (ReqBlock block) = tagObj "block" ["body" .= block]

instance Aeson.FromJSON ReqBody where
    parseJSON = parseTagged
        [ ("create",    parseNullary ReqCreate)
        , ("join",      parseUnary   ReqJoin      "secret")
        , ("candidate", parseUnary   ReqCandidate "body")
        , ("vote",      parseUnary   ReqVote      "userId")
        , ("block",     parseUnary   ReqBlock     "body")
        ]

instance Aeson.ToJSON RespBody where
    toJSON (RespStory story) = tagObj "story" ["contents" .= Aeson.toJSON story]
    toJSON (RespError err) = tagObj "error" ["message" .= Aeson.toJSON err]
    toJSON (RespJoined uid) = tagObj "joined" ["userId" .= Aeson.toJSON uid]
    toJSON (RespCreated sid) = tagObj "created" ["userId" .= Aeson.toJSON sid]
    toJSON RespOk = tagObj "ok" []

instance Aeson.FromJSON RespBody where
    parseJSON = parseTagged
        [ ("story", parseUnary RespStory "contents")
        , ("error", parseUnary RespError "message")
        , ("joined", parseUnary RespJoined "userId")
        , ("created", parseUnary RespCreated "storyId")
        , ("ok", parseNullary RespOk)
        ]

----------------------------------------------------------------------

makeLenses ''Req
makeLenses ''ReqAuth
makeLenses ''Resp
