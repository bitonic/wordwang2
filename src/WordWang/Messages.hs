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
    , respRecipients
    , respBody
    , RespError
    , RespRecipients(..)
    , RespBody(..)
    , respToThis
    , respToAll
    , respError
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
    } deriving (Eq, Show)

data ReqStory = ReqStory
    { _reqStoryStory :: Story
    , _reqStoryAuth  :: Maybe ReqAuth
    } deriving (Eq, Show)

data ReqAuth = ReqAuth
    { _reqAuthId     :: UserId
    , _reqAuthSecret :: UserSecret
    } deriving (Eq, Show)

data ReqBody
    = ReqCreate
    | ReqJoin
    | ReqCandidate CandidateBody
    | ReqVote UserId
    | ReqBlock Block
    deriving (Eq, Show)

data RespRecipients = All | This
    deriving (Eq, Show)

data Resp = Resp
    { _respRecipients :: RespRecipients
    , _respBody       :: RespBody
    } deriving (Eq, Show)

type RespError = Text
data RespBody
    = RespStory Story
    | RespError RespError
    | RespJoined UserId UserSecret
    | RespCreated StoryId
    | RespOk
    deriving (Eq, Show)

respToThis :: RespBody -> Resp
respToThis body = Resp{_respRecipients = This, _respBody = body}

respToAll :: RespBody -> Resp
respToAll body = Resp{_respRecipients = All, _respBody = body}

respError :: RespError -> Resp
respError err = respToThis (RespError err)

----------------------------------------------------------------------

Aeson.deriveJSON (wwJSON $ delPrefix "_req")      ''Req
Aeson.deriveJSON (wwJSON $ delPrefix "_reqStory") ''ReqStory
Aeson.deriveJSON (wwJSON $ delPrefix "_reqAuth")  ''ReqAuth

instance Aeson.ToJSON ReqBody where
    toJSON ReqCreate = tagObj "create" []
    toJSON ReqJoin = tagObj "join" []
    toJSON (ReqCandidate body) = tagObj "candidate" ["body" .= body]
    toJSON (ReqVote uid) = tagObj "vote" ["user" .= uid]
    toJSON (ReqBlock block) = tagObj "block" ["body" .= block]

instance Aeson.FromJSON ReqBody where
    parseJSON = parseTagged
        [ ("create",    parseNullary ReqCreate)
        , ("join",      parseNullary ReqJoin)
        , ("candidate", parseUnary   ReqCandidate "body")
        , ("vote",      parseUnary   ReqVote      "user")
        , ("block",     parseUnary   ReqBlock     "body")
        ]

instance Aeson.ToJSON RespBody where
    toJSON (RespStory story) = tagObj "story" ["contents" .= Aeson.toJSON story]
    toJSON (RespError err) = tagObj "error" ["message" .= Aeson.toJSON err]
    toJSON (RespJoined uid secret) =
        tagObj "joined" ["user" .= Aeson.toJSON uid, "secret" .= Aeson.toJSON secret]
    toJSON (RespCreated sid) = tagObj "created" ["story" .= Aeson.toJSON sid]
    toJSON RespOk = tagObj "ok" []

instance Aeson.FromJSON RespBody where
    parseJSON = parseTagged
        [ ("story", parseUnary RespStory "contents")
        , ("error", parseUnary RespError "message")
        , ("joined", parseBinary RespJoined "user" "secret")
        , ("created", parseUnary RespCreated "story")
        , ("ok", parseNullary RespOk)
        ]

----------------------------------------------------------------------

makeLenses ''Req
makeLenses ''ReqAuth
makeLenses ''Resp
