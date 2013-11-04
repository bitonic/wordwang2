module WordWang.Messages
    ( Req(..)
    , reqStory
    , reqAuth
    , reqBody

    , ReqAuth(..)
    , reqAuthUser
    , reqAuthSecret

    , ReqBody(..)

    , Resp(..)
    , respRecipients
    , respBody
    , RespError
    , RespRecipients(..)
    , RespBody(..)
    , respStory
    , respToThis
    , respToAll
    , respError
    , UserStory
    , uStoryBlocks
    , uStoryCandidates
    , uStoryId
    ) where

import           Data.HashMap.Strict (HashMap)

import           Control.Lens (makeLenses, (^.))
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson

import           WordWang.Objects
import           WordWang.Utils

data Req = Req
    { _reqStory :: !(Maybe StoryId)
    , _reqAuth  :: !(Maybe ReqAuth)
    , _reqBody  :: !ReqBody
    } deriving (Eq, Show)

data ReqAuth = ReqAuth
    { _reqAuthUser   :: !UserId
    , _reqAuthSecret :: !UserSecret
    } deriving (Eq, Show)

data ReqBody
    = ReqCreate
    | ReqJoin
    | ReqCandidate Block
    | ReqVote UserId
    | ReqCloseVoting -- TODO used for debugging, remove
    deriving (Eq, Show)

data RespRecipients = All | This
    deriving (Eq, Show)

data Resp = Resp
    { _respRecipients :: !RespRecipients
    , _respBody       :: !RespBody
    } deriving (Eq, Show)

type RespError = Text
data RespBody
    = RespStory !UserStory
    | RespError !RespError
    | RespJoined !UserId !UserSecret
    | RespCreated !StoryId
    | RespVotingClosed !Block
    | RespCandidate !Candidate
    | RespVote {- Candidate -} !UserId {- Vote -} !UserId
    deriving (Eq, Show)

data UserStory = UserStory
    { _uStoryId         :: !StoryId
    , _uStoryBlocks     :: ![Block]
    , _uStoryCandidates :: !(HashMap UserId Candidate)
    } deriving (Eq, Show)

respStory :: Story -> RespBody
respStory story =
    RespStory UserStory{ _uStoryId         = story^.storyId
                       , _uStoryBlocks     = story^.storyBlocks
                       , _uStoryCandidates = story^.storyCandidates
                       }

respToThis :: RespBody -> Resp
respToThis body = Resp{_respRecipients = This, _respBody = body}

respToAll :: RespBody -> Resp
respToAll body = Resp{_respRecipients = All, _respBody = body}

respError :: RespError -> Resp
respError err = respToThis (RespError err)

----------------------------------------------------------------------

Aeson.deriveFromJSON (wwJSON $ delPrefix "_req")      ''Req
Aeson.deriveFromJSON (wwJSON $ delPrefix "_reqAuth")  ''ReqAuth

instance Aeson.FromJSON ReqBody where
    parseJSON = parseTagged
        [ ("create",      parseNullary ReqCreate)
        , ("join",        parseNullary ReqJoin)
        , ("candidate",   parseUnary   ReqCandidate "block")
        , ("vote",        parseUnary   ReqVote      "user")
        , ("closeVoting", parseNullary ReqCloseVoting)
        ]

Aeson.deriveToJSON (wwJSON $ delPrefix "_uStory") ''UserStory

instance Aeson.ToJSON RespBody where
    toJSON (RespStory story) = tagObj "story" ["body" .= Aeson.toJSON story]
    toJSON (RespError err) = tagObj "error" ["message" .= Aeson.toJSON err]
    toJSON (RespJoined uid secret) =
        tagObj "joined" [ "user"   .= Aeson.toJSON uid
                        , "secret" .= Aeson.toJSON secret
                        ]
    toJSON (RespCreated sid) = tagObj "created" ["story" .= Aeson.toJSON sid]
    toJSON (RespVotingClosed block) =
        tagObj "votingClosed" ["block" .= Aeson.toJSON block]
    toJSON (RespCandidate cand) =
        tagObj "candidate" ["body" .= Aeson.toJSON cand]
    toJSON (RespVote candUid voteUid) =
        tagObj "vote" [ "candidate" .= Aeson.toJSON candUid
                      , "vote"      .= Aeson.toJSON voteUid
                      ]

----------------------------------------------------------------------

makeLenses ''Req
makeLenses ''ReqAuth
makeLenses ''Resp
makeLenses ''UserStory
