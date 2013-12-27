module WordWang.Messages
    ( -- * Generic request/response
      Req(..)
    , reqStory
    , reqAuth
    , reqBody

    , ReqAuth(..)
    , reqAuthUser
    , reqAuthSecret

    , Resp(..)
    , RespError(..)

      -- * Story request/response
    , StoryReq(..)
    , StoryResp(..)

      -- * User request/response
    , UserReq(..)
    , UserResp(..)
    ) where

import           Data.Foldable (Foldable)
import           Data.Traversable (Traversable)

import           Control.Lens (makeLenses)
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson

import           WordWang.Objects
import           WordWang.Utils

------------------------------------------------------------------------
-- Generic request/response

data Req r = Req
    { _reqStory :: !StoryId
    , _reqAuth  :: !(Maybe ReqAuth)
    , _reqBody  :: !r
    } deriving (Eq, Show, Functor, Foldable, Traversable)

data ReqAuth = ReqAuth
    { _reqAuthUser   :: !UserId
    , _reqAuthSecret :: !UserSecret
    } deriving (Eq, Show)

data Resp r
    = RespOK !r
    | RespError !RespError
    deriving (Eq, Show, Functor, Foldable, Traversable)

data RespError
    = StoryNotPresent !StoryId
    | InternalError !Text
    | NoCredentials
    | InvalidCredentials
    | ErrorDecodingReq !Text
    | NoStory
    deriving (Eq, Show)

------------------------------------------------------------------------
-- Stories

data StoryReq
    = StoryReqJoin
    | StoryReqCandidate Block
    | StoryReqVote UserId
    | StoryReqCloseVoting
    deriving (Eq, Show)

data StoryResp
    = StoryRespJoined !UserId
    | StoryRespVotingClosed !Block
    | StoryRespNewCandidate !Candidate
    | StoryRespVote !CandidateId !UserId
    deriving (Eq, Show)

------------------------------------------------------------------------
-- Stories

data UserReq
    = UserReqJoin
    | UserReqStory
    deriving (Eq, Show)

data UserResp
    = UserRespJoined !UserId !UserSecret
    | UserRespStory !Story
    deriving (Eq, Show)

------------------------------------------------------------------------
-- JSON instances

Aeson.deriveJSON (wwJSON $ delPrefix "_req")      ''Req
Aeson.deriveJSON (wwJSON $ delPrefix "_reqAuth")  ''ReqAuth

instance Aeson.ToJSON r => Aeson.ToJSON (Resp r) where
    toJSON = toTaggedJSON $ \case
        RespOK resp   -> ("ok",    ["body" .= Aeson.toJSON resp])
        RespError err -> ("error", ["error" .= Aeson.toJSON err])

instance Aeson.FromJSON r => Aeson.FromJSON (Resp r) where
    parseJSON = parseTagged
        [ ("ok",    parseUnary RespOK "body")
        , ("error", parseUnary RespError "error")
        ]

instance Aeson.ToJSON RespError where
    toJSON = toTaggedJSON $ \case
        StoryNotPresent sid -> ("storyNotPresent",    ["story" .= sid])
        InternalError msg   -> ("internalError",      ["msg" .= msg])
        NoCredentials       -> ("noCredentials",      [])
        InvalidCredentials  -> ("invalidCredentials", [])
        ErrorDecodingReq t  -> ("errorDecodingReq",   ["msg" .= Aeson.toJSON t])
        NoStory             -> ("noStory",            [])

instance Aeson.FromJSON RespError where
    parseJSON = parseTagged
        [ ("storyNotPresent",    parseUnary   StoryNotPresent "story")
        , ("internalError",      parseUnary   InternalError "msg")
        , ("noCredentials",      parseNullary NoCredentials)
        , ("invalidCredentials", parseNullary InvalidCredentials)
        , ("errorDecodingReq",   parseUnary   ErrorDecodingReq "msg")
        , ("noStory",            parseNullary NoStory)
        ]

instance Aeson.ToJSON StoryReq where
    toJSON = toTaggedJSON $ \case
        StoryReqJoin           -> ("join",        [])
        StoryReqCandidate cand -> ("candidate",   ["body" .= cand])
        StoryReqVote vote      -> ("vote",        ["user" .= vote])
        StoryReqCloseVoting    -> ("closeVoting", [])

instance Aeson.FromJSON StoryReq where
    parseJSON = parseTagged
        [ ("join",        parseNullary StoryReqJoin)
        , ("candidate",   parseUnary   StoryReqCandidate "block")
        , ("vote",        parseUnary   StoryReqVote      "user")
        , ("closeVoting", parseNullary StoryReqCloseVoting)
        ]

instance Aeson.ToJSON StoryResp where
    toJSON = toTaggedJSON $ \case
        StoryRespJoined uid          -> ("joined",       ["user" .= uid ])
        StoryRespVotingClosed block  -> ("votingClosed", ["block" .= block])
        StoryRespNewCandidate cand   -> ("candidate",    ["body" .= cand])
        StoryRespVote candId voteUid -> ("vote",         ["candidate" .= candId , "vote" .= voteUid])

instance Aeson.FromJSON StoryResp where
    parseJSON = parseTagged
        [ ("joined",       parseUnary  StoryRespJoined "user")
        , ("votingClosed", parseUnary  StoryRespVotingClosed "block")
        , ("newCandidate", parseUnary  StoryRespNewCandidate "body")
        , ("vote",         parseBinary StoryRespVote "candidate" "vote")
        ]

instance Aeson.ToJSON UserReq where
    toJSON = toTaggedJSON $ \case
        UserReqJoin  -> ("join",  [])
        UserReqStory -> ("story", [])

instance Aeson.FromJSON UserReq where
    parseJSON = parseTagged
        [ ("join",  parseNullary UserReqJoin)
        , ("story", parseNullary UserReqStory)
        ]

instance Aeson.ToJSON UserResp where
    toJSON = toTaggedJSON $ \case
        UserRespJoined uid secret -> ("joined", ["user" .= uid, "secret" .= secret])
        UserRespStory story       -> ("story",  ["body" .= story])

instance Aeson.FromJSON UserResp where
    parseJSON = parseTagged
        [ ("joined", parseBinary UserRespJoined "user" "secret")
        , ("story",  parseUnary  UserRespStory "body")
        ]

----------------------------------------------------------------------

makeLenses ''Req
makeLenses ''ReqAuth
makeLenses ''Resp
