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

      -- * Join request/response
    , JoinReq(..)
    , JoinResp

    , DBResp(..)
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
    | StoryReqStory
    | StoryReqCloseVoting
    deriving (Eq, Show)

data StoryResp
    = StoryRespStory !Story
    | StoryRespJoined !UserId
    | StoryRespVotingClosed !Block
    | StoryRespNewCandidate !Candidate
    | StoryRespVote !CandidateId !UserId
    deriving (Eq, Show)

------------------------------------------------------------------------
-- Stories

data JoinReq = JoinReq
    deriving (Eq, Show)

data JoinResp = JoinResp !UserId !UserSecret
    deriving (Eq, Show)

------------------------------------------------------------------------
-- Stuff stored in db

data DBResp
    = DBRespStory !StoryResp
    | DBRespJoin  !JoinResp
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
        StoryReqStory          -> ("story",       [])
        StoryReqCloseVoting    -> ("closeVoting", [])

instance Aeson.FromJSON StoryReq where
    parseJSON = parseTagged
        [ ("join",        parseNullary StoryReqJoin)
        , ("candidate",   parseUnary   StoryReqCandidate "block")
        , ("vote",        parseUnary   StoryReqVote      "user")
        , ("story",       parseNullary StoryReqStory)
        , ("closeVoting", parseNullary StoryReqCloseVoting)
        ]

instance Aeson.ToJSON StoryResp where
    toJSON = toTaggedJSON $ \case
        StoryRespStory story         -> ("story",        ["body" .= story])
        StoryRespJoined uid          -> ("joined",       ["user" .= uid ])
        StoryRespVotingClosed block  -> ("votingClosed", ["block" .= block])
        StoryRespNewCandidate cand   -> ("candidate",    ["body" .= cand])
        StoryRespVote candId voteUid -> ("vote",         ["candidate" .= candId , "vote" .= voteUid])

instance Aeson.FromJSON StoryResp where
    parseJSON = parseTagged
        [ ("story",        parseUnary  StoryRespStory "body")
        , ("joined",       parseUnary  StoryRespJoined "user")
        , ("votingClosed", parseUnary  StoryRespVotingClosed "block")
        , ("newCandidate", parseUnary  StoryRespNewCandidate "body")
        , ("vote",         parseBinary StoryRespVote "candidate" "vote")
        ]

instance Aeson.ToJSON JoinReq where
    toJSON = toTaggedJSON $ \case
        JoinReq -> ("join", [])

instance Aeson.FromJSON JoinReq where
    parseJSON = parseTagged [ ("join", parseNullary JoinReq) ]

Aeson.deriveJSON (wwJSON $ delPrefix "_joinResp") ''JoinResp

instance Aeson.ToJSON DBResp where
    toJSON = toTaggedJSON $ \case
        DBRespStory storyResp -> ("story", ["body" .= storyResp])
        DBRespJoin  joinResp  -> ("join",  ["body" .= joinResp])

instance Aeson.FromJSON DBResp where
    parseJSON = parseTagged
        [ ("story",        parseUnary  DBRespStory "body")
        , ("join",         parseUnary  DBRespStory "body")
        ]

----------------------------------------------------------------------

makeLenses ''Req
makeLenses ''ReqAuth
makeLenses ''Resp

