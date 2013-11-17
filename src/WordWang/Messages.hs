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
    , RespError(..)
    , RespRecipients(..)
    , RespBody(..)
    , respStory
    , respToThis
    , respToAll
    , respError
    , UserStory
    , uStoryBlocks
    , uStoryUsers
    , uStoryCandidates
    , uStoryId
    ) where

import           Data.Foldable (Foldable)

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Traversable (Traversable)

import           Control.Lens (makeLenses, (^.))
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson

import           WordWang.Objects
import           WordWang.Utils

data Req = Req
    { _reqStory :: !StoryId
    , _reqAuth  :: !(Maybe ReqAuth)
    , _reqBody  :: !ReqBody
    } deriving (Eq, Show)

data ReqAuth = ReqAuth
    { _reqAuthUser   :: !UserId
    , _reqAuthSecret :: !UserSecret
    } deriving (Eq, Show)

data ReqBody
    = ReqJoin
    | ReqCandidate Block
    | ReqVote UserId
    | ReqStory
    | ReqCloseVoting
    deriving (Eq, Show)

data RespRecipients conn = All | This conn
    deriving (Eq, Show, Functor, Foldable, Traversable)

data Resp conn = Resp
    { _respRecipients :: !(RespRecipients conn)
    , _respBody       :: !RespBody
    } deriving (Eq, Show, Functor, Foldable, Traversable)

data RespError
    = StoryNotPresent !StoryId
    | InternalError !Text
    | NoCredentials
    | InvalidCredentials
    | ErrorDecodingReq !Text
    | NoStory
    deriving (Eq, Show)

data RespBody
    = RespStory !UserStory
    | RespError !RespError
    | RespJoined !UserId !UserSecret
    | RespUser !UserId
    | RespVotingClosed !Block
    | RespCandidate !Candidate
    | RespVote {- Candidate -} !UserId {- Vote -} !UserId
    deriving (Eq, Show)

data UserStory = UserStory
    { _uStoryId           :: !StoryId
    , _uStoryUsers        :: !(HashSet UserId)
    , _uStoryBlocks       :: ![Block]
    , _uStoryCandidates   :: !(HashMap UserId Candidate)
    } deriving (Eq, Show)

respStory :: Story -> RespBody
respStory story = RespStory UserStory
    { _uStoryId         = story^.storyId
    , _uStoryUsers      = HashSet.fromList (HashMap.keys (story^.storyUsers))
    , _uStoryBlocks     = story^.storyBlocks
    , _uStoryCandidates = story^.storyCandidates
    }

respToThis :: RespBody -> Resp ()
respToThis body = Resp{_respRecipients = This (), _respBody = body}

respToAll :: RespBody -> Resp a
respToAll body = Resp{_respRecipients = All, _respBody = body}

respError :: RespError -> Resp ()
respError err = respToThis (RespError err)

----------------------------------------------------------------------

Aeson.deriveJSON (wwJSON $ delPrefix "_req")      ''Req
Aeson.deriveJSON (wwJSON $ delPrefix "_reqAuth")  ''ReqAuth

instance Aeson.ToJSON ReqBody where
    toJSON = toTaggedJSON $ \case
        ReqJoin           -> ("join",        [])
        ReqCandidate cand -> ("candidate",   ["body" .= cand])
        ReqVote vote      -> ("vote",        ["user" .= vote])
        ReqStory          -> ("story",       [])
        ReqCloseVoting    -> ("closeVoting", [])

instance Aeson.FromJSON ReqBody where
    parseJSON = parseTagged
        [ ("join",        parseNullary ReqJoin)
        , ("candidate",   parseUnary   ReqCandidate "block")
        , ("vote",        parseUnary   ReqVote      "user")
        , ("story",       parseNullary ReqStory)
        , ("closeVoting", parseNullary ReqCloseVoting)
        ]

instance Aeson.ToJSON RespBody where
    toJSON = toTaggedJSON $ \case
        RespStory story          -> ("story",        ["body" .= story])
        RespError err            -> ("error",        ["error" .= err])
        RespJoined uid secret    -> ("joined",       ["user" .= uid , "secret" .= secret])
        RespUser uid             -> ("user",         ["user" .= uid])
        RespVotingClosed block   -> ("votingClosed", ["block" .= block])
        RespCandidate cand       -> ("candidate",    ["body" .= cand])
        RespVote candUid voteUid -> ("vote",         ["candidate" .= candUid , "vote" .= voteUid])

instance Aeson.FromJSON RespBody where
    parseJSON = parseTagged
        [ ("story",        parseUnary  RespStory "body")
        , ("joined",       parseBinary RespJoined "user" "secret")
        , ("user",         parseUnary  RespUser "user")
        , ("votingClosed", parseUnary  RespVotingClosed "block")
        , ("candidate",    parseUnary  RespCandidate "body")
        , ("vote",         parseBinary RespVote "candidate" "vote")
        , ("error",        parseUnary  RespError "error")
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

Aeson.deriveJSON (wwJSON $ delPrefix "_uStory") ''UserStory

----------------------------------------------------------------------

makeLenses ''Req
makeLenses ''ReqAuth
makeLenses ''Resp
makeLenses ''UserStory
