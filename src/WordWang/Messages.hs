module WordWang.Messages
    ( -- * Request
      Req(..)
    , reqRoomId
    , reqAuth
    , reqBody
    , ReqAuth(..)
    , reqAuthUserId
    , reqAuthSecret
    , ReqBody(..)

      -- * Response
    , Resp(..)
    , RespError(..)
    ) where

import           Data.Typeable                         (Typeable)
import           Control.Lens                          (makeLenses)
import           Data.Aeson                            ((.=))
import qualified Data.Aeson                            as Aeson
import qualified Data.Aeson.TH                         as Aeson

import           WordWang.Objects
import           WordWang.JSON

------------------------------------------------------------------------
-- Request

data Req = Req
    { _reqRoomId :: !RoomId
    , _reqAuth   :: !(Maybe ReqAuth)
    , _reqBody   :: !ReqBody
    } deriving (Eq, Show, Typeable)

data ReqAuth = ReqAuth
    { _reqAuthUserId :: !UserId
    , _reqAuthSecret :: !UserSecret
    } deriving (Eq, Show, Typeable)

data ReqBody
    = ReqCandidate !Block
    | ReqCloseVoting
    | ReqVote !CandidateId
    | ReqStory
    | ReqJoin
    deriving (Eq, Show, Typeable)

data Resp
    = RespPatch !Patch
    | RespStory !Story
    | RespJoin !UserId !User
    | RespError !RespError
    deriving (Eq, Show, Typeable)

data RespError
    = RoomNotPresent !RoomId
    | InternalError !String
    | NoCredentials
    | InvalidCredentials
    | ErrorDecodingReq !String
    | ErrorApplyingPatch !String
    deriving (Eq, Show, Typeable)

------------------------------------------------------------------------
-- JSON instances

Aeson.deriveJSON (wwJSON $ delPrefix "_req")      ''Req
Aeson.deriveJSON (wwJSON $ delPrefix "_reqAuth")  ''ReqAuth

instance Aeson.ToJSON ReqBody where
    toJSON = toTaggedJSON $ \case
        ReqCandidate block -> ("candidate",   ["block" .= block])
        ReqCloseVoting     -> ("closeVoting", [])
        ReqVote candId     -> ("vote",        ["candidateId" .= candId])
        ReqStory           -> ("story",       [])
        ReqJoin            -> ("join",        [])

instance Aeson.FromJSON ReqBody where
    parseJSON = parseTagged
        [ ("candidate",   parseUnary   ReqCandidate "block")
        , ("closeVoting", parseNullary ReqCloseVoting)
        , ("vote",        parseUnary   ReqVote "candidateId")
        , ("story",       parseNullary ReqStory)
        , ("join",        parseNullary ReqJoin)
        ]

instance Aeson.ToJSON Resp where
    toJSON = toTaggedJSON $ \case
        RespJoin userId user ->
          ("join",  ["userId" .= userId, "user" .= user])
        RespStory story ->
          ("story", ["body" .= story])
        RespPatch patch ->
          ("patch", ["body" .= patch])
        RespError err ->
          ("error", ["body" .= err])

instance Aeson.FromJSON Resp where
    parseJSON = parseTagged
        [ ("join",   parseBinary RespJoin  "userId" "user")
        , ("story",  parseUnary  RespStory "body")
        , ("patch",  parseUnary  RespPatch "body")
        , ("error",  parseUnary  RespError "body")
        ]

instance Aeson.ToJSON RespError where
    toJSON = toTaggedJSON $ \case
        RoomNotPresent roomId  -> ("roomNotPresent",     ["roomId" .= roomId])
        InternalError err      -> ("internalError",      ["msg" .= err])
        NoCredentials          -> ("noCredentials",      [])
        InvalidCredentials     -> ("invalidCredentials", [])
        ErrorDecodingReq err   -> ("errorDecodingReq",   ["msg" .= err])
        ErrorApplyingPatch err -> ("errorApplyingResp",  ["msg" .= err])

instance Aeson.FromJSON RespError where
    parseJSON = parseTagged
        [ ("roomNotPresent",     parseUnary   RoomNotPresent "roomId")
        , ("internalError",      parseUnary   InternalError "msg")
        , ("noCredentials",      parseNullary NoCredentials)
        , ("invalidCredentials", parseNullary InvalidCredentials)
        , ("errorDecodingReq",   parseUnary   ErrorDecodingReq "msg")
        , ("errorApplyingPatch", parseUnary   ErrorApplyingPatch "msg")
        ]

----------------------------------------------------------------------

makeLenses ''Req
makeLenses ''ReqAuth
makeLenses ''Resp
