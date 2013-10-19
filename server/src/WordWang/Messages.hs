{-# LANGUAGE TemplateHaskell #-}
module WordWang.Messages
    ( ClientMsg(..)
    , clientMsgStory
    , clientMsgAuth
    , clientMsgBody

    , ClientAuth(..)
    , clientAuthId
    , clientAuthSecret

    , ClientMsgBody(Join, NewCandidate, Vote, Refresh)
    ) where

import           Control.Lens (makeLenses)
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
    = Join
    | NewCandidate CandidateBody
    | Vote UserId
    | Refresh

----------------------------------------------------------------------

Aeson.deriveJSON (wwJSON $ delPrefix "_clientMsg")    ''ClientMsg
Aeson.deriveJSON (wwJSON $ delPrefix "_clientAuth")   ''ClientAuth

----------------------------------------------------------------------

makeLenses ''ClientMsg
makeLenses ''ClientAuth
