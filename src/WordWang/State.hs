module WordWang.State
    ( StoryState(..)
    , ssStory
    , ssUsers
    , RootState(..)
    , rsStories

    , ApplyResp
    , applyStoryResp
    , applyUserResp
    ) where

import           Control.Concurrent.MVar (MVar)

import           Data.HashMap.Strict (HashMap)

import           Control.Lens (makeLenses)

import           WordWang.Messages
import           WordWang.Objects

data StoryState = StoryState
    { _ssStory :: !Story
    , _ssUsers :: !(HashMap UserId User)
    } deriving (Eq, Show)

data RootState = RootState
    { _rsStories :: !(HashMap StoryId (MVar StoryState))
    }

type ApplyResp resp state = resp -> state -> Either String (Maybe state)

applyStoryResp :: ApplyResp StoryResp StoryState
applyStoryResp = undefined

applyUserResp :: ApplyResp UserResp StoryState
applyUserResp = undefined

------------------------------------------------------------------------
-- lenses

makeLenses ''StoryState
makeLenses ''RootState
