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

import           Control.Monad.Trans (lift)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import           Control.Monad.Trans.Either (EitherT, left)
import           Control.Lens (makeLenses, at, (.~), (?~), (%~), (^.))

import           WordWang.Messages
import           WordWang.Objects

data StoryState = StoryState
    { _ssStory :: !Story
    , _ssUsers :: !(HashMap UserId User)
    } deriving (Eq, Show)

data RootState = RootState
    { _rsStories :: !(HashMap StoryId (MVar StoryState))
    }

makeLenses ''StoryState
makeLenses ''RootState


type ApplyResp resp state = resp -> state -> EitherT String Maybe state

applyStoryResp :: ApplyResp StoryResp StoryState
applyStoryResp (StoryRespJoined _uid) _storyState =
    lift Nothing
applyStoryResp (StoryRespVotingClosed block) storyState =
    return $ (ssStory . sCandidates .~ HashMap.empty)
           . (ssStory . sBlocks     %~ (++ [block]))
           $ storyState
applyStoryResp (StoryRespNewCandidate candId cand) storyState = do
    let mbCand = storyState ^. ssStory ^. sCandidates ^. at candId
    case mbCand of
      Just _ ->
        -- The user has already proposed a candidate.
        lift Nothing
      Nothing -> do
        return $ (ssStory . sCandidates . at candId ?~ cand) $ storyState
applyStoryResp (StoryRespVote candId userId) storyState =
    case storyState ^. ssStory ^. sCandidates ^. at candId of
      Nothing ->
        left $ "candidate " ++ show candId ++ " not present."
      Just cand | HashSet.member userId (cand ^. cVotes) ->
        -- The user has already voted.
        lift Nothing
      Just cand -> do
        let cand' = cVotes %~ HashSet.insert userId $ cand
        return $ (ssStory . sCandidates . at candId ?~ cand') $ storyState

applyUserResp :: ApplyResp UserResp StoryState
applyUserResp (UserRespJoined uid user) storyState =
    -- TODO should we check that the user doesn't exist?
    return $ (ssUsers . at uid ?~ user) $ storyState
applyUserResp (UserRespStory _story) _storyState =
    lift Nothing
