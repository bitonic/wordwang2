module WordWang.State
    ( StoryState(..)
    , ssStory
    , ssUsers

    , ApplyResp
    , applyStoryResp
    , applyUserResp
    ) where

import           Control.Monad.Trans (lift)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import           Control.Monad.Trans.Maybe (MaybeT(MaybeT))
import           Control.Lens (makeLenses, at, (.~), (?~), (%~), (^.))

import           WordWang.Messages
import           WordWang.Objects


data StoryState = StoryState
    { _ssStory       :: !Story
    , _ssUsers       :: !(HashMap UserId User)
    } deriving (Eq, Show)

makeLenses ''StoryState


type ApplyResp resp state = resp -> state -> MaybeT (Either String) state

nothing :: Monad m => MaybeT m a
nothing = MaybeT $ return Nothing

applyStoryResp :: ApplyResp StoryResp StoryState
applyStoryResp (StoryRespJoined _uid) _storyState = do
    nothing
applyStoryResp (StoryRespVotingClosed block) storyState = do
    return $ (ssStory . sCandidates .~ HashMap.empty)
           . (ssStory . sBlocks     %~ (++ [block]))
           $ storyState
applyStoryResp (StoryRespNewCandidate candId cand) storyState = do
    let mbCand = storyState ^. ssStory ^. sCandidates ^. at candId
    case mbCand of
      Just _ -> do
        -- The user has already proposed a candidate.
        nothing
      Nothing -> do
        return $ (ssStory . sCandidates . at candId ?~ cand) $ storyState
applyStoryResp (StoryRespVote candId userId) storyState = do
    case storyState ^. ssStory ^. sCandidates ^. at candId of
      Nothing -> do
        lift $ Left $ "candidate " ++ show candId ++ " not present."
      Just cand | HashSet.member userId (cand ^. cVotes) -> do
        -- The user has already voted.
        nothing
      Just cand -> do
        let cand' = cVotes %~ HashSet.insert userId $ cand
        return $ (ssStory . sCandidates . at candId ?~ cand') $ storyState

applyUserResp :: ApplyResp UserResp StoryState
applyUserResp (UserRespJoined uid user) storyState = do
    -- TODO should we check that the user doesn't exist?
    return $ (ssUsers . at uid ?~ user) $ storyState
applyUserResp (UserRespStory _story) _storyState = do
    nothing
