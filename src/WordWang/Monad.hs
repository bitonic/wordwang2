{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WordWang.Monad
    ( -- * State
      WWState(..)
    , wwStoryState
    , wwChanged
    , wwStoryResps
    , wwUserResps

      -- * The monad
    , WW
    , runWW

      -- * Operations
    , terminate
    , respondToUser
    , respondToAll
    ) where

import           Control.Applicative (Applicative)

import           Control.Monad.Reader (ReaderT(runReaderT), MonadReader)
import           Control.Monad.State (StateT(runStateT), runStateT, MonadState)
import           Control.Monad.Trans (MonadIO)

import           Control.Lens (makeLenses, use, (%=), (.=))
import           Control.Monad.Trans.Either (EitherT(runEitherT), left)
import           Control.Monad.Trans.Maybe (runMaybeT)

import           WordWang.Bwd
import           WordWang.Messages
import           WordWang.State

data WWState = WWState
    { _wwStoryState :: !StoryState
    , _wwChanged    :: !Bool
    , _wwStoryResps :: !(Bwd StoryResp)
    , _wwUserResps  :: !(Bwd UserResp)
    } deriving (Eq, Show)

makeLenses ''WWState

newtype WW r a =
    WW {unWW :: EitherT RespError (ReaderT (Req r) (StateT WWState IO)) a}
    deriving (Functor, Applicative, Monad, MonadState WWState, MonadReader (Req r), MonadIO)

runWW :: Req r -> StoryState -> WW r a -> IO (Either RespError a, WWState)
runWW req storyState m = do
    let wwst = WWState{ _wwStoryState = storyState
                      , _wwChanged    = False
                      , _wwStoryResps = B0
                      , _wwUserResps  = B0
                      }
    flip runStateT wwst $ flip runReaderT req $ runEitherT $ unWW m

terminate :: RespError -> WW r a
terminate = WW . left

respondToAll :: StoryResp -> WW r ()
respondToAll resp = do
    storyState <- use wwStoryState
    case runMaybeT (applyStoryResp resp storyState) of
      Left err -> do
        terminate $ ErrorApplyingResp err
      Right Nothing -> do
        return ()
      Right (Just storyState') -> do
        wwStoryResps %= (:< resp)
        wwChanged    .= True
        wwStoryState .= storyState'

respondToUser :: UserResp -> WW r ()
respondToUser resp = wwUserResps %= (:< resp)
