{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WordWang.Monad
    ( -- * State
      RespRecipient(..)
    , WWState
    , wwReq
    , wwRoom
    , wwPatches
    , wwResps

      -- * The monad
    , WW
    , runWW

      -- * Operations
    , terminate
    , patchRoom
    , respond
    , patchStoryAndRespond
    ) where

import           Control.Applicative (Applicative)
import           Control.Monad (when)

import           Control.Monad.State.Strict (StateT(runStateT), runStateT, MonadState)
import           Control.Monad.Trans (MonadIO, lift)
import           Control.Monad.Trans.Maybe (runMaybeT)

import           Control.Lens (makeLenses, use, (%=), (.=))
import           Control.Monad.Trans.Either (EitherT(runEitherT), left)

import           WordWang.Objects
import           WordWang.Bwd
import           WordWang.Messages

data RespRecipient = All | This
    deriving (Eq, Show)

data WWState = WWState
    { _wwReq     :: !Req
    , _wwRoom    :: !Room
    , _wwResps   :: !(Bwd (RespRecipient, Resp))
    , _wwPatches :: !(Bwd Patch)
    } deriving (Eq, Show)

makeLenses ''WWState

newtype WW a =
    WW {unWW :: StateT WWState (EitherT RespError IO) a}
    deriving (Functor, Applicative, Monad, MonadState WWState, MonadIO)

runWW :: Req -> Room -> WW a -> IO (Either RespError (a, WWState))
runWW req room m = do
    let wwst = WWState{ _wwReq     = req
                      , _wwRoom    = room
                      , _wwPatches = B0
                      , _wwResps   = B0
                      }
    runEitherT $ flip runStateT wwst $ unWW m

terminate :: RespError -> WW a
terminate = WW . lift . left

patchRoom :: Patch -> WW Bool
patchRoom patch = do
    room <- use wwRoom
    case runMaybeT (applyPatch patch room) of
      Left err -> do
        terminate $ ErrorApplyingPatch err
      Right Nothing -> do
        return False
      Right (Just room') -> do
        wwRoom    .= room'
        wwPatches %= (:< patch)
        return True

respond :: RespRecipient -> Resp -> WW ()
respond recipient resp =
    wwResps %= (:< (recipient, resp))

patchStoryAndRespond :: PatchStory -> WW ()
patchStoryAndRespond patchStory = do
    let patch = PStory patchStory
    wasPatched <- patchRoom patch
    when wasPatched $ respond All $ RespPatch patch
