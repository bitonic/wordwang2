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
runWW req root m = do
    let wwst = WWState{ _wwReq     = req
                      , _wwRoom    = root
                      , _wwPatches = B0
                      , _wwResps   = B0
                      }
    runEitherT $ flip runStateT wwst $ unWW m

terminate :: RespError -> WW a
terminate = WW . lift . left

patchRoom :: Patch -> WW ()
patchRoom patch = do
    root <- use wwRoom
    case runMaybeT (applyPatch patch root) of
      Left err -> do
        terminate $ ErrorApplyingPatch err
      Right Nothing -> do
        return ()
      Right (Just root') -> do
        wwRoom    .= root'
        wwPatches %= (:< patch)

respond :: RespRecipient -> Resp -> WW ()
respond recipient resp =
    wwResps %= (:< (recipient, resp))

patchStoryAndRespond :: PatchStory -> WW ()
patchStoryAndRespond patchStory = do
    let patch = PStory patchStory
    patchRoom patch
    respond All $ RespPatch patch
