{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WordWang.Monad
    ( -- * Class
      RespRecipient(All, This)
    , MonadWW(patchRoom, respond, terminate, viewRoom)

      -- * State
    , WWState
    , wwRoom
    , wwPatches
    , wwResps
    , wwHasStoryChanged

      -- * The monad
    , WW
    , runWW

      -- * Operations
    , patchStoryAndRespond
    ) where

import           Control.Applicative                   (Applicative)
import           Control.Lens                          (makeLenses, use, (%=), (.=), Getting)
import           Control.Monad                         (when)
import           Control.Monad.Reader                  (ReaderT)
import           Control.Monad.State.Strict            (StateT(runStateT), runStateT, MonadState(get, put))
import           Control.Monad.Trans                   (MonadIO, lift)
import           Control.Monad.Trans.Either            (EitherT(runEitherT), left)
import           Control.Monad.Trans.Maybe             (runMaybeT)

import           WordWang.Bwd
import           WordWang.Messages
import           WordWang.Objects

data RespRecipient = All | This
    deriving (Eq, Show)

class Monad m => MonadWW m where
    patchRoom :: Patch -> m Bool
    respond   :: RespRecipient -> Resp  -> m ()
    terminate :: RespError -> m a
    viewRoom  :: Getting a Room a -> m a

data WWState = WWState
    { _wwRoom            :: !Room
    , _wwResps           :: !(Bwd (RespRecipient, Resp))
    , _wwPatches         :: !(Bwd Patch)
    , _wwHasStoryChanged :: !Bool
    } deriving (Eq, Show)

makeLenses ''WWState

newtype WW a =
    WW {unWW :: StateT WWState (EitherT RespError IO) a}
    deriving (Functor, Applicative, Monad, MonadIO)

runWW :: Room -> WW a -> IO (Either RespError (a, WWState))
runWW room m = do
    let wwst = WWState{ _wwRoom            = room
                      , _wwPatches         = B0
                      , _wwResps           = B0
                      , _wwHasStoryChanged = False
                      }
    runEitherT $ flip runStateT wwst $ unWW m

instance MonadWW WW where
    terminate = WW . lift . left

    patchRoom patch = do
        room <- WW $ use wwRoom
        case runMaybeT (applyPatch patch room) of
          Left err -> do
            terminate $ ErrorApplyingPatch err
          Right Nothing -> do
            return False
          Right (Just room') -> WW $ do
            wwRoom    .= room'
            wwPatches %= (:< patch)
            case patch of
              PStory _ -> wwHasStoryChanged .= True
              _        -> return ()
            return True

    respond recipient resp = WW $ do
        wwResps %= (:< (recipient, resp))

    viewRoom f = WW $ do
        use (wwRoom . f)

instance MonadState Room WW where
    get = WW $ use wwRoom
    put room = WW $ wwRoom .= room

instance MonadWW m => MonadWW (ReaderT a m) where
    terminate = lift . terminate
    patchRoom = lift . patchRoom
    respond recipient = lift . respond recipient
    viewRoom = lift . viewRoom

instance MonadWW m => MonadWW (StateT a m) where
    terminate = lift . terminate
    patchRoom = lift . patchRoom
    respond recipient = lift . respond recipient
    viewRoom = lift . viewRoom

patchStoryAndRespond :: MonadWW m => PatchStory -> m ()
patchStoryAndRespond patchStory = do
    let patch = PStory patchStory
    wasPatched <- patchRoom patch
    when wasPatched $ respond All $ RespPatch patch
