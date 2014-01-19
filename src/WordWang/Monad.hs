{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WordWang.Monad
    ( -- * The monad
      WW
      -- ** State
    , RespRecipient(All, This)
    , WWState
    , wwRoomId
    , wwStoryId
    , wwHasStoryChanged
    , wwResps
    , wwPGPool

      -- ** Operations
    , runWW
    , terminate
    , patchRoom
    , viewRoom
    , patchStory
    , viewStory
    , respond
    , createUser
    , lookupUser

      -- * Class
    , MonadWW(liftWW)
    ) where

import           Control.Applicative                   (Applicative)
import           Control.Lens                          (makeLenses, use, (%=), (.=), Getting, (^.))
import           Control.Monad                         (when)
import           Control.Monad.Reader                  (ReaderT)
import           Control.Monad.State.Strict            (StateT(runStateT), runStateT)
import           Control.Monad.Trans                   (MonadIO(liftIO), lift)
import           Control.Monad.Trans.Either            (EitherT(runEitherT), left)
import           Crypto.Random                         (genBytes, newGenIO)
import           Crypto.Random.DRBG                    (HashDRBG)
import qualified Data.ByteString.Base64.URL            as Base64.URL
import           Data.Functor                          ((<$>))
import           Data.Monoid                           ((<>))
import           Data.Pool                             (Pool, withResource)
import qualified Database.PostgreSQL.Simple            as PG

import           WordWang.Bwd                          (Bwd(..))
import qualified WordWang.Bwd                          as Bwd
import           WordWang.Messages
import           WordWang.Objects
import qualified WordWang.PostgreSQL                   as WWPG

#include "../impossible.h"

data RespRecipient = All | This
    deriving (Eq, Show)

data WWState = WWState
    { _wwRoomId          :: !RoomId
    , _wwStoryId         :: !StoryId
    , _wwHasStoryChanged :: !Bool
    , _wwResps           :: !(Bwd (RespRecipient, Resp))
    , _wwPGPool          :: !(Pool PG.Connection)
    }

makeLenses ''WWState

newtype WW a =
    WW {unWW :: StateT WWState (EitherT RespError IO) a}
    deriving (Functor, Applicative, Monad, MonadIO)

runWW :: RoomId -> Pool PG.Connection
      -> WW a -> IO (Either RespError (a, WWState))
runWW roomId pgPool m = do
    mbStoryId <- withResource pgPool $ \conn ->
      WWPG.lookupRoomStory conn roomId
    case mbStoryId of
      Nothing -> do
        return $ Left $ RoomNotPresent roomId
      Just storyId -> do
        let wwst = WWState{ _wwRoomId = roomId
                          , _wwStoryId = storyId
                          , _wwHasStoryChanged = False
                          , _wwResps = Bwd.B0
                          , _wwPGPool = pgPool
                          }
        runEitherT $ flip runStateT wwst $ unWW m

patchObj :: WWPG.Object obj => Id -> [Patch obj] -> WW [Versioned (Patch obj)]
patchObj objId patches = do
    pgPool <- WW $ use wwPGPool
    mbVerPatches <- liftIO $ withResource pgPool $ \pgConn ->
      WWPG.patch pgConn objId patches
    case mbVerPatches of
      Nothing -> do
        error $ "WordWang.Monad.patchObj: could not find object " ++ show objId
      Just verPatches -> do
        return verPatches

lookupObj :: WWPG.Object obj => Id -> WW (Versioned obj)
lookupObj objId = do
    pgPool <- WW $ use wwPGPool
    mbObj <- liftIO $ liftIO $ withResource pgPool $ \pgConn ->
      WWPG.lookup pgConn objId
    case mbObj of
      Nothing -> do
        error $ "WordWang.Monad.lookupObj: could not find object " ++ show objId
      Just obj -> do
        return obj

terminate_ :: RespError -> WW a
terminate_ = WW . lift . left

patchRoom_ :: [Patch Room] -> WW Bool
patchRoom_ patches = do
    roomId <- WW $ use wwRoomId
    not . null <$> patchObj roomId patches

viewRoom_ :: Getting b (Versioned Room) b -> WW b
viewRoom_ f = do
    roomId <- WW $ use wwRoomId
    room <- lookupObj roomId
    return $ room ^. f

patchStory_ :: [Patch Story] -> WW Bool
patchStory_ patches = do
    storyId <- WW $ use wwStoryId
    verPatches <- patchObj storyId patches
    let hasChanged = not $ null verPatches
    WW $ when hasChanged $ do
      wwResps %=
        (<> Bwd.fromList [(All, RespPatch verPatch) | verPatch <- verPatches])
      wwHasStoryChanged .= True
    return hasChanged

viewStory_ :: Getting b (Versioned Story) b -> WW b
viewStory_ f = do
    storyId <- WW $ use wwStoryId
    story <- lookupObj storyId
    return $ story ^. f

respond_ :: RespRecipient -> Resp -> WW ()
respond_ recipient resp = WW $ do
    wwResps %= (:< (recipient, resp))

createUser_ :: WW (UserId, User)
createUser_ = do
    user <- User <$> makeSecret
    roomId <- WW $ use wwRoomId
    pgPool <- WW $ use wwPGPool
    mbUserId <- liftIO $ withResource pgPool $ \conn ->
      WWPG.createUser conn user roomId
    case mbUserId of
      Nothing     -> ERROR("Non existant room: " ++ show roomId)
      Just userId -> return (userId, user)
  where
    makeSecret = do
      gen <- liftIO (newGenIO :: IO HashDRBG)
      case genBytes 15 gen of
        Left err      -> terminate $ InternalError $ show err
        Right (bs, _) -> return $ Base64.URL.encode bs

lookupUser_ :: UserId -> WW (Maybe User)
lookupUser_ userId = do
    roomId <- WW $ use wwRoomId
    pgPool <- WW $ use wwPGPool
    liftIO $ withResource pgPool $ \conn -> WWPG.lookupUser conn userId roomId

------------------------------------------------------------------------

class (Functor m, Monad m) => MonadWW m where
    liftWW :: WW a -> m a

instance MonadWW WW where
    liftWW = id

instance MonadWW m => MonadWW (ReaderT a m) where
    liftWW = lift . liftWW

instance MonadWW m => MonadWW (StateT a m) where
    liftWW = lift . liftWW

terminate :: MonadWW m => RespError -> m a
terminate = liftWW . terminate_

patchRoom :: MonadWW m => [Patch Room] -> m Bool
patchRoom = liftWW . patchRoom_

viewRoom :: MonadWW m => Getting b (Versioned Room) b -> m b
viewRoom = liftWW . viewRoom_

patchStory :: MonadWW m => [Patch Story] -> m Bool
patchStory = liftWW . patchStory_

viewStory :: MonadWW m => Getting b (Versioned Story) b -> m b
viewStory = liftWW . viewStory_

respond :: MonadWW m => RespRecipient -> Resp -> m ()
respond recipient resp = liftWW $ respond_ recipient resp

createUser :: MonadWW m => m (UserId, User)
createUser = liftWW createUser_

lookupUser :: MonadWW m => UserId -> m (Maybe User)
lookupUser = liftWW . lookupUser_
