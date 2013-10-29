module WordWang
    ( module WordWang.Messages
    , module WordWang.Monad
    , module WordWang.Objects
    , wordwang
    ) where

import           Control.Concurrent.MVar (modifyMVar_, readMVar)
import           Data.Functor ((<$>))
import           Data.Monoid ((<>))

import           Control.Monad.Trans (liftIO)
import qualified Data.Text as Text

import           Control.Lens
import           Crypto.Random (newGenIO, genBytes)
import           Crypto.Random.DRBG (HashDRBG)
import qualified Data.ByteString.Base64.URL as Base64.URL

import           WordWang.Messages
import           WordWang.Monad
import           WordWang.Objects

internalError :: Text -> WWT IO a
internalError err = do
    -- TODO do some logging
    terminate (RespError ("internal error: " <> err))

makeSecret :: WWT IO UserSecret
makeSecret = do
    gen <- liftIO (newGenIO :: IO HashDRBG)
    case genBytes 15 gen of
        Left err -> internalError (Text.pack (show err))
        Right (bs, _) -> return (Base64.URL.encode bs)

modifyStory :: (Story -> IO Story) -> WWT IO ()
modifyStory f = do
    storyMv <- view wwStory
    liftIO $ modifyMVar_ storyMv $ \(story, queue) -> (, queue) <$> f story

modifyStory' :: (Story -> Story) -> WWT IO ()
modifyStory' f = modifyStory (return . f)

viewStory :: WWT IO Story
viewStory = fst <$> (liftIO . readMVar =<< view wwStory)

authenticated :: WWT IO UserId
authenticated = do
    authM <- view (wwReq . reqAuth)
    case authM of
        Nothing -> authErr "no credentials provided"
        Just auth -> do
            story <- viewStory
            let uid = auth^.reqAuthUser
            maybe (authErr "invalid credentials") (const (return uid))
                  (story^.storyUsers.at uid)
  where
    authErr err = terminate (RespError ("authentication required, but " <> err))

wordwang :: WWT IO ()
wordwang = do
    req <- view wwReq
    case req^.reqBody of
        ReqCreate ->
            internalError "wordwang: received ReqCreate"
        ReqJoin -> do
            -- TODO should we check if the user is already authenticated?
            user <- liftIO . newUser =<< makeSecret
            modifyStory' (& storyUsers. at (user^.userId) ?~ user)
            terminate (RespJoined (user^.userId) (user^.userSecret))
        ReqCandidate candidate -> do
            uid <- authenticated
            undefined
        ReqVote uid -> do
            uid <- authenticated
            undefined
