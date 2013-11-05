-- TODO track WS connections requests in the logging
module WordWang
    ( module WordWang.Messages
    , module WordWang.Monad
    , module WordWang.Objects
    , wordwang
    ) where

import           Control.Concurrent.MVar (modifyMVar, readMVar)
import           Data.Functor ((<$>))
import           Data.List (maximumBy)
import           Data.Ord (comparing)

import           Control.Monad.Trans (liftIO)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text

import           Control.Lens
import           Crypto.Random (newGenIO, genBytes)
import           Crypto.Random.DRBG (HashDRBG)
import qualified Data.ByteString.Base64.URL as Base64.URL

import           WordWang.Messages
import           WordWang.Monad
import           WordWang.Objects

internalError :: Text -> WWT IO a
internalError = terminate . RespError . InternalError

makeSecret :: WWT IO UserSecret
makeSecret = do
    gen <- liftIO (newGenIO :: IO HashDRBG)
    case genBytes 15 gen of
        Left err -> internalError (Text.pack (show err))
        Right (bs, _) -> return (Base64.URL.encode bs)

modifyStory :: (Story -> IO (Story, a)) -> WWT IO a
modifyStory f = do
    storyMv <- view wwStory
    liftIO $ modifyMVar storyMv f

modifyStory_ :: (Story -> IO Story) -> WWT IO ()
modifyStory_ f = modifyStory $ \story -> (, ()) <$> f story

modifyStory' :: (Story -> (Story, a)) -> WWT IO a
modifyStory' f = modifyStory (return . f)

-- modifyStory'_ :: (Story -> Story) -> WWT IO ()
-- modifyStory'_ f = modifyStory_ (return . f)

viewStory :: WWT IO Story
viewStory = liftIO . readMVar =<< view wwStory

authenticated :: WWT IO UserId
authenticated = do
    authM <- view (wwReq . reqAuth)
    case authM of
        Nothing -> terminate (RespError NoCredentials)
        Just auth -> do
            story <- viewStory
            let uid = auth^.reqAuthUser
            maybe (terminate (RespError InvalidCredentials))
                  (const (return uid))
                  (story^.storyUsers.at uid)

wordwang :: WWT IO ()
wordwang = do
    req <- view wwReq
    case req^.reqBody of
        ReqCreate ->
            internalError "wordwang: received ReqCreate"
        ReqStory -> do
            story <- viewStory
            respond (respToThis (respStory story))
        ReqJoin -> do
            -- TODO should we check if the user is already authenticated?
            user <- liftIO . newUser =<< makeSecret
            modifyStory' $ \story ->
                let story' = story & storyUsers.at (user^.userId) ?~ user
                in  (story', story')
            respond (respToThis (RespJoined (user^.userId) (user^.userSecret)))
        ReqCandidate body -> do
            uid <- authenticated
            let cand = candidate uid body
            resp <- respondInIO
            modifyStory_ $ \story -> do
                case story^.storyCandidates^.at uid of
                    Just _ -> return story
                    Nothing -> do
                        resp (respToAll (RespCandidate cand))
                        return (story & storyCandidates.at uid ?~ cand)
        ReqVote candUid -> do
            voteUid <- authenticated
            resp <- respondInIO
            modifyStory_ $ \story -> case story^.storyCandidates^.at candUid of
                Just cand | not (HashSet.member voteUid (cand^.candVotes)) -> do
                    resp (respToAll (RespVote candUid voteUid))
                    let cand' = cand & candVotes %~ HashSet.insert voteUid
                    return (story & storyCandidates.at candUid ?~ cand')
                _ -> return story
        ReqCloseVoting -> do
            resp <- respondInIO
            modifyStory_ $ \story -> case HashMap.elems (story^.storyCandidates) of
                [] -> return story -- TODO should we return an error?
                cands@(_:_) -> do
                    let cand  = maximumBy (comparing (HashSet.size . _candVotes))
                                          cands
                        block = cand^.candBlock
                    resp (respToAll (RespVotingClosed block))
                    let story' = story & storyCandidates .~ HashMap.empty
                    return (story' & storyBlocks %~ (++ [block]))
