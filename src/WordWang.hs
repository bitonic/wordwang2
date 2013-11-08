-- TODO track WS connections requests in the logging
module WordWang
    ( module WordWang.Messages
    , module WordWang.Monad
    , module WordWang.Objects

    , Stories
    , createStory
    , serverWW
    , wordwang
    ) where

import           Control.Applicative ((<*>))
import           Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import           Control.Exception (catch)
import           Control.Monad (filterM, unless)
import           Data.Foldable (toList)
import           Data.Functor ((<$>), (<$))

import           Control.Monad.Reader (ask)
import           Control.Monad.Trans (liftIO)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import           System.Random (random, randomIO)

import           Control.Lens
import           Crypto.Random (genBytes)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64.URL as Base64.URL
import qualified Network.WebSockets as WS
import           Snap (Snap)
import qualified Snap as Snap

import           WordWang.Messages
import           WordWang.Monad
import           WordWang.Objects
import           WordWang.Utils
import           WordWang.Bwd

type Stories = MVar (HashMap StoryId (MVar Story, MVar [WS.Connection]))

internalError :: Text -> WW a
internalError = terminate . RespError . InternalError

makeSecret :: WW UserSecret
makeSecret = do
    gen <- use wwDRBG
    case genBytes 15 gen of
        Left err -> internalError (Text.pack (show err))
        Right (bs, gen') -> wwDRBG .= gen' >> return (Base64.URL.encode bs)

newId :: WW Id
newId = do
    gen <- use wwRG
    let (i, gen') = random gen
    i <$ (wwRG .= gen')

-- -- TODO the Incremental stuff relies on the interplay between the
-- -- 'putMVar' and 'takeMVar'.  It would be better for this game to be
-- -- self-contained in some module to make it less fragile.

-- startIncre :: WWT IO (IO ())
-- startIncre = do
--     increMv <- view wwIncre
--     storyMv <- view wwStory
--     resp <- respondInIO
--     return $ do
--         incre <- Incre.start startT halveMaybe
--         putMVar increMv incre
--         void $ forkIO $
--             Incre.wait incre >> takeMVar increMv >> closeVoting storyMv resp
--   where
--     startT = 10000000

--     halveMaybe n | n >= startT `div` 2 = Just (n `div` 2)
--     halveMaybe _ = Nothing

--     closeVoting storyMv resp = modifyMVar_ storyMv $ \story ->
--         case HashMap.elems (story^.storyCandidates) of
--             [] -> return story -- TODO should we return an error?
--             cands@(_:_) -> do
--                 let cand  = maximumBy (comparing (HashSet.size . _candVotes))
--                                       cands
--                     block = cand^.candBlock
--                 resp (respToAll (RespVotingClosed block))
--                 let story' = story & storyCandidates .~ HashMap.empty
--                 return (story' & storyBlocks %~ (++ [block]))


-- bumpIncre :: WWT IO (IO ())
-- bumpIncre = do
--     increMv <- view wwIncre
--     return (Incre.bump =<< readMVar increMv)

authenticated :: WW UserId
authenticated = do
    authM <- view reqAuth
    case authM of
        Nothing -> terminate (RespError NoCredentials)
        Just auth -> do
            story <- use wwStory
            let uid = auth^.reqAuthUser
            maybe (terminate (RespError InvalidCredentials))
                  (const (return uid))
                  (story^.storyUsers.at uid)

createStory :: Stories -> Snap ()
createStory storiesMv = do
    sid <- liftIO go
    Snap.modifyResponse $
        Snap.setResponseCode 200 . Snap.setContentType "text/json"
    Snap.writeLBS (Aeson.encode sid)
  where
    go = modifyMVar storiesMv $ \stories -> do
        sid <- randomIO
        storyMv <- newMVar (emptyStory sid)
        connsMv <- newMVar []
        return (HashMap.insert sid (storyMv, connsMv) stories, sid)

serverWW :: Stories -> WW () -> WS.ServerApp
serverWW storiesMv m pending = do
    conn <- WS.acceptRequest pending
    go conn False
  where
    go conn isReg = do
        reqm <- Aeson.eitherDecode <$> WS.receiveData conn
        case reqm of
            Left err ->
                sendErr conn (ErrorDecodingReq (Text.pack err))
            Right (req :: Req) -> do
                debugMsg "received request `{}'" (Only (Shown req))
                handleReq conn isReg req

    handleReq conn isReg req = do
        let sid = req^.reqStory
        stories <- readMVar storiesMv
        case stories ^. at sid of
            Nothing -> sendErr conn NoStory >> go conn isReg
            Just (storyMv, connsMv) -> do
                unless isReg (modifyMVar_ connsMv (return . (conn :)))
                modifyMVar_ storyMv $ \story -> do
                    (res, wwst) <- runWW req story m
                    let resps  = wwst^.wwResps
                        resps' = either ((resps :<) . respToThis) (const resps) res
                        (map _respBody -> forAll, map _respBody -> forThis) =
                            break ((== This) . _respRecipients) (toList resps')
                    modifyMVar_ connsMv $ \conns -> flip filterM conns $ \conn' ->
                        (True <$ mapM_ (sendJSON conn') forAll) `catch`
                        \(_ :: WS.ConnectionException) -> return False
                    mapM_ (sendJSON conn) forThis
                    return (wwst^.wwStory)
                go conn True

    sendErr conn err = sendJSON conn (RespError err)

wordwang :: WW ()
wordwang = do
    req <- ask
    case req^.reqBody of
        ReqStory -> respond . respToThis . respStory =<< use wwStory
        ReqJoin -> do
            -- TODO should we check if the user is already authenticated?
            user <- User <$> newId <*> makeSecret
            wwStory %= (storyUsers.at (user^.userId) ?~ user)
            respond (respToAll (RespUser (user^.userId)))
            respond (respToThis (RespJoined (user^.userId) (user^.userSecret)))
        ReqCandidate body -> do
            uid <- authenticated
            let cand = candidate uid body
            candsM <- use (wwStory . storyCandidates . at uid)
            case candsM of
                Just _ -> return ()
                Nothing -> do
                    respond (respToAll (RespCandidate cand))
                    wwStory %= (storyCandidates.at uid ?~ cand)
        ReqVote candUid -> do
            voteUid <- authenticated
            candM <- use (wwStory . storyCandidates . at candUid)
            case candM of
                Just cand | not (HashSet.member voteUid (cand^.candVotes)) -> do
                    respond (respToAll (RespVote candUid voteUid))
                    let cand' = cand & candVotes %~ HashSet.insert voteUid
                    wwStory %= (storyCandidates.at candUid ?~ cand')
                _ -> return ()
