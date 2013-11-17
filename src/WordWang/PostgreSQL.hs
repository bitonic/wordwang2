module WordWang.PostgreSQL
    ( pgWorker
    , loadStories
    ) where

import           Control.Monad (void, liftM, forM)
import           Data.Functor ((<$), (<$>))
import           Data.Monoid ((<>))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text

import           Control.Lens ((^.))
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Transaction as PG
import           Database.PostgreSQL.Simple.SqlQQ (sql)

import           WordWang.Objects
import           WordWang.Messages
import           WordWang.Worker (Worker(..))

respQuery :: PG.Connection -> StoryId -> RespBody -> IO ()
respQuery _    _ (RespStory _) = return ()
respQuery _    _ (RespError _) = return ()
respQuery conn sid (RespJoined uid secret) = void $
    PG.execute conn "INSERT INTO users VALUES (?, ?, ?)" (uid, secret, sid)
respQuery _    _ (RespUser _) = return ()
respQuery conn sid (RespVotingClosed block) = withRetryingTransaction conn $ do
    PG.begin conn
    PG.execute conn
        [sql| DELETE FROM candidates WHERE story_id = ? |] (PG.Only sid)
    PG.execute conn
        [sql| INSERT INTO blocks VALUES (DEFAULT, ?, ?) |] (sid, block)
    PG.commit conn
    return ()
respQuery conn sid (RespCandidate cand) = void $ withRetryingTransaction conn $ do
    PG.execute conn
         [sql| INSERT INTO candidates VALUES (?, ?, ?) |]
         (sid, cand^.candUser, cand^.candBlock)
    mapM_ (respQuery conn sid . RespVote (cand^.candUser))
          (HashSet.toList (cand^.candVotes))
respQuery conn sid (RespVote cand vote) = void $
    PG.execute conn
         [sql| INSERT INTO votes VALUES (?, ?, ?) |]
         (sid, cand, vote)
respQuery _ sid resp@(RespCreated _) =
    error ("WordWang.PostgreSQL.respQuery: " ++ show sid ++ ", " ++ show resp)

pgWorker :: StoryId -> PG.ConnectInfo -> Worker RespBody
pgWorker sid ci = Worker
    { workerName    = "pg-" <> Text.pack (show sid)
    , workerStart   = do
           conn <- PG.connect ci
           PG.execute conn
               [sql| INSERT INTO stories (id)
                       SELECT (?) FROM stories
                         WHERE NOT EXISTS (SELECT id FROM stories WHERE id = ?)
               |]
               (sid, sid)
           return conn
    , workerRestart = \_conn _ _ -> return (Left (error "TODO handle closed conn"))
    , workerReceive = \conn resp -> Just conn <$ respQuery conn sid resp
    }

loadStories :: PG.ConnectInfo -> IO [Story]
loadStories ci = do
    conn <- PG.connect ci
    withRetryingTransaction conn $ do
        sids <- map PG.fromOnly <$>
                PG.query conn [sql| SELECT * FROM stories |] ()
        mapM (loadStory conn) sids
  where
    loadStory conn (sid :: StoryId) = do
        users <- HashMap.fromList . map (\(i, s) -> (i, User i s)) <$>
                 PG.query conn
                     [sql| SELECT id, secret FROM users WHERE story_id = ? |]
                     (PG.Only sid)
        cands <- PG.query conn
                     [sql| SELECT user_id, block FROM candidates
                             WHERE story_id = ? |]
                     (PG.Only sid)
        cands' <- liftM HashMap.fromList $ forM cands $ \(uid, block) -> do
            votes <- map PG.fromOnly <$> PG.query conn
                         [sql| SELECT vote FROM votes
                                 WHERE story_id = ? AND user_id = ? |]
                         (sid, uid)
            return (uid, Candidate uid block (HashSet.fromList votes))
        blocks <- map PG.fromOnly <$> PG.query conn
                      [sql| SELECT block FROM blocks
                               WHERE story_id = ? ORDER BY id DESC |]
                      (PG.Only sid)
        return (Story sid users blocks cands')

withRetryingTransaction :: PG.Connection -> IO a -> IO a
withRetryingTransaction =
    PG.withTransactionModeRetry PG.defaultTransactionMode (const True)
