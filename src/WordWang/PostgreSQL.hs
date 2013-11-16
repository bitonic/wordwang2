module WordWang.PostgreSQL (pgWorker) where

import           Control.Monad (void)
import           Data.Monoid ((<>))

import qualified Data.Text as Text

import           Control.Lens ((^.))
import qualified Database.PostgreSQL.Simple as PG

import           WordWang.Objects
import           WordWang.Messages
import           WordWang.Worker (Worker(..))

respQuery :: PG.Connection -> StoryId -> RespBody -> IO ()
respQuery _    _ (RespStory _) = return ()
respQuery _    _ (RespError _) = return ()
respQuery conn sid (RespJoined uid secret) = void $
    PG.execute conn "INSERT INTO users VALUES (?, ?, ?)" (uid, secret, sid)
respQuery _    _ (RespUser _) = return ()
respQuery conn sid (RespVotingClosed block) = PG.withTransaction conn $ do
    PG.begin conn
    PG.execute conn "DELETE FROM candidates WHERE story_id = ?" (PG.Only sid)
    PG.execute conn "INSERT INTO blocks VALUES (DEFAULT, ?, ?)" (sid, block)
    PG.commit conn
    return ()
respQuery conn sid (RespCandidate cand) = void $
    PG.execute conn "INSERT INTO candidates VALUES (?, ?, ?)"
               (sid, cand^.candUser, cand^.candBlock)
respQuery conn sid (RespVote cand vote) = void $
    PG.execute conn "INSERT INTO votes VALUES (?, ?, ?)"
               (sid, cand, vote)
respQuery _ sid resp@(RespCreated _) =
    error ("WordWang.PostgreSQL.respQuery: " ++ show sid ++ ", " ++ show resp)

pgWorker :: StoryId -> PG.Connection -> IO (Worker () RespBody)
pgWorker sid conn = do
    PG.execute conn "INSERT INTO stories VALUES (?)" (PG.Only sid)
    return Worker
        { workerName    = "pg-" <> Text.pack (show sid)
        , workerStart   = return ()
        -- TODO handle closed connection
        , workerRestart = \() _ -> return (Right ())
        , workerProcess = \() -> mapM_ (respQuery conn sid)
        }
