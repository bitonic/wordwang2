{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExistentialQuantification #-}
module WordWang.Utils
    ( delPrefix
    , wwJSON
    , mapKeyVal
    , toTaggedJSON
    , parseTagged
    , parseNullary
    , parseUnary
    , parseBinary
    , sendJSON
    , debugMsg
    , infoMsg
    , errorMsg
    , eitherUnzip
    , Only(..)
    , Shown(..)
    , JSONP(JSONP)
    , supervise
    ) where

import           Control.Arrow (first, second)
import           Control.Concurrent (ThreadId, forkIO, throwTo, myThreadId)
import           Control.Exception (mask, catch, SomeException)
import           Control.Monad (when, liftM)
import           Data.Char (isUpper, toLower)
import           Data.List (stripPrefix)
import           Data.Monoid ((<>))
import           System.IO (stderr)

import           Control.Monad.Trans (MonadIO(..))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable (Hashable)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.String.Combinators (quotes)
import           Data.Text.Buildable (Buildable(..))
import           Data.Text.Format (Format, Only(..), Shown(..), format)
import           Data.Text.Format.Params (Params)
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.PostgreSQL.Simple.TypeInfo.Macro as PGTI
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as PGTI
import qualified Network.WebSockets as WS

import           WordWang.Config

delPrefix :: String -> String -> String
delPrefix prefix fieldName =
    case stripPrefix prefix fieldName of
        Just ccs@(c:cs)
            | isUpper c -> toLower c : cs
            | null prefix -> ccs
            | otherwise -> error $ "The field name after the prefix " ++
                                   "must be written in CamelCase"
        Just [] -> error $ "The field name after the prefix may not be empty"
        Nothing -> error $ "The field name " ++ quotes fieldName ++
                           " does not begin with the required prefix " ++
                           quotes prefix

wwJSON :: (String -> String) -> Aeson.Options
wwJSON f = Aeson.defaultOptions{Aeson.fieldLabelModifier = f}

-- | Transform the keys and values of a 'HashMap'.
mapKeyVal :: (Eq k2, Hashable k2) => (k1 -> k2) -> (v1 -> v2)
          -> HashMap k1 v1 -> HashMap k2 v2
mapKeyVal fk kv =
    HashMap.foldrWithKey (\k v -> HashMap.insert (fk k) (kv v)) HashMap.empty
{-# INLINE mapKeyVal #-}

toTaggedJSON :: (a -> (Text, [Aeson.Pair])) -> a -> Aeson.Value
toTaggedJSON f (f -> (tag, obj)) = Aeson.object $ (("tag" Aeson..= tag) : obj)

parseTagged :: [(Text, Aeson.Object -> Aeson.Parser a)]
            -> Aeson.Value -> Aeson.Parser a
parseTagged table x = do
    obj :: Aeson.Object <- Aeson.parseJSON x
    case HashMap.lookup "tag" obj of
        Nothing -> fail "parseTagged: no `tag'"
        Just (Aeson.String tag) ->
            case lookup tag table of
                Just p -> p (HashMap.delete "tag" obj)
                Nothing ->
                    fail ("parseTagged: `" ++ T.unpack tag ++ "' not present")
        Just _ -> fail "parseTagged: expected object"

parseNullary :: a -> Aeson.Object -> Aeson.Parser a
parseNullary x obj | HashMap.null obj = return x
                   | otherwise        = fail "nullary: expecting empty object"

parseUnary :: Aeson.FromJSON a
           => (a -> b) -> Text -> Aeson.Object -> Aeson.Parser b
parseUnary f field obj | Just x <- HashMap.lookup field obj = do
    x' <- Aeson.parseJSON x
    parseNullary (f x') (HashMap.delete field obj)
parseUnary _ _ _ =
    fail "unary: expecting one field"

parseBinary :: (Aeson.FromJSON a, Aeson.FromJSON b)
            => (a -> b -> c) -> Text -> Text -> Aeson.Object -> Aeson.Parser c
parseBinary f field1 field2 obj | Just x1 <- HashMap.lookup field1 obj
                                , Just x2 <- HashMap.lookup field2 obj = do
    x1' <- Aeson.parseJSON x1
    x2' <- Aeson.parseJSON x2
    parseNullary (f x1' x2') (HashMap.delete field1 (HashMap.delete field2 obj))
parseBinary _ _ _ _ =
    fail "binary: expecting two fields"

sendJSON :: (Aeson.ToJSON a, Show a) => WS.Connection -> a -> IO ()
sendJSON conn req = do
    debugMsg "sending response `{}'" (Only (JSONP req))
    WS.sendTextData conn (Aeson.encode req)

stderrMsg :: (MonadIO m, Params ps) => TL.Text -> Format -> ps -> m ()
stderrMsg pre fmt pars = liftIO (TL.hPutStrLn stderr (pre <> format fmt pars))

logMsg :: (MonadIO m, Params ps) => LogLevel -> Format -> ps -> m ()
logMsg pri fmt ps = do
    minPri <- _cLogLevel `liftM` getConfig
    when (pri >= minPri) (stderrMsg ("[" <> TL.pack (show pri) <> "] ") fmt ps)

debugMsg :: (MonadIO m, Params ps) => Format -> ps -> m ()
debugMsg = logMsg DEBUG

infoMsg :: (MonadIO m, Params ps) => Format -> ps -> m ()
infoMsg = logMsg INFO

errorMsg :: (MonadIO m, Params ps) => Format -> ps -> m ()
errorMsg = logMsg ERROR

eitherUnzip :: (a -> Either b c) -> [a] -> ([b], [c])
eitherUnzip _ [] = ([], [])
eitherUnzip f (x : xs) =
    either (first . (:)) (second . (:)) (f x) (eitherUnzip f xs)

instance PG.ToField UUID where
    toField = PG.Escape . UUID.toASCIIBytes

instance PG.FromField UUID where
    fromField fld bsm =
        if PG.typeOid fld /= $(PGTI.inlineTypoid PGTI.uuid)
        then PG.returnError PG.Incompatible fld ""
        else case bsm of
            Nothing -> PG.returnError PG.UnexpectedNull fld ""
            Just bs | Just u <- UUID.fromASCIIBytes bs -> return u
            _ -> fail "WordWang.Utils FromField UUID: invalid bytes"

newtype JSONP a = JSONP {unJSONP :: a}

instance Aeson.ToJSON a => Buildable (JSONP a) where
    build = TL.fromLazyText . TL.decodeUtf8 . Aeson.encode . unJSONP

supervise :: IO () -> IO ThreadId
supervise m = mask $ \restore -> do
    tid <- myThreadId
    forkIO (restore m `catch` \(e :: SomeException) -> throwTo tid e)
