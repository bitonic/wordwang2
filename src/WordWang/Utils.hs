{-# OPTIONS_GHC -fno-warn-orphans #-}
module WordWang.Utils
    ( delPrefix
    , wwJSON
    , mapKeyVal
    , tagJSON
    , toTaggedJSON
    , parseTagged
    , parseNullary
    , parseUnary
    , parseBinary
    , TaggedConn(..)
    , tcConn
    , tcTag
    , sendJSON
    , debugMsg
    , infoMsg
    , errorMsg
    , Only(..)
    , Shown(..)
    , JSONed(..)
    , supervise
    ) where

import           Control.Concurrent                    (ThreadId, forkIO, throwTo, myThreadId)
import           Control.Exception                     (mask, catches, SomeException, Handler(Handler), AsyncException, throwIO)
import           Control.Lens                          ((^.), makeLenses)
import           Control.Monad                         (when)
import           Control.Monad.Trans                   (MonadIO(..))
import qualified Data.Aeson                            as Aeson
import qualified Data.Aeson.Types                      as Aeson
import qualified Data.ByteString.Lazy                  as BL
import           Data.Char                             (isUpper, toLower)
import           Data.HashMap.Strict                   (HashMap)
import qualified Data.HashMap.Strict                   as HashMap
import           Data.Hashable                         (Hashable)
import           Data.Int                              (Int64)
import           Data.List                             (stripPrefix)
import           Data.Monoid                           ((<>))
import           Data.String.Combinators               (quotes)
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Text.Buildable                   (Buildable(..))
import           Data.Text.Format                      (Format, Only(..), Shown(..), format)
import           Data.Text.Format.Params               (Params)
import qualified Data.Text.Lazy                        as TL
import qualified Data.Text.Lazy.Builder                as TL
import qualified Data.Text.Lazy.Encoding               as TL
import           Data.Typeable                         (Typeable)
import           Data.UUID                             (UUID)
import qualified Data.UUID                             as UUID
import qualified Database.PostgreSQL.Simple.FromField  as PG
import qualified Database.PostgreSQL.Simple.ToField    as PG
import qualified Database.PostgreSQL.Simple.TypeInfo.Macro  as PGTI
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as PGTI
import qualified Network.WebSockets                    as WS

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

tagJSON :: (Text, [Aeson.Pair]) -> Aeson.Value
tagJSON (tag, obj) = Aeson.object (("tag" Aeson..= tag) : obj)

toTaggedJSON :: (a -> (Text, [Aeson.Pair])) -> a -> Aeson.Value
toTaggedJSON f (f -> (tag, obj)) = tagJSON (tag, obj)

parseTagged :: [(Text, Aeson.Object -> Aeson.Parser a)]
            -> Aeson.Value -> Aeson.Parser a
parseTagged table x = do
    obj :: Aeson.Object <- Aeson.parseJSON x
    case HashMap.lookup "tag" obj of
      Nothing -> failWithLoc "no `tag'"
      Just (Aeson.String tag) ->
        case lookup tag table of
          Just p -> p (HashMap.delete "tag" obj)
          Nothing ->
            failWithLoc $ "`" ++ T.unpack tag ++ "' not present"
      Just _ -> failWithLoc "expected object"
  where
    failWithLoc s = fail $"WorgWang.Utils.parseTagged: " ++ s

parseNullary :: a -> Aeson.Object -> Aeson.Parser a
parseNullary x obj
    | HashMap.null obj = return x
    | otherwise        = fail "WordWang.Utils.parseNullary: expecting empty object"

parseUnary :: Aeson.FromJSON a
           => (a -> b) -> Text -> Aeson.Object -> Aeson.Parser b
parseUnary f field obj | Just x <- HashMap.lookup field obj = do
    x' <- Aeson.parseJSON x
    parseNullary (f x') (HashMap.delete field obj)
parseUnary _ _ _ =
    fail "WordWang.Utils.parseUnary: expecting one field"

parseBinary :: (Aeson.FromJSON a, Aeson.FromJSON b)
            => (a -> b -> c) -> Text -> Text -> Aeson.Object -> Aeson.Parser c
parseBinary f field1 field2 obj | Just x1 <- HashMap.lookup field1 obj
                                , Just x2 <- HashMap.lookup field2 obj = do
    x1' <- Aeson.parseJSON x1
    x2' <- Aeson.parseJSON x2
    parseNullary (f x1' x2') (HashMap.delete field1 (HashMap.delete field2 obj))
parseBinary _ _ _ _ =
    fail "WordWang.Utils.parseBinary: expecting two fields"

data TaggedConn = TaggedConn
    { _tcConn :: WS.Connection
    , _tcTag  :: Int64
    }

makeLenses ''TaggedConn

sendJSON :: (Aeson.ToJSON a, MonadIO m) => TaggedConn -> a -> m ()
sendJSON taggedConn req = liftIO $ do
    debugMsg "[{}] sending response `{}'" (taggedConn ^. tcTag, JSONed req)
    WS.sendTextData (taggedConn ^. tcConn) (Aeson.encode req)

logMsg :: (MonadIO m, Params ps) => LogLevel -> Format -> ps -> m ()
logMsg pri fmt ps = do
    conf <- getConfig
    let minPri = conf ^. confLogLevel
        logf   = conf ^. confLogFunction
    when (pri >= minPri) $ liftIO $ logf $
      ("[" <> TL.pack (show pri) <> "] ") <> format fmt ps

debugMsg :: (MonadIO m, Params ps) => Format -> ps -> m ()
debugMsg = logMsg DEBUG

infoMsg :: (MonadIO m, Params ps) => Format -> ps -> m ()
infoMsg = logMsg INFO

errorMsg :: (MonadIO m, Params ps) => Format -> ps -> m ()
errorMsg = logMsg ERROR

instance PG.ToField UUID where
    toField = PG.Escape . UUID.toASCIIBytes

instance PG.FromField UUID where
    fromField fld bsm =
        if PG.typeOid fld /= $(PGTI.inlineTypoid PGTI.uuid)
        then PG.returnError PG.Incompatible fld ""
        else case bsm of
          Nothing ->
            PG.returnError PG.UnexpectedNull fld ""
          Just (UUID.fromASCIIBytes -> Just u) ->
            return u
          _ ->
            fail "WordWang.Utils PG.FromField UUID: invalid bytes"

newtype JSONed a = JSONed {unJSONed :: a}
    deriving (Eq, Show, Typeable)

instance Aeson.ToJSON a => Buildable (JSONed a) where
    build = TL.fromLazyText . TL.decodeUtf8 . Aeson.encode . unJSONed

instance Aeson.ToJSON a => PG.ToField (JSONed a) where
    toField = PG.Escape . BL.toStrict . Aeson.encode . unJSONed

instance (Aeson.FromJSON a, Typeable a) => PG.FromField (JSONed a) where
    fromField fld mbBs =
        if PG.typeOid fld /= $(PGTI.inlineTypoid PGTI.json)
        then PG.returnError PG.Incompatible fld ""
        else case mbBs of
          Nothing ->
            PG.returnError PG.UnexpectedNull fld ""
          -- TODO for some reson Aeson.decodeStrict doesn't work...
          Just bs | Just x <- Aeson.decode (BL.fromStrict bs) ->
            return $ JSONed x
          Just bs ->
            fail $ "WordWang.Utils PG.FromField JSONed a: " ++
                   "couldn't decode JSON " ++ show bs

supervise :: IO () -> IO ThreadId
supervise m = mask $ \restore -> do
    tid <- myThreadId
    forkIO $
      restore m `catches` [ Handler $ \(e :: AsyncException) -> throwIO e
                          , Handler $ \(e :: SomeException)  -> throwTo tid e
                          ]
