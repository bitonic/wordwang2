module WordWang.JSON
    ( delPrefix
    , wwJSON
    , tagJSON
    , toTaggedJSON
    , parseTagged
    , parseNullary
    , parseUnary
    , parseBinary
    , JSONed(..)
    ) where

import qualified Data.Aeson                            as Aeson
import qualified Data.Aeson.Types                      as Aeson
import qualified Data.ByteString.Lazy                  as BL
import           Data.Char                             (isUpper, toLower)
import qualified Data.HashMap.Strict                   as HashMap
import           Data.List                             (stripPrefix)
import           Data.String.Combinators               (quotes)
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Text.Buildable                   (Buildable(..))
import qualified Data.Text.Lazy.Builder                as TL
import qualified Data.Text.Lazy.Encoding               as TL
import           Data.Typeable                         (Typeable)
import qualified Database.PostgreSQL.Simple.FromField  as PG
import qualified Database.PostgreSQL.Simple.ToField    as PG

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

newtype JSONed a = JSONed {unJSONed :: a}
    deriving (Eq, Show, Typeable)

instance Aeson.ToJSON a => Buildable (JSONed a) where
    build = TL.fromLazyText . TL.decodeUtf8 . Aeson.encode . unJSONed

instance Aeson.ToJSON a => PG.ToField (JSONed a) where
    toField = PG.toField . BL.toStrict . Aeson.encode . unJSONed

instance (Aeson.FromJSON a) => PG.FromField (JSONed a) where
    fromField fld mbBs = do
        value <- PG.fromField fld mbBs
        case Aeson.parseMaybe Aeson.parseJSON value of
          Nothing -> fail $ "WordWang.Utils PG.FromField JSONed a: " ++
                            "couldn't decode JSON " ++ show value
          Just x  -> return $ JSONed x
