module WordWang.Utils
    ( delPrefix
    , wwJSON
    , mapKeyVal
    , tagObj
    , parseTagged
    , parseNullary
    , parseUnary
    , parseBinary
    , sendJSON
    ) where

import           Data.Char (isUpper, toLower)
import           Data.List (stripPrefix)

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable (Hashable)
import           Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.String.Combinators (quotes)
import qualified Network.WebSockets as WS

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

tagObj :: Text -> [Aeson.Pair] -> Aeson.Value
tagObj tag obj = Aeson.object $ (("tag" Aeson..= tag) : obj)

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
            => (a -> b -> c) -> Text -> Text -> Aeson.Object
            -> Aeson.Parser c
parseBinary f field1 field2 obj
    | Just x <- HashMap.lookup field1 obj, Just y <- HashMap.lookup field2 obj = do
        x' <- Aeson.parseJSON x
        y' <- Aeson.parseJSON y
        parseNullary (f x' y') (HashMap.delete field1 (HashMap.delete field2 obj))
parseBinary _ _ _ _ =
    fail "binary: expeting two fields"

sendJSON :: Aeson.ToJSON a => WS.Connection -> a -> IO ()
sendJSON conn = WS.sendTextData conn . Aeson.encode
