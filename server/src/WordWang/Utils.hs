{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module WordWang.Utils
    ( delPrefix
    , wwJSON
    , mapKeyVal
    , tagObj
    , parseTagged
    , parseNullary
    , parseUnary
    ) where

import           Data.Char (isUpper, toLower)
import           Data.List (stripPrefix)

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable (Hashable)
import           Data.Text (Text)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.String.Combinators (quotes)

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
parseTagged = undefined

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
