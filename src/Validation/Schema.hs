{-# LANGUAGE OverloadedStrings #-}

module Validation.Schema
    ( Schema(..)
    , runSchema
    , choiceSchema
    -- Primitives.
    , requireText, tryText
    , requireTextWith, tryTextWith
    , requireFloat, requireFloatWith, tryFloatWith
    , requireInteger, requireIntegerWith, tryIntegerWith
    , requireBool, tryBool
    , ensureValidKeys
    -- Generic validators.
    , validateNumInst
    , validateEnum
    ) where


import Datatypes.ParseTokens
import Datatypes.ValidatedTokens
import Common

import Control.Applicative
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import Data.Validation

-- This data type is used to validate lists of options for commands. The Applicative instance allows for sequential validations.
newtype Schema a = Schema {
    runSchema :: [POptionPair] -> Validation [String] a
}

-- The important definition here is the Applicative one, it allows for the sequential application of Schemas.
instance Functor Schema where
    fmap f (Schema g) = Schema (\o -> fmap f (g o))

instance Applicative Schema where
    pure x = Schema (\_ -> Success x)
    (Schema f) <*> (Schema g) = Schema (\o -> f o <*> g o)


--------------------
-- VALUE VALIDATORS
--------------------
-- Takes a number, returns it if it satisfies given function returns true, otherwise Nothing.
validateNumInst :: (Num a, Ord a) => (a -> Bool) -> (a -> b) -> a -> Maybe b
validateNumInst vf i n = if vf n then Just (i n) else Nothing

validateEnum :: [(Text, a)] -> Text -> Maybe a
validateEnum o t = lookup (T.toLower t) o


--------------------
-- INTERNAL AUXILIARY FUNCTIONS
--------------------
-- Generic error function.
failType :: Text -> String
failType k = "Option key " ++ quote k ++ " has wrong type"

-- Returns a valid schema with the corresponding values, or an error if no valid matches are found.
choiceSchema :: [Schema a] -> Schema a
choiceSchema [] = Schema (\_ -> Failure ["Options did not match any valid form"])
choiceSchema ((Schema s):xs) =
    Schema $ \o ->
        case s o of
            Success v -> Success v
            -- Errors are accumulated, to give the user more information.
            Failure e -> case runSchema (choiceSchema xs) o of
                Success v' -> Success v'
                Failure e' -> Failure (e <> e') -- Errors have to implement <>, they are a semigroup.

-- Takes a key, if it corresponds to a value in the options and the given function returns "Just" then a "Success" is returned, otherwise a
-- "Failure". If the key is not present a "Success" with "Nothing" is returned.
getMaybeWith :: Text -> (POptionValue -> Maybe a) -> String -> Schema (Maybe a)
getMaybeWith k vf err = Schema $ \o ->
    case lookup k o of
        Just ov -> case vf ov of
            Just v -> Success (Just v)
            Nothing -> Failure [err]
        Nothing -> Success Nothing

-- Takes a key, if it corresponds to a value in the options and the given function returns "Just" then a "Success" is returned, otherwise a
-- "Failure".
getRequiredWith :: Text -> (POptionValue -> Maybe a) -> String -> Schema a
getRequiredWith k vf err = Schema $ \o ->
    case lookup k o of
        Just ov -> case vf ov of
            Just v -> Success v
            Nothing -> Failure [err]
        Nothing -> Failure ["Missing key " ++ quote k]

asText :: POptionValue -> Maybe Text
asText (PText t) = Just t
asText _ = Nothing

-- This function has automatic value promotion. This is done so that if a user writes an integer in a float field it will still be accepted.
asFloat :: POptionValue -> Maybe Double
asFloat (PFloat n) = Just n
asFloat (PInteger n) = Just $ fromIntegral n
asFloat _ = Nothing

-- This function has automatic value promotion. This is done so that if a user writes an float style value, with a 0 in the decimal part the
-- value will still be accepted. For example "10.0" would be accepted but "10.2" would not.
asInteger :: POptionValue -> Maybe Int
asInteger (PInteger n) = Just n
asInteger (PFloat n)
    | n == fromInteger (round n) = Just $ round n
    | otherwise = Nothing
asInteger _ = Nothing

asBool :: POptionValue -> Maybe Bool
asBool (PBool b) = Just b
asBool _ = Nothing

--------------------
-- COMBINATOR FUNCTIONS
--------------------
{-
Combinator types:
- Require: The key must be present.
- Try: The key may be present, if not "Nothing" is returned.
- With: They only accept the value associated with the key if the value causes the given function to return "Just <v>".
-}
requireText :: Text -> Schema Text
requireText k = getRequiredWith k asText ("Expected text key for " ++ quote k)

tryText :: Text -> Schema (Maybe Text)
tryText k = getMaybeWith k asText (failType k)

requireTextWith :: Text -> (Text -> Maybe a) -> String -> Schema a
requireTextWith k vf err = getRequiredWith k (asText >=> vf) err

tryTextWith :: Text -> (Text -> Maybe a) -> String -> Schema (Maybe a)
tryTextWith k vf err = getMaybeWith k (asText >=> vf) err


requireFloat :: Text -> Schema Double
requireFloat k = getRequiredWith k asFloat ("Expected number key for " ++ quote k)

requireFloatWith :: Text -> (Double -> Maybe a) -> String -> Schema a
requireFloatWith k vf err = getRequiredWith k (asFloat >=> vf) err

tryFloatWith :: Text -> (Double -> Maybe a) -> String -> Schema (Maybe a)
tryFloatWith k vf err = getMaybeWith k (asFloat >=> vf) err


requireInteger :: Text -> Schema Int
requireInteger k = getRequiredWith k asInteger ("Expected integer key for " ++ quote k)

requireIntegerWith :: Text -> (Int -> Maybe a) -> String -> Schema a
requireIntegerWith k vf err = getRequiredWith k (asInteger >=> vf) err

tryIntegerWith :: Text -> (Int -> Maybe a) -> String -> Schema (Maybe a)
tryIntegerWith k vf err = getMaybeWith k (asInteger >=> vf) err


requireBool :: Text -> Schema Bool
requireBool k = getRequiredWith k asBool ("Expected boolean key for " ++ quote k)

tryBool :: Text -> Schema (Maybe Bool)
tryBool k = getMaybeWith k asBool (failType k)


-- Ensures only valid keys are present, fails if an element not in the given key list is in the options.
ensureValidKeys :: String -> [Text] -> Schema a -> Schema a
ensureValidKeys err keys s = Schema $ \o ->
    let invalidKey = filter (\e -> notElem e keys) (map fst o)
    in if null invalidKey
        then (runSchema s o) -- If the key check succeeded the inner validation schema is run.
        else Failure ["Invalid keys: " ++ quoteList invalidKey ++ ". " ++ err]