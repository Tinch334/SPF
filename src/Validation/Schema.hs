{-# LANGUAGE OverloadedStrings #-}

module Validation.Schema
    ( Schema(..)
    , runSchema
    , choiceSchema
    -- Primitives.
    , requireText
    , tryText
    , requireTextWith
    , tryTextWith
    , requireNumber
    , requireNumberWith
    , tryNumberWith
    , ensureValidKeys
    , requireBool
    , tryBool
    -- Generic validators.
    , validateNumInst
    , validateSize
    , validateNumbering
    , validateFont
    , validateJustification
    , validateListStyle
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

validateSize :: Text -> Maybe PageSize
validateSize t = case T.toLower t of
    "a4"    -> Just SizeA4
    "a3"    -> Just SizeA3
    "legal" -> Just SizeLegal
    _       -> Nothing

validateNumbering :: Text -> Maybe PageNumbering
validateNumbering t = case T.toLower t of
    "arabic" -> Just NumberingArabic
    "roman"  -> Just NumberingRoman
    "none"   -> Just NumberingNone
    _        -> Nothing

validateFont :: Text -> Maybe Font
validateFont t = case T.toLower t of
    "helvetica" -> Just Helvetica
    "courier"   -> Just Courier
    "times"     -> Just Times
    _           -> Nothing

validateJustification :: Text -> Maybe Justification
validateJustification t = case T.toLower t of
    "left"   -> Just JustifyLeft
    "right"  -> Just JustifyRight
    "center" -> Just JustifyCenter
    "full"   -> Just JustifyFull
    _        -> Nothing

validateListStyle :: Text -> Maybe ListStyle
validateListStyle s = case T.toLower s of
  "bullet" -> Just ListBullet
  "square" -> Just ListSquare
  "arrow"  -> Just ListArrow
  "number" -> Just ListNumber
  _        -> Nothing


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

asNumber :: POptionValue -> Maybe Double
asNumber (PNumber n) = Just n
asNumber _ = Nothing

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


requireNumber :: Text -> Schema Double
requireNumber k = getRequiredWith k asNumber ("Expected number key for " ++ quote k)

requireNumberWith :: Text -> (Double -> Maybe a) -> String -> Schema a
requireNumberWith k vf err = getRequiredWith k (asNumber >=> vf) err

tryNumberWith :: Text -> (Double -> Maybe a) -> String -> Schema (Maybe a)
tryNumberWith k vf err = getMaybeWith k (asNumber >=> vf) err


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