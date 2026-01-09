{-# LANGUAGE GeneralisedNewtypeDeriving #-} -- For generic Functor and Applicative instances.

module Validation.Schema where

import Control.Applicative
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import Data.Validation

import Datatypes.ParseTokens
import Common

-- This data type is used to validate lists of options for commands. A newtype is used for generic Functor and Applicative instances. The
-- Applicative instance allows for sequential validations to be performed.
newtype Schema a b = Schema {
    runSchema :: [a] -> Validation [String] b
}

-- The important definition here is the Applicative one, it allows for the sequential application of Schemas.
instance Functor (Schema env) where
    fmap f (Schema g) = Schema (\o -> fmap f (g o))

instance Applicative (Schema env) where
    pure x = Schema (\_ -> Success x)
    (Schema f) <*> (Schema g) = Schema (\o -> f o <*> g o)


-- Returns a valid schema with the corresponding values, or an error if no valid matches are found.
choiceSchema :: [Schema a b] -> Schema a b
choiceSchema [] = Schema (\_ -> Failure ["Options did not match any valid form"])
choiceSchema ((Schema s):xs) =
    Schema $ \o ->
        case s o of
            Success v -> Success v
            -- Errors are accumulated, to give the user more information.
            Failure e -> case runSchema (choiceSchema xs) o of
                Success v' -> Success v'
                Failure e' -> Failure (e <> e') -- Errors have to implement <>, they are a semigroup.

-- Takes a key, if it corresponds to a numeric value has it then a "Success" is returned, otherwise a "Failure".
requireNumber :: Text -> Schema POptionPair Double
requireNumber k = Schema $ \o ->
    case lookup k o of
        Just (PNumber n) -> Success n
        Just _ -> Failure ["Map option key " ++ quote k ++ " has wrong type"]
        Nothing -> Failure ["Missing key " ++ quote k]

-- Takes a key, if it corresponds to a text value has it and the given function returns "Just" then a "Success" is returned, otherwise a
-- "Failure". Could be implemented using requireNumber, however that would require more lines, a small amount of repetition is acceptable.
requireNumberWith :: Text -> (Double -> Maybe b) -> String -> Schema POptionPair b
requireNumberWith k vf err = Schema $ \o ->
    case lookup k o of
        Just (PNumber n) -> validate [err] vf n
        Just _ -> Failure ["Map option key " ++ quote k ++ " has wrong type"]
        Nothing -> Failure ["Missing key " ++ quote k]

-- Takes a key, if it corresponds to a text value has it then a "Success" is returned, otherwise a "Failure".
requireText :: Text -> Schema POptionPair Text
requireText k = Schema $ \o ->
    case lookup k o of
        Just (PText t) -> Success t
        Just _ -> Failure ["Map option key " ++ quote k ++ " has wrong type"]
        Nothing -> Failure ["Missing key " ++ quote k]

-- Takes a key, if it corresponds to a text value has it and the given function returns "Just" then a "Success" is returned, otherwise a
-- "Failure".
requireTextWith :: Text -> (Text -> Maybe b) -> String -> Schema POptionPair b
requireTextWith k vf err = Schema $ \o ->
    case lookup k o of
        Just (PText t) -> validate [err] vf t
        Just _ -> Failure ["Map option key " ++ quote k ++ " has wrong type"]
        Nothing -> Failure ["Missing key " ++ quote k]


-- Ensures only valid keys are present, fails if an element not in the given key list is in the options.
ensureValidKeys :: String -> [Text] -> Schema POptionPair b -> Schema POptionPair b
ensureValidKeys err keys s = Schema $ \o ->
    let invalidKey = filter (\e -> notElem e keys) (map fst o) in
    if null invalidKey
        then (runSchema s o) -- If the key check succeeded the inner validation schema is run.
        else Failure ["Invalid keys: " ++ quoteList invalidKey ++ ". " ++ err]