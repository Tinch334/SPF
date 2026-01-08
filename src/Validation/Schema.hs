{-# LANGUAGE GeneralisedNewtypeDeriving #-} -- For generic Functor and Applicative instances.

module Validation.Schema where

import Control.Applicative
import Control.Monad

import Data.Validation

-- This data type is used to validate lists of options for commands. A newtype is used to allow for generic Functor and Applicative instances,
-- which allow for sequential validations to be performed.
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