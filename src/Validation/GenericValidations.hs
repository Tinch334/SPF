{-# LANGUAGE OverloadedStrings #-}

module Validation.GenericValidations where

import Datatypes.ValidatedTokens
import Common

import Control.Applicative
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T

--------------------
-- GENERAL VALIDATION FUNCTIONS
--------------------
-- Takes a number, returns it if it's positive, otherwise Nothing.
validateNumInst :: (Num a, Ord a) => (a -> Bool) -> (a -> b) -> a -> Maybe b
validateNumInst vf i n = if vf n then Just (i n) else Nothing

-- Page size validation.
validateSize :: Text -> Maybe PageSize
validateSize t = case T.toLower t of
    "a4"    -> Just SizeA4
    "a3"    -> Just SizeA3
    "legal" -> Just SizeLegal
    _   -> Nothing

-- Page numbering validation.
validateNumbering :: Text -> Maybe PageNumbering
validateNumbering t = case T.toLower t of
    "arabic" -> Just NumberingArabic
    "roman" -> Just NumberingRoman
    "none" -> Just NumberingNone
    _ -> Nothing

-- Font validation.
validateFont :: Text -> Maybe Font
validateFont t = case T.toLower t of
    "helvetica" -> Just Helvetica
    "courier" -> Just Courier
    "times" -> Just Times
    _ -> Nothing

-- Text justification validation.
validateJustification :: Text -> Maybe Justification
validateJustification t = case T.toLower t of
    "left" -> Just JustifyLeft
    "right" -> Just JustifyRight
    "center" -> Just JustifyCenter
    "full" -> Just JustifyFull
    _ -> Nothing