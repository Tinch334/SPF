{-# LANGUAGE OverloadedStrings #-}

module Validation.GenericValidations where

import Datatypes.ParseTokens
import Datatypes.ValidatedTokens
import Validation.Schema
import Common

import Control.Applicative
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import Data.Validation (Validation(..))


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

-- List style validation.
validateListStyle :: Text -> Maybe ListStyle
validateListStyle s = case T.toLower s of
  "bullet" -> Just ListBullet
  "square" -> Just ListSquare
  "arrow" -> Just ListArrow
  "number" -> Just ListNumber
  _ -> Nothing


--------------------
-- GENERAL SCHEMA VALIDATION FUNCTIONS
--------------------
-- Converts parsed text into validated text, no actual validation is required, just conversion.
convertText :: [PText] -> [VText]
convertText tLst = map cnvInner tLst
  where
    cnvInner (PNormal t) = VText {text = t, style = Normal}
    cnvInner (PBold t) = VText {text = t, style = Bold}
    cnvInner (PItalic t) = VText {text = t, style = Italic}
    cnvInner (PEmphasised t) = VText {text = t, style = Emphasised}
    cnvInner (PVerbatim t) = VText {text = t, style = Verbatim}
    cnvInner (PQuoted t) = VText {text = t, style = Quoted}

namedFontSizeSchema :: Schema (Maybe Pt)
namedFontSizeSchema = tryNumberWith "size" (validateNumInst (> 0) Pt) "Font size must be positive"

namedFontNameSchema :: Schema (Maybe Font)
namedFontNameSchema =
  tryTextWith
    "font"
    validateFont
    ("Expected field " ++ quote "font" ++ " to be one of " ++ quoteList ["helvetica", "courier", "times"] ++ ".")

-- Validates options for text, with options for font type and size.
-- namedFontWithSize :: ([VText] -> Maybe Font -> Maybe Pt -> VComm) [PText] -> POption -> CommandValidationType
namedFontWithSize c t (POptionMap o) =
  runSchema
    ( ensureValidKeys
        ("Expected some of fields " ++ quoteList ["font", "size"])
        ["font", "size"]
        -- The double map is required to lift the font size inside the schema and the FontSize constructor, this is required because <*> expects
        -- "f a" as it's second argument.
        (c (convertText t) <$> namedFontNameSchema <*> ((fmap FontSize) <$> namedFontSizeSchema))
    ) o
namedFontWithSize c t POptionNone = Success $ c (convertText t) Nothing Nothing