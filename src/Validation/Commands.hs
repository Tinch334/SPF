{-# LANGUAGE OverloadedStrings #-}

module Validation.Commands where

import Datatypes.ParseTokens
import Datatypes.ValidatedTokens
import Validation.Schema
import Validation.Configuration
import Validation.GenericValidations
import Common

import Control.Applicative
import Control.Monad

import Data.Validation

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L


--------------------
-- AUXILIARY FUNCTIONS
--------------------
convertText :: [PText] -> [VText]
convertText tLst = map cnvInner tLst where
    cnvInner (PNormal      t) = VText {text = t, style = Normal}
    cnvInner (PBold        t) = VText {text = t, style = Bold}
    cnvInner (PItalic      t) = VText {text = t, style = Italic}
    cnvInner (PEmphasised  t) = VText {text = t, style = Emphasised}
    cnvInner (PVerbatim    t) = VText {text = t, style = Verbatim}
    cnvInner (PQuoted      t) = VText {text = t, style = Quoted}


--------------------
-- SCHEMA VALIDATION FUNCTIONS
--------------------
namedFontSizeSchema :: Schema POptionPair (Maybe Pt)
namedFontSizeSchema = tryNumberWith "size" (validatePositiveInst Pt) "Font size must be positive"

namedFontNameSchema :: Schema POptionPair (Maybe Font)
namedFontNameSchema = tryTextWith
    "font"
    validateFont
    ("Expected field " ++ quote "font" ++ " to be one of " ++ quoteList ["helvetica", "courier", "times"] ++ ".")


-- Validates options for text, with options for font type and size.
namedFontWithSize :: [PText] -> POption -> Validation [String] VComm
namedFontWithSize t (POptionMap o) = runSchema (ensureValidKeys
    "Expected font and size"
    ["font", "size"]
    (VTitle (convertText t) <$> namedFontNameSchema <*> namedFontSizeSchema)) o
namedFontWithSize t POptionNone = Success $ VTitle (convertText t) Nothing Nothing


validateCommand :: PCommOpt -> Validation [String] VComm
validateCommand (PCommOpt (PConfig cfg) opts) = VConfigComm <$> validateConfig cfg opts
validateCommand (PCommOpt (PTitle text) opts) = namedFontWithSize text opts
