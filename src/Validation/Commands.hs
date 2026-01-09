{-# LANGUAGE OverloadedStrings #-}

module Validation.Commands where

import Datatypes.ParseTokens
import Datatypes.ValidatedTokens
import Validation.Schema
import Validation.Configuration
import Common

import Control.Applicative
import Control.Monad

import Data.Validation

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L

-- Font validation.
validateNamedFont :: Text -> Maybe FontName
validateNamedFont t = case T.toLower t of
    "helvetica" -> Just TextFontHelvetica
    "courier" -> Just TextFontCourier
    "times" -> Just TextFontTimes
    _ -> Nothing
-- Text justification validation.
validateJustification :: Text -> Maybe TextJustify
validateJustification t = case T.toLower t of
    "left" -> Just TextJustifyLeft
    "right" -> Just TextJustifyRight
    "centred" -> Just TextJustifyCentred
    "full" -> Just TextJustifyFull
    _ -> Nothing

{-
namedTitleSchema :: Schema POptionPair (Text -> VTitle)
namedTitleSchema = (\t -> VTitle t) <$>
    requireTextWith "font" validateFont
-}

validateCommand :: PCommOpt -> Validation [String] VComm
validateCommand (PCommOpt (PConfig cfg) opts) = VConfig <$> validateConfig cfg opts
validateCommand (PCommOpt (PTitle text) opts) = undefined