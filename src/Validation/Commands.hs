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

import GHC.Float (double2Int)


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
namedFontSizeSchema :: Schema (Maybe Pt)
namedFontSizeSchema = tryNumberWith "size" (validateNumInst (> 0) Pt) "Font size must be positive"

namedFontNameSchema :: Schema (Maybe Font)
namedFontNameSchema = tryTextWith
    "font"
    validateFont
    ("Expected field " ++ quote "font" ++ " to be one of " ++ quoteList ["helvetica", "courier", "times"] ++ ".")


-- Validates options for text, with options for font type and size.
--namedFontWithSize :: ([VText] -> Maybe Font -> Maybe Pt -> VComm) [PText] -> POption -> Validation [String] VComm
namedFontWithSize c t (POptionMap o) = runSchema (ensureValidKeys
    "Expected font and/or size"
    ["font", "size"]
    -- The double map is required to lift the font size inside the schema and the FontSize constructor, this is required because <*> expects
    -- "f a" as it's second argument.
    (c (convertText t) <$> namedFontNameSchema <*> ((fmap FontSize) <$> namedFontSizeSchema))) o
namedFontWithSize c t POptionNone = Success $ c (convertText t) Nothing Nothing

namedFigure :: String -> POption -> Validation [String] VComm
namedFigure p (POptionMap o) = runSchema (
    ensureValidKeys "Expected page width and/or caption" ["width", "caption"]
        (VFigure p <$>
            tryNumberWith "width" (validateNumInst (\n -> n > 0 && n <= 1) PageWidth) "Figure width must be between 0 and 1"
            <*> ((fmap Caption) <$> tryText "caption"))
    ) o
namedFigure p POptionNone = Success $ VFigure p Nothing Nothing

validateTable :: [[[PText]]] -> Int -> Validation [String] [[[VText]]]
validateTable c rLen = let cLen = map length c in
    if all (\l -> l == rLen) cLen
        then Success $ map (\r -> map convertText r) c
        else Failure ["Rows of invalid length in table, all rows must have the same length"]

namedTable :: [[[PText]]] -> POption -> Validation [String] VComm
namedTable cnt (POptionMap o) =
    let columnCount = runSchema (ensureValidKeys "Expected column count" ["columns"] (requireNumberWith "columns" (validateNumInst (> 0) (\d -> TableColumns (double2Int d))) "Column number must be positive")) o
    in case columnCount of
        Success t@(TableColumns tc) -> (\cnt' -> VTable cnt' t) <$> (validateTable cnt tc)
        Failure e -> Failure e
namedTable c POptionNone = Failure ["Expected column quantity"]

namedList :: [[PText]] -> POption -> Validation [String] VComm
namedList lst (POptionMap o) = undefined
namedList lst POptionNone = Success $ VList (map convertText lst) Nothing

validateCommand :: PCommOpt -> Validation [String] VComm
validateCommand (PCommOpt (PConfig cfg) opts) = VConfigComm <$> validateConfig cfg opts
validateCommand (PCommOpt (PTitle text) opts) = namedFontWithSize VTitle text opts
validateCommand (PCommOpt (PAuthor text) opts) = namedFontWithSize VAuthor text opts
validateCommand (PCommOpt (PDate text) opts) = namedFontWithSize VDate text opts
validateCommand (PCommOpt (PSection text) opts) = namedFontWithSize VSection text opts
validateCommand (PCommOpt (PSubsection text) opts) = namedFontWithSize VSubsection text opts
validateCommand (PCommOpt (PFigure path) opts) = namedFigure path opts
validateCommand (PCommOpt (PTable rows) opts) = namedTable rows opts
validateCommand (PCommOpt (PList lst) opts) = undefined
validateCommand (PCommOpt PNewpage POptionNone) = Success VNewpage
validateCommand (PCommOpt PNewpage _) = Failure ["The command " ++ quote "newpage" ++ " doesn't accept any options"]
validateCommand (PCommOpt PHLine POptionNone) = Success VHLine
validateCommand (PCommOpt PHLine _) = Failure ["The command " ++ quote "hline" ++ " doesn't accept any options"]
