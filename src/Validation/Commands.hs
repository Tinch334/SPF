{-# LANGUAGE OverloadedStrings #-}

module Validation.Commands (validateCommand) where

import Validation.Configuration
import Validation.GenericValidations
import Validation.Schema
import Datatypes.Located
import Datatypes.ParseTokens
import Datatypes.ValidatedTokens
import Common

import Control.Applicative
import Control.Monad

import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validation
import GHC.Float (double2Int)
import Text.Megaparsec (SourcePos)

type CommandValidationType = Validation [String] VComm
type LocatedCommandValidationType = Validation [LocatedError] (Located VComm)


--------------------
-- AUXILIARY FUNCTIONS
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

withPos :: SourcePos -> CommandValidationType -> LocatedCommandValidationType
withPos p v = case v of
  Failure errs -> Failure (map (at p) errs)
  Success s -> Success (Located p s) -- Return position for further error reporting.

--------------------
-- SCHEMA VALIDATION FUNCTIONS
--------------------
namedFontSizeSchema :: Schema (Maybe Pt)
namedFontSizeSchema = tryNumberWith "size" (validateNumInst (> 0) Pt) "Font size must be positive"

namedFontNameSchema :: Schema (Maybe Font)
namedFontNameSchema =
  tryTextWith
    "font"
    validateFont
    ("Expected field " ++ quote "font" ++ " to be one of " ++ quoteList ["helvetica", "courier", "times"] ++ ".")

--------------------
-- COMMAND VALIDATION FUNCTIONS
--------------------
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

-- Figure validation.
namedFigure :: String -> POption -> CommandValidationType
namedFigure p (POptionMap o) =
  runSchema
    ( ensureValidKeys
        ("Expected some of fields " ++ quoteList ["width", "caption"])
        ["width", "caption"]
        ( VFigure p
            <$> requireNumberWith "width" (validateNumInst (\n -> n > 0 && n <= 1) PageWidth) "Figure width must be between 0 and 1"
            <*> ((fmap Caption) <$> tryText "caption")
        )
    ) o
namedFigure p POptionNone = Failure ["Expected one numeric value (width)"]

-- Table validation.
validateTable :: [[[PText]]] -> Int -> Validation [String] [[[VText]]]
validateTable c rLen =
  let cLen = map length c
   in if all (\l -> l == rLen) cLen
        then Success $ map (\r -> map convertText r) c
        else Failure ["Rows of different length in table"]

-- Table row validation.
namedTable :: [[[PText]]] -> POption -> CommandValidationType
namedTable cnt (POptionMap o) =
  let columnCount = runSchema (ensureValidKeys "Expected one numeric value (columns)" ["columns"] (requireNumberWith "columns" (validateNumInst (> 0) (\d -> TableColumns (double2Int d))) "Column number must be positive")) o
    in case columnCount of
        Success t@(TableColumns tc) -> VTable <$> validateTable cnt tc <*> pure t
        Failure e -> Failure e
namedTable c POptionNone = Failure ["Expected one numeric value (columns)"]

-- Validates a list.
namedList :: [[PText]] -> POption -> CommandValidationType
namedList lst (POptionMap o) =
  runSchema
    ( ensureValidKeys
        ("Expected field " ++ quote "style" ++ " to be one of " ++ quoteList ["bullet", "square", "arrow", "number"])
        ["style"]
        ( VList (map convertText lst)
            <$> tryTextWith "style" validateListStyle ("Unknown list style. Expected one of " ++ quoteList ["bullet", "square", "arrow", "number"])
        )
    ) o
namedList lst POptionNone = Success $ VList (map convertText lst) Nothing

-- Paragraph validation.
namedParagraph :: [PText] -> POption -> CommandValidationType
namedParagraph txt (POptionMap o) =
  runSchema
    ( ensureValidKeys
        ("Expected some of fields " ++ quoteList ["font", "size", "justification"])
        ["font", "size", "justification"]
        ( VParagraph (convertText txt)
            <$> namedFontNameSchema
            <*> ((fmap FontSize) <$> namedFontSizeSchema)
            <*> tryTextWith "justification" validateJustification ("Expected field " ++ quote "justification" ++ " to be one of " ++ quoteList ["left", "right", "centred", "full"])
        )
    ) o
namedParagraph txt POptionNone = Success $ VParagraph (convertText txt) Nothing Nothing Nothing


--------------------
-- COMMAND VALIDATION
--------------------
validateCommand :: Located PCommOpt -> LocatedCommandValidationType
validateCommand (Located pos comm) =
  withPos pos $ case comm of
    PCommOpt (PConfig cfg) opts       -> VConfigComm <$> validateConfig cfg opts
    PCommOpt (PTitle text) opts       -> namedFontWithSize VTitle text opts
    PCommOpt (PAuthor text) opts      -> namedFontWithSize VAuthor text opts
    PCommOpt (PDate text) opts        -> namedFontWithSize VDate text opts
    PCommOpt (PSection text) opts     -> namedFontWithSize VSection text opts 
    PCommOpt (PSubsection text) opts  -> namedFontWithSize VSubsection text opts
    PCommOpt (PFigure path) opts      -> namedFigure path opts
    PCommOpt (PTable rows) opts       -> namedTable rows opts
    PCommOpt (PList lst) opts         -> namedList lst opts
    PCommOpt (PParagraph txt) opts    -> namedParagraph txt opts
    PCommOpt PNewpage POptionNone     -> Success VNewpage
    PCommOpt PNewpage _               -> Failure ["The command " ++ quote "newpage" ++ " doesn't accept any options"]
    PCommOpt PHLine POptionNone       -> Success VHLine
    PCommOpt PHLine _                 -> Failure ["The command " ++ quote "hline" ++ " doesn't accept any options"]