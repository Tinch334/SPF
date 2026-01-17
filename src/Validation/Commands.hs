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
import System.FilePath (isValid)

import Text.Megaparsec (SourcePos)

type CommandValidationType = Validation [String] VComm


--------------------
-- COMMAND VALIDATION FUNCTIONS
--------------------
-- Figure validation.
namedFigure :: String -> POption -> CommandValidationType
namedFigure p (POptionMap o) =
  if isValid p
    then runSchema
      ( ensureValidKeys
        ("Expected some of fields " ++ quoteList ["width", "caption"])
        ["width", "caption"]
        ( VFigure p
            <$> requireNumberWith "width" (validateNumInst (\n -> n > 0 && n <= 1) PageWidth) "Figure width must be between 0 and 1"
            <*> ((fmap Caption) <$> tryText "caption")
        )
      ) o
    else Failure ["Invalid filepath " ++ quote (T.pack p)]
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
validateCommand :: Located PCommOpt -> Validation [LocatedError] (Located VComm)
validateCommand (Located pos comm) =
  withPos pos $ case comm of
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
    _                                 -> error "INTERNAL: Attempt to validate unknown command"