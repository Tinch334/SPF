module Validation.Document (validateDocument) where

import Datatypes.ParseTokens
import Datatypes.ValidatedTokens
import Datatypes.Located
import Validation.Commands
import Validation.Configuration
import Validation.Metadata

import Control.Applicative

import Data.Validation (Validation(..))


validateDocument :: ParsedDocument -> Validation [LocatedError] ValidatedDocument
validateDocument (ParsedDocument cfg meta cnt) =
    let cfgOrErr = mergeOpts <$> traverse validateConfig cfg
        metaOrErr = validateMeta meta
        cntOrErr = traverse validateCommand cnt in
        ValidatedDocument <$> cfgOrErr <*> metaOrErr <*> cntOrErr


-- Receives a list of commands and returns a single "VConfig" with the merged options of all configuration commands.
mergeOpts :: [Located VConfig] -> VConfig
mergeOpts opts = foldl merge defaultVConfig $ map (\(Located _ op) -> op) opts

-- The second configuration is prioritized, to preserve user configurations if possible.
merge :: VConfig -> VConfig -> VConfig
merge op1 op2 = VConfig
    { cfgPageSize         = cfgPageSize op2         <|> cfgPageSize op1
    , cfgPageNumbering    = cfgPageNumbering op2    <|> cfgPageNumbering op1
    , cfgSectionSpacing   = cfgSectionSpacing op2   <|> cfgSectionSpacing op1
    , cfgParagraphSpacing = cfgParagraphSpacing op2 <|> cfgParagraphSpacing op1
    , cfgListSpacing      = cfgListSpacing op2      <|> cfgListSpacing op1
    , cfgTableSpacing     = cfgTableSpacing op2     <|> cfgTableSpacing op1
    , cfgFigureSpacing    = cfgFigureSpacing op2    <|> cfgFigureSpacing op1
    , cfgSpacingGlue      = cfgSpacingGlue op2      <|> cfgSpacingGlue op1
    , cfgTextGlue         = cfgTextGlue op2         <|> cfgTextGlue op1
    , cfgParIndent        = cfgParIndent op2        <|> cfgParIndent op1
    , cfgFont             = cfgFont op2             <|> cfgFont op1
    , cfgParSize          = cfgParSize op2          <|> cfgParSize op1
    , cfgTitleSize        = cfgTitleSize op2        <|> cfgTitleSize op1
    , cfgSectionSize      = cfgSectionSize op2      <|> cfgSectionSize op1
    , cfgSubsectionSize   = cfgSubsectionSize op2   <|> cfgSubsectionSize op1
    , cfgJustification    = cfgJustification op2    <|> cfgJustification op1
    , cfgListStyle        = cfgListStyle op2        <|> cfgListStyle op1
    }