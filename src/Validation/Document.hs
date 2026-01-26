module Validation.Document (validateDocument) where

import Datatypes.ParseTokens
import Datatypes.ValidatedTokens
import Datatypes.Located
import Validation.Rules

import Control.Applicative

import Data.Validation (Validation(..))


validateDocument :: ParsedDocument -> Validation [LocatedError] ValidatedDocument
validateDocument (ParsedDocument cfg meta cnt) =
    let cfgOrErr = mergeOpts <$> traverse validateConfig cfg
        cnvMeta = convertMeta meta
        cntOrErr = traverse validateCommand cnt in
        ValidatedDocument <$> cfgOrErr <*> cnvMeta <*> cntOrErr


-- Receives a list of commands and returns a single "VConfig" with the merged options of all configuration commands.
mergeOpts :: [Located VConfig] -> VConfig
mergeOpts opts = merge
    (foldl merge emptyVConfig $ map (\(Located _ op) -> op) opts)
    defaultVConfig -- Completes any configuration values not specified in the document.

-- The second configuration is prioritized, to preserve user configurations if possible.
merge :: VConfig -> VConfig -> VConfig
merge op1 op2 = VConfig
    { cfgPageSize           = cfgPageSize op1           <|> cfgPageSize op2
    , cfgPageNumbering      = cfgPageNumbering op1      <|> cfgPageNumbering op2
    , cfgSectionSpacing     = cfgSectionSpacing op1     <|> cfgSectionSpacing op2
    , cfgParagraphSpacing   = cfgParagraphSpacing op1   <|> cfgParagraphSpacing op2
    , cfgListSpacing        = cfgListSpacing op1        <|> cfgListSpacing op2
    , cfgTableSpacing       = cfgTableSpacing op1       <|> cfgTableSpacing op2
    , cfgFigureSpacing      = cfgFigureSpacing op1      <|> cfgFigureSpacing op2
    , cfgSpacingGlue        = cfgSpacingGlue op1        <|> cfgSpacingGlue op2
    , cfgTextGlue           = cfgTextGlue op1           <|> cfgTextGlue op2
    , cfgParIndent          = cfgParIndent op1          <|> cfgParIndent op2
    , cfgFont               = cfgFont op1               <|> cfgFont op2
    , cfgParSize            = cfgParSize op1            <|> cfgParSize op2
    , cfgTitleSize          = cfgTitleSize op1          <|> cfgTitleSize op2
    , cfgSectionSize        = cfgSectionSize op1        <|> cfgSectionSize op2
    , cfgSubsectionSize     = cfgSubsectionSize op1     <|> cfgSubsectionSize op2
    , cfgJustification      = cfgJustification op1      <|> cfgJustification op2
    , cfgListStyle          = cfgListStyle op1          <|> cfgListStyle op2
    , cfgVertMargin         = cfgVertMargin op1         <|> cfgVertMargin op2
    , cfgHozMargin          = cfgHozMargin op1          <|> cfgHozMargin op2
    , cfgSectionNumbering   = cfgSectionNumbering op1   <|> cfgSectionNumbering op2
    , cfgFigureNumbering    = cfgFigureNumbering op1    <|> cfgFigureNumbering op2
    }