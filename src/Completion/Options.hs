module Completion.Options (mergeOpts) where

import Control.Applicative

import Datatypes.ValidatedTokens
import Datatypes.Located
import Common (filterMap)

-- Receives a list of commands and returns a single "VConfig" with the merged options of all configuration commands.
mergeOpts :: [Located VComm] -> VConfig
mergeOpts opts = foldl merge defaultVConfig $ filterMap isConfigComm (\(Located _ op) -> op) opts

merge :: VConfig -> VComm -> VConfig
merge op1 (VConfigComm op2) = VConfig
    { cfgPageSize         = cfgPageSize op1         <|> cfgPageSize op2
    , cfgPageNumbering    = cfgPageNumbering op1    <|> cfgPageNumbering op2
    , cfgTitleSpacing     = cfgTitleSpacing op1     <|> cfgTitleSpacing op2
    , cfgParagraphSpacing = cfgParagraphSpacing op1 <|> cfgParagraphSpacing op2
    , cfgListSpacing      = cfgListSpacing op1      <|> cfgListSpacing op2
    , cfgTableSpacing     = cfgTableSpacing op1     <|> cfgTableSpacing op2
    , cfgFigureSpacing    = cfgFigureSpacing op1    <|> cfgFigureSpacing op2
    , cfgSpacingGlue      = cfgSpacingGlue op1      <|> cfgSpacingGlue op2
    , cfgTextGlue         = cfgTextGlue op1         <|> cfgTextGlue op2
    , cfgParIndent        = cfgParIndent op1        <|> cfgParIndent op2
    , cfgFont             = cfgFont op1             <|> cfgFont op2
    , cfgParSize          = cfgParSize op1          <|> cfgParSize op2
    , cfgTitleSize        = cfgTitleSize op1        <|> cfgTitleSize op2
    , cfgSubtitleSize     = cfgSubtitleSize op1     <|> cfgSubtitleSize op2
    , cfgJustification    = cfgJustification op1    <|> cfgJustification op2
    , cfgListStyle        = cfgListStyle op1        <|> cfgListStyle op2
    }

isConfigComm :: Located VComm -> Bool
isConfigComm (Located _ (VConfigComm _)) = True
isConfigComm _ = False