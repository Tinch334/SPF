{-# LANGUAGE OverloadedStrings #-}

module Validation.Document (validateDocument) where

import Datatypes.ParseTokens
import Datatypes.ValidatedTokens
import Datatypes.Located
import Validation.Rules
import Common

import Control.Applicative

import Data.Validation (Validation(..))
import Data.List (foldl')


-- Returns the validated document,
validateDocument :: ParsedDocument -> Validation [LocatedError] (ValidatedDocument)
validateDocument (ParsedDocument cfg meta cnt) =
    let cfgOrErr = mergeOpts <$> traverse validateConfig cfg
        metaOrErr = validateMeta meta
        vCommOrErr = traverse validateCommand cnt 
        blockOrErr = case vCommOrErr of
            Success lst -> makeBlock lst
            Failure err -> Failure err in
        ValidatedDocument <$> cfgOrErr <*> metaOrErr <*> blockOrErr


------------------------
-- OPTION HANDLING FUNCTIONS
------------------------
-- Receives a list of commands and returns a single "VConfig" with the merged options of all configuration commands.
mergeOpts :: [Located VConfig] -> VConfig
mergeOpts opts = merge
    (foldl' merge emptyVConfig $ map (\(Located _ op) -> op) opts) -- A strict foldl is used in case there are many options.
    defaultVConfig -- This is done to complete any configurations not specified by the document.

-- The second configuration is prioritized, to preserve user configurations if possible.
merge :: VConfig -> VConfig -> VConfig
merge op1 op2 = VConfig
    { cfgPageSize         = cfgPageSize op1         <|> cfgPageSize op2
    , cfgPageNumbering    = cfgPageNumbering op1    <|> cfgPageNumbering op2
    , cfgSectionSpacing   = cfgSectionSpacing op1   <|> cfgSectionSpacing op2
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
    , cfgSectionSize      = cfgSectionSize op1      <|> cfgSectionSize op2
    , cfgSubsectionSize   = cfgSubsectionSize op1   <|> cfgSubsectionSize op2
    , cfgJustification    = cfgJustification op1    <|> cfgJustification op2
    , cfgListStyle        = cfgListStyle op1        <|> cfgListStyle op2
    , cfgVertMargin       = cfgVertMargin op1       <|> cfgVertMargin op2
    , cfgHozMargin        = cfgHozMargin op1        <|> cfgHozMargin op2
    }


------------------------
-- BLOCK CREATION FUNCTIONS
------------------------
-- Converts a list of located commands into a tree separated by sections.
makeBlock :: [Located VComm] -> Validation [LocatedError] [DocBlock]
makeBlock [] = Success []
makeBlock ((Located pos (VSubsection _ _ _)):rest) =
    Failure [at pos $ "Invalid document structure " ++ quote "subsection" ++ " without preceding " ++ quote "section"]
makeBlock ((Located pos s@(VSection _ _ _)):rest) =
    let (cnt, notC) = span isNotSection rest -- Get all elements until next section.
        ch = groupSubsections cnt
    in ((BlockSection s ch) :) <$> (makeBlock notC) -- Applies concatenation in case of success.
    where
        isNotSection (Located _ (VSection _ _ _)) = False
        isNotSection _ = True
makeBlock ((Located pos comm):rest) =
    ((BlockLeaf comm) :) <$> makeBlock rest

-- Groups all elements under their corresponding subsections.
groupSubsections :: [Located VComm] -> [DocBlock]
groupSubsections [] = []
groupSubsections ((Located _ s@(VSubsection _ _ _)):rest) =
    let (cnt, notC) = span isNotHeader rest -- Get all elements until next subsection.
        ch = map (\(Located _ comm) -> BlockLeaf comm) cnt
    in (BlockSubsection s ch):(groupSubsections notC)

    where
        isNotHeader (Located _ (VSection _ _ _)) = False
        isNotHeader (Located _ (VSubsection _ _ _)) = False
        isNotHeader _ = True
groupSubsections ((Located pos comm):rest) = (BlockLeaf comm):(groupSubsections rest)