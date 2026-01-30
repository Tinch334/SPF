module Validation.Document (validateDocument) where

import Datatypes.ParseTokens
import Datatypes.ValidatedTokens
import Datatypes.Located
import Validation.Rules

import Control.Applicative

import Data.Validation (Validation(..))


-- Validate the entire document.
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
merge old new = VConfig
    { layout    = mergeLayout (layout old) (layout new)
    , styles    = mergeStyles (styles old) (styles new)
    , sizes     = mergeSizes (sizes old) (sizes new)
    , spacing   = mergeSpacing (spacing old) (spacing new)
    , toggles   = mergeToggle (toggles old) (toggles new)
    }

-- Helpers for merging sub-records
mergeLayout :: LayoutConfig -> LayoutConfig -> LayoutConfig
mergeLayout o n = LayoutConfig
    { pageSize      = pageSize   o  <|> pageSize   n
    , numbering     = numbering  o  <|> numbering  n
    , marginVert    = marginVert o  <|> marginVert n
    , marginHoz     = marginHoz  o  <|> marginHoz  n
    }

mergeStyles :: StyleConfig -> StyleConfig -> StyleConfig
mergeStyles o n = StyleConfig
    { font          = font          o   <|> font          n
    , justification = justification o   <|> justification n
    , listType      = listType      o   <|> listType      n
    }

mergeSizes :: SizeConfig -> SizeConfig -> SizeConfig
mergeSizes o n = SizeConfig
    { paragraphSize  = paragraphSize  o <|> paragraphSize  n
    , titleSize      = titleSize      o <|> titleSize      n
    , sectionSize    = sectionSize    o <|> sectionSize    n
    , subsectionSize = subsectionSize o <|> subsectionSize n
    , verbatimSize   = verbatimSize   o <|> verbatimSize   n
    }

mergeSpacing :: SpacingConfig -> SpacingConfig -> SpacingConfig
mergeSpacing o n = SpacingConfig
    { sectionSp   = sectionSp   o   <|> sectionSp   n
    , paragraphSp = paragraphSp o   <|> paragraphSp n
    , listSp      = listSp      o   <|> listSp      n
    , tableSp     = tableSp     o   <|> tableSp     n
    , figureSp    = figureSp    o   <|> figureSp    n
    , verbatimSp  = verbatimSp  o   <|> verbatimSp  n
    , parIndent   = parIndent   o   <|> parIndent   n
    }

mergeToggle :: ToggleConfig -> ToggleConfig -> ToggleConfig
mergeToggle o n = ToggleConfig
    { sectionNumbering  = sectionNumbering  o   <|> sectionNumbering  n
    , figureNumbering   = figureNumbering   o   <|> figureNumbering   n
    , verbatimNumbering = verbatimNumbering o   <|> verbatimNumbering n
    }