{-# LANGUAGE OverloadedStrings #-}

module Validation.Rules 
    ( validateCommand
    , validateConfig
    , validateMeta
    ) where

import Validation.Schema
import Datatypes.Located
import Datatypes.ParseTokens
import Datatypes.ValidatedTokens
import Common

import Control.Applicative
import Control.Monad

import Data.Text (Text)
import Data.Validation
import qualified Data.Text as T
import qualified Data.List as L

import GHC.Float (double2Int)
import System.FilePath (isValid)
import Text.Megaparsec (SourcePos)


type CommandValidationType = Validation [String] VComm


--------------------
-- SHARED HELPERS
--------------------
-- Auxiliary functions used for command, metadata and configuration validation.
convertText :: [PText] -> [VText]
convertText = map cnvInner
  where
    cnvInner (PNormal t)     = VText {text = t, style = Normal}
    cnvInner (PBold t)       = VText {text = t, style = Bold}
    cnvInner (PItalic t)     = VText {text = t, style = Italic}
    cnvInner (PEmphasised t) = VText {text = t, style = Emphasised}
    cnvInner (PVerbatim t)   = VText {text = t, style = Verbatim}
    cnvInner (PQuoted t)     = VText {text = t, style = Quoted}

namedFontSizeSchema :: Schema (Maybe Pt)
namedFontSizeSchema = tryNumberWith "size" (validateNumInst (> 0) Pt) "Font size must be positive"

namedFontNameSchema :: Schema (Maybe Font)
namedFontNameSchema =
  tryTextWith
    "font"
    validateFont
    ("Expected field " ++ quote "font" ++ " to be one of " ++ quoteList ["helvetica", "courier", "times"] ++ ".")

namedFontWithSize :: ([VText] -> Maybe Font -> Maybe FontSize -> a) -> [PText] -> POption -> Validation [String] a
namedFontWithSize c t (POptionMap o) =
  runSchema
    ( ensureValidKeys
        ("Expected some of fields " ++ quoteList ["font", "size"])
        ["font", "size"]
        (c (convertText t) <$> namedFontNameSchema <*> ((fmap FontSize) <$> namedFontSizeSchema))
    ) o
namedFontWithSize c t POptionNone = Success $ c (convertText t) Nothing Nothing

--------------------
-- COMMAND VALIDATION
--------------------
-- Helpers for creating Schemas.
namedFigure :: String -> POption -> CommandValidationType
namedFigure p (POptionMap o) =
  if isValid p -- Only checks that the path's format is correct, not that the file exists.
    then runSchema
      ( ensureValidKeys
        ("Expected some of fields " ++ quoteList ["width", "caption"])
        ["width", "caption"]
        ( VFigure p
            <$> requireNumberWith "width" (validateNumInst (\n -> n > 0 && n <= 1) PageWidth) "Figure width must be between 0 and 1"
            <*> (fmap Caption <$> tryText "caption")
        )
      ) o
    else Failure ["Invalid filepath " ++ quote (T.pack p)]
namedFigure p POptionNone = Failure ["Expected one numeric value (width)"]

validateTableMatrix :: [[[PText]]] -> Int -> Validation [String] [[[VText]]]
validateTableMatrix c rLen =
  let cLen = map length c
   in if all (== rLen) cLen
        then Success $ map (map convertText) c
        else Failure ["Rows of different length in table"]

namedTable :: [[[PText]]] -> POption -> CommandValidationType
namedTable cnt (POptionMap o) =
  let columnCount = runSchema (ensureValidKeys "Expected one numeric value (columns)" ["columns"] (requireNumberWith "columns" (validateNumInst (> 0) (\d -> TableColumns (double2Int d))) "Column number must be positive")) o
    in case columnCount of
        Success t@(TableColumns tc) -> VTable <$> validateTableMatrix cnt tc <*> pure t
        Failure e -> Failure e
namedTable _ POptionNone = Failure ["Expected one numeric value (columns)"]

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

namedParagraph :: [PText] -> POption -> CommandValidationType
namedParagraph txt (POptionMap o) =
  runSchema
    ( ensureValidKeys
        ("Expected some of fields " ++ quoteList ["font", "size", "justification"])
        ["font", "size", "justification"]
        ( VParagraph (convertText txt)
            <$> namedFontNameSchema
            <*> (fmap FontSize <$> namedFontSizeSchema)
            <*> tryTextWith "justification" validateJustification ("Expected field " ++ quote "justification" ++ " to be one of " ++ quoteList ["left", "right", "centred", "full"])
        )
    ) o
namedParagraph txt POptionNone = Success $ VParagraph (convertText txt) Nothing Nothing Nothing

-- Validates the given command.
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


--------------------
-- METADATA VALIDATION
--------------------
locateMeta :: SourcePos -> Validation [String] a -> Validation [LocatedError] a
locateMeta p v = case v of
  Failure errs -> Failure $ map (at p) errs
  Success s    -> Success s

validateMeta :: DocumentMetadata -> Validation [LocatedError] ValidatedMetadata
validateMeta (DocumentMetadata t a d) = ValidatedMetadata
  <$> maybeValidate t <*> maybeValidate a <*> maybeValidate d where
    maybeValidate (Just (Located pos (PMetaOpt (PTitle txt) op)))  = locateMeta pos $ Just <$> namedFontWithSize VTitle txt op
    maybeValidate (Just (Located pos (PMetaOpt (PAuthor txt) op))) = locateMeta pos $ Just <$> namedFontWithSize VAuthor txt op
    maybeValidate (Just (Located pos (PMetaOpt (PDate txt) op)))   = locateMeta pos $ Just <$> namedFontWithSize VDate txt op
    maybeValidate Nothing = Success Nothing


--------------------
-- CONFIGURATION VALIDATION
--------------------
-- Auxiliary functions for creating Schemas.
withVal :: (a -> VConfig) -> Schema a -> Schema VConfig
withVal setter s = setter <$> s

namedBeforeAndAfterSchema :: (Spacing -> VConfig) -> Schema VConfig
namedBeforeAndAfterSchema c = c <$> (Spacing <$>
    (Pt <$> requireNumber "before") <*> (Pt <$> requireNumber "after"))

namedGlueSchema :: (Glue -> VConfig) -> Schema VConfig
namedGlueSchema c = c <$> (Glue <$> 
    requireNumberWith "stretch" (validateNumInst (> 0) Pt) "Stretch must be positive"
    <*> requireNumberWith "shrink"  (validateNumInst (> 0) Pt) "Shrink must be positive")

namedFontsizeSchema :: (FontSize -> VConfig) -> Schema VConfig
namedFontsizeSchema c = c <$> (FontSize <$> requireNumberWith "size" (validateNumInst (> 0) Pt) "Font size must be positive")


-- All configuration options fail with no arguments, an auxiliary function avoids repetition.
validateConfigOption :: PConfigArg -> POption -> String -> [Text] -> Schema VConfig -> Validation [String] VConfig
validateConfigOption _ (POptionMap m) err keys schema = runSchema (ensureValidKeys err keys schema) m
validateConfigOption arg POptionNone err _ _ = Failure [err ++ configErrorString arg]

-- Error generator.
configErrorString :: PConfigArg -> String
configErrorString arg = case arg of
    PSize -> "Expected one of: " ++ quoteList ["a4", "a3", "legal"] ++ " or two numeric values (width: pt, height: pt)."
    PPagenumbering -> "Expected field " ++ quote "numbering" ++ " to be one of " ++ quoteList ["arabic", "roman", "none"] ++ ""
    PSectionspacing -> "Expected two numeric values (before: pt, after: pt)"
    PParagraphspacing -> "Expected two numeric values (before: pt, after: pt)" 
    PListspacing -> "Expected two numeric values (before: pt, after: pt)" 
    PTablespacing -> "Expected two numeric values (before: pt, after: pt)" 
    PFigurespacing -> "Expected two numeric values (before: pt, after: pt)" 
    PSpacingglue -> "Expected two numeric values (stretchability: pt, shrinkability: pt)" 
    PTextglue -> "Expected two numeric values (stretchability: pt, shrinkability: pt)" 
    PFont -> "Expected field " ++ quote "font" ++ " to be one of " ++ quoteList ["helvetica", "courier", "times"] ++ "."
    PParsize -> "Expected a numeric value (size: pt)"
    PTitleSize -> "Expected a numeric value (size: pt)"
    PSectionSize -> "Expected a numeric value (size: pt)"
    PSubsectionSize -> "Expected a numeric value (size: pt)"
    PJustification -> "Expected field " ++ quote "justification" ++ " to be one of " ++ quoteList ["left", "right", "centred", "full"]
    PListstyle -> "Expected field " ++ quote "style" ++ " to be one of " ++ quoteList ["bullet", "square", "arrow", "number"]

validateConfig :: Located PConfig -> Validation [LocatedError] (Located VConfig)
validateConfig (Located pos (PConfig arg opt)) = withPos pos $ vc arg opt
  where 
    -- Setters for options.
    sPageSize ps = emptyVConfig { cfgPageSize = Just ps }
    sPageNum p = emptyVConfig { cfgPageNumbering = Just p }
    sTitleSp s = emptyVConfig { cfgSectionSpacing = Just s }
    sParSp s = emptyVConfig { cfgParagraphSpacing = Just s }
    sListSp s = emptyVConfig { cfgListSpacing = Just s }
    sTableSp s = emptyVConfig { cfgTableSpacing = Just s }
    sFigSp s = emptyVConfig { cfgFigureSpacing = Just s }
    sSpGlue g = emptyVConfig { cfgSpacingGlue = Just g }
    sTxtGlue g = emptyVConfig { cfgTextGlue = Just g }
    sFont f = emptyVConfig { cfgFont = Just f }
    sParSz s = emptyVConfig { cfgParSize = Just s }
    sTitleSz s = emptyVConfig { cfgSectionSize = Just s }
    sSubTitleSz s = emptyVConfig { cfgSubsectionSize = Just s }
    sJust j = emptyVConfig { cfgJustification = Just j }
    sListSt s = emptyVConfig { cfgListStyle = Just s }

    -- Command validation.
    vc PSize (POptionMap m) = runSchema
        (choiceSchema
            [ ensureValidKeys (configErrorString PSize) ["size"] (withVal sPageSize $ requireTextWith "size" validateSize ("Unknown page size. " ++ configErrorString PSize))
            , ensureValidKeys (configErrorString PSize) ["width", "height"] (withVal sPageSize $ SizeCustom <$> requireNumberWith "width" (validateNumInst (> 0) Pt) "Pos width" <*> requireNumberWith "height" (validateNumInst (> 0) Pt) "Pos height") 
            ]) m
    vc PSize POptionNone = Failure ["Invalid form for page size. " ++ configErrorString PSize]

    vc PPagenumbering o = validateConfigOption PPagenumbering o "Invalid form for page numbering. " ["numbering"] 
        (withVal sPageNum $ requireTextWith "numbering" validateNumbering ("Unknown type. " ++ configErrorString PPagenumbering))

    vc PSectionspacing o = validateConfigOption PSectionspacing o "Title spacing requires arguments. " ["before", "after"] (namedBeforeAndAfterSchema sTitleSp)
    vc PParagraphspacing o = validateConfigOption PParagraphspacing o "Par spacing requires arguments. " ["before", "after"] (namedBeforeAndAfterSchema sParSp)
    vc PListspacing o = validateConfigOption PListspacing o "List spacing requires arguments. " ["before", "after"] (namedBeforeAndAfterSchema sListSp)
    vc PTablespacing o = validateConfigOption PTablespacing o "Table spacing requires arguments. " ["before", "after"] (namedBeforeAndAfterSchema sTableSp)
    vc PFigurespacing o = validateConfigOption PFigurespacing o "Figure spacing requires arguments. " ["before", "after"] (namedBeforeAndAfterSchema sFigSp)
    
    vc PSpacingglue o = validateConfigOption PSpacingglue o "Spacing glue requires arguments. " ["stretch", "shrink"] (namedGlueSchema sSpGlue)
    vc PTextglue o = validateConfigOption PTextglue o "Text glue requires arguments. " ["stretch", "shrink"] (namedGlueSchema sTxtGlue)
    
    vc PFont o = validateConfigOption PFont o "Font type requires arguments. " ["font"] 
        (withVal sFont $ requireTextWith "font" validateFont ("Unknown font. " ++ configErrorString PFont))
    
    vc PParsize o = validateConfigOption PParsize o "Par font size requires arguments. " ["size"] (namedFontsizeSchema sParSz)
    vc PTitleSize o = validateConfigOption PTitleSize o "Title font size requires arguments. " ["size"] (namedFontsizeSchema sTitleSz)
    vc PSectionSize o = validateConfigOption PSectionSize o "Section font size requires arguments. " ["size"] (namedFontsizeSchema sTitleSz) -- Map PSectionSize to TitleSize logic as per original
    vc PSubsectionSize o = validateConfigOption PSubsectionSize o "Subtitle font size requires arguments. " ["size"] (namedFontsizeSchema sSubTitleSz)
    
    vc PJustification o = validateConfigOption PJustification o "Justification requires arguments. " ["justification"] 
        (withVal sJust $ requireTextWith "justification" validateJustification ("Unknown just. " ++ configErrorString PJustification))
    
    vc PListstyle o = validateConfigOption PListstyle o "List style requires arguments. " ["style"]
        (withVal sListSt $ requireTextWith "style" validateListStyle ("Unknown style. " ++ configErrorString PListstyle))