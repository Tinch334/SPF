{-# LANGUAGE OverloadedStrings #-}

module Validation.Configuration (validateConfig) where

import Datatypes.ParseTokens
import Datatypes.ValidatedTokens
import Validation.Schema
import Validation.GenericValidations
import Common

import Control.Applicative
import Control.Monad

import Data.Validation

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L


--------------------
-- AUXILIARY FUNCTIONS
--------------------
configErrorString :: PConfigOption -> String
configErrorString PSize =
    "Expected one of: " ++ quoteList ["a4", "a3", "legal"] ++ " or two numeric values (width: pt, height: pt)."
configErrorString PPagenumbering =
    "Expected field " ++ quote "numbering" ++ " to be one of " ++ quoteList ["arabic", "roman", "none"] ++ ""
configErrorString PSectionspacing =
    "Expected two numeric values (before: pt, after: pt)"
configErrorString PParagraphspacing =
    "Expected two numeric values (before: pt, after: pt)" 
configErrorString PListspacing =
    "Expected two numeric values (before: pt, after: pt)" 
configErrorString PTablespacing =
    "Expected two numeric values (before: pt, after: pt)" 
configErrorString PFigurespacing =
    "Expected two numeric values (before: pt, after: pt)" 
configErrorString PSpacingglue =
    "Expected two numeric values (stretchability: pt, shrinkability: pt)" 
configErrorString PTextglue =
    "Expected two numeric values (stretchability: pt, shrinkability: pt)" 
configErrorString PFont =
    "Expected field " ++ quote "font" ++ " to be one of " ++ quoteList ["helvetica", "courier", "times"] ++ "."
configErrorString PParsize =
    "Expected a numeric value (size: pt)"
configErrorString PTitleSize =
    "Expected a numeric value (size: pt)"
configErrorString PSectionSize =
    "Expected a numeric value (size: pt)"
configErrorString PSubsectionSize =
    "Expected a numeric value (size: pt)"
configErrorString PJustification = 
    "Expected field " ++ quote "justification" ++ " to be one of " ++ quoteList ["left", "right", "centred", "full"]
configErrorString PListstyle = 
    "Expected field " ++ quote "style" ++ " to be one of " ++ quoteList ["bullet", "square", "arrow", "number"]

noArgumentFail :: String -> PConfigOption -> Validation [String] VConfig
noArgumentFail err opt = Failure [err ++ configErrorString opt]

--------------------
-- SETTER FUNCTIONS
--------------------
withPageSize :: PageSize -> VConfig
withPageSize ps = emptyVConfig { cfgPageSize = Just ps }

withPageNumbering :: PageNumbering -> VConfig
withPageNumbering p = emptyVConfig { cfgPageNumbering = Just p }

withTitleSpacing :: Spacing -> VConfig
withTitleSpacing s = emptyVConfig { cfgSectionSpacing = Just s }

withParagraphSpacing :: Spacing -> VConfig
withParagraphSpacing s = emptyVConfig { cfgParagraphSpacing = Just s }

withListSpacing :: Spacing -> VConfig
withListSpacing s = emptyVConfig { cfgListSpacing = Just s }

withTableSpacing :: Spacing -> VConfig
withTableSpacing s = emptyVConfig { cfgTableSpacing = Just s }

withFigureSpacing :: Spacing -> VConfig
withFigureSpacing s = emptyVConfig { cfgFigureSpacing = Just s }

withSpacingGlue :: Glue -> VConfig
withSpacingGlue g = emptyVConfig { cfgSpacingGlue = Just g }

withTextGlue :: Glue -> VConfig
withTextGlue g = emptyVConfig { cfgTextGlue = Just g }

withFont :: Font -> VConfig
withFont f = emptyVConfig { cfgFont = Just f }

withParSize :: FontSize -> VConfig
withParSize s = emptyVConfig { cfgParSize = Just s }

withTitleSize :: FontSize -> VConfig
withTitleSize s = emptyVConfig { cfgSectionSize = Just s }

withSubtitleSize :: FontSize -> VConfig
withSubtitleSize s = emptyVConfig {cfgSubsectionSize = Just s}

withJustification :: Justification -> VConfig
withJustification j = emptyVConfig { cfgJustification = Just j }

withListStyle :: ListStyle -> VConfig
withListStyle s = emptyVConfig { cfgListStyle = Just s }

--------------------
-- SCHEMA VALIDATION FUNCTIONS
--------------------
-- Generalized schemas.
namedBeforeAndAfterSchema :: (Spacing -> b) -> Schema b
namedBeforeAndAfterSchema c = c <$> (Spacing <$>
    (Pt <$> requireNumber "before") <*> (Pt <$> requireNumber "after"))

namedGlueSchema :: (Glue -> b) -> Schema b
namedGlueSchema c = c <$> (Glue <$> 
    requireNumberWith "stretch" (validateNumInst (> 0) Pt) "Stretch must be positive"
    <*> requireNumberWith "shrink"  (validateNumInst (> 0) Pt) "Shrink must be positive")

namedFontSizeSchema :: (FontSize -> b) -> Schema b
namedFontSizeSchema c = c <$> (FontSize <$> requireNumberWith "size" (validateNumInst (> 0) Pt) "Font size must be positive")

-- Option specific schemas.
namedSizeSchema :: Schema VConfig
namedSizeSchema = withPageSize <$>
  requireTextWith "size" validateSize ("Unknown page size. " ++ configErrorString PSize)

customSizeSchema :: Schema VConfig
customSizeSchema = withPageSize <$> (SizeCustom <$> 
    requireNumberWith "width" (validateNumInst (> 0) Pt) ("Page width must be positive. " ++ configErrorString PSize)
    <*> requireNumberWith "height" (validateNumInst (> 0) Pt) ("Page height must be positive. " ++ configErrorString PSize))

namedPagenumberingSchema :: Schema VConfig
namedPagenumberingSchema = withPageNumbering <$>
  requireTextWith "numbering" validateNumbering ("Unknown page numbering type. " ++ configErrorString PPagenumbering)

-- concrete spacing schemas built from the generic helper
titleSpacingSchema :: Schema VConfig
titleSpacingSchema = namedBeforeAndAfterSchema withTitleSpacing

paragraphSpacingSchema :: Schema VConfig
paragraphSpacingSchema = namedBeforeAndAfterSchema withParagraphSpacing

listSpacingSchema :: Schema VConfig
listSpacingSchema = namedBeforeAndAfterSchema withListSpacing

tableSpacingSchema :: Schema VConfig
tableSpacingSchema = namedBeforeAndAfterSchema withTableSpacing

figureSpacingSchema :: Schema VConfig
figureSpacingSchema = namedBeforeAndAfterSchema withFigureSpacing

-- glue
spacingGlueSchema :: Schema VConfig
spacingGlueSchema = namedGlueSchema withSpacingGlue

textGlueSchema :: Schema VConfig
textGlueSchema = namedGlueSchema withTextGlue

-- font and sizes
fontSchema :: Schema VConfig
fontSchema = withFont <$> requireTextWith "font" validateFont ("Unknown font type. " ++ configErrorString PFont)

parSizeSchema :: Schema VConfig
parSizeSchema = namedFontSizeSchema withParSize

titleSizeSchema :: Schema VConfig
titleSizeSchema = namedFontSizeSchema withTitleSize

sectionSizeSchema :: Schema VConfig
sectionSizeSchema = namedFontSizeSchema withTitleSize

subsectionSizeSchema :: Schema VConfig
subsectionSizeSchema = namedFontSizeSchema withSubtitleSize

justifySchema :: Schema VConfig
justifySchema = withJustification <$>
    requireTextWith "justification" validateJustification ("Unknown text justification. " ++ configErrorString PJustification)

listStyleSchema :: Schema VConfig
listStyleSchema = withListStyle <$>
    requireTextWith "style" validateListStyle ("Unknown text justification. " ++ configErrorString PJustification)


--------------------
-- CONFIGURATION VALIDATION
--------------------
validateConfig :: PConfigOption -> POption -> Validation [String] VConfig
validateConfig PSize (POptionMap m) = runSchema
    (choiceSchema
        [ ensureValidKeys (configErrorString PSize) ["size"] namedSizeSchema
        , ensureValidKeys (configErrorString PSize)  ["width", "height"] customSizeSchema ])
    m
validateConfig PSize POptionNone = noArgumentFail "Invalid form for page size. " PSize

validateConfig PPagenumbering (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PPagenumbering) ["numbering"] namedPagenumberingSchema)
    m
validateConfig PPagenumbering POptionNone = noArgumentFail "Invalid form for page numbering. " PPagenumbering

validateConfig PSectionspacing (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PSectionspacing) ["before", "after"] (titleSpacingSchema))
    m
validateConfig PSectionspacing POptionNone = noArgumentFail "Title spacing requires arguments. " PSectionspacing

validateConfig PParagraphspacing (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PParagraphspacing) ["before", "after"] (paragraphSpacingSchema))
    m
validateConfig PParagraphspacing POptionNone = noArgumentFail "Paragraph spacing requires arguments. " PParagraphspacing

validateConfig PListspacing (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PListspacing) ["before", "after"] (listSpacingSchema))
    m
validateConfig PListspacing POptionNone = noArgumentFail "List spacing requires arguments. " PListspacing

validateConfig PTablespacing (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PTablespacing) ["before", "after"] (tableSpacingSchema))
    m
validateConfig PTablespacing POptionNone = noArgumentFail "Table spacing requires arguments. " PTablespacing

validateConfig PFigurespacing (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PFigurespacing) ["before", "after"] (figureSpacingSchema))
    m
validateConfig PFigurespacing POptionNone = noArgumentFail "Figure spacing requires arguments. " PFigurespacing

validateConfig PSpacingglue (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PSpacingglue) ["stretch", "shrink"] (spacingGlueSchema))
    m
validateConfig PSpacingglue POptionNone = noArgumentFail "Spacing glue requires arguments. " PSpacingglue

validateConfig PTextglue (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PTextglue) ["stretch", "shrink"] (textGlueSchema))
    m
validateConfig PTextglue POptionNone = noArgumentFail "Paragraph glue requires arguments. " PTextglue

validateConfig PFont (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PFont) ["font"] fontSchema)
    m
validateConfig PFont POptionNone = noArgumentFail "Font type requires arguments. " PFont

validateConfig PParsize (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PParsize) ["size"] parSizeSchema)
    m
validateConfig PParsize POptionNone = noArgumentFail "Paragraph font size requires arguments. " PParsize

validateConfig PTitleSize (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PTitleSize) ["size"] titleSizeSchema)
    m
validateConfig PTitleSize POptionNone = noArgumentFail "Title font size requires arguments. " PSectionSize

validateConfig PSectionSize (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PSectionSize) ["size"] sectionSizeSchema)
    m
validateConfig PSectionSize POptionNone = noArgumentFail "Title font size requires arguments. " PSectionSize

validateConfig PSubsectionSize (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PSubsectionSize) ["size"] subsectionSizeSchema)
    m
validateConfig PSubsectionSize POptionNone = noArgumentFail "Subtitle font size requires arguments. " PSectionSize

validateConfig PJustification (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PJustification) ["justification"] justifySchema)
    m
validateConfig PJustification POptionNone = noArgumentFail "Text justification requires arguments" PJustification

validateConfig PListstyle (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PJustification) ["style"] listStyleSchema)
    m
validateConfig PJustification POptionNone = noArgumentFail "List style requires arguments" PJustification