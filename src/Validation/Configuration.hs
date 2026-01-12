{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

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
configErrorString PTitlespacing =
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
configErrorString PTitlesize =
    "Expected a numeric value (size: pt)"
configErrorString PJustification = 
    "Expected field " ++ quote "justification" ++ " to be one of " ++ quoteList ["left", "right", "centred", "full"]

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
withTitleSpacing s = emptyVConfig { cfgTitleSpacing = Just s }

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

withParSize :: Pt -> VConfig
withParSize s = emptyVConfig { cfgParSize = Just s }

withTitleSize :: Pt -> VConfig
withTitleSize s = emptyVConfig { cfgTitleSize = Just s }

withJustification :: Justification -> VConfig
withJustification j = emptyVConfig { cfgJustification = Just j }

--------------------
-- SCHEMA VALIDATION FUNCTIONS
--------------------
-- Generalized schemas.
namedBeforeAndAfterSchema :: (Spacing -> b) -> Schema b
namedBeforeAndAfterSchema c = c <$> (Spacing <$> (tuple <$>
    (Pt <$> requireNumber "before") <*> (Pt <$> requireNumber "after")))

namedGlueSchema :: (Glue -> b) -> Schema b
namedGlueSchema c = c <$> (Glue <$> (tuple <$> 
    requireNumberWith "stretch" (validateNumInst (> 0) Pt) "Stretch must be positive"
    <*> requireNumberWith "shrink"  (validateNumInst (> 0) Pt) "Shrink must be positive"))

namedFontSizeSchema :: (Pt -> b) -> Schema b
namedFontSizeSchema c = c <$> requireNumberWith "size" (validateNumInst (> 0) Pt) "Font size must be positive"

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
namedFontSchema :: Schema VConfig
namedFontSchema = withFont <$> requireTextWith "font" validateFont ("Unknown font type. " ++ configErrorString PFont)

parSizeSchema :: Schema VConfig
parSizeSchema = namedFontSizeSchema withParSize

titleSizeSchema :: Schema VConfig
titleSizeSchema = namedFontSizeSchema withTitleSize

namedJustifySchema :: Schema VConfig
namedJustifySchema = withJustification <$>
  requireTextWith "justification" validateJustification ("Unknown text justification. " ++ configErrorString PJustification)

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

validateConfig PTitlespacing (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PTitlespacing) ["before", "after"] (titleSpacingSchema))
    m
validateConfig PTitlespacing POptionNone = noArgumentFail "Title spacing requires arguments. " PTitlespacing

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
    (ensureValidKeys (configErrorString PSpacingglue) ["stretchability", "shrinkability"] (spacingGlueSchema))
    m
validateConfig PSpacingglue POptionNone = noArgumentFail "Spacing glue requires arguments. " PSpacingglue

validateConfig PTextglue (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PTextglue) ["stretchability", "shrinkability"] (textGlueSchema))
    m
validateConfig PTextglue POptionNone = noArgumentFail "Paragraph glue requires arguments. " PTextglue

validateConfig PFont (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PFont) ["font"] namedFontSchema)
    m
validateConfig PFont POptionNone = noArgumentFail "Font type requires arguments. " PFont

validateConfig PParsize (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PParsize) ["size"] (parSizeSchema))
    m
validateConfig PParsize POptionNone = noArgumentFail "Paragraph font size requires arguments. " PParsize

validateConfig PTitlesize (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PTitlesize) ["size"] (titleSizeSchema))
    m
validateConfig PTitlesize POptionNone = noArgumentFail "Title font size requires arguments. " PTitlesize

validateConfig PJustification (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PJustification) ["justification"] namedJustifySchema)
    m
validateConfig PJustification POptionNone = noArgumentFail "Text justification requires arguments" PJustification