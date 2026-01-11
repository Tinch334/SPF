{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Validation.Configuration where

import Datatypes.ParseTokens
import Datatypes.ValidatedTokens
import Validation.Schema
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
-- GENERAL VALIDATION FUNCTIONS
--------------------
-- Page size validation.
validateNamedSize :: Text -> Maybe PageSize
validateNamedSize t = case T.toLower t of
    "a4"    -> Just SizeA4
    "a3"    -> Just SizeA3
    "legal" -> Just SizeLegal
    _   -> Nothing
-- Page numbering validation.
validateNamedNumbering :: Text -> Maybe PageNumbering
validateNamedNumbering t = case T.toLower t of
    "arabic" -> Just NumberingArabic
    "roman" -> Just NumberingRoman
    "none" -> Just NumberingNone
    _ -> Nothing
-- Font validation.
validateNamedFont :: Text -> Maybe Font
validateNamedFont t = case T.toLower t of
    "helvetica" -> Just Helvetica
    "courier" -> Just Courier
    "times" -> Just Times
    _ -> Nothing
-- Text justification validation.
validateJustification :: Text -> Maybe Justification
validateJustification t = case T.toLower t of
    "left" -> Just JustifyLeft
    "right" -> Just JustifyRight
    "center" -> Just JustifyCenter
    "full" -> Just JustifyFull
    _ -> Nothing

-- Takes a number, returns it if it's positive, otherwise Nothing.
validatePositiveInst :: (Num a, Ord a) => (a -> b) -> a -> Maybe b
validatePositiveInst i n = if n > 0 then Just (i n) else Nothing

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
namedBeforeAndAfterSchema :: (Spacing -> b) -> Schema POptionPair b
namedBeforeAndAfterSchema c = c <$> (Spacing <$> (tuple <$>
    (requireNumberInst "before" Pt) <*> (requireNumberInst "after" Pt)))

namedGlueSchema :: (Glue -> b) -> Schema POptionPair b
namedGlueSchema c = c <$> (Glue <$> (tuple <$> 
    requireNumberWith "stretch" (validatePositiveInst Pt) "Stretch must be positive"
    <*> requireNumberWith "shrink"  (validatePositiveInst Pt) "Shrink must be positive"))

namedFontSizeSchema :: (Pt -> b) -> Schema POptionPair b
namedFontSizeSchema c = c <$> requireNumberWith "size" (validatePositiveInst Pt) "Font size must be positive"

-- Option specific schemas.
namedSizeSchema :: Schema POptionPair VConfig
namedSizeSchema = withPageSize <$>
  requireTextWith "size" validateNamedSize ("Unknown page size. " ++ configErrorString PSize)

customSizeSchema :: Schema POptionPair VConfig
customSizeSchema = withPageSize <$> (SizeCustom <$> 
    requireNumberWith "width" (validatePositiveInst Pt) ("Page width must be positive. " ++ configErrorString PSize)
    <*> requireNumberWith "height" (validatePositiveInst Pt) ("Page height must be positive. " ++ configErrorString PSize))

namedPagenumberingSchema :: Schema POptionPair VConfig
namedPagenumberingSchema = withPageNumbering <$>
  requireTextWith "numbering" validateNamedNumbering ("Unknown page numbering type. " ++ configErrorString PPagenumbering)

-- concrete spacing schemas built from the generic helper
titleSpacingSchema :: Schema POptionPair VConfig
titleSpacingSchema = namedBeforeAndAfterSchema withTitleSpacing

paragraphSpacingSchema :: Schema POptionPair VConfig
paragraphSpacingSchema = namedBeforeAndAfterSchema withParagraphSpacing

listSpacingSchema :: Schema POptionPair VConfig
listSpacingSchema = namedBeforeAndAfterSchema withListSpacing

tableSpacingSchema :: Schema POptionPair VConfig
tableSpacingSchema = namedBeforeAndAfterSchema withTableSpacing

figureSpacingSchema :: Schema POptionPair VConfig
figureSpacingSchema = namedBeforeAndAfterSchema withFigureSpacing

-- glue
spacingGlueSchema :: Schema POptionPair VConfig
spacingGlueSchema = namedGlueSchema withSpacingGlue

textGlueSchema :: Schema POptionPair VConfig
textGlueSchema = namedGlueSchema withTextGlue

-- font and sizes
namedFontSchema :: Schema POptionPair VConfig
namedFontSchema = withFont <$> requireTextWith "font" validateNamedFont ("Unknown font type. " ++ configErrorString PFont)

parSizeSchema :: Schema POptionPair VConfig
parSizeSchema = namedFontSizeSchema withParSize

titleSizeSchema :: Schema POptionPair VConfig
titleSizeSchema = namedFontSizeSchema withTitleSize

namedJustifySchema :: Schema POptionPair VConfig
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
    (ensureValidKeys (configErrorString PTitlesize) ["size"] (titleSpacingSchema))
    m
validateConfig PTitlesize POptionNone = noArgumentFail "Title font size requires arguments. " PTitlesize

validateConfig PJustification (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PJustification) ["justification"] namedJustifySchema)
    m
validateConfig PJustification POptionNone = noArgumentFail "Text justification requires arguments" PJustification