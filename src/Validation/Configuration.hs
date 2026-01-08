{-# LANGUAGE OverloadedStrings #-}

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

noArgumentFail :: String -> PConfigOption -> Validation [String] VConfigOpt
noArgumentFail err opt = Failure [err ++ configErrorString opt]


--------------------
-- GENERAL VALIDATION FUNCTIONS
--------------------
-- Page size validation.
validateNamedSize :: Text -> Maybe VPageSizeOpt
validateNamedSize t = case T.toLower t of
    "a4"    -> Just SizeA4
    "a3"    -> Just SizeA3
    "legal" -> Just SizeLegal
    other   -> Nothing
-- Page numbering validation.
validateNamedNumbering :: Text -> Maybe VPageNumberingOpt
validateNamedNumbering t = case T.toLower t of
    "arabic" -> Just NumberingArabic
    "roman" -> Just NumberingRoman
    "none" -> Just NumberingNone
    other -> Nothing
-- Font validation.
validateNamedFont :: Text -> Maybe VFontOpt
validateNamedFont t = case T.toLower t of
    "helvetica" -> Just FontHelvetica
    "courier" -> Just FontCourier
    "times" -> Just FontTimes
    other -> Nothing
-- Text justification validation.
validateJustification :: Text -> Maybe VJustificationOpt
validateJustification t = case T.toLower t of
    "left" -> Just JustifyLeft
    "right" -> Just JustifyRight
    "centred" -> Just JustifyCentred
    "full" -> Just JustifyFull
    other -> Nothing

-- Takes a number, returns it if it's positive, otherwise Nothing.
validatePositive :: Double -> Maybe Double
validatePositive n = if n > 0 then Just n else Nothing

--------------------
-- MAP SCHEMA VALIDATION FUNCTIONS
--------------------
-- Takes a key, if it corresponds to a numeric value has it then a "Success" is returned, otherwise a "Failure".
requireNumberMap :: Text -> Schema POptionPair Double
requireNumberMap k = Schema $ \o ->
    case lookup k o of
        Just (PNumber n) -> Success n
        Just _ -> Failure ["Map option key " ++ quote k ++ " has wrong type"]
        Nothing -> Failure ["Missing key " ++ quote k]

-- Takes a key, if it corresponds to a text value has it and the given function returns "Just" then a "Success" is returned, otherwise a
-- "Failure". Could be implemented using requireNumberMap, however that would require more lines, a small amount of repetition is acceptable.
requireNumberMapWith :: Text -> (Double -> Maybe b) -> String -> Schema POptionPair b
requireNumberMapWith k vf err = Schema $ \o ->
    case lookup k o of
        Just (PNumber n) -> validate [err] vf n
        Just _ -> Failure ["Map option key " ++ quote k ++ " has wrong type"]
        Nothing -> Failure ["Missing key " ++ quote k]

-- Takes a key, if it corresponds to a text value has it then a "Success" is returned, otherwise a "Failure".
requireTextMap :: Text -> Schema POptionPair Text
requireTextMap k = Schema $ \o ->
    case lookup k o of
        Just (PText t) -> Success t
        Just _ -> Failure ["Map option key " ++ quote k ++ " has wrong type"]
        Nothing -> Failure ["Missing key " ++ quote k]

-- Takes a key, if it corresponds to a text value has it and the given function returns "Just" then a "Success" is returned, otherwise a
-- "Failure".
requireTextMapWith :: Text -> (Text -> Maybe b) -> String -> Schema POptionPair b
requireTextMapWith k vf err = Schema $ \o ->
    case lookup k o of
        Just (PText t) -> validate [err] vf t
        Just _ -> Failure ["Map option key " ++ quote k ++ " has wrong type"]
        Nothing -> Failure ["Missing key " ++ quote k]


-- Ensures only valid keys are present, fails if an element not in the given key list is in the options.
ensureValidKeys :: String -> [Text] -> Schema POptionPair b -> Schema POptionPair b
ensureValidKeys err keys s = Schema $ \o ->
    let fLst = filter (\e -> notElem e (map fst o)) keys in
    if null fLst
        then (runSchema s o) -- If the key check succeeded the inner validation schema is run.
        else Failure ["Invalid key" ++ quote (head fLst) ++ ". " ++ err]

namedSizeSchema :: Schema POptionPair VConfigOpt
namedSizeSchema = VPageSize <$>
    requireTextMapWith "size" validateNamedSize ("Unknown page size. " ++ (configErrorString PSize))

customSizeSchema :: Schema POptionPair VConfigOpt
customSizeSchema = VPageSize <$> (SizeCustom <$> 
    requireNumberMapWith "width" validatePositive ("Page width must be positive. " ++ (configErrorString PSize))
    <*> requireNumberMapWith "height" validatePositive ("Page height must be positive. " ++ (configErrorString PSize)))

namedPagenumberingSchema :: Schema POptionPair VConfigOpt
namedPagenumberingSchema = VPageNumbering <$>
    requireTextMapWith "numbering" validateNamedNumbering ("Unknown page numbering type. " ++ (configErrorString PPagenumbering))

-- Before and after spacing validation. Takes a constructor, so the function can be used with any constructor of Double -> Double -> a.
namedBeforeAndAfterSchema :: (Double -> Double -> b) -> Schema POptionPair b
namedBeforeAndAfterSchema = (\c -> c <$> requireNumberMap "before" <*> requireNumberMap "after")

-- Glue validation, same idea as the previous validator.
namedGlueSchema :: (Double -> Double -> b) -> Schema POptionPair b
namedGlueSchema = (\c -> c <$> 
    requireNumberMapWith "stretch" validatePositive ("Stretch must be positive")
    <*> requireNumberMapWith "shrink" validatePositive ("Shrink height must be positive"))

namedFontSchema :: Schema POptionPair VConfigOpt
namedFontSchema = VFont <$> requireTextMapWith "font" validateNamedFont ("Unknown font type. " ++ configErrorString PFont)

-- Text size validation.
namedFontSizeSchema :: (Double -> b) -> Schema POptionPair b
namedFontSizeSchema = (\c -> c <$> requireNumberMapWith "size" validatePositive ("Font size must be positive"))

namedJustifySchema :: Schema POptionPair VConfigOpt
namedJustifySchema = VJustification <$>
    requireTextMapWith "justification" validateJustification ("Unknown text justification. " ++ configErrorString PJustification)


--------------------
-- CONFIGURATION VALIDATION
--------------------
validateConfig :: PConfigOption -> POption -> Validation [String] VConfigOpt
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

--validateConfig PTitlespacing (POptionMap m) = runSchema (namedBeforeAndAfterSchema TitleSpacing) m
validateConfig PTitlespacing (POptionMap m) = VTitleSpacing <$> runSchema
    (ensureValidKeys (configErrorString PTitlespacing) ["before", "after"] (namedBeforeAndAfterSchema TitleSpacing))
    m
validateConfig PTitlespacing POptionNone = noArgumentFail "Title spacing requires arguments. " PTitlespacing

validateConfig PParagraphspacing (POptionMap m) = VParagraphSpacing <$> runSchema
    (ensureValidKeys (configErrorString PParagraphspacing) ["before", "after"] (namedBeforeAndAfterSchema ParagraphSpacing))
    m
validateConfig PParagraphspacing POptionNone = noArgumentFail "Paragraph spacing requires arguments. " PParagraphspacing

validateConfig PListspacing (POptionMap m) = VListSpacing <$> runSchema
    (ensureValidKeys (configErrorString PListspacing) ["before", "after"] (namedBeforeAndAfterSchema ListSpacing))
    m
validateConfig PListspacing POptionNone = noArgumentFail "List spacing requires arguments. " PListspacing

validateConfig PTablespacing (POptionMap m) = VTableSpacing <$> runSchema
    (ensureValidKeys (configErrorString PTablespacing) ["before", "after"] (namedBeforeAndAfterSchema TableSpacing))
    m
validateConfig PTablespacing POptionNone = noArgumentFail "Table spacing requires arguments. " PTablespacing

validateConfig PFigurespacing (POptionMap m) = VFigureSpacing <$> runSchema
    (ensureValidKeys (configErrorString PFigurespacing) ["before", "after"] (namedBeforeAndAfterSchema FigureSpacing))
    m
validateConfig PFigurespacing POptionNone = noArgumentFail "Figure spacing requires arguments. " PFigurespacing

validateConfig PSpacingglue (POptionMap m) = VSpacingGlue <$> runSchema
    (ensureValidKeys (configErrorString PSpacingglue) ["stretchability", "shrinkability"] (namedGlueSchema SpacingGlue))
    m
validateConfig PSpacingglue POptionNone = noArgumentFail "Spacing glue requires arguments. " PSpacingglue

validateConfig PTextglue (POptionMap m) = VTextGlue <$> runSchema
    (ensureValidKeys (configErrorString PTextglue) ["stretchability", "shrinkability"] (namedGlueSchema TextGlue))
    m
validateConfig PTextglue POptionNone = noArgumentFail "Paragraph glue requires arguments. " PTextglue

validateConfig PFont (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PFont) ["font"] namedFontSchema)
    m
validateConfig PFont POptionNone = noArgumentFail "Font type requires arguments. " PFont

validateConfig PParsize (POptionMap m) = VParSize <$> runSchema
    (ensureValidKeys (configErrorString PParsize) ["size"] (namedFontSizeSchema ParSize))
    m
validateConfig PParsize POptionNone = noArgumentFail "Paragraph font size requires arguments. " PParsize

validateConfig PTitlesize (POptionMap m) = VTitleSize <$> runSchema
    (ensureValidKeys (configErrorString PTitlesize) ["size"] (namedFontSizeSchema TitleSize))
    m
validateConfig PTitlesize POptionNone = noArgumentFail "Title font size requires arguments. " PTitlesize

validateConfig PJustification (POptionMap m) = runSchema
    (ensureValidKeys (configErrorString PJustification) ["justification"] namedJustifySchema)
    m
validateConfig PJustification POptionNone = noArgumentFail "Text justification requires arguments" PJustification