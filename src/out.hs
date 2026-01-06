{-# LANGUAGE OverloadedStrings #-}

module Validation where

import Control.Applicative
import Control.Monad
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validation
import Datatypes.ParseTokens
import Datatypes.ValidatedTokens

--------------------
-- AUXILIARY FUNCTIONS
--------------------
quote :: Text -> String
quote t = "\"" ++ (T.unpack t) ++ "\""

quoteList :: [Text] -> String
quoteList l = L.intercalate ", " (map quote l)

configErrorString :: PConfigOption -> String
configErrorString PSize =
  "Expected one of: " ++ quoteList ["a4", "a3", "legal"] ++ " or two numeric values (width: pt, height: pt)."
configErrorString PPagenumbering =
  "Expected one of: " ++ quoteList ["arabic", "roman", "none"] ++ "."
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
  "Expected one of: " ++ quoteList ["helvetica", "courier", "times"] ++ "."
configErrorString PParsize =
  "Expected a numeric value (size: pt)"
configErrorString PTitlesize =
  "Expected a numeric value (size: pt)"
configErrorString PJustification =
  "Expected one of: " ++ quoteList ["left", "right", "centred", "full"]

--------------------
-- SCHEMA DEFINITION AND FUNCTIONS
--------------------
data Schema a = Schema
  { runSchema :: [POptionPair] -> Validation [String] a
  }

-- The important definition here is the Applicative one, it allows for the concatenation of Schemas.
instance Functor Schema where
  fmap f (Schema g) = Schema (\o -> fmap f (g o)) -- Note that "g o" is a validation, therefore we cannot apply f directly.

instance Applicative Schema where
  pure x = Schema (\_ -> Success x)
  (Schema f) <*> (Schema g) = Schema (\o -> f o <*> g o) -- "f o" and "g o" are a validations, therefore they are applicative.

-- Returns a valid schema with the corresponding values, or an error if no valid matches are found.
choiceSchema :: [Schema a] -> Schema a
choiceSchema [] = Schema (\_ -> Failure ["Options did not match any valid form"])
choiceSchema ((Schema s) : xs) =
  Schema $ \o ->
    case s o of
      Success v -> Success v
      -- Errors are accumulated, to give the user more information.
      Failure e -> case runSchema (choiceSchema xs) o of
        Success v' -> Success v'
        Failure e' -> Failure (e <> e') -- Errors have to implement <>, they are a semigroup.

-- Takes a key, if it corresponds to a numeric value has it then a "Success" is returned, otherwise a "Failure".
requireNumber :: Text -> Schema Double
requireNumber k = Schema $ \o ->
  case lookup k o of
    Just (PNumber n) -> Success n
    Just _ -> Failure ["Map option key " ++ quote k ++ " has wrong type"]
    Nothing -> Failure ["Missing key " ++ quote k]

-- Takes a key, if it corresponds to a text value has it and the given function returns "Just" then a "Success" is returned, otherwise a
-- "Failure". Could be implemented using requireNumber, however that would require more lines, a small amount of repetition is acceptable.
requireNumberWith :: Text -> (Double -> Maybe a) -> String -> Schema a
requireNumberWith k vf err = Schema $ \o ->
  case lookup k o of
    Just (PNumber n) -> validate [err] vf n
    Just _ -> Failure ["Map option key " ++ quote k ++ " has wrong type"]
    Nothing -> Failure ["Missing key " ++ quote k]

-- Takes a key, if it corresponds to a text value has it then a "Success" is returned, otherwise a "Failure".
requireText :: Text -> Schema Text
requireText k = Schema $ \o ->
  case lookup k o of
    Just (PText t) -> Success t
    Just _ -> Failure ["Map option key " ++ quote k ++ " has wrong type"]
    Nothing -> Failure ["Missing key " ++ quote k]

-- Takes a key, if it corresponds to a text value has it and the given function returns "Just" then a "Success" is returned, otherwise a
-- "Failure".
requireTextWith :: Text -> (Text -> Maybe a) -> String -> Schema a
requireTextWith k vf err = Schema $ \o ->
  case lookup k o of
    Just (PText t) -> validate [err] vf t
    Just _ -> Failure ["Map option key " ++ quote k ++ " has wrong type"]
    Nothing -> Failure ["Missing key " ++ quote k]

ensureValidKeys :: String -> [Text] -> Schema (a -> a)
ensureValidKeys err keys = Schema $ \o ->
  let optKeys = map fst o
   in if filter (\e -> notElem e optKeys) keys /= []
        then Failure [err]
        else Success id

--------------------
-- VALIDATION FUNCTIONS
--------------------
-- Page size validation.
validateNamedSize :: Text -> Maybe VPageSizeOpt
validateNamedSize t = case T.toLower t of
  "a4" -> Just SizeA4
  "a3" -> Just SizeA3
  "legal" -> Just SizeLegal
  other -> Nothing

namedSizeSchema :: Schema VConfigOpt
namedSizeSchema =
  VPageSize
    <$> requireTextWith "size" validateNamedSize ("Unknown page size. " ++ (configErrorString PSize))

customSizeSchema :: Schema VConfigOpt
customSizeSchema =
  VPageSize
    <$> ( SizeCustom
            <$> requireNumberWith "width" positiveNumber ("Page width must be positive. " ++ (configErrorString PSize))
            <*> requireNumberWith "height" positiveNumber ("Page height must be positive. " ++ (configErrorString PSize))
        )

-- Page numbering validation.
validateNamedNumbering :: Text -> Maybe VPageNumberingOpt
validateNamedNumbering t = case T.toLower t of
  "arabic" -> Just NumberingArabic
  "roman" -> Just NumberingRoman
  "none" -> Just NumberingNone
  other -> Nothing

namedPagenumberingSchema :: Schema VConfigOpt
namedPagenumberingSchema =
  VPageNumbering
    <$> requireTextWith "numbering" validateNamedNumbering ("Unknown page numbering type. " ++ (configErrorString PPagenumbering))

-- Before and after spacing validation. Takes a constructor, so the function can be used with any constructor of Double -> Double -> a.
namedBeforeAndAfterSchema :: (Double -> Double -> a) -> Schema a
namedBeforeAndAfterSchema = (\c -> c <$> requireNumber "before" <*> requireNumber "after")

-- Glue validation, same idea as the previous validator.
namedGlueSchema :: (Double -> Double -> a) -> Schema a
namedGlueSchema =
  ( \c ->
      c
        <$> requireNumberWith "stretch" positiveNumber ("Stretch must be positive")
        <*> requireNumberWith "shrink" positiveNumber ("Shrink height must be positive")
  )

-- Font validation.
validateNamedFont :: Text -> Maybe VFontOpt
validateNamedFont t = case T.toLower t of
  "helvetica" -> Just FontHelvetica
  "courier" -> Just FontCourier
  "times" -> Just FontTimes
  other -> Nothing

namedFontSchema :: Schema VConfigOpt
namedFontSchema = VFont <$> requireTextWith "font" validateNamedFont ("Unknown font type. " ++ configErrorString PFont)

-- Text size validation.
namedFontSizeSchema :: (Double -> a) -> Schema a
namedFontSizeSchema = (\c -> c <$> requireNumberWith "size" positiveNumber ("Font size must be positive"))

-- Text justification validation.
validateJustification :: Text -> Maybe VJustificationOpt
validateJustification t = case T.toLower t of
  "left" -> Just JustifyLeft
  "right" -> Just JustifyRight
  "centred" -> Just JustifyCentred
  "full" -> Just JustifyFull
  other -> Nothing

namedJustifySchema :: Schema VConfigOpt
namedJustifySchema =
  VJustification
    <$> requireTextWith "justification" validateJustification ("Unknown text justification. " ++ configErrorString PJustification)

-- Generic failure function
noArgumentFail :: String -> PConfigOption -> Validation [String] VConfigOpt
noArgumentFail err opt = Failure [err ++ configErrorString opt]

-- Takes a number, returns it if it's positive.
positiveNumber :: Double -> Maybe Double
positiveNumber n = if n > 0 then Just n else Nothing

-- Configuration options validation.
validateConfig :: PConfigOption -> POption -> Validation [String] VConfigOpt
validateConfig PSize (POptionMap m) =
  runSchema
    ( choiceSchema
        [ (ensureValidKeys ("Invalid option for page size. " ++ configErrorString PSize) ["size"]) <*> namedSizeSchema,
          customSizeSchema
        ]
    )
    m
validateConfig PSize (POptionValue l) = undefined
validateConfig PSize POptionNone = noArgumentFail "Invalid form for page size. " PSize
validateConfig PPagenumbering (POptionMap m) = runSchema namedPagenumberingSchema m
validateConfig PPagenumbering (POptionValue l) = undefined
validateConfig PPagenumbering POptionNone = noArgumentFail "Invalid form for page numbering. " PPagenumbering
-- validateConfig PTitlespacing (POptionMap m) = runSchema (namedBeforeAndAfterSchema TitleSpacing) m
validateConfig PTitlespacing (POptionMap m) = VTitleSpacing <$> runSchema (namedBeforeAndAfterSchema TitleSpacing) m
validateConfig PTitlespacing (POptionValue l) = undefined
validateConfig PTitlespacing POptionNone = noArgumentFail "Title spacing requires arguments. " PTitlespacing
validateConfig PParagraphspacing (POptionMap m) = VParagraphSpacing <$> runSchema (namedBeforeAndAfterSchema ParagraphSpacing) m
validateConfig PParagraphspacing (POptionValue l) = undefined
validateConfig PParagraphspacing POptionNone = noArgumentFail "Paragraph spacing requires arguments. " PParagraphspacing
validateConfig PListspacing (POptionMap m) = VListSpacing <$> runSchema (namedBeforeAndAfterSchema ListSpacing) m
validateConfig PListspacing (POptionValue l) = undefined
validateConfig PListspacing POptionNone = noArgumentFail "List spacing requires arguments. " PListspacing
validateConfig PTablespacing (POptionMap m) = VTableSpacing <$> runSchema (namedBeforeAndAfterSchema TableSpacing) m
validateConfig PTablespacing (POptionValue l) = undefined
validateConfig PTablespacing POptionNone = noArgumentFail "Table spacing requires arguments. " PTablespacing
validateConfig PFigurespacing (POptionMap m) = VFigureSpacing <$> runSchema (namedBeforeAndAfterSchema FigureSpacing) m
validateConfig PFigurespacing (POptionValue l) = undefined
validateConfig PFigurespacing POptionNone = noArgumentFail "Figure spacing requires arguments. " PFigurespacing
validateConfig PSpacingglue (POptionMap m) = VSpacingGlue <$> runSchema (namedGlueSchema SpacingGlue) m
validateConfig PSpacingglue (POptionValue l) = undefined
validateConfig PSpacingglue POptionNone = noArgumentFail "Spacing glue requires arguments. " PSpacingglue
validateConfig PTextglue (POptionMap m) = VTextGlue <$> runSchema (namedGlueSchema TextGlue) m
validateConfig PTextglue (POptionValue l) = undefined
validateConfig PTextglue POptionNone = noArgumentFail "Paragraph glue requires arguments. " PTextglue
validateConfig PFont (POptionMap m) = runSchema namedFontSchema m
validateConfig PFont (POptionValue l) = undefined
validateConfig PFont POptionNone = noArgumentFail "Font type requires arguments. " PFont
validateConfig PParsize (POptionMap m) = VParSize <$> runSchema (namedFontSizeSchema ParSize) m
validateConfig PParsize (POptionValue l) = undefined
validateConfig PParsize POptionNone = noArgumentFail "Paragraph font size requires arguments. " PParsize
validateConfig PTitlesize (POptionMap m) = VTitleSize <$> runSchema (namedFontSizeSchema TitleSize) m
validateConfig PTitlesize (POptionValue l) = undefined
validateConfig PTitlesize POptionNone = noArgumentFail "Title font size requires arguments. " PTitlesize
validateConfig PJustification (POptionMap m) = runSchema namedJustifySchema m
validateConfig PJustification (POptionValue l) = undefined
validateConfig PJustification POptionNone = noArgumentFail "Text justification requires arguments" PJustification
