{-# LANGUAGE OverloadedStrings #-}

module Validation where

import Datatypes.ParseTokens
import Datatypes.ValidatedTokens

import Control.Applicative
import Control.Monad

import Data.Validation

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import qualified Data.List as L


--------------------
-- AUXILIARY FUNCTIONS
--------------------
quote :: Text -> String
quote t = "\"" ++ (T.unpack t) ++ "\""

quoteList :: [Text] -> String
quoteList l = L.intercalate ", " (map quote l)

configErrorString :: PConfigOption -> String
configErrorString PSize =
    "Expected " ++ quoteList ["a4", "a3", "legal"] ++ " or two numeric values (width: pt, height: pt)."
configErrorString PPagenumbering =
    "Expected " ++ quoteList ["arabic", "roman", "none"]
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
    "Expected two numeric values (before: pt, after: pt)" 
configErrorString PTextglue =
    "Expected two numeric values (before: pt, after: pt)" 
configErrorString PFont = undefined
configErrorString PParsize = undefined
configErrorString PTitlesize = undefined
configErrorString PJustification = undefined 

--------------------
-- SCHEMA DEFINITION AND FUNCTIONS
--------------------
data Schema a = Schema {
    runSchema :: [POptionPair] -> Validation [String] a
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
choiceSchema ((Schema s):xs) =
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

positiveNumber :: Double -> Maybe Double
positiveNumber n = if n > 0 then Just n else Nothing

--------------------
-- VALIDATION FUNCTIONS
--------------------
-- Page size validation.
validateNamedSize :: Text -> Maybe VPageSizeOpt
validateNamedSize t = case T.toLower t of
    "a4"    -> Just SizeA4
    "a3"    -> Just SizeA3
    "legal" -> Just SizeLegal
    other   -> Nothing

namedSizeSchema :: Schema VConfigOpt
namedSizeSchema = VPageSize <$>
    requireTextWith "size" validateNamedSize ("Unknown page size. " ++ (configErrorString PSize))

customSizeSchema :: Schema VConfigOpt
customSizeSchema = VPageSize <$> (SizeCustom <$> 
    requireNumberWith "width" positiveNumber ("Page width must be positive. " ++ (configErrorString PSize))
    <*> requireNumberWith "height" positiveNumber ("Page height must be positive. " ++ (configErrorString PSize)))

-- Page numbering validation.
validateNamedNumbering :: Text -> Maybe VPageNumberingOpt
validateNamedNumbering t = case T.toLower t of
        "arabic" -> Just NumberingArabic
        "roman" -> Just NumberingRoman
        "none" -> Just NumberingNone

namedPagenumberingSchema :: Schema VConfigOpt
namedPagenumberingSchema = VPageNumbering <$>
    requireTextWith "numbering" validateNamedNumbering ("Unknown page numbering type. " ++ (configErrorString PPagenumbering))

-- Before and after spacing validation. Takes a constructor, so the function can be used with any constructor of Double -> Double -> a.
namedBeforeAndAfterSchema :: (Double -> Double -> a) -> Schema a
namedBeforeAndAfterSchema = (\c -> c <$> requireNumber "before" <*> requireNumber "after")

customGlueSchema :: (Double -> Double -> a) -> Schema a
customGlueSchema = (\c -> c <$> 
    requireNumberWith "stretch" positiveNumber ("Stretch must be positive")
    <*> requireNumberWith "shrink" positiveNumber ("Shrink height must be positive"))

-- Configuration options validation.
validateConfig :: PConfigOption -> POption -> Validation [String] VConfigOpt
validateConfig PSize (POptionMap m) = runSchema
    (choiceSchema
        [ namedSizeSchema
        , customSizeSchema ])
    m
validateConfig PSize (POptionValue l) = undefined
validateConfig PSize POptionNone = Failure ["Invalid form for page size. " ++ configErrorString PSize]

validateConfig PPagenumbering (POptionMap m) = runSchema namedPagenumberingSchema m 
validateConfig PPagenumbering (POptionValue l) = undefined
validateConfig PPagenumbering POptionNone = Failure ["Invalid form for page numbering. " ++ configErrorString PPagenumbering]

--validateConfig PTitlespacing (POptionMap m) = runSchema (namedBeforeAndAfterSchema TitleSpacing) m
validateConfig PTitlespacing (POptionMap m) = VTitleSpacing <$> runSchema (namedBeforeAndAfterSchema TitleSpacing) m
validateConfig PTitlespacing (POptionValue l) = undefined
validateConfig PTitlespacing POptionNone = Failure ["Title spacing requires arguments. " ++ configErrorString PTitlespacing]

validateConfig PParagraphspacing (POptionMap m) = VParagraphSpacing <$> runSchema (namedBeforeAndAfterSchema ParagraphSpacing) m
validateConfig PParagraphspacing (POptionValue l) = undefined
validateConfig PParagraphspacing POptionNone = Failure ["Paragraph spacing requires arguments. " ++ configErrorString PParagraphspacing]

validateConfig PListspacing (POptionMap m) = VListSpacing <$> runSchema (namedBeforeAndAfterSchema ListSpacing) m
validateConfig PListspacing (POptionValue l) = undefined
validateConfig PListspacing POptionNone = Failure ["List spacing requires arguments. " ++ configErrorString PListspacing]

validateConfig PTablespacing (POptionMap m) = VTableSpacing <$> runSchema (namedBeforeAndAfterSchema TableSpacing) m
validateConfig PTablespacing (POptionValue l) = undefined
validateConfig PTablespacing POptionNone = Failure ["Table spacing requires arguments. " ++ configErrorString PTablespacing]

validateConfig PFigurespacing (POptionMap m) = VFigureSpacing <$> runSchema (namedBeforeAndAfterSchema FigureSpacing) m
validateConfig PFigurespacing (POptionValue l) = undefined
validateConfig PFigurespacing POptionNone = Failure ["Figure spacing requires arguments. " ++ configErrorString PFigurespacing]

validateConfig PSpacingglue (POptionMap m) = VSpacingGlue <$> runSchema (namedBeforeAndAfterSchema SpacingGlue) m
validateConfig PSpacingglue (POptionValue l) = undefined
validateConfig PSpacingglue POptionNone = undefined

validateConfig PTextglue (POptionMap m) = VTextGlue <$> runSchema (namedBeforeAndAfterSchema TextGlue) m
validateConfig PTextglue (POptionValue l) = undefined
validateConfig PTextglue POptionNone = undefined

validateConfig PFont (POptionMap m) = undefined
validateConfig PFont (POptionValue l) = undefined
validateConfig PFont POptionNone = undefined

validateConfig PParsize (POptionMap m) = undefined
validateConfig PParsize (POptionValue l) = undefined
validateConfig PParsize POptionNone = undefined

validateConfig PTitlesize (POptionMap m) = undefined
validateConfig PTitlesize (POptionValue l) = undefined
validateConfig PTitlesize POptionNone = undefined

validateConfig PJustification (POptionMap m) = undefined
validateConfig PJustification (POptionValue l) = undefined
validateConfig PJustification POptionNone = undefined

{-
data RequirementsField  = Required (POptionValue -> Bool)
                        | Optional (POptionValue -> Bool)

type ValueRequirements = [RequirementsField]
type MapRequirements = [(Text, RequirementsField)]

validateMapOptions :: MapRequirements -> POption -> Validation String ()
validateMapOptions l m = let
    keys = map fst m

    -- Check for missing required keys
    missing = filter (\(k, Required _) -> notElem k keys) l in Failure "Bad"

validateConfig :: PConfigOption -> POption -> Validation String VConfigOpt
validateConfig PSize (POptionMap m) = 
    case m of
        [("size", PText t)] -> validate ("Unknown page size. " ++ (configErrorString PSize)) validateNamedSize t
        [("width", PNumber w), ("height", PNumber h)] -> Success $ VPageSize (SizeCustom w h)
        [("height", PNumber h), ("width", PNumber w)] -> Success $ VPageSize (SizeCustom w h)
        _ -> Failure $ "Invalid form for page size. " ++ configErrorString PSize

validateConfig PSize POptionNone = Failure $ "Invalid form for page size. " ++ configErrorString PSize



-}