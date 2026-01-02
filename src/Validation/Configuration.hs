{-# LANGUAGE OverloadedStrings #-}

module Validation.Configuration where

import Datatypes.ParseTokens
import Datatypes.ValidatedTokens

import Data.Validation

import qualified Data.List as L
import qualified Data.Text as T

-- The error type in "Validation" must be a semigroup; In this case it's a list.
type ValidationType = Validation [String] VComm


quote :: T.Text -> String
quote t = "\"" ++ (T.unpack t) ++ "\""

quoteList :: [T.Text] -> String
quoteList l = L.intercalate ", " (map quote l)

formatString :: String -> String
formatString PSize =
  "Expected " ++ quoteList ["a4", "a3", "legal"] ++ " or two numeric values (width height or width: pt, height: pt)."


validateConfig :: PConfigOption -> POption -> Validation String VConfigOpt
validateConfig PSize (POptionValue l) =
    let formatString = "Expected " ++ quoteList ["a4", "a3", "legal"] ++ " or two numeric values" in 
    case l of
        [PText t] -> case T.toLower t of
            "a4"    -> Success $ VPageSize SizeA4
            "a3"    -> Success $ VPageSize SizeA3
            "legal" -> Success $ VPageSize SizeLegal
            other   -> Failure $ "Unknown page size" ++ quote other ++ ". " ++ formatString

        [PInt w, PInt h] -> Success $ VPageSize (SizeCustom (fromIntegral w) (fromIntegral h))
        [PFloat w, PFloat h] -> Success $ VPageSize (SizeCustom w h)
        [PFloat w, PInt h] -> Success $ VPageSize (SizeCustom w (fromIntegral h))
        [PInt w, PFloat h] -> Success $ VPageSize (SizeCustom (fromIntegral w) h)
        _ -> Failure $ "Invalid form for page size. " ++ formatString
validateConfig PSize (POptionMap m) = 
    let formatString = "Expected size: {" ++ quoteList ["a4", "a3", "legal"] ++ "} or " ++ quote "width: pt, height: pt" in 
    case m of
        [("size", PText t)] -> case T.toLower t of
            "a4"    -> Success $ VPageSize SizeA4
            "a3"    -> Success $ VPageSize SizeA3
            "legal" -> Success $ VPageSize SizeLegal
            other   -> Failure $ "Unknown page size" ++ quote other ++ ". " ++ formatString
        [("width", PInt w), ("height", PInt h)] -> Success $ VPageSize (SizeCustom (fromIntegral w) (fromIntegral h))
        [("width", PFloat w), ("height", PFloat h)] -> Success $ VPageSize (SizeCustom w h)
        [("width", PFloat w), ("height", PInt h)] -> Success $ VPageSize (SizeCustom w (fromIntegral h))
        [("width", PInt w), ("height", PFloat h)] -> Success $ VPageSize (SizeCustom (fromIntegral w) h)
        [("height", PInt h), ("width", PInt w)] -> Success $ VPageSize (SizeCustom (fromIntegral w) (fromIntegral h))
        [("height", PFloat h), ("width", PFloat w)] -> Success $ VPageSize (SizeCustom w h)
        [("height", PFloat h), ("width", PInt w)] -> Success $ VPageSize (SizeCustom (fromIntegral w) h)
        [("height", PInt h), ("width", PFloat w)] -> Success $ VPageSize (SizeCustom w (fromIntegral h))
        _ -> Failure $ "Invalid form for page size. " ++ formatString
validateConfig PSize POptionNone = Failure $ "Invalid form for page size. Expected size: {"
    ++ quoteList ["a4", "a3", "legal"] ++ "} or " ++ quote "width: pt, height: pt"


validateConfig PPagenumbering opt = undefined
validateConfig PTitlespacing opt = undefined
validateConfig PParagraphspacing opt = undefined
validateConfig PListspacing opt = undefined
validateConfig PTablespacing opt = undefined
validateConfig PFigurespacing opt = undefined
validateConfig PSpacingglue opt = undefined
validateConfig PTextglue opt = undefined
validateConfig PFont opt = undefined
validateConfig PParsize opt = undefined
validateConfig PTitlesize opt = undefined
validateConfig PJustification opt = undefined