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


quote :: Text -> String
quote t = "\"" ++ (T.unpack t) ++ "\""

quoteList :: [Text] -> String
quoteList l = L.intercalate ", " (map quote l)


configErrorString :: PConfigOption -> String
configErrorString PSize =
  "Expected " ++ quoteList ["a4", "a3", "legal"] ++ " or two numeric values (width height or width: pt, height: pt)."

validateNamedSize :: Text -> Maybe VConfigOpt
validateNamedSize t = case T.toLower t of
    "a4"    -> Just $ VPageSize SizeA4
    "a3"    -> Just $ VPageSize SizeA3
    "legal" -> Just $ VPageSize SizeLegal
    other   -> Nothing

validateConfig :: PConfigOption -> POption -> Validation String VConfigOpt
validateConfig PSize (POptionValue l) =
    case l of
        [PText t] -> validate ("Unknown page size. " ++ (configErrorString PSize)) validateNamedSize t
        [PNumber w, PNumber h] -> Success $ VPageSize (SizeCustom w h)
        _ -> Failure $ "Invalid form for page size. " ++ configErrorString PSize
        
validateConfig PSize (POptionMap m) = 
    case m of
        [("size", PText t)] -> validate ("Unknown page size. " ++ (configErrorString PSize)) validateNamedSize t
        [("width", PNumber w), ("height", PNumber h)] -> Success $ VPageSize (SizeCustom w h)
        [("height", PNumber h), ("width", PNumber w)] -> Success $ VPageSize (SizeCustom w h)
        _ -> Failure $ "Invalid form for page size. " ++ configErrorString PSize

validateConfig PSize POptionNone = Failure $ "Invalid form for page size. " ++ configErrorString PSize


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