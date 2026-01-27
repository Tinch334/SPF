{-# LANGUAGE OverloadedStrings #-}

module Typesetting.Helpers
    ( mergeVText
    , pageSizeToRect
    , convertFontSize
    , convertAdjustFontSize
    , fromPt
    , toRoman
    , generateDocInfo )
    where

import Datatypes.ValidatedTokens

import Data.Text (Text)
import qualified Data.Text as T

import GHC.Float (double2Int)
import Graphics.PDF


------------------------
-- CONVERSION FUNCTIONS
------------------------
-- Takes the text from a list of VText.
mergeVText :: [VText] -> Text
mergeVText = foldl (\s vt -> s <> textCnt vt) ""

-- Converts a page size token to it's size in points.
pageSizeToRect :: PageSize -> PDFRect
pageSizeToRect SizeA4 = PDFRect 0 0 595 842
pageSizeToRect SizeA3 = PDFRect 0 0 841 1190
pageSizeToRect SizeLegal = PDFRect 0 0 612 1009
pageSizeToRect (SizeCustom (Pt w) (Pt h)) = PDFRect 0 0 w h

-- HPDF only accepts integer font sizes.
convertFontSize :: Datatypes.ValidatedTokens.FontSize -> Int
convertFontSize (FontSize pt) = double2Int pt

-- Scale the given font size.
convertAdjustFontSize :: Datatypes.ValidatedTokens.FontSize -> Double -> Int
convertAdjustFontSize (FontSize pt) a = double2Int (pt * a)

fromPt :: Pt -> Double
fromPt (Pt s) = s

-- Converts and integer to a Roman numeral.
toRoman :: Int -> String
toRoman 0 = ""
toRoman x
    | x >= 1000 = "M"  ++ toRoman (x - 1000)
    | x >= 900  = "CM" ++ toRoman (x - 900)
    | x >= 500  = "D"  ++ toRoman (x - 500)
    | x >= 400  = "CD" ++ toRoman (x - 400)
    | x >= 100  = "C"  ++ toRoman (x - 100)
    | x >= 90   = "XC" ++ toRoman (x - 90)
    | x >= 50   = "L"  ++ toRoman (x - 50)
    | x >= 40   = "XL" ++ toRoman (x - 40)
    | x >= 10   = "X"  ++ toRoman (x - 10)
    | x >= 9    = "IX" ++ toRoman (x - 9)
    | x >= 5    = "V"  ++ toRoman (x - 5)
    | x >= 4    = "IV" ++ toRoman (x - 4)
    | x >= 1    = "I"  ++ toRoman (x - 1)
    | otherwise = ""

------------------------
-- ELEMENT MAKER FUNCTIONS
------------------------
-- Generates the document information from the metadata.
generateDocInfo :: ValidatedMetadata -> PDFDocumentInfo
generateDocInfo meta = let
    baseInfo = standardDocInfo {compressed = False}
    withAuthor = case vmAuthor meta of
        Just a -> baseInfo {author = mergeVText a}
        Nothing -> baseInfo
    withSubject = case vmTitle meta of
        Just t -> withAuthor {subject = mergeVText t}
        Nothing -> withAuthor in withSubject