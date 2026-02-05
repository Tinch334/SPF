{-# LANGUAGE OverloadedStrings #-}

module Typesetting.Helpers
    ( mergeVText
    , pageSizeToRect
    , adjustFontSize
    , toRoman
    , getFont
    , getVerbatimFont
    , generateDocInfo
    ) where

import Datatypes.ValidatedTokens
import Datatypes.Resources
import Typesetting.Styles

import Data.Text (Text)

import GHC.Float (double2Int, int2Double)

import Graphics.PDF
import Graphics.PDF.Fonts.Font (AnyFont)


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

-- Scale the given font size.
adjustFontSize :: Datatypes.ValidatedTokens.FontSize -> Double -> Datatypes.ValidatedTokens.FontSize
adjustFontSize (FontSize pt) a = FontSize (double2Int $ (int2Double pt) * a)

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

-- Takes a font, style and returns the appropriate font.
--getFont :: LoadedFonts -> Font -> Datatypes.ValidatedTokens.TextType -> Datatypes.ValidatedTokens.FontSize -> StandardStyle
getFont fonts family style (Datatypes.ValidatedTokens.FontSize size) =
    let
        f = case family of
            Datatypes.ValidatedTokens.Helvetica -> helvetica
            Datatypes.ValidatedTokens.Courier -> courier
            Datatypes.ValidatedTokens.Times -> times
            _ -> error "INTERNAL: Given font has no family"
        s = case style of
            Bold -> bold
            Italic -> italic
            Emphasised -> boldItalic
            _ -> normal

    in (Font (PDFFont (s $ f fonts) size) black black)

--getVerbatimFont :: LoadedFonts -> AnyFont
getVerbatimFont fonts (Datatypes.ValidatedTokens.FontSize size) = (Font (PDFFont (normal $ courier fonts) size) black black)
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