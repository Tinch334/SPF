{-# LANGUAGE OverloadedStrings #-}

module Typesetting.Helpers where

import Datatypes.ValidatedTokens
import Resources (LoadedFonts(..), getFont)

import Data.Text (Text)
import GHC.Float (double2Int)

import Graphics.PDF


------------------------
-- CONVERSION FUNCTIONS
------------------------
-- Takes the text from a list of VText.
mergeVText :: [VText] -> Text
mergeVText = foldl (\s vt -> s <> textCnt vt) ""

pageSizeToRect :: PageSize -> PDFRect
pageSizeToRect SizeA4 = PDFRect 0 0 595 842
pageSizeToRect SizeA3 = PDFRect 0 0 841 1190
pageSizeToRect SizeLegal = PDFRect 0 0 612 1009
pageSizeToRect (SizeCustom (Pt w) (Pt h)) = PDFRect 0 0 w h

convertFontSize :: Datatypes.ValidatedTokens.FontSize -> Int
convertFontSize (FontSize (Pt pt)) = double2Int pt

fromPt :: Pt -> Double
fromPt (Pt s) = s
------------------------
-- ELEMENT MAKER FUNCTIONS
------------------------
makeTestBlock :: TM StandardParagraphStyle StandardStyle ()
makeTestBlock = do
    paragraph $ do
        --setStyle $ Font (PDFFont (hbi lf) 10) black black
        txt $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor "
        txt $ "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud "
        txt $ "exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
        txt $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor "
        txt $ "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud "
        txt $ "exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
        txt $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor "
        txt $ "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud "
        txt $ "exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
        txt $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor "
        txt $ "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud "
        txt $ "exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
        txt $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor "
        txt $ "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud "
        txt $ "exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."


-- Generates the document information from the metadata.
generateDocInfo :: ValidatedMetadata -> PDFDocumentInfo
generateDocInfo meta = let
    baseInfo = standardDocInfo {compressed = False}
    withAuthor = case vmAuthor meta of
        Just (VAuthor a _ _) -> baseInfo {author = mergeVText a}
        Nothing -> baseInfo
    withSubject = case vmTitle meta of
        Just (VTitle t _ _) -> withAuthor {subject = mergeVText t}
        Nothing -> withAuthor in withSubject