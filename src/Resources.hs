{-# LANGUAGE OverloadedStrings #-}

module Resources 
    ( loadResources
    , loadFonts
    )
    where

import Datatypes.ValidatedTokens
import Datatypes.Located
import Datatypes.Resources
import Common

import Control.Concurrent.Async (mapConcurrently)

import Data.Validation
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.List (nub)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Storable as V
import qualified Data.Text as T

import GHC.Float (double2Int)

import System.FilePath
import System.Directory (doesFileExist)
-- An auto-generated module that returns the path names for data files
import Paths_SPF (getDataFileName)

import Codec.Picture
import Graphics.PDF.Fonts.Font (AnyFont)
import qualified Graphics.PDF.Fonts.StandardFont as SF
import Graphics.Svg (loadSvgFile)
import Graphics.Text.TrueType (buildCache)
import Graphics.Rasterific.Svg (loadCreateFontCache, renderSvgDocument)
import qualified Graphics.Svg.Types as ST


--------------------
-- RESOURCE LOADING FUNCTIONS
--------------------
-- Loads all resources from the document, returns them as a map where each filepath is associated with the given element.
-- The filepath is that of the .spf that the resources are being loaded for.
loadResources :: [Located VComm] -> FilePath -> IO (Validation [LocatedError] ResourceMap)
loadResources comms sFilepath = do
    let uniqueRes = nub $ mapMaybe getResource comms -- Get all unique resources with their location.
    let absoluteRes = Prelude.map (\(Located p rp) -> Located p $ completePath sFilepath rp) uniqueRes
    -- Load resources concurrently, the original path is needed for resource loading during typesetting..
    validRes <- mapConcurrently loadResource (Prelude.zip absoluteRes uniqueRes)

    return $ M.fromList <$> sequenceA validRes

-- Tries to get the resource from the given command.
getResource :: Located VComm -> Maybe (Located FilePath)
getResource (Located pos (VFigure rp _ _)) = Just (Located pos rp)
getResource _ = Nothing

-- Tries to get the data from the given filepath, if successful returns both.
loadResource :: (Located FilePath, Located FilePath) -> IO (Validation [LocatedError] (FilePath, FileInfo))
loadResource (Located pos fullPath, Located _ originalPath) = do
    let ext = takeExtension fullPath
    exists <- doesFileExist fullPath
    
    if not exists
        then return $ Failure [at pos $ "File does not exist: " ++ quote (T.pack fullPath)]
        else handleFile ext
  where
    handleFile ext =
        case ext of
        e | Prelude.elem e [".bmp", ".png", ".jpg", ".jpeg"] -> do
            res <- readImage fullPath
            case res of
                Left _ -> accessError fullPath
                Right img -> makeBytes img
                
        ".svg" -> do
            res <- loadSvgFile fullPath
            case res of
                Nothing -> accessError fullPath
                Just doc -> do
                    -- Set rendering DPI(Dots per inch).
                    let dpi = 96
                    -- Ensures the image has high resolution when rendered, otherwise the library defaults to a low resolution.
                    let imageWidth = 3840.0
                    let size = case (ST._width doc, ST._height doc) of
                            (Just wNum, Just hNum) ->
                                let w = extractDouble wNum
                                    h = extractDouble hNum
                                in Just $ (double2Int imageWidth, double2Int $ (h / w) * imageWidth)
                            _ -> Nothing

                    -- Font cache for font rendering.
                    cache <- buildCache
                    (render, _) <- renderSvgDocument cache size dpi doc

                    -- Convert the image buffer into a "DynamicImage".
                    makeBytes $ ImageRGBA8 render

        e -> return $ Failure [at pos $ "The file extension " ++ quote (T.pack e) ++ " is invalid"]

    accessError path = 
        return $ Failure [at pos $ "The file " ++ quote (T.pack path) ++ " could not be accessed"]

    -- Extracts the raw data and converts it to a ByteString.
    makeBytes img = do
        -- Handles colour space by forcing conversion to RGB8.
        let rgbImage = convertRGB8 img
        -- Get image dimensions.
        let width = imageWidth rgbImage
        let height = imageHeight rgbImage
        let vector = imageData rgbImage
        let bytes = BL.pack (V.toList vector)

        return $ Success (originalPath, FileInfo bytes width height)
    -- Extracts the size out of an SVG "Number".
    extractDouble n = case n of
        ST.Num d     -> d
        ST.Px d      -> d
        ST.Em d      -> d
        ST.Percent d -> d
        ST.Pc d      -> d
        ST.Mm d      -> d
        ST.Cm d      -> d
        ST.Point d   -> d
        ST.Inches d  -> d


--------------------
-- FONT LOADING FUNCTIONS
--------------------
{-
    Getting fonts is not straightforward, all fonts, incluing built-in ones have to be loaded, which can fail. This means that we cannot simply
    have a function doing pattern matching for fonts returning a constructor. Instead if fonts are loaded successfully they are stored in a
    data structure which the pattern matching function then uses to get the corresponding font.
-}
-- Loads all supported fonts.
loadFonts :: IO LoadedFonts
loadFonts = do
    hInner <- loadStandardFont SF.Helvetica
    hbInner <- loadStandardFont SF.Helvetica_Bold
    hiInner <- loadStandardFont SF.Helvetica_Oblique
    hbiInner <- loadStandardFont SF.Helvetica_BoldOblique
    tInner <- loadStandardFont SF.Times_Roman
    tbInner <- loadStandardFont SF.Times_Bold
    tiInner <- loadStandardFont SF.Times_Italic
    tbiInner <- loadStandardFont SF.Times_BoldItalic 
    cInner <- loadStandardFont SF.Courier
    cbInner <- loadStandardFont SF.Courier_Bold
    ciInner <- loadStandardFont SF.Courier_Oblique
    cbiInner <- loadStandardFont SF.Courier_BoldOblique
    symbolsInner <- loadStandardFont SF.Symbol
    zapfInner <- loadStandardFont SF.ZapfDingbats

    let helveticaFamily = FontFamily hInner hbInner hiInner hbiInner
    let timesFamily = FontFamily tInner tbInner tiInner tbiInner
    let courierFamily = FontFamily cInner cbInner ciInner cbiInner

    return LoadedFonts 
        { helvetica = helveticaFamily
        , times = timesFamily
        , courier = courierFamily
        , code = cInner
        , zapf = zapfInner
        }

-- Attempts to load a standard font.
loadStandardFont :: SF.FontName -> IO AnyFont
loadStandardFont f = do
    fontOrErr <- SF.mkStdFont f
    case fontOrErr of
        Right lFont -> return lFont -- Loading standard fonts cannot fail.
        Left _ -> error $ "INTERNAL: Failed to load font " ++ quote (T.pack $ show f)