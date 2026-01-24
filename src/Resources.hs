{-# LANGUAGE OverloadedStrings #-}

module Resources 
    ( loadResources
    , loadFonts
    , getFont)
    where

import Datatypes.ValidatedTokens
import Datatypes.Located
import Datatypes.Resources
import Common

import Control.Applicative
import Control.Monad
import Control.Concurrent.Async (mapConcurrently)

import Data.Validation
import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe
import Data.List (nub)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Storable as V
import Data.ByteString (ByteString)
import qualified Data.Text as T

import System.FilePath
import System.Directory (doesFileExist)

import Codec.Picture
import Codec.Picture.Saving
import Graphics.PDF.Image
import Graphics.PDF.Fonts.Font (AnyFont)
import qualified Graphics.PDF.Fonts.StandardFont as SF


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
loadResource (Located pos completePath, Located _ originalPath) = do
    let ext = takeExtension completePath
    exists <- doesFileExist completePath
    
    if not exists
        then return $ Failure [at pos $ "File does not exist: " ++ quote (T.pack completePath)]
        else handleFile ext
        where
            handleFile ext =
                case ext of
                e | Prelude.elem e [".bmp", ".png", ".jpg", ".jpeg"] -> do
                    res <- readImage completePath
                    case res of
                        -- The default error does not follow the style of the rest of the program.
                        Left _ -> return $ Failure [at pos $ "The file " ++ quote (T.pack completePath) ++ " could not be accessed"]
                        Right img -> do
                            -- Handles colour space by forcing conversion to RGB8.
                            let rgbImage = convertRGB8 img
                            -- Get image dimensions.
                            let width = imageWidth rgbImage
                            let height = imageHeight rgbImage
                            -- Extracts the raw data ad converts it to a ByteString.
                            let vector = imageData rgbImage
                            let bytes = BL.pack (V.toList vector)

                            return $ Success (originalPath, FileInfo bytes width height)
                e -> return $ Failure [at pos $ "The file extension " ++ quote (T.pack e) ++ " is invalid"]


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
    hInner <- loadFont SF.Helvetica
    hbInner <- loadFont SF.Helvetica_Bold
    hiInner <- loadFont SF.Helvetica_Oblique
    hbiInner <- loadFont SF.Helvetica_BoldOblique
    tInner <- loadFont SF.Times_Roman
    tbInner <- loadFont SF.Times_Bold
    tiInner <- loadFont SF.Times_Italic
    tbiInner <- loadFont SF.Times_BoldItalic 
    cInner <- loadFont SF.Courier
    cbInner <- loadFont SF.Courier_Bold
    ciInner <- loadFont SF.Courier_Oblique
    cbiInner <- loadFont SF.Courier_BoldOblique
    symbolsInner <- loadFont SF.Symbol
    zapfInner <- loadFont SF.ZapfDingbats

    return $ LoadedFonts 
        { h   = hInner
        , hb  = hbInner
        , hi  = hiInner
        , hbi = hbiInner
        , t   = tInner
        , tb  = tbInner
        , ti  = tiInner
        , tbi = tbiInner
        , c   = cInner
        , cb  = cbInner
        , ci  = ciInner
        , cbi = cbiInner
        , symbols = symbolsInner
        , zapf = zapfInner
        }

-- Attempts to load a standard font.
loadFont :: SF.FontName -> IO AnyFont
loadFont f = do
        fontOrErr <- SF.mkStdFont f
        case fontOrErr of
            Right lFont -> return lFont -- Loading standard fonts cannot fail.
            Left _ -> error $ "INTERNAL: Failed to load font " ++ quote (T.pack $ show f)

-- Takes a font and style and returns the appropriate font 
getFont :: LoadedFonts -> Font -> TextStyle -> AnyFont
getFont fonts family style = case family of
    Helvetica -> case style of
        Bold       -> hb fonts
        Italic     -> hi fonts
        Emphasised -> hbi fonts
        _          -> h fonts
    Courier -> case style of
        Bold       -> cb fonts
        Italic     -> ci fonts
        Emphasised -> cbi fonts
        _          -> c fonts
    Times -> case style of
        Bold       -> tb fonts
        Italic     -> ti fonts
        Emphasised -> tbi fonts
        _          -> t fonts
