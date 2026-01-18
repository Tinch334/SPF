{-# LANGUAGE OverloadedStrings #-}

module Resources 
    (loadResources
    , loadFonts
    , LoadedFonts)
    where

import Datatypes.ValidatedTokens
import Datatypes.Located
import Common

import Control.Applicative
import Control.Monad
import Control.Concurrent.Async (mapConcurrently)

import Data.Validation
import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe
import Data.List (nub)
import Data.ByteString (ByteString)
import Data.ByteString as BS
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
loadResources :: [Located VComm] -> FilePath -> IO (Validation [LocatedError] (Map FilePath ByteString))
loadResources comms sFilepath = do
    let uniqueRes = nub $ mapMaybe getResource comms -- Get all unique resources with their location.
    let absoluteRes = Prelude.map (\(Located p rp) -> Located p $ completePath sFilepath rp) uniqueRes
    -- Load resources concurrently.
    validRes <- mapConcurrently loadResource absoluteRes

    return $ M.fromList <$> sequenceA validRes

-- Tries to get the resource from the given command.
getResource :: Located VComm -> Maybe (Located FilePath)
getResource (Located pos (VFigure rp _ _)) = Just (Located pos rp)
getResource _ = Nothing

-- Tries to get the data from the given filepath, if successful returns both.
loadResource :: Located FilePath -> IO (Validation [LocatedError] (FilePath, ByteString))
loadResource (Located pos rp) = do
    let ext = takeExtension rp
    exists <- doesFileExist rp
    
    if not exists
        then return $ Failure [at pos $ "File does not exist: " ++ quote (T.pack rp)]
        else handleFile ext
        where
            handleFile ext =
                case ext of
                -- If the image is in one of these formats we can load the bytes directly, avoiding unnecessary processing.
                e | Prelude.elem e [".png", ".jpg", ".jpeg"] -> do
                    bytes <- BS.readFile rp
                    return $ Success (rp, bytes)
                ".bmp" -> do
                    res <- readImage rp
                    case res of
                        -- The default error does not follow the style of the rest of the program.
                        Left _ -> return $ Failure [at pos $ "The file " ++ quote (T.pack rp) ++ " could not be accessed"]
                        Right img -> return $ Success (rp, BS.toStrict $ imageToPng img) -- "imageToPng" returns lazy a ByteString.
                e -> return $ Failure [at pos $ "The file extension " ++ quote (T.pack e) ++ " is invalid"]


--------------------
-- FONT LOADING FUNCTIONS
--------------------
data LoadedFonts = LoadedFonts
    { h   :: AnyFont
    , hb  :: AnyFont
    , hi  :: AnyFont
    , hbi :: AnyFont
    , t   :: AnyFont
    , tb  :: AnyFont
    , ti  :: AnyFont
    , tbi :: AnyFont
    , c   :: AnyFont
    , cb  :: AnyFont
    , ci  :: AnyFont
    , cbi :: AnyFont
    }
    deriving (Show, Eq)

{-
    Getting fonts is not straightforward, built-in fonts have to be loaded, which can fail. This means that we cannot simply have a function
    doing pattern matching for fonts returning a constructor. Instead if fonts are loaded successfully they are stored in a data structure
    which the pattern matching function then uses to get the corresponding font.
-}
-- Loads all supported fonts.
loadFonts :: IO LoadedFonts
loadFonts = do
    hInner <- loadFont SF.Helvetica
    hbInner <- loadFont SF.Helvetica_Bold
    hiInner <- loadFont SF.Helvetica_Oblique
    hbiInner <- loadFont SF.Helvetica_BoldOblique
    tInner <- loadFont SF.Courier
    tbInner <- loadFont SF.Courier_Bold
    tiInner <- loadFont SF.Courier_Oblique
    tbiInner <- loadFont SF.Courier_BoldOblique
    cInner <- loadFont SF.Times_Roman
    cbInner <- loadFont SF.Times_Bold
    ciInner <- loadFont SF.Times_Italic
    cbiInner <- loadFont SF.Times_BoldItalic

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
        }

-- Attempts to load a font, loading standard fonts cannot fail.
loadFont :: SF.FontName -> IO AnyFont
loadFont f = do
        fontOrErr <- SF.mkStdFont f
        case fontOrErr of
            Right lFont -> return lFont
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