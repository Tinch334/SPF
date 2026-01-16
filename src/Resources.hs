{-# LANGUAGE OverloadedStrings #-}

module Resources (loadResources) where

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


-- Loads all resources from the document, returns them as a map where each filepath is associated with the given element.
-- The filepath is that of the .spf that the resources are being loaded for.
loadResources :: [Located VComm] -> FilePath -> IO (Validation [LocatedError] (Map FilePath ByteString))
loadResources comms sFilepath = do
    let uniqueRes = nub $ mapMaybe getResource comms -- Get all unique resources with their location.
    let absoluteRes = Prelude.map (\(Located p rp) -> Located p $ completePath sFilepath rp) uniqueRes
    -- Load resources.
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
                -- If the image is in one of this formats we can load the bytes directly, avoiding unnecessary processing.
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