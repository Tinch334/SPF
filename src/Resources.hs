{-# LANGUAGE OverloadedStrings #-}

module Resources (loadResources) where

import Datatypes.ValidatedTokens
import Datatypes.Located
import Common

import Control.Applicative
import Control.Monad

import Data.Validation
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.List (nub)
import Data.ByteString (ByteString, toStrict)
import qualified Data.Text as T

import System.FilePath

import Codec.Picture
import Codec.Picture.Saving
import Graphics.PDF.Image


-- Loads all resources from the document, returns them as a map where each filepath is associated with the given element.
-- The filepath is that of the .spf that the resources are being loaded for.
loadResources :: [Located VComm] -> FilePath -> IO (Validation [LocatedError] (Map FilePath ByteString))
loadResources comms sFilepath = do
    let lRes = nub $ mapMaybe getResource comms -- Get all unique resources with their location.
    let cRes = map (completePath sFilepath) lRes
    vRes <- mapM loadResource cRes
    return $ collectValidations vRes

-- Tries to get the resource from the given command.
getResource :: Located VComm -> Maybe (Located FilePath)
getResource (Located pos (VFigure rp _ _)) = Just (Located pos rp)
getResource _ = Nothing

-- Completes resource paths, in case they aren't absolute.
completePath :: FilePath -> Located FilePath -> Located FilePath
completePath sp p@(Located pos rp) = if isAbsolute rp
    then p
    else let (dir, _) = splitFileName sp in
        Located pos (dir </> rp)

-- Tries to get the data from the given filepath, if successful returns both.
loadResource :: Located FilePath -> IO (Validation [LocatedError] (FilePath, ByteString))
loadResource (Located pos rp) = case takeExtension rp of
    e | elem e [".png", ".bmp", ".jpg", ".jpeg"] -> do
        res <- readImage rp
        case res of
            -- The default error does not follow the style of the rest of the program.
            Left _ -> return $ Failure [at pos $ "The file " ++ quote (T.pack rp) ++ " could not be accessed"]
            Right img -> return $ Success (rp, toStrict $ imageToPng img) -- "imageToPng" returns lazy a ByteString.
    e -> return $ Failure [at pos $ "The file extension " ++ quote (T.pack e) ++ " is invalid"]

-- Collects all validations into a map, collects all errors if any occur.
collectValidations :: [Validation [LocatedError] (FilePath, ByteString)] -> Validation [LocatedError] (Map FilePath ByteString)
collectValidations v = foldl collect (Success M.empty) v where
    collect (Failure e1) (Failure e2) = Failure (e1 <> e2) -- Concatenate errors.
    collect (Failure e)  (Success _)  = Failure e
    collect (Success _)  (Failure e)  = Failure e
    collect (Success m)  (Success (fp, bs)) = Success (M.insert fp bs m)