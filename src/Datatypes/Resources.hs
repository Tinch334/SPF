{-# LANGUAGE StrictData #-}

module Datatypes.Resources 
    ( FileInfo(..)
    , ResourceMap
    , FontFamily(..)
    , LoadedFonts(..)
    )
where

import Data.Map (Map)
import qualified Data.ByteString.Lazy as BL

import Graphics.PDF.Fonts.Font (AnyFont)


data FileInfo = FileInfo
    { bytes     :: BL.ByteString
    , width     :: Int
    , height    :: Int
    }
    deriving (Show, Eq, Ord)

type ResourceMap = Map FilePath FileInfo

-- Avoids code repetition.
data FontFamily = FontFamily
    { normal        :: AnyFont
    , bold          :: AnyFont
    , italic        :: AnyFont
    , boldItalic    :: AnyFont
    } deriving (Show, Eq)

data LoadedFonts = LoadedFonts
    { helvetica :: FontFamily
    , times     :: FontFamily
    , courier   :: FontFamily
    , symbols   :: AnyFont
    , zapf      :: AnyFont
    } deriving (Show, Eq)