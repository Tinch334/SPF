{-# LANGUAGE StrictData #-}

module Datatypes.Resources where

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


data LoadedFonts = LoadedFonts
    { h         :: AnyFont
    , hb        :: AnyFont
    , hi        :: AnyFont
    , hbi       :: AnyFont
    , t         :: AnyFont
    , tb        :: AnyFont
    , ti        :: AnyFont
    , tbi       :: AnyFont
    , c         :: AnyFont
    , cb        :: AnyFont
    , ci        :: AnyFont
    , cbi       :: AnyFont
    -- These styles are not for text fonts, but symbols.
    , symbols   :: AnyFont
    , zapf      :: AnyFont
    }
    deriving (Show, Eq)