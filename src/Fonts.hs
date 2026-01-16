module Fonts (loadFonts) where

import Common
import qualified Datatypes.ValidatedTokens as VT

import qualified Data.Text as T

import Graphics.PDF.Fonts.StandardFont
import Graphics.PDF.Fonts.Font


-- Structure to store all loaded fonts.
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


{-
    Getting fonts is not straightforward, built-in fonts have to be loaded, which can fail. This means that we cannot simply have a function
    doing pattern matching for fonts returning a constructor. Instead if fonts are loaded successfully they are stored in a data structure
    which the pattern matching function then uses to get the corresponding font.
-}
-- Loads all supported fonts.
loadFonts :: IO LoadedFonts
loadFonts = do
    hInner <- loadFont Helvetica
    hbInner <- loadFont Helvetica_Bold
    hiInner <- loadFont Helvetica_Oblique
    hbiInner <- loadFont Helvetica_BoldOblique
    tInner <- loadFont Courier
    tbInner <- loadFont Courier_Bold
    tiInner <- loadFont Courier_Oblique
    tbiInner <- loadFont Courier_BoldOblique
    cInner <- loadFont Times_Roman
    cbInner <- loadFont Times_Bold
    ciInner <- loadFont Times_Italic
    cbiInner <- loadFont Times_BoldItalic

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
loadFont :: FontName -> IO AnyFont
loadFont f = do
        fontOrErr <- mkStdFont f
        case fontOrErr of
            Right lFont -> return lFont
            Left _ -> error $ "INTERNAL: Failed to load font " ++ quote (T.pack $ show f)

-- Takes a font and style and returns the appropriate font 
getFont :: LoadedFonts -> VT.Font -> VT.TextStyle -> AnyFont
getFont fonts family style = case family of
    VT.Helvetica -> case style of
        VT.Bold       -> hb fonts
        VT.Italic     -> hi fonts
        VT.Emphasised -> hbi fonts
        _             -> h fonts
    VT.Courier -> case style of
        VT.Bold       -> cb fonts
        VT.Italic     -> ci fonts
        VT.Emphasised -> cbi fonts
        _             -> c fonts
    VT.Times -> case style of
        VT.Bold       -> tb fonts
        VT.Italic     -> ti fonts
        VT.Emphasised -> tbi fonts
        _             -> t fonts