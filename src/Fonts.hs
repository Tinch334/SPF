module Fonts (loadFonts) where

import Common
import qualified Datatypes.ValidatedTokens as VT

import Data.Validation
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as M

import Graphics.PDF.Fonts.StandardFont
import Graphics.PDF.Fonts.Font

type FontMap = Map String AnyFont

fonts :: [FontName]
fonts = 
    [ Helvetica  
    , Helvetica_Bold   
    , Helvetica_Oblique    
    , Helvetica_BoldOblique    
    , Times_Roman  
    , Times_Bold   
    , Times_Italic     
    , Times_BoldItalic     
    , Courier  
    , Courier_Bold     
    , Courier_Oblique  
    , Courier_BoldOblique
    ]


{-
    Getting fonts is not completely straightforward, built-in fonts have to be loaded, which can fail. This means that we cannot simply have
    a function doing pattern matching for fonts returning a constructor. Instead if fonts are loaded successfully they are stored in a map,
    which the pattern matching function then uses to get the corresponding font.
-}

-- Attempts to load all supported fonts.
loadFonts :: IO (Validation [String] FontMap)
loadFonts = do
    f <- mapM loadFont fonts
    return $ collectValidations f

-- Attempts to load a font.
loadFont :: FontName -> IO (Validation [String] (String, AnyFont))
loadFont f = do
        fontOrErr <- mkStdFont f
        case fontOrErr of
            Right lFont -> return $ Success (show f, lFont)
            Left _ -> return $ Failure ["INTERNAL: Failed to load font " ++ quote (T.pack $ show f)]

-- Gets the loaded AnyFont for the given font and style.
getFont :: FontMap -> VT.Font -> VT.TextStyle -> AnyFont 
getFont fm VT.Helvetica VT.Normal       = fromJust $ M.lookup (show Helvetica) fm
getFont fm VT.Helvetica VT.Bold         = fromJust $ M.lookup (show Helvetica_Bold) fm
getFont fm VT.Helvetica VT.Italic       = fromJust $ M.lookup (show Helvetica_Oblique) fm
getFont fm VT.Helvetica VT.Emphasised   = fromJust $ M.lookup (show Helvetica_BoldOblique) fm
getFont fm VT.Helvetica VT.Verbatim     = fromJust $ M.lookup (show Helvetica) fm
getFont fm VT.Helvetica VT.Quoted       = fromJust $ M.lookup (show Helvetica) fm

getFont fm VT.Courier VT.Normal         = fromJust $ M.lookup (show Courier) fm
getFont fm VT.Courier VT.Bold           = fromJust $ M.lookup (show Courier_Bold) fm
getFont fm VT.Courier VT.Italic         = fromJust $ M.lookup (show Courier_Oblique) fm
getFont fm VT.Courier VT.Emphasised     = fromJust $ M.lookup (show Courier_BoldOblique) fm
getFont fm VT.Courier VT.Verbatim       = fromJust $ M.lookup (show Courier) fm
getFont fm VT.Courier VT.Quoted         = fromJust $ M.lookup (show Courier) fm

getFont fm VT.Times VT.Normal           = fromJust $ M.lookup (show Times_Roman) fm
getFont fm VT.Times VT.Bold             = fromJust $ M.lookup (show Times_Bold) fm
getFont fm VT.Times VT.Italic           = fromJust $ M.lookup (show Times_Italic) fm
getFont fm VT.Times VT.Emphasised       = fromJust $ M.lookup (show Times_BoldItalic) fm
getFont fm VT.Times VT.Verbatim         = fromJust $ M.lookup (show Times_Roman) fm
getFont fm VT.Times VT.Quoted           = fromJust $ M.lookup (show Times_Roman) fm