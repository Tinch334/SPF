module Completion (completeCommand, completeMeta) where

import Datatypes.ValidatedTokens
import Datatypes.Located
import Common (filterMap)

import Control.Applicative
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import Data.Validation
import Data.Maybe (fromJust)

-- Completes a command's missing options using the default configuration.
completeCommand :: VConfig -> Located VComm -> VComm
completeCommand cfg (Located _ comm) = cc comm where
    cc (VSection t f s) = VSection t (f <|> cfgFont cfg) (s <|> cfgSectionSize cfg)
    cc (VSubsection t f s) = VSubsection t (f <|> cfgFont cfg) (s <|> cfgSubsectionSize cfg)
    cc f@(VFigure _ _ _) = f
    cc t@(VTable _ _) = t
    cc (VList l s) = VList l (s <|> cfgListStyle cfg)
    cc (VParagraph t f s j) = VParagraph t (f <|> cfgFont cfg) (s <|> cfgParSize cfg) (j <|> cfgJustification cfg)
    cc c = c -- Handles commands with no options.

-- Completes metadata's commands missing options using the default configuration.
completeMeta :: VConfig -> VMeta -> VMeta
completeMeta cfg (VTitle t f s) = VTitle t (f <|> cfgFont cfg) (s <|> cfgTitleSize cfg)
-- "fromJust" can be used safely in these functions since the configuration will be complete.
completeMeta cfg (VAuthor t f s) = VAuthor t (f <|> cfgFont cfg) (s <|> modifyFontSize 0.8 (fromJust $ cfgTitleSize cfg))
completeMeta cfg (VDate t f s) = VDate t (f <|> cfgFont cfg) (s <|> modifyFontSize 0.7 (fromJust $ cfgTitleSize cfg))

-- Changes the given font by the specified percentage.
modifyFontSize :: Double -> FontSize -> Maybe FontSize
modifyFontSize p (FontSize (Pt fs)) = Just $ FontSize $ Pt (fs * p)