module Completion.Commands (completeCommands) where

import Datatypes.ValidatedTokens
import Datatypes.Located
import Common (filterMap)

import Control.Applicative
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import Data.Validation

type LVComm = Located VComm

-- Receives a list of commands and returns all non configuration commands with their options completed.
completeCommands :: VConfig -> [LVComm] -> [LVComm]
completeCommands cfg comms = filterMap isNotConfigComm (complete cfg) comms--map (complete cfg) (filter isNotConfigComm comms)

-- Completes all commands with missing options using the default configuration.
complete :: VConfig -> LVComm -> LVComm
complete cfg (Located pos comm) = Located pos $ cInner comm where
    cInner (VTitle t f s) = VTitle t (f <|> cfgFont cfg) (s <|> cfgTitleSize cfg)
    cInner (VAuthor t f s) = VAuthor t (f <|> cfgFont cfg) (s <|> cfgParSize cfg)
    cInner (VDate t f s) = VDate t (f <|> cfgFont cfg) (s <|> cfgParSize cfg)
    cInner (VSection t f s) = VSection t (f <|> cfgFont cfg) (s <|> cfgSectionSize cfg)
    cInner (VSubsection t f s) = VSubsection t (f <|> cfgFont cfg) (s <|> cfgSubsectionSize cfg)
    cInner f@(VFigure _ _ _) = f
    cInner t@(VTable _ _) = t
    cInner (VList l s) = VList l (s <|> cfgListStyle cfg)
    cInner (VParagraph t f s j) = VParagraph t (f <|> cfgFont cfg) (s <|> cfgParSize cfg) (j <|> cfgJustification cfg)
    cInner c = c -- Handles commands with no options.

isNotConfigComm :: LVComm -> Bool
isNotConfigComm (Located _ (VConfigComm _)) = False
isNotConfigComm _ = True