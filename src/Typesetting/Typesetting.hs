module Typesetting.Typesetting (typesetDocument) where

import Control.Applicative

import Datatypes.ValidatedTokens
import Datatypes.Located
 

typesetDocument :: [Located VComm] -> VConfig -> FilePath -> IO ()
typesetDocument comms cfg outFile = print "hello"--do
