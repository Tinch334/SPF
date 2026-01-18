module Typesetting.Typesetting (typesetDocument) where

import Control.Applicative

import Data.Map (Map)
import Data.ByteString (ByteString)
import Resources (LoadedFonts)

import Datatypes.ValidatedTokens
import Datatypes.Located

 

typesetDocument :: ValidatedDocument -> Map FilePath ByteString -> LoadedFonts -> FilePath -> IO ()
typesetDocument doc res fonts outFile = print "hello"
------------------------
-- COMPLETE COMMANDS; NO LONGER DONE IN Main.hs!!!!!!!!!!
------------------------