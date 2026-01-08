module Common where

import qualified Data.Text as T
import qualified Data.List as L


--------------------
-- AUXILIARY FUNCTIONS
--------------------
quote :: T.Text -> String
quote t = "\"" ++ (T.unpack t) ++ "\""

quoteList :: [T.Text] -> String
quoteList l = L.intercalate ", " (map quote l)


--------------------
-- CONSTANTS
--------------------
fileExtension :: String
fileExtension = "spf"

outputExtension :: String
outputExtension = "pdf"