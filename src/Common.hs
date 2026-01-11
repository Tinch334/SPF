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

-- Useful for creating tuples in a monadic context.
tuple :: a -> b -> (a, b)
tuple a b = (a, b)

--------------------
-- CONSTANTS
--------------------
fileExtension :: String
fileExtension = "spf"

outputExtension :: String
outputExtension = "pdf"