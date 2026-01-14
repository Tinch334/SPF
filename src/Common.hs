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

-- Filters a list and applies the given function to all non filtered elements.
filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap _ _ [] = []
filterMap f m (x:xs) = if f x then (m x):(filterMap f m xs) else filterMap f m xs

--------------------
-- CONSTANTS
--------------------
fileExtension :: String
fileExtension = "spf"

outputExtension :: String
outputExtension = "pdf"