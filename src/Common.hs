module Common where

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Validation as V
import qualified Data.Map as M

import qualified System.FilePath as SF


--------------------
-- AUXILIARY FUNCTIONS
--------------------
-- For these functions ad-hoc polymorphism isn't used; Because overloaded strings are used so would type annotations.
quote :: T.Text -> String
quote t = "\"" ++ (T.unpack t) ++ "\""

quoteList :: [T.Text] -> String
quoteList l = L.intercalate ", " (map quote l)

-- Filters a list and applies the given function to all non filtered elements.
filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap _ _ [] = []
filterMap f m (x:xs) = if f x then (m x):(filterMap f m xs) else filterMap f m xs

-- Completes resource paths, in case they aren't absolute.
completePath :: FilePath -> FilePath -> FilePath
completePath sp rp = if SF.isAbsolute rp
    then rp
    else let (dir, _) = SF.splitFileName sp in dir SF.</> rp

--------------------
-- CONSTANTS
--------------------
fileExtension :: String
fileExtension = "spf"

outputExtension :: String
outputExtension = "pdf"