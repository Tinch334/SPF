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

-- Collects all validations into a map, collects all errors if any occur.
collectValidations :: Ord a => [V.Validation [e] (a, b)] -> V.Validation [e] (M.Map a b)
collectValidations v = foldl collect (V.Success M.empty) v where
    collect (V.Failure e1) (V.Failure e2) = V.Failure (e1 <> e2) -- Concatenate errors.
    collect (V.Failure e)  (V.Success _)  = V.Failure e
    collect (V.Success _)  (V.Failure e)  = V.Failure e
    collect (V.Success m)  (V.Success (fp, bs)) = V.Success (M.insert fp bs m)

--------------------
-- CONSTANTS
--------------------
fileExtension :: String
fileExtension = "spf"

outputExtension :: String
outputExtension = "pdf"