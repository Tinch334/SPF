module Datatypes.Located
    ( Located(..)
    , LocatedError(..)
    , at
    ) where

import Text.Megaparsec (SourcePos)

-- To generate more informative errors during the validation stage, otherwise there is no way to show where in the file the error comes from.
data Located a = Located SourcePos a
-- Showing the location when printing tokens creates clutter.
instance Show a => Show (Located a) where
    show (Located _ e) = show e
-- When checking for located it's not of interest whether the location is the same, since if it is then clearly the data is the same.
instance Eq a => Eq (Located a) where
    (Located _ a) == (Located _ b) = a == b


data LocatedError = LocatedError SourcePos String
    deriving (Show)

at :: SourcePos -> String -> LocatedError
at p s = LocatedError p s