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


data LocatedError = LocatedError SourcePos String
    deriving (Show)

at :: SourcePos -> String -> LocatedError
at p s = LocatedError p s