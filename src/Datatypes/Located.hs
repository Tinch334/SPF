module Datatypes.Located
    ( Located(..)
    , LocatedError(..)
    , at
    ) where

import Text.Megaparsec (SourcePos)

-- To generate more informative errors during the validation stage, otherwise there is no way to show where in the file the error comes from.
data Located a = Located SourcePos a
    deriving (Show)
data LocatedError = LocatedError SourcePos String
    deriving (Show)

at :: SourcePos -> String -> LocatedError
at p s = LocatedError p s