module Datatypes.ParseTokens where

import Data.Text (Text)


--------------------
-- DATATYPE DEFINITIONS
--------------------
-- A separate option datatype is used for more modular parsing.
data POption    = POptionValue  [POptionValue]
                | POptionMap     [POptionPair]
                | POptionNone
                deriving (Show, Eq, Ord)

type PLang = [PCommOpt]
data PCommOpt = PCommOpt PComm POption deriving (Show, Eq, Ord)
data PComm  = PConfig      PConfigOption
            | PTitle       [PText]
            | PAuthor      [PText]
            | PDate        [PText]
            | PSection     [PText]
            | PSubsection  [PText]
            | PFigure      FilePath
            -- The \begin and \end tags can be detected during parsing, an removed in favour of a singular tag. The "document" tag is not
            -- included since there has to be only one per document.
            | PTable       [[[PText]]] -- The type a list of lists of lists, represents the rows(a list), having multiple columns(a list) each
                                       -- of which is a block of PText(a list).
            | PList        [[PText]]
            | PParagraph   [PText] -- Used for both regular paragraphs and those enclosed in begin/end.
            | PNewpage
            | PHLine
            deriving (Show, Eq, Ord)


data PText  = PNormal      Text
            | PBold        Text
            | PItalic      Text
            | PEmphasised  Text
            | PVerbatim    Text
            | PQuoted      Text
            deriving (Show, Eq, Ord)


data POptionValue   = PNumber  Double
                    | PText   Text
                    deriving (Show, Eq, Ord)

type POptionPair = (Text, POptionValue)

data PConfigOption  = PSize
                    | PPagenumbering
                    | PTitlespacing
                    | PParagraphspacing
                    | PListspacing
                    | PTablespacing
                    | PFigurespacing
                    | PSpacingglue
                    | PTextglue
                    | PFont
                    | PParsize
                    | PTitlesize
                    | PJustification
                    deriving (Show, Eq, Ord)