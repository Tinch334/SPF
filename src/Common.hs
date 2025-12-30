module Common where

import qualified Data.Text as T 


--------------------
-- DATATYPE DEFINITIONS
--------------------
-- A separate option datatype is used for more modular parsing.
data POption    = POptionDirect [OptionValue]
                | POptionMap [OptionPair]
                | POptionNone
                deriving (Show)

type PLang = [PCommOpt]
data PCommOpt = PCommOpt PComm POption deriving (Show)
data PComm  = PConfig ConfigOption
            | PTitle [PText]
            | PAuthor [PText]
            | PDate [PText]
            | PSection [PText]
            | PSubsection [PText]
            | PFigure FilePath
            -- The \begin and \end tags can be detected during parsing, an removed in favour of a singular tag. The "document" tag is not
            -- included since there has to be only one per document.
            | PTextblock [PText] -- A block of text is represented as a list of text data blocks, in order.
            | PTable [[[PText]]] -- The type a list of lists of lists, represents the rows(a list), having multiple columns(a list) each of
                                 -- which is a block of PText(a list).
            | PList [[PText]]
            | PParagraph [PText]
            | PNewpage
            | PHLine
            deriving (Show)


data PText  = PNormal T.Text
            | PBold T.Text
            | PItalic T.Text
            | PEmphasised T.Text
            | PVerbatim T.Text
            | PQuoted T.Text
            deriving (Show)


data OptionValue    = OVInt Int
                    | OVFloat Double
                    | OVText T.Text
                    deriving (Show)

type OptionPair = (T.Text, OptionValue)

data ConfigOption   = Size
                    | Pagenumbering
                    | Titlespacing
                    | Paragraphspacing
                    | Listspacing
                    | Tablespacing
                    | Figurespacing
                    | Spacingglue
                    | Textglue
                    | Font
                    | Parsize
                    | Titlesize
                    | Justification
                    deriving (Show, Eq)


--------------------
-- CONSTANTS
--------------------
fileExtension :: String
fileExtension = "spf"

outputExtension :: String
outputExtension = "pdf"