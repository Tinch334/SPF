module Common where

import qualified Data.Text as T 

-- A separate option datatype is used for more modular parsing.
data POption    = POptionDirect [OptionValue]
                | POptionMap [OptionPair]
                deriving (Show)

type PLang = [PCommOpt]
data PCommOpt = PCommOpt PComm [POption]
data PComm  = PConfig ConfigOption
            | PBeginEnd PBeginEndContent -- The \begin and \end tags can be detected during parsing, an removed in favour of a singular tag.
            | PTitle [PText]
            | PAuthor [PText]
            | PDate [PText]
            | PSection [PText]
            | PSubsection [PText]
            | PFigure FilePath
            | PNewpage
            | PHLine
            
-- The "document" tag is not included since there has to be only one per document.
data PBeginEndContent   = PTextblock [PText] -- A block of text is represented as a list of text data blocks, in order.
                        | PTable [[PText]]
                        | PList [PText]

data PText  = PNormal T.Text
            | PBold T.Text
            | PItalic T.Text
            | PEmphasised T.Text
            | PVerbatim T.Text
            | PQuoted T.Text
            deriving (Show)


data OptionValue    = OVInt Int
                    | OVFloat Float
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