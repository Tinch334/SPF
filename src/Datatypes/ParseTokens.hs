module Datatypes.ParseTokens where

import Datatypes.Located (Located(..))
import Data.Text (Text)


--------------------
-- TOP LEVEL STRUCTURE DEFINITIONS
--------------------
-- These structures are used for easier document processing, and to avoid later searches of a list of tokens, which would be inefficient.
data ParsedDocument = ParsedDocument
    { pdConfig      :: [Located PConfig] -- List for configurations.
    , pdMetadata    :: DocumentMetadata        -- Stores the Title/Author/Date.
    , pdContent     :: [Located PCommOpt]      -- The body of the document.
    } deriving (Eq)


data DocumentMetadata = DocumentMetadata
    { mdTitle   :: Maybe (Located PMetaOpt) 
    , mdAuthor  :: Maybe (Located PMetaOpt)
    , mdDate    :: Maybe (Located PMetaOpt)
    } deriving (Eq)

emptyMetadata :: DocumentMetadata
emptyMetadata = DocumentMetadata Nothing Nothing Nothing

-- Make verbose output more legible.
instance Show ParsedDocument where
    show (ParsedDocument cfg meta cnt) = 
        "\nConfiguration\n-------------\n" ++ concatMap (\e -> show e ++ "\n") cfg ++
        "\nMetadata\n--------\n" ++ show meta ++ 
        "\nDocument\n--------\n" ++ concatMap (\e -> show e ++ "\n") cnt

instance Show DocumentMetadata where
    show (DocumentMetadata t a d) =
       "Title: " ++ maybe "None" show t ++ "\n" ++
       "Author: " ++ maybe "None" show a ++ "\n" ++
       "Date: " ++ maybe "None" show d ++ "\n"


--------------------
-- DATATYPE DEFINITIONS
--------------------
data POptionValue   = PNumber  Double
                    | PText   Text
                    deriving (Show, Eq, Ord)

type POptionPair = (Text, POptionValue)

-- A separate option datatype is used for more modular parsing.
data POption    = POptionMap     [POptionPair]
                | POptionNone
                deriving (Eq, Ord)

instance Show POption where
    show (POptionMap m) = show m
    show POptionNone = "-"


-- Different data definitions are used to reduce ambiguity and avoid representing incorrect information and. For example a metadata field with
-- a command.
data PConfig = PConfig PConfigArg POption
    deriving (Show, Eq, Ord)

data PMetaOpt = PMetaOpt PMeta POption
    deriving (Show, Eq)
data PMeta  = PTitle       [PText]
            | PAuthor      [PText]
            | PDate        [PText]
            deriving (Show, Eq, Ord)


data PCommOpt = PCommOpt PComm POption
   deriving (Eq, Ord)

instance Show PCommOpt where
    show (PCommOpt comm opts) = show comm ++ "\n" ++ replicate 4 ' ' ++ show opts

data PComm  = PSection     [PText]
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
            deriving (Show, Eq, Ord)

data PConfigArg = PSize
                | PPagenumbering
                | PSectionspacing
                | PParagraphspacing
                | PListspacing
                | PTablespacing
                | PFigurespacing
                | PSpacingglue
                | PTextglue
                | PFont
                | PParsize
                | PTitleSize
                | PSectionSize
                | PSubsectionSize
                | PJustification
                | PListstyle
                deriving (Show, Eq, Ord)