{-# LANGUAGE StrictData #-}

module Datatypes.ParseTokens 
    ( -- Top level structure.
      ParsedDocument(..)
    , DocumentMetadata(..)
      -- Data types.
    , POptionValue(..)
    , POptionPair
    , POption(..)
    , PConfig(..)
    , PCommOpt(..)
    , PComm(..)
    , PText(..)
    , PConfigArg(..)
    ) where

import Datatypes.Located (Located(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)


--------------------
-- TOP LEVEL STRUCTURE DEFINITIONS
--------------------
-- These structures are used for easier document processing, and to avoid later searches of a list of tokens, which would be inefficient.
data ParsedDocument = ParsedDocument
    { pdConfig      :: [Located PConfig]    -- List for configurations.
    , pdMetadata    :: DocumentMetadata     -- Stores the Title/Author/Date.
    , pdContent     :: [Located PCommOpt]   -- The body of the document.
    } deriving (Eq)

-- Location information is not included since there's no options to validate.
data DocumentMetadata = DocumentMetadata
    { mdTitle  :: Maybe [PText]
    , mdAuthor :: Maybe [PText]
    , mdDate   :: Maybe [PText]
    } deriving (Eq)


--------------------
-- DATATYPE DEFINITIONS
--------------------
data POptionValue   = PNumber   Double
                    | PText     Text
                    | PBool     Bool
                    deriving (Eq, Ord)

type POptionPair = (Text, POptionValue)

-- A separate option datatype is used for more modular parsing.
data POption    = POptionMap     [POptionPair]
                | POptionNone
                deriving (Eq, Ord)

-- Different data definitions are used to reduce ambiguity and avoid representing incorrect information and. For example a metadata field with
-- a command.
data PConfig = PConfig PConfigArg POption
    deriving (Eq, Ord)

data PCommOpt = PCommOpt PComm POption
   deriving (Eq, Ord)

data PComm  = PSection      [PText]
            | PSubsection   [PText]
            | PFigure       FilePath
            -- The \begin and \end tags can be detected during parsing, an removed in favour of a singular tag. The "document" tag is not
            -- included since there has to be only one per document.
            | PTable        [[[PText]]] -- The type a list of lists of lists, represents the rows(a list), having multiple columns(a list) each
                                       -- of which is a block of PText(a list).
            | PList         [[PText]]
            | PParagraph    [PText] -- Used for both regular paragraphs and those enclosed in begin/end.
            | PVerbatim     [Text]
            | PNewpage
            | PHLine
            deriving (Eq, Ord)


data PText  = PNormal       Text
            | PBold         Text
            | PItalic       Text
            | PEmphasised   Text
            deriving (Eq, Ord)

data PConfigArg = PSize
                | PPagenumbering
                | PSectionspacing
                | PParagraphspacing
                | PListspacing
                | PTablespacing
                | PFigurespacing
                | PVerbatimSpacing
                | PParIndent
                | PFont
                | PParsize
                | PTitleSize
                | PSectionSize
                | PSubsectionSize
                | PVerbatimSize
                | PJustification
                | PListstyle
                | PVerMargin
                | PHozMargin
                | PSectionNumbering
                | PFigureNumbering
                | PVerbatimNumbering
                deriving (Show, Eq, Ord)


--------------------
-- SHOW INSTANCES
--------------------
-- Flattens a list of PText into a single string for display.
showPTextList :: [PText] -> String
showPTextList = unwords . map show

-- Make verbose output more legible.
instance Show ParsedDocument where
    show (ParsedDocument cfg meta cnt) = unlines
        [ "\nConfiguration\n-------------"
        , if null cfg then "  (No Configuration)" else unlines (map (("  • " ++) . show) cfg)
        , "\nMetadata\n--------\n"
        , show meta
        , "\nDocument\n--------\n"
        , unlines (map show cnt)
        ]

instance Show DocumentMetadata where
    show (DocumentMetadata t a d) = unlines 
        [ "  Title:  " ++ formatMaybe t
        , "  Author: " ++ formatMaybe a
        , "  Date:   " ++ formatMaybe d
        ]
      where 
        formatMaybe Nothing = "(None)"
        formatMaybe (Just txts) = showPTextList txts

instance Show POption where
    show (POptionMap opts) = "{" ++ intercalate ", " (map showPair opts) ++ "}"
      where showPair (k, v) = T.unpack k ++ ": " ++ show v
    show POptionNone = "(No Options)"

instance Show POptionValue where
    show (PNumber n) = show n
    show (PText t)   = show t
    show (PBool b)   = show b

instance Show PConfig where
    show (PConfig arg opt) = show arg ++ " " ++ show opt

instance Show PCommOpt where
    show (PCommOpt comm opts) = case opts of
        POptionNone -> show comm
        _           -> show comm ++ " " ++ show opts

instance Show PComm where
    show (PSection txt)     = "\n[SECTION] " ++ showPTextList txt
    show (PSubsection txt)  = "\n  [SUB] " ++ showPTextList txt
    show (PFigure path)     = "  [FIG] Path: " ++ path
    show (PTable rows)      = "  [TABLE] (" ++ show (length rows) ++ " rows)"
    show (PList items)      = "  [LIST]\n" ++ unlines (map (\i -> "    - " ++ showPTextList i) items)
    show (PParagraph txt)   = "  [PAR] " ++ showPTextList txt
    show (PVerbatim code)   = "  [VERBATIM]\n" ++ unlines (map (\l -> "    |" ++ T.unpack l) code)
    show PNewpage           = "  [NEWPAGE]"
    show PHLine             = "  [HLINE]"

instance Show PText where
    show (PNormal t)        = T.unpack t
    show (PBold t)          = "*" ++ T.unpack t ++ "*"
    show (PItalic t)        = "_" ++ T.unpack t ++ "_"
    show (PEmphasised t)    = "!" ++ T.unpack t ++ "!"