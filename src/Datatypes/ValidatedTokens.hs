{-# LANGUAGE StrictData #-}

module Datatypes.ValidatedTokens
    ( -- Top level structure.
      ValidatedDocument(..)
    , ValidatedMetadata(..)
    -- Units and synonyms.
    , Pt(..)
    , PageWidth(..)
    , FontSize(..)
    , Caption
    , TableColumns
    -- Main data types.
    , VComm(..)
    , VConfig(..)
    , VText(..)
    -- Configuration values.
    , emptyVConfig
    , defaultVConfig
    -- Enums and attributes.
    , PageSize(..)
    , PageNumbering(..)
    , Spacing(..)
    , Font(..)
    , TextStyle(..)
    , Justification(..)
    , ListStyle(..)
    ) where

import Datatypes.Located (Located(..))

import Data.Text (Text)
import qualified Data.Text as T


--------------------
-- TOP LEVEL STRUCTURE DEFINITIONS
--------------------
data ValidatedDocument = ValidatedDocument
    { vConfig   :: VConfig
    , vMetadata :: ValidatedMetadata
    , vContent  :: [Located VComm]
    } deriving (Eq)

-- Once the metadata has been validated location data is no longer needed.
data ValidatedMetadata = ValidatedMetadata
    { vmTitle  :: Maybe [VText]
    , vmAuthor :: Maybe [VText]
    , vmDate   :: Maybe [VText]
    } deriving (Eq)

--------------------
-- DATATYPE DEFINITIONS
--------------------
-- Standard size units.
newtype Pt = Pt Double
    deriving (Eq, Ord) -- Derive Num and Fractional instances for easier calculations.

newtype PageWidth = PageWidth Double
    deriving (Eq, Ord)

newtype FontSize = FontSize Double
    deriving (Eq, Ord)

type Caption = Text
type TableColumns = Int

-- The maybe in the options indicates that they are optional. If found they are added with "Just", otherwise "Nothing" is used.
-- The Nothing's are then replaced with the default values for those arguments.
data VComm  = VSection      [VText] (Maybe Font) (Maybe FontSize)
            | VSubsection   [VText] (Maybe Font) (Maybe FontSize)
            | VFigure       FilePath PageWidth (Maybe Caption)
            | VTable        [[[VText]]] TableColumns
            | VList         [[VText]] (Maybe ListStyle)
            | VParagraph    [VText] (Maybe Font) (Maybe FontSize) (Maybe Justification)
            | VVerbatim     [Text] (Maybe FontSize) (Maybe Bool)
            | VHLine        PageWidth (Maybe Pt)
            | VNewpage
            deriving (Eq, Ord)


-- Options for configuration commands.
data VConfig = VConfig
    { cfgPageSize           :: Maybe PageSize
    , cfgPageNumbering      :: Maybe PageNumbering
    , cfgSectionSpacing     :: Maybe Spacing
    , cfgParagraphSpacing   :: Maybe Spacing
    , cfgListSpacing        :: Maybe Spacing
    , cfgTableSpacing       :: Maybe Spacing
    , cfgFigureSpacing      :: Maybe Spacing
    , cfgParIndent          :: Maybe Pt
    , cfgFont               :: Maybe Font
    , cfgParSize            :: Maybe FontSize
    , cfgTitleSize          :: Maybe FontSize
    , cfgSectionSize        :: Maybe FontSize
    , cfgSubsectionSize     :: Maybe FontSize
    , cfgVerbatimSize       :: Maybe FontSize
    , cfgJustification      :: Maybe Justification
    , cfgListStyle          :: Maybe ListStyle
    , cfgVertMargin         :: Maybe Pt
    , cfgHozMargin          :: Maybe Pt
    , cfgSectionNumbering   :: Maybe Bool
    , cfgFigureNumbering    :: Maybe Bool
    , cfgVerbatimNumbering  :: Maybe Bool
    } deriving (Show, Eq, Ord)

-- Empty config for easy instantiation.
emptyVConfig :: VConfig
emptyVConfig = VConfig
    { cfgPageSize           = Nothing
    , cfgPageNumbering      = Nothing
    , cfgSectionSpacing     = Nothing
    , cfgParagraphSpacing   = Nothing
    , cfgListSpacing        = Nothing
    , cfgTableSpacing       = Nothing
    , cfgFigureSpacing      = Nothing
    , cfgParIndent          = Nothing
    , cfgFont               = Nothing
    , cfgParSize            = Nothing
    , cfgTitleSize          = Nothing
    , cfgSectionSize        = Nothing
    , cfgSubsectionSize     = Nothing
    , cfgVerbatimSize       = Nothing
    , cfgJustification      = Nothing
    , cfgListStyle          = Nothing
    , cfgVertMargin         = Nothing
    , cfgHozMargin          = Nothing
    , cfgSectionNumbering   = Nothing
    , cfgFigureNumbering    = Nothing
    , cfgVerbatimNumbering  = Nothing
    }

-- Default configuration values for commands, determined manually, to make the document aesthetically pleasant.
defaultVConfig :: VConfig
defaultVConfig = VConfig
    { cfgPageSize           = Just SizeA4
    , cfgPageNumbering      = Just NumberingArabic
    , cfgSectionSpacing     = Just $ Spacing (Pt 5) (Pt 7.5)
    , cfgParagraphSpacing   = Just $ Spacing (Pt 7.5) (Pt 10)
    , cfgListSpacing        = Just $ Spacing (Pt 5) (Pt 5)
    , cfgTableSpacing       = Just $ Spacing (Pt 10) (Pt 10)
    , cfgFigureSpacing      = Just $ Spacing (Pt 6) (Pt 4)
    , cfgParIndent          = Just $ Pt 20
    , cfgFont               = Just Times
    , cfgParSize            = Just $ FontSize 12
    , cfgTitleSize          = Just $ FontSize 32
    , cfgSectionSize        = Just $ FontSize 18
    , cfgSubsectionSize     = Just $ FontSize 16
    , cfgVerbatimSize  = Just $ FontSize 10
    , cfgJustification      = Just JustifyLeft
    , cfgListStyle          = Just ListBullet
    , cfgVertMargin         = Just $ Pt 45
    , cfgHozMargin          = Just $ Pt 50
    , cfgSectionNumbering   = Just True
    , cfgFigureNumbering    = Just True
    , cfgVerbatimNumbering  = Just True
    }
    

-- General data types.
data PageSize   = SizeA4
                | SizeA3
                | SizeLegal
                | SizeCustom Pt Pt   -- width, height
                deriving (Show, Eq, Ord)

data PageNumbering = NumberingArabic | NumberingRoman | NumberingNone
    deriving (Show, Eq, Ord)

data Spacing = Spacing Pt Pt
    deriving (Show, Eq, Ord)

data Font = Helvetica | Courier | Times
    deriving (Show, Eq, Ord)

data TextStyle = Normal | Bold | Italic | Emphasised
    deriving (Show, Eq, Ord)

data Justification = JustifyLeft | JustifyRight | JustifyCenter | JustifyFull
    deriving (Show, Eq, Ord)

data ListStyle = ListBullet | ListSquare | ListArrow | ListNumber
    deriving (Show, Eq, Ord)

-- Text definition.
data VText = VText
    { textCnt  :: Text
    , style :: TextStyle
    } deriving (Eq, Ord)


--------------------
-- SHOW INSTANCES
--------------------
-- Flattens a list of VText into a single string for display.
showVTextList :: [VText] -> String
showVTextList = unwords . map show

instance Show ValidatedDocument where
    show (ValidatedDocument cfg meta cnt) = unlines
        [ "\nConfiguration\n-------------\n"
        , show cfg
        , "\nMetadata\n--------\n"
        , show meta 
        , "\nDocument\n--------\n"
        , unlines (map show cnt)
        ]

instance Show ValidatedMetadata where
    show (ValidatedMetadata t a d) = unlines
        [ "  Title:  " ++ maybe "-" showVTextList t
        , "  Author: " ++ maybe "-" showVTextList a
        , "  Date:   " ++ maybe "-" showVTextList d
        ]

instance Show VText where
    show (VText t style) = case style of
        Normal      -> T.unpack t
        Bold        -> "*" ++ T.unpack t ++ "*"
        Italic      -> "_" ++ T.unpack t ++ "_"
        Emphasised  -> "!" ++ T.unpack t ++ "!"

instance Show VComm where
    show (VSection txt _ _)     = "\n[SECTION] " ++ showVTextList txt
    show (VSubsection txt _ _)  = "\n  [SUB] " ++ showVTextList txt
    show (VParagraph txt _ _ _) = "  [PAR] " ++ showVTextList txt
    show (VFigure fp w c)       = "  [FIG] " ++ fp ++ " (Width: " ++ show w ++ ")" ++ maybe "" (\x -> " Cap: " ++ T.unpack x) c
    show (VTable _ cols)        = "  [TABLE] (" ++ show cols ++ " columns)"
    show (VList items _)        = "  [LIST] (" ++ show (length items) ++ " items)"
    show (VVerbatim code _ _)   = "  [VERBATIM]\n"  ++ unlines (map (\l -> "    |" ++ T.unpack l) code)
    show (VHLine w _)           = "  [HLINE] Width: " ++ show w
    show VNewpage               = "  [NEWPAGE]"

instance Show Pt where
    show (Pt x) = show x ++ "pt"

instance Show PageWidth where
    show (PageWidth x) = show x ++ "w"

instance Show FontSize where
    show (FontSize x) = show x ++ "pt"