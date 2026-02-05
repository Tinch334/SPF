{-# LANGUAGE StrictData #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
    , VPara(..)
    -- Configuration data types.
    , StyleConfig(..)
    , LayoutConfig(..)
    , SpacingConfig(..)
    , SizeConfig(..)
    , ToggleConfig(..)
    -- Configuration values.
    , emptyVConfig
    , defaultVConfig
    -- Enums and attributes.
    , PageSize(..)
    , PageNumbering(..)
    , Spacing(..)
    , Font(..)
    , TextType(..)
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
    { vmTitle  :: Maybe [VPara]
    , vmAuthor :: Maybe [VPara]
    , vmDate   :: Maybe [VPara]
    } deriving (Eq)

--------------------
-- DATATYPE DEFINITIONS
--------------------
-- Standard size units.
newtype Pt = Pt Double
    deriving (Eq, Ord) -- Derive Num and Fractional instances for easier calculations.

newtype PageWidth = PageWidth Double
    deriving (Eq, Ord)

newtype FontSize = FontSize Int
    deriving (Eq, Ord)

type Caption = Text
type TableColumns = Int

-- The maybe in the options indicates that they are optional. If found they are added with "Just", otherwise "Nothing" is used.
-- The Nothing's are then replaced with the default values for those arguments.
data VComm  = VSection      [VPara] (Maybe Font) (Maybe FontSize)
            | VSubsection   [VPara] (Maybe Font) (Maybe FontSize)
            | VFigure       FilePath PageWidth (Maybe Caption)
            | VTable        [[[VPara]]] TableColumns
            | VList         [[VPara]] (Maybe ListStyle)
            | VParagraph    [VPara] (Maybe Font) (Maybe FontSize) (Maybe Justification)
            | VVerbatim     [Text] (Maybe FontSize) (Maybe Bool)
            | VHLine        PageWidth (Maybe Pt)
            | VNewpage
            deriving (Eq, Ord)


data StyleConfig = StyleConfig
    { font          :: Maybe Font
    , justification :: Maybe Justification
    , listType      :: Maybe ListStyle
    } deriving (Show, Eq, Ord)

data LayoutConfig = LayoutConfig
    { pageSize      :: Maybe PageSize
    , numbering     :: Maybe PageNumbering
    , marginVert    :: Maybe Pt
    , marginHoz     :: Maybe Pt
    } deriving (Show, Eq, Ord)

data SpacingConfig = SpacingConfig
    { sectionSp     :: Maybe Spacing
    , paragraphSp   :: Maybe Spacing
    , listSp        :: Maybe Spacing
    , tableSp       :: Maybe Spacing
    , figureSp      :: Maybe Spacing
    , verbatimSp    :: Maybe Spacing
    , parIndent     :: Maybe Pt
    } deriving (Show, Eq, Ord)

data SizeConfig = SizeConfig
    { paragraphSize     :: Maybe FontSize
    , titleSize         :: Maybe FontSize
    , sectionSize       :: Maybe FontSize
    , subsectionSize    :: Maybe FontSize
    , verbatimSize      :: Maybe FontSize
    } deriving (Show, Eq, Ord)

data ToggleConfig = ToggleConfig
    { sectionNumbering  :: Maybe Bool
    , figureNumbering   :: Maybe Bool
    , verbatimNumbering :: Maybe Bool
    } deriving (Show, Eq, Ord)

data VConfig = VConfig
    { layout    :: LayoutConfig
    , styles    :: StyleConfig
    , sizes     :: SizeConfig
    , spacing   :: SpacingConfig
    , toggles   :: ToggleConfig
    } deriving (Eq, Ord)

-- Empty config for easy instantiation.
emptyStyle :: StyleConfig
emptyStyle = StyleConfig
    { font          = Nothing
    , justification = Nothing
    , listType      = Nothing
    }

emptyLayout :: LayoutConfig
emptyLayout = LayoutConfig
    { pageSize      = Nothing
    , numbering     = Nothing
    , marginVert    = Nothing
    , marginHoz     = Nothing
    }

emptySpacing :: SpacingConfig
emptySpacing = SpacingConfig
    { sectionSp     = Nothing
    , paragraphSp   = Nothing
    , listSp        = Nothing
    , tableSp       = Nothing
    , figureSp      = Nothing
    , verbatimSp    = Nothing
    , parIndent     = Nothing
    }

emptySize :: SizeConfig
emptySize = SizeConfig
    { paragraphSize     = Nothing
    , titleSize         = Nothing
    , sectionSize       = Nothing
    , subsectionSize    = Nothing
    , verbatimSize      = Nothing
    }

emptyToggle :: ToggleConfig
emptyToggle = ToggleConfig
    { sectionNumbering  = Nothing
    , figureNumbering   = Nothing
    , verbatimNumbering = Nothing
    }

emptyVConfig :: VConfig
emptyVConfig = VConfig
    { layout    = emptyLayout
    , styles    = emptyStyle
    , sizes     = emptySize
    , spacing   = emptySpacing
    , toggles   = emptyToggle
    }

-- Default configuration values for commands, determined manually, to make the document aesthetically pleasant.
defaultStyle :: StyleConfig
defaultStyle = StyleConfig
    { font          = Just Times
    , justification = Just JustifyLeft
    , listType      = Just ListBullet
    }

defaultLayout :: LayoutConfig
defaultLayout = LayoutConfig
    { pageSize      = Just SizeA4
    , numbering     = Just NumberingArabic
    , marginVert    = Just $ Pt 45
    , marginHoz     = Just $ Pt 50
    }

defaultSpacing :: SpacingConfig
defaultSpacing = SpacingConfig
    { sectionSp     = Just $ Spacing (Pt 5) (Pt 7.5)
    , paragraphSp   = Just $ Spacing (Pt 7.5) (Pt 10)
    , listSp        = Just $ Spacing (Pt 5) (Pt 5)
    , tableSp       = Just $ Spacing (Pt 10) (Pt 10)
    , figureSp      = Just $ Spacing (Pt 6) (Pt 4)
    , verbatimSp    = Just $ Spacing (Pt 10) (Pt 10)
    , parIndent     = Just $ Pt 20
    }

defaultSize :: SizeConfig
defaultSize = SizeConfig
    { paragraphSize     = Just $ FontSize 12
    , titleSize         = Just $ FontSize 32
    , sectionSize       = Just $ FontSize 18
    , subsectionSize    = Just $ FontSize 16
    , verbatimSize      = Just $ FontSize 10
    }

defaultToggles :: ToggleConfig
defaultToggles = ToggleConfig
    { sectionNumbering  = Just True
    , figureNumbering   = Just True
    , verbatimNumbering = Just True
    }

defaultVConfig :: VConfig
defaultVConfig = VConfig
    { layout    = defaultLayout
    , styles    = defaultStyle
    , sizes     = defaultSize
    , spacing   = defaultSpacing
    , toggles   = defaultToggles
    }


--------------------
-- GENERAL DATA TYPES
--------------------
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

data TextType = Normal | Bold | Italic | Emphasised | Quoted | Verbatim
    deriving (Show, Eq, Ord)

data Justification = JustifyLeft | JustifyRight | JustifyCenter | JustifyFull
    deriving (Show, Eq, Ord)

data ListStyle = ListBullet | ListSquare | ListArrow | ListNumber
    deriving (Show, Eq, Ord)

-- Text definition.
data VPara = VPara
        { textCnt   :: Text
        , textType  :: TextType
        } deriving (Eq, Ord)


--------------------
-- SHOW INSTANCES
--------------------
-- Flattens a list of VPara into a single string for display.
showVTextList :: [VPara] -> String
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

instance Show VPara where
    show (VPara t style) = case style of
        Normal      -> T.unpack t
        Bold        -> "*" ++ T.unpack t ++ "*"
        Italic      -> "_" ++ T.unpack t ++ "_"
        Emphasised  -> "!" ++ T.unpack t ++ "!"
        Quoted      -> "\"" ++ T.unpack t ++ "\""
        Verbatim    -> "|" ++ T.unpack t ++ "|"

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

instance Show VConfig where
    show cfg = unlines
        [ show $ layout cfg
        , " "
        , show $ styles cfg
        , " "
        , show $ sizes cfg
        , " "
        , show $ spacing cfg
        , " "
        , show $ toggles cfg
        ]

instance Show Pt where
    show (Pt x) = show x ++ "pt"

instance Show PageWidth where
    show (PageWidth x) = show x ++ "w"

instance Show FontSize where
    show (FontSize x) = show x ++ "pt"