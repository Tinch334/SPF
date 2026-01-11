module Datatypes.ValidatedTokens where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

-- Standard size units.
newtype Pt = Pt Double
    deriving (Show, Eq, Ord)
newtype PageWidth = PageWidth Double
    deriving (Show, Eq, Ord)

-- Table aliases.
type Cell = [VText]
type Row = [Cell]
type Table = [Row]

-- Language definition
type VLang = [VComm]
-- The maybe in the options indicates that they are optional. If parsed they are added, with Just, otherwise Nothing is used.
-- The Nothing's are then replaced with the default values for those arguments.
data VComm  = VConfigComm   VConfig
            | VTitle        (NonEmpty VText) (Maybe Font) (Maybe FontSize)
            | VAuthor       (NonEmpty VText) (Maybe Font) (Maybe FontSize)
            | VDate         (NonEmpty VText) (Maybe Font) (Maybe FontSize)
            | VSection      (NonEmpty VText) (Maybe Font) (Maybe FontSize)
            | VSubsection   (NonEmpty VText) (Maybe Font) (Maybe FontSize)
            | VFigure       FilePath (Maybe PageWidth) (Maybe Caption)
            | VTable        Table TableColumns
            | VList         [[VText]] (Maybe ListStyle)
            | VParagraph    [VText] (Maybe Font) (Maybe FontSize) (Maybe Justification)
            | VNewpage
            | VHLine
            deriving (Show, Eq, Ord)


-- Options for configuration.
data VConfig = VConfig
    { cfgPageSize           :: Maybe PageSize
    , cfgPageNumbering      :: Maybe PageNumbering
    , cfgTitleSpacing       :: Maybe Spacing
    , cfgParagraphSpacing   :: Maybe Spacing
    , cfgListSpacing        :: Maybe Spacing
    , cfgTableSpacing       :: Maybe Spacing
    , cfgFigureSpacing      :: Maybe Spacing
    , cfgSpacingGlue        :: Maybe Glue
    , cfgTextGlue           :: Maybe Glue
    , cfgParIndent          :: Maybe Pt
    , cfgFont               :: Maybe Font
    , cfgParSize            :: Maybe Pt
    , cfgTitleSize          :: Maybe Pt
    , cfgJustification      :: Maybe Justification
    } deriving (Show, Eq, Ord)

-- Empty config for easy instantiation.
emptyVConfig :: VConfig
emptyVConfig = VConfig
    { cfgPageSize      = Nothing
    , cfgPageNumbering = Nothing
    , cfgTitleSpacing  = Nothing
    , cfgParagraphSpacing = Nothing
    , cfgListSpacing   = Nothing
    , cfgTableSpacing  = Nothing
    , cfgFigureSpacing = Nothing
    , cfgSpacingGlue   = Nothing
    , cfgTextGlue      = Nothing
    , cfgParIndent     = Nothing
    , cfgFont          = Nothing
    , cfgParSize       = Nothing
    , cfgTitleSize     = Nothing
    , cfgJustification = Nothing
    }

-- General data types, meant for reusability, to avoid repetition.
data PageSize   = SizeA4
                | SizeA3
                | SizeLegal
                | SizeCustom Pt Pt   -- width, height
                deriving (Show, Eq, Ord)

data PageNumbering = NumberingArabic | NumberingRoman | NumberingNone
    deriving (Show, Eq, Ord)

newtype Spacing = Spacing { unSpacing :: (Pt, Pt) }   -- before, after
    deriving (Show, Eq, Ord)

newtype Glue = Glue { unGlue :: (Pt, Pt) }            -- stretch, shrink
    deriving (Show, Eq, Ord)

data Font = Helvetica | Courier | Times
    deriving (Show, Eq, Ord)

newtype FontSize = FontSize Pt
    deriving (Show, Eq, Ord)

data TextStyle = Bold | Italic | Emphasised | Verbatim | Quoted
    deriving (Show, Eq, Ord)

data Justification = JustifyLeft | JustifyRight | JustifyCenter | JustifyFull
    deriving (Show, Eq, Ord)

-- Text definition.
data VText = VText
    { vtText    :: Text
    , vtStyles  :: [TextStyle]
    } deriving (Show, Eq, Ord)

-- Command specific types.
newtype TableColumns = TableColumns Int
    deriving (Show, Eq, Ord)

data ListStyle = ListBullet | ListSquare | ListArrow | ListNumber
    deriving (Show, Eq, Ord)

newtype Caption = Caption Text
    deriving (Show, Eq, Ord)