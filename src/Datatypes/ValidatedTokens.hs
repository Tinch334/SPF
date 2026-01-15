module Datatypes.ValidatedTokens where

import Datatypes.Located (Located(..))

import Data.Text (Text)


-- Standard size units.
newtype Pt = Pt Double
    deriving (Show, Eq, Ord)
newtype PageWidth = PageWidth Double
    deriving (Show, Eq, Ord)
newtype FontSize = FontSize Pt
    deriving (Show, Eq, Ord)

-- Language definition.
type VLocatedLang = [Located VComm]
-- The maybe in the options indicates that they are optional. If found they are added with "Just", otherwise "Nothing" is used.
-- The Nothing's are then replaced with the default values for those arguments.
data VComm  = VConfigComm   VConfig
            | VTitle        [VText] (Maybe Font) (Maybe FontSize)
            | VAuthor       [VText] (Maybe Font) (Maybe FontSize)
            | VDate         [VText] (Maybe Font) (Maybe FontSize)
            | VSection      [VText] (Maybe Font) (Maybe FontSize)
            | VSubsection   [VText] (Maybe Font) (Maybe FontSize)
            | VFigure       FilePath PageWidth (Maybe Caption)
            | VTable        [[[VText]]] TableColumns
            | VList         [[VText]] (Maybe ListStyle)
            | VParagraph    [VText] (Maybe Font) (Maybe FontSize) (Maybe Justification)
            | VNewpage
            | VHLine
            deriving (Show, Eq, Ord)


-- Options for configuration commands.
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
    , cfgParSize            :: Maybe FontSize
    , cfgTitleSize          :: Maybe FontSize
    , cfgSectionSize        :: Maybe FontSize
    , cfgSubsectionSize     :: Maybe FontSize
    , cfgJustification      :: Maybe Justification
    , cfgListStyle          :: Maybe ListStyle
    } deriving (Show, Eq, Ord)

-- Empty config for easy instantiation.
emptyVConfig :: VConfig
emptyVConfig = VConfig
    { cfgPageSize           = Nothing
    , cfgPageNumbering      = Nothing
    , cfgTitleSpacing       = Nothing
    , cfgParagraphSpacing   = Nothing
    , cfgListSpacing        = Nothing
    , cfgTableSpacing       = Nothing
    , cfgFigureSpacing      = Nothing
    , cfgSpacingGlue        = Nothing
    , cfgTextGlue           = Nothing
    , cfgParIndent          = Nothing
    , cfgFont               = Nothing
    , cfgParSize            = Nothing
    , cfgTitleSize          = Nothing
    , cfgSectionSize        = Nothing
    , cfgSubsectionSize     = Nothing
    , cfgJustification      = Nothing
    , cfgListStyle          = Nothing
    }

-- Default configuration values for commands, determined by hand, to make the document aesthetically pleasant.
defaultVConfig :: VConfig
defaultVConfig = VConfig
    { cfgPageSize           = Just $ SizeA4
    , cfgPageNumbering      = Just $ NumberingArabic
    , cfgTitleSpacing       = Just $ Spacing (Pt 5) (Pt 5)
    , cfgParagraphSpacing   = Just $ Spacing (Pt 3) (Pt 3)
    , cfgListSpacing        = Just $ Spacing (Pt 5) (Pt 5)
    , cfgTableSpacing       = Just $ Spacing (Pt 4) (Pt 4)
    , cfgFigureSpacing      = Just $ Spacing (Pt 4) (Pt 4)
    , cfgSpacingGlue        = Just $ Glue (Pt 2) (Pt 2)
    , cfgTextGlue           = Just $ Glue (Pt 2) (Pt 2)
    , cfgParIndent          = Just $ Pt 6
    , cfgFont               = Just $ Helvetica
    , cfgParSize            = Just $ FontSize (Pt 12)
    , cfgTitleSize          = Just $ FontSize (Pt 18)
    , cfgSectionSize        = Just $ FontSize (Pt 16)
    , cfgSubsectionSize     = Just $ FontSize (Pt 14)
    , cfgJustification      = Just $ JustifyLeft
    , cfgListStyle          = Just $ ListBullet
    }

-- General data types, meant for reusability, to avoid repetition.
data PageSize   = SizeA4
                | SizeA3
                | SizeLegal
                | SizeCustom Pt Pt   -- width, height
                deriving (Show, Eq, Ord)

data PageNumbering = NumberingArabic | NumberingRoman | NumberingNone
    deriving (Show, Eq, Ord)

data Spacing = Spacing Pt Pt
    deriving (Show, Eq, Ord)

data Glue = Glue Pt Pt
    deriving (Show, Eq, Ord)

data Font = Helvetica | Courier | Times
    deriving (Show, Eq, Ord)

data TextStyle = Normal | Bold | Italic | Emphasised | Verbatim | Quoted
    deriving (Show, Eq, Ord)

data Justification = JustifyLeft | JustifyRight | JustifyCenter | JustifyFull
    deriving (Show, Eq, Ord)

-- Text definition.
data VText = VText
    { text  :: Text
    , style :: TextStyle
    } deriving (Show, Eq, Ord)

-- Command specific types.
newtype TableColumns = TableColumns Int
    deriving (Show, Eq, Ord)

data ListStyle = ListBullet | ListSquare | ListArrow | ListNumber
    deriving (Show, Eq, Ord)

newtype Caption = Caption Text
    deriving (Show, Eq, Ord)