module Datatypes.ValidatedTokens where

import Data.Text (Text)


type VLang = [VComm]
-- The maybe in the options indicates that they are optional. If parsed they are added, with Just, otherwise Nothing is used.
-- The Nothing's are then replaced with the default values for those arguments.
data VComm  = VConfig       VConfigOpt
            | VTitle        [VText] (Maybe FontName) (Maybe FontSize)
            | VAuthor       [VText] (Maybe FontName) (Maybe FontSize)
            | VDate         [VText] (Maybe FontName) (Maybe FontSize)
            | VSection      [VText] 
            | VSubsection   [VText]
            | VFigure       FilePath (Maybe FigureWidth) (Maybe FigureCaption)
            | VTable        [[[VText]]] TableColumns
            | VList         [[VText]] (Maybe ListStyle)
            | VParagraph    [VText] (Maybe FontName) (Maybe FontSize) (Maybe TextJustify)
            | VNewpage
            | VHLine
            deriving (Show, Eq, Ord)


-- Options for configuration.
data VPageSizeOpt = SizeA4 | SizeA3 | SizeLegal | SizeCustom Double Double deriving (Show, Eq, Ord) -- width pt, height pt
data VPageNumberingOpt = NumberingArabic | NumberingRoman | NumberingNone deriving (Show, Eq, Ord)
data VTitleSpacingOpt = TitleSpacing Double Double deriving (Show, Eq, Ord) -- before pt, after pt
data VParagraphSpacingOpt = ParagraphSpacing Double Double deriving (Show, Eq, Ord) -- before pt, after pt
data VListSpacingOpt = ListSpacing Double Double deriving (Show, Eq, Ord) -- before pt, after pt
data VTableSpacingOpt = TableSpacing Double Double deriving (Show, Eq, Ord) -- before pt, after pt
data VFigureSpacingOpt = FigureSpacing Double Double deriving (Show, Eq, Ord)  -- before pt, after pt
data VSpacingGlueOpt = SpacingGlue Double Double deriving (Show, Eq, Ord) -- stretchability pt, shrinkability pt
data VTextGlueOpt = TextGlue Double Double deriving (Show, Eq, Ord) -- stretchability pt, shrinkability pt
data VParIndentOpt = ParIndent Double deriving (Show, Eq, Ord) -- indent pt
data VFontOpt = FontHelvetica | FontCourier | FontTimes deriving (Show, Eq, Ord)
data VParSizeOpt = ParSize Double deriving (Show, Eq, Ord) -- paragraph size
data VTitleSizeOpt = TitleSize Double deriving (Show, Eq, Ord) -- title size
data VJustificationOpt = JustifyLeft | JustifyRight | JustifyCentred | JustifyFull deriving (Show, Eq, Ord)

data VConfigOpt = VPageSize             VPageSizeOpt
                | VPageNumbering        VPageNumberingOpt
                | VTitleSpacing         VTitleSpacingOpt
                | VParagraphSpacing     VParagraphSpacingOpt
                | VListSpacing          VListSpacingOpt
                | VTableSpacing         VTableSpacingOpt
                | VFigureSpacing        VFigureSpacingOpt
                | VSpacingGlue          VSpacingGlueOpt
                | VTextGlue             VTextGlueOpt
                | VParIndent            VParIndentOpt
                | VFont                 VFontOpt
                | VParSize              VParSizeOpt
                | VTitleSize            VTitleSizeOpt
                | VJustification        VJustificationOpt
                deriving (Show, Eq, Ord)

-- Options for commands.
data FontName = TextFontHelvetica | TextFontCourier | TextFontTimes deriving (Show, Eq, Ord)
data FontSize = FontSize Double deriving (Show, Eq, Ord)
data TextJustify = TextJustifyLeft | TextJustifyRight | TextJustifyCentred | TextJustifyFull deriving (Show, Eq, Ord)
data TextIndent = TextIndent Double deriving (Show, Eq, Ord)
data TableColumns = TableColumns Int deriving (Show, Eq, Ord)
data ListStyle = ListBullet | ListSquare | ListArrow | ListNumber deriving (Show, Eq, Ord)
data FigureWidth = FigureWidth Double deriving (Show, Eq, Ord)
data FigureCaption = FigureCaption Text deriving (Show, Eq, Ord)

-- The types are the same as those obtained in the parser; The new definition is simply for consistency.
data VText  = VNormal       Text
            | VBold         Text
            | VItalic       Text
            | VEmphasised   Text
            | VVerbatim     Text
            | VQuoted       Text
            deriving (Show, Eq, Ord)