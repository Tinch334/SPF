module Datatypes.ValidatedTokens where

import Data.Text (Text)


type VLang = [VComm]
-- The maybe in most of the options indicates that they are optional. If parsed they are added, with Just, otherwise Nothing is left.
-- The Nothing's are later be replaced with the default values for those arguments.
data VComm  = VConfig VConfigOpt
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


-- Options for configuration.
data VPageSizeOpt = SizeA4 | SizeA3 | SizeLegal | SizeCustom Double Double -- width pt, height pt
data VPageNumberingOpt = NumberingArabic | NumberingRoman | NumberingNone
data VTitleSpacingOpt = TitleSpacing Double Double -- before pt, after pt
data VParagraphSpacingOpt = ParagraphSpacing Double Double -- before pt, after pt
data VListSpacingOpt = ListSpacing Double Double -- before pt, after pt
data VTableSpacingOpt = TableSpacing Double Double -- before pt, after pt
data VFigureSpacingOpt = FigureSpacing Double Double -- before pt, after pt
data VSpacingGlueOpt = SpacingGlue Double Double -- stretchability pt, shrinkability pt
data VTextGlueOpt = TextGlue Double Double -- stretchability pt, shrinkability pt
data VParIndentOpt = ParIndent Double -- indent pt
data VFontOpt = FontHelvetica | FontCourier | FontTimes
data VParSizeOpt = ParSize Double -- paragraph size
data VTitleSizeOpt = TitleSize Double -- title size
data VJustificationOpt = JustifyLeft | JustifyRight | JustifyCentered | JustifyFull

data VConfigOpt = VPageSize VPageSizeOpt
                | VPageNumbering VPageNumberingOpt
                | VTitleSpacing VTitleSpacingOpt
                | VParagraphSpacing VParagraphSpacingOpt
                | VListSpacing VListSpacingOpt
                | VTableSpacing VTableSpacingOpt
                | VFigureSpacing VFigureSpacingOpt
                | VSpacingGlue VSpacingGlueOpt
                | VTextGlue VTextGlueOpt
                | VParIndent VParIndentOpt
                | VFont VFontOpt
                | VParSize VParSizeOpt
                | VTitleSize VTitleSizeOpt
                | VJustification VJustificationOpt

-- Options for commands.
data FontName = FontName Text
data FontSize = FontSize Double
data TextJustify = TextJustifyLeft | TextJustifyRight | TextJustifyCentered | TextJustifyFull
data TextIndent = TextIndent Double
data TableColumns = TableColumns Int
data ListStyle = ListBullet | ListSquare | ListArrow | ListNumber
data FigureWidth = FigureWidth Double
data FigureCaption = FigureCaption Text

-- The types are the same as those obtained in the parser; The new definition is simply for consistency.
data VText  = VNormal       Text
            | VBold         Text
            | VItalic       Text
            | VEmphasised   Text
            | VVerbatim     Text
            | VQuoted       Text