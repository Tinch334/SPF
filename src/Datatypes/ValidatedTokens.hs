module Datatypes.ValidatedTokens where

import Data.Text (Text)

-- Options for configuration.
data VPageSize = SizeA4 | SizeA3 | SizeLegal | SizeCustom Double Double -- width pt, height pt
data VPageNumbering = NumberingArabic | NumberingRoman | NumberingNone
data VTitleSpacing = TitleSpacing Double Double -- before pt, after pt
data VParagraphSpacing = ParagraphSpacing Double Double -- before pt, after pt
data VListSpacing = ListSpacing Double Double -- before pt, after pt
data VTableSpacing = TableSpacing Double Double -- before pt, after pt
data VFigureSpacing = FigureSpacing Double Double -- before pt, after pt
data VSpacingGlue = SpacingGlue Double Double -- stretchability pt, shrinkability pt
data VTextGlue = TextGlue Double Double -- stretchability pt, shrinkability pt
data VParIndent = ParIndent Double -- indent pt
data VFont = FontHelvetica | FontCourier | FontTimes
data VParSize = ParSize Double -- paragraph size
data VTitleSize = TitleSize Double -- title size
data VJustification = JustifyLeft | JustifyRight | JustifyCentered | JustifyFull

data VConfigOption  = VPageSize
                    | VPageNumbering
                    | VTitleSpacing
                    | VParagraphSpacing
                    | VListSpacing
                    | VTableSpacing
                    | VFigureSpacing
                    | VSpacingGlue
                    | VTextGlue
                    | VParIndent
                    | VFont
                    | VParSize
                    | VTitleSize
                    | VJustification

-- Options for commands.
data FontName = FontName Text
data FontSize = FontSize Double
data TextJustify = TextJustifyLeft | TextJustifyRight | TextJustifyCentered | TextJustifyFull
data TextIndent = TextIndent Double
data TableColumns = TableColumns Int
data ListStyle = ListBullet | ListSquare | ListArrow | ListNumber
data FigureWidth = FigureWidth Double
data FigureCaption = FigureCaption Text


type VLang = [VComm]
data VComm  = VConfig       VConfigOption
            | VTitle        [VText] (Maybe FontName) (Maybe FontSize)
            | VAuthor       [VText] (Maybe FontName) (Maybe FontSize)
            | VDate         [VText] (Maybe FontName) (Maybe FontSize)
            | PSection      [VText] 
            | PSubsection   [VText]
            | PFigure       FilePath (Maybe FigureWidth) (Maybe FigureCaption)
            | PTable        [[[VText]]] TableColumns
            | PList         [[VText]] (Maybe ListStyle)
            | PParagraph    [VText] (Maybe FontName) (Maybe FontSize) (Maybe TextJustify)
            | PNewpage
            | PHLine

-- The types are the same as those obtained in the parser; The new definition is simply for consistency.
data VText  = VNormal       Text
            | VBold         Text
            | VItalic       Text
            | VEmphasised   Text
            | VVerbatim     Text
            | VQuoted       Text