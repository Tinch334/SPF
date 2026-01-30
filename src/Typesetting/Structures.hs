module Typesetting.Structures
    ( RCLayout(..)
    , RCStyles(..)
    , RCSizes(..)
    , RCSpacing(..)
    , RCToggle(..)
    , RenderConfig(..)
    , RenderEnv(..)
    , RenderState(..)
    , DocumentCounters(..)
    , Typesetter
    , HPDFParagraph
    ) where

import qualified Datatypes.ValidatedTokens as VT
import Datatypes.Resources

import Control.Monad.Reader
import Control.Monad.State.Lazy

import Graphics.PDF


------------------------
-- TYPES AND STATE
------------------------
-- Replaces VConfig for internal use, to avoid having "fromJust" everywhere.
data RCLayout = RCLayout
    { rcPageSize      :: VT.PageSize
    , rcNumbering     :: VT.PageNumbering
    , rcMarginVert    :: VT.Pt
    , rcMarginHoz     :: VT.Pt
    }

data RCStyles = RCStyles
    { rcFont          :: VT.Font
    , rcJustification :: VT.Justification
    , rcListType      :: VT.ListStyle
    }

data RCSizes = RCSizes
    { rcParSize       :: VT.FontSize
    , rcTitleSize     :: VT.FontSize
    , rcSectionSize   :: VT.FontSize
    , rcSubsectionSize:: VT.FontSize
    , rcVerbatimSize  :: VT.FontSize
    }

data RCSpacing = RCSpacing
    { rcSectionSp     :: VT.Spacing
    , rcParagraphSp   :: VT.Spacing
    , rcListSp        :: VT.Spacing
    , rcTableSp       :: VT.Spacing
    , rcFigureSp      :: VT.Spacing
    , rcVerbatimSp    :: VT.Spacing
    , rcParIndent     :: VT.Pt
    }

data RCToggle = RCToggle
    { rcSectionNumbering  :: Bool
    , rcFigureNumbering   :: Bool
    , rcVerbatimNumbering :: Bool
    }

data RenderConfig = RenderConfig
    { layout    :: RCLayout
    , styles    :: RCStyles
    , sizes     :: RCSizes
    , spacing   :: RCSpacing
    , toggles   :: RCToggle 
    }

-- Read only environment.
data RenderEnv = RenderEnv
    { envConfig     :: RenderConfig
    , envFonts      :: LoadedFonts
    , envResources  :: ResourceMap
    , envPageWidth  :: Double
    , envPageHeight :: Double
    , envVertMargin :: (Double, Double) -- Top, Bottom
    , envDebug      :: Bool
    }

-- Read/Write environment, stores the state of the document whilst typesetting.
data RenderState = RenderState
    { rsCurrentY        :: Double
    , rsCurrentPage     :: PDFReference PDFPage
    , rsCounters        :: DocumentCounters
    }

data DocumentCounters = DocumentCounters
    { dcPage        :: Int
    , dcSection     :: Int
    , dcSubsection  :: Int
    , dcFigure      :: Int
    }

-- Typesetter monad stack, provides access to current document state as well as the underlying PDF.
type Typesetter a = ReaderT RenderEnv (StateT RenderState PDF) a
type HPDFParagraph = TM StandardParagraphStyle StandardStyle ()