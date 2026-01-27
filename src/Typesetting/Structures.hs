module Typesetting.Structures 
    ( RenderConfig(..)
    , RenderEnv(..)
    , RenderState(..)
    , DocumentCounters(..)
    , Typesetter
    , HPDFParagraph
    ) where

import Datatypes.ValidatedTokens
import Datatypes.Resources

import Control.Monad.Reader
import Control.Monad.State.Lazy

import Graphics.PDF
import Graphics.PDF.Typesetting


------------------------
-- TYPES AND STATE
------------------------
-- Replaces VConfig for internal use, to avoid having "fromJust" everywhere.
data RenderConfig = RenderConfig
    { rcPageSize            :: PageSize
    , rcPageNumbering       :: PageNumbering
    , rcSectionSpacing      :: Spacing
    , rcParagraphSpacing    :: Spacing
    , rcListSpacing         :: Spacing
    , rcTableSpacing        :: Spacing
    , rcFigureSpacing       :: Spacing
    , rcSpacingGlue         :: Glue
    , rcTextGlue            :: Glue
    , rcParIndent           :: Pt
    , rcFont                :: Font
    , rcTitleSize           :: Datatypes.ValidatedTokens.FontSize
    , rcParSize             :: Datatypes.ValidatedTokens.FontSize
    , rcSectionSize         :: Datatypes.ValidatedTokens.FontSize
    , rcSubsectionSize      :: Datatypes.ValidatedTokens.FontSize
    , rcJustification       :: Datatypes.ValidatedTokens.Justification
    , rcListStyle           :: ListStyle
    , rcVertMargin          :: Pt
    , rcHozMargin           :: Pt
    , rcSectionNumbering    :: Bool
    , rcFigureNumbering     :: Bool
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