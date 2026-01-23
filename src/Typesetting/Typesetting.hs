{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Typesetting.Typesetting (typesetDocument) where

import Datatypes.ValidatedTokens
import Datatypes.Located
import Datatypes.Resources
import Typesetting.Helpers
import Resources (getFont)

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as M
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromJust)

import GHC.Float (int2Double)

import Graphics.PDF
import Graphics.PDF.Typesetting

import qualified Debug.Trace as DT


-- Stores the current state of the sections.
data SectionState = SectionState
    { currentSection    :: Int
    , currentSubsection :: Int
    }

data RenderState = RenderState
    { currentY      :: Double                   -- Vertical cursor position.
    , currentPage   :: PDFReference PDFPage     -- The page currently being typeset.
    , config        :: VConfig                  -- Configuration.
    , loadedFonts   :: LoadedFonts              -- Loaded fonts.
    , resources     :: ResourceMap              -- All loaded resources, with their associated path.
    , pageX         :: Double                   -- Page dimensions are needed for state resetting, the PDF monad doesn't provide access.
    , pageY         :: Double
    , remainingText :: [VText]                  -- Any text that cannot fit on the current page gets stored here.
    , sectionState  :: SectionState
    }

-- Provides access to all rendering information, additionally it handles the current state of the PDF monad.
type Typesetter a = StateT RenderState PDF a
type HPDFParagraph = TM StandardParagraphStyle StandardStyle ()

-- Helper function to run the typesetter.
runTypesetter :: RenderState -> Typesetter a -> PDF a
runTypesetter initial action = evalStateT action initial

typesetDocument :: ValidatedDocument -> ResourceMap -> LoadedFonts -> FilePath -> IO ()
typesetDocument (ValidatedDocument cfg meta cnt) res fonts outPath = do
    let pageRect = pageSizeToRect (fromJust $ cfgPageSize cfg)
    let (PDFRect _ _ px py) = pageRect
    let startY = py - fromPt (fromJust $ cfgVertMargin cfg)
    let sectionState = SectionState 0 0

    runPdf outPath (generateDocInfo meta) pageRect $ do
        initialPage <- addPage Nothing

        -- Initialize render state.
        let initialState = RenderState {
          currentY = startY
        , currentPage = initialPage -- makeNewPage cannot be used, since there needs to be a reference when initializing the state.
        , config = cfg
        , loadedFonts = fonts
        , resources = res
        , pageX = px
        , pageY = py
        , remainingText = []
        , sectionState = sectionState
        }


        evalStateT (typesetElements cnt) initialState

------------------------
-- COMPLETE COMMANDS; NO LONGER DONE IN Main.hs!!!!!!!!!!
------------------------
------------------------
-- BOOKMARKS NOT WORKING; THEY TAKE YOU TO THE START OF THE FILE.
------------------------
typesetElements :: [Located VComm] -> Typesetter ()
typesetElements elements = do
    forM_ elements $ \(Located _ comm) -> do
        case comm of
            VParagraph text mFont mSize mJust -> 
                typesetParagraph text mFont mSize mJust

            VSection text font size -> do
                typesetHeader text font size True

            VSubsection text font size -> do
                typesetHeader text font size False

            VFigure path width caption -> do
                typesetFigure path width caption

            VNewpage ->
                makeNewPage

            VHLine ->
                drawHLine

        cs <- checkSpace
        if cs then makeNewPage else return ()


------------------------
-- AUXILIARY FUNCTIONS
------------------------
-- Checks if the cursor is below the bottom margin, if so returns "True".
checkSpace :: Typesetter (Bool)
checkSpace = do
    RenderState{..} <- get

    let marginY = fromPt (fromJust $ cfgVertMargin config)

    return $ currentY < marginY

-- Creates a new page.
makeNewPage :: Typesetter ()
makeNewPage = do
    RenderState{..} <- get
    -- Create new page of standard size in PDF Monad.
    newPage <- lift $ addPage Nothing
    
    -- Reset state, since we are in a new page.
    modify $ \s -> s { 
        currentPage = newPage, 
        currentY = pageY - fromPt (fromJust $ cfgVertMargin config)
    }

drawHLine :: Typesetter ()
drawHLine = do
    RenderState{..} <- get

    -- Get horizontal start and end position.
    let lineSpan = 0.9
    let xStart = (1 - lineSpan) * pageX
    let xEnd = lineSpan * pageX

    lift $ drawWithPage currentPage $ do
        strokeColor black
        stroke $ Line xStart currentY xEnd currentY

-- Typesets the given VText or paragraph, with the given options. Note that justification and indentation only have an effect in VText mode.
typesetContent :: (Either [VText] HPDFParagraph) -> Font -> Datatypes.ValidatedTokens.FontSize -> Datatypes.ValidatedTokens.Justification -> Double -> Double -> Double -> Typesetter ()
typesetContent content font size just indent beforeSpace afterSpace = do
    RenderState{..} <- get

    -- Maps VText tokens to PDF typesetting instructions. Defined with in function so we can use "let" defined variables.
    let textGenerator vText = do
            let cSize = convertFontSize size
            -- Get justification and convert to PDF data.
            let cJust = case just of
                    JustifyLeft   -> LeftJustification
                    JustifyRight  -> RightJustification
                    JustifyCenter -> Centered
                    JustifyFull   -> FullJustification

            setJustification cJust
            paragraph $ do
                kern indent
                -- Convert all text into HPDF paragraphs.
                forM_ vText $ \(VText txtContent style) -> do
                    -- Apply styling to segment.
                    let styledFont = getFont loadedFonts font style
                    setStyle (Font (PDFFont styledFont cSize) black black)
                    txt txtContent

    -- The content is converted into a list of renderable boxes. The style 'NormalParagraph' is used as the baseline context.
    let cnt = case content of
            Left text -> textGenerator text
            Right para -> para
    let boxes = getBoxes NormalParagraph (Font (PDFFont (getFont loadedFonts font Normal) (convertFontSize size)) black black) cnt

    fillBoxLoop boxes
  where
    -- Fills containers page by page.
    fillBoxLoop [] = return () -- Done
    fillBoxLoop boxes = do
        RenderState{..} <- get
            
        -- Calculate available height from currentY down to the bottom margin and make a container with that size, taking into account the
        -- given spacing.
        let marginX = fromPt (fromJust $ cfgHozMargin config)
        let marginY = fromPt (fromJust $ cfgVertMargin config)
        let width   = pageX - (marginX * 2) 
        let height  = currentY - marginY 

        -- The mkContainer function defines a container based on it's top left point.
        let container = mkContainer marginX (currentY - beforeSpace) width height 0
        let verState = defaultVerState NormalParagraph

        -- Fill container.
        let (drawAction, usedContainer, remainingBoxes) = fillContainer verState container boxes

        -- Execute the resulting draw action on the current page.
        lift $ drawWithPage currentPage drawAction

        lift $ drawWithPage currentPage $ do
            strokeColor blue
            stroke $ containerContentRectangle usedContainer

        -- Update State, containerContentRectangle tells us the bounds of the text explicitly placed.
        let (Rectangle (_ :+ newBottomY) _) = containerContentRectangle usedContainer
            
        if null remainingBoxes
            then do
                -- Text fits on page, update cursor to the bottom of the placed text plus paragraph spacing.
                modify $ \s -> s { currentY = newBottomY - afterSpace }
            else do
                -- Overflow, force new page and render remaining boxes.
                makeNewPage
                fillBoxLoop remainingBoxes

-- Typesets the given paragraph.
typesetParagraph :: [VText] -> Maybe Font -> Maybe Datatypes.ValidatedTokens.FontSize -> Maybe Datatypes.ValidatedTokens.Justification -> Typesetter ()
typesetParagraph vText mFont mSize mJust = do
    cfg <- gets config

    let font = fromJust $ mFont <|> cfgFont cfg
    let size = fromJust $ mSize <|> cfgParSize cfg
    let just = fromJust $ mJust <|> cfgJustification cfg
    let (Pt indent) = fromJust $ cfgParIndent cfg
    let (Spacing (Pt beforeSpace) (Pt afterSpace)) = fromJust $ cfgParagraphSpacing cfg

    typesetContent (Left vText) font size just indent beforeSpace afterSpace

-- Typesets both sections and subsections. 
typesetHeader :: [VText] -> Maybe Font -> Maybe Datatypes.ValidatedTokens.FontSize -> Bool -> Typesetter ()
typesetHeader vText mFont mSize isSection = do
    RenderState{..} <- get
    let SectionState{..} = sectionState

    let font = fromJust $ mFont <|> cfgFont config
    let sectionSize = fromJust $ mSize <|> cfgSectionSize config
    let subsectionSize = fromJust $ mSize <|> cfgSubsectionSize config
    let (Spacing (Pt beforeSpace) (Pt afterSpace)) = fromJust $ cfgSectionSpacing config
    let numberingEnabled = fromJust $ cfgSectionNumbering config

    let (label, newSecState, finalSize) = if isSection
        then
            let nextSec = currentSection + 1
                lbl = show nextSec
            in (lbl, SectionState nextSec 0, sectionSize)
        else
            let nextSub = currentSubsection + 1
                lbl = show currentSection ++ "." ++ show nextSub
            in (lbl, SectionState currentSection nextSub, subsectionSize)

    -- Update section state.
    modify $ \s -> s { sectionState = newSecState }

    let numbering = if numberingEnabled then T.pack $ label ++ ". " else ""
    let fullText = if numberingEnabled then (VText numbering Bold):vText else vText

    -- Create bookmark in PDF.
    lift $ newSectionWithPage (numbering <> mergeVText vText) Nothing Nothing currentPage (return ())

    -- Typeset section text.
    typesetContent (Left fullText) font finalSize JustifyLeft 0 beforeSpace afterSpace

typesetFigure :: FilePath -> PageWidth -> Maybe Caption -> Typesetter ()
typesetFigure path (PageWidth givenWidth) mCap = do
    RenderState{..} <- get

    -- Validation guarantees that all paths have a valid resource.
    let (FileInfo bs w h) = fromJust $ M.lookup path resources
    -- Get figure spacing.
    let (Spacing (Pt beforeSpace) (Pt afterSpace)) = fromJust $ cfgFigureSpacing config

    -- This function expects raw pixel values; Or so I think, from reading the source code.
    img <- lift $ createPDFRawImageFromByteString w h True NoFilter bs

    -- Calculate image scale and position.
    let imageRatio = (int2Double w) / (int2Double h)
    let figureWidth = pageX * givenWidth
    let figureHeight = figureWidth / imageRatio

    let figureX = (pageX - figureWidth) / 2
    let figureY = currentY - figureHeight - beforeSpace

    let figureScaleX = (1 / (int2Double w)) * figureWidth
    let figureScaleY = (1 / (int2Double h)) * figureHeight

    let drawFigure =
            lift (drawWithPage currentPage $ do -- Indented clearly past 'drawFigure'
                withNewContext $ do
                    applyMatrix (translate (figureX :+ figureY))
                    applyMatrix (scale figureScaleX figureScaleY)
                    drawXObject img)

    -- Update cursor by the height of the figure, spacing after it is added later..
    modify $ \s -> s { currentY = figureY }
    cs <- checkSpace

    -- Check if there's enough space to fit the figure, if there's not create a new page and typeset it there.
    if cs
        then do
            makeNewPage
            typesetFigure path (PageWidth givenWidth) mCap
        else -- Draw image and potentially caption.
            case mCap of
                Just (Caption caption) -> do
                    mkRes <- makeFigureCaption caption afterSpace
                    -- Check if there's enough space to fit the figure and caption, if there's not create a new page and typeset it there.
                    if mkRes then drawFigure else typesetFigure path (PageWidth givenWidth) (Just $ Caption caption)

                -- No caption, draw figure and spacing directly..
                Nothing -> do
                    modify $ \s -> s { currentY = currentY - afterSpace }
                    drawFigure

    return ()
  where
    -- The signature is required, otherwise Haskell complains about inferring a concrete type.
    makeFigureCaption :: Text -> Double -> Typesetter Bool
    makeFigureCaption caption afterSpace = do
        RenderState{..} <- get

        let font = fromJust $ cfgFont config

        -- Generate and format the text, then put it in a box.
        let boxes = getBoxes NormalParagraph (Font (PDFFont (getFont loadedFonts font Normal) 12) black black) $ do
                setJustification Centered
                paragraph $ do 
                    txt caption
        
        -- Space between figure and caption.
        let figureCaptionSpacing = 10
        -- Calculate available height from currentY down to the bottom margin and make a container with that size, taking into account the
        -- given spacing.
        let marginX = fromPt (fromJust $ cfgHozMargin config)
        let marginY = fromPt (fromJust $ cfgVertMargin config)
        let width   = pageX - (marginX * 2) 
        let height  = currentY - marginY

        -- The mkContainer function defines a container based on it's top left point.
        let container = mkContainer marginX (currentY - figureCaptionSpacing) width height 0
        let verState = defaultVerState NormalParagraph

        -- Fill container.
        let (drawAction, usedContainer, remainingBoxes) = fillContainer verState container boxes

        -- Update State, containerContentRectangle tells us the bounds of the text explicitly placed.
        let (Rectangle (_ :+ newBottomY) _) = containerContentRectangle usedContainer
        
        if null remainingBoxes
            then do
                -- Text fits on page, add caption; Then update cursor to the bottom of the placed text plus paragraph spacing.
                lift $ drawWithPage currentPage drawAction
                modify $ \s -> s { currentY = newBottomY - afterSpace }

                return True
            else
                return False

typesetList :: [[VText]] -> Font -> Datatypes.ValidatedTokens.FontSize -> Datatypes.ValidatedTokens.Justification -> Typesetter ()
typesetList vText font size just = do
    RenderState{..} <- get

    return ()