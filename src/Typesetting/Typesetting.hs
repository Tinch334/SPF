{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

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
data DocumentState = DocumentState
    { pageNumber        :: Int
    , currentSection    :: Int
    , currentSubsection :: Int
    , currentFigure     :: Int
    }

data RenderState = RenderState
    { currentY      :: Double                   -- Vertical cursor position.
    , currentPage   :: PDFReference PDFPage     -- The page currently being typeset.
    , config        :: VConfig                  -- Configuration.
    , loadedFonts   :: LoadedFonts              -- Loaded fonts.
    , resources     :: ResourceMap              -- All loaded resources, with their associated path.
    , pageX         :: Double                   -- Page dimensions are needed for state resetting, the PDF monad doesn't provide access.
    , pageY         :: Double
    , vertMargin    :: (Double, Double)         -- The value of the vertical margin can be different, depending on page numbers and titles.
    , documentState  :: DocumentState
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
    let vm = calculateVertMargin cfg
    let documentState = DocumentState 0 0 0 0

    runPdf outPath (generateDocInfo meta) pageRect $ do

        -- Initialize render state.
        let initialState = RenderState {
          currentY = startY
        , currentPage = error "INTERNAL: Attempted to access currentPage before the first page was created."
        , config = cfg
        , loadedFonts = fonts
        , resources = res
        , vertMargin = vm
        , pageX = px
        , pageY = py
        , documentState = documentState
        }

        evalStateT 
            (do
                makeNewPage -- Create first page.
                typesetElements cnt)
            initialState


-- Calculates the vertical margin based on what kind of page numbering was chosen.
calculateVertMargin :: VConfig -> (Double, Double)
calculateVertMargin config = do

    let (Pt margin) = fromJust $ cfgVertMargin config
    let numbering = fromJust $ cfgPageNumbering config
    let cSize = convertFontSize (fromJust $ cfgParSize config)

    -- Calculate bottom margin based on numbering.
    let bottom = margin + 
            case numbering of
                NumberingNone -> 0.0
                _ -> 16

    (margin, bottom)

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

            VTable table columns ->
                typesetTable table columns

            VList list style -> do
                typesetList list style

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

    let (_, marginY) = vertMargin

    return $ currentY < marginY

-- Creates a new page.
makeNewPage :: Typesetter ()
makeNewPage = do
    RenderState{..} <- get
    -- Create new page of standard size in PDF Monad.
    newPage <- lift $ addPage Nothing

    let (marginY, _) = vertMargin
    let currentPage = pageNumber documentState
    
    -- Reset state, since we are in a new page.
    modify $ \s -> s { 
        currentPage = newPage, 
        currentY = pageY - marginY,
        documentState = documentState { pageNumber = currentPage + 1 }
    }

    -- Typeset line number onto new page.
    typesetLineNumber (fromJust $ cfgPageNumbering config)

  where
    typesetLineNumber NumberingNone = return ()
    typesetLineNumber numbering = do
        RenderState{..} <- get

        let font = fromJust $ cfgFont config
        let (Pt hMargin) = fromJust $ cfgHozMargin config
        let (_, marginY) = vertMargin
        -- Get the page number centred in the bottom area of the margin.
        let rect = Rectangle (hMargin :+ (marginY * 0.2)) ((pageX - hMargin) :+ (marginY * 0.6))

        let pnStr = case numbering of
                NumberingArabic -> show $ pageNumber documentState
                NumberingRoman  -> toRoman $ pageNumber documentState

        -- Whilst this function doesn't handle overflows from the render area well they should never happen; Since a number would have to be
        -- as wide as the page for that to be a problem.
        lift $ drawWithPage currentPage $ do
            displayFormattedText rect NormalParagraph (Font (PDFFont (getFont loadedFonts font Normal) 12) black black) $ do
                setJustification Centered
                paragraph $ do
                    txt $ T.pack pnStr

showRect (Rectangle (bx :+ by) (tx :+ ty)) = show bx ++ " - " ++ show by ++ " - " ++ show tx ++ " - " ++ show ty

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
        let (_, marginY) = vertMargin
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
    let DocumentState{..} = documentState

    let font = fromJust $ mFont <|> cfgFont config
    let sectionSize = fromJust $ mSize <|> cfgSectionSize config
    let subsectionSize = fromJust $ mSize <|> cfgSubsectionSize config
    let (Spacing (Pt beforeSpace) (Pt afterSpace)) = fromJust $ cfgSectionSpacing config
    let numberingEnabled = fromJust $ cfgSectionNumbering config

    let (label, newSecState, finalSize) = if isSection
        then
            let nextSec = currentSection + 1
                lbl = show nextSec
            in (lbl, documentState { currentSection = nextSec, currentSubsection = 0}, sectionSize)
        else
            let nextSub = currentSubsection + 1
                lbl = show currentSection ++ "." ++ show nextSub
            in (lbl, documentState { currentSection = currentSection, currentSubsection = nextSub}, subsectionSize)

    -- Update section state.
    modify $ \s -> s { documentState = newSecState }

    let numbering = if numberingEnabled then T.pack $ label ++ ". " else ""
    let fullText = if numberingEnabled then (VText numbering Bold):vText else vText

    -- Create bookmark in PDF.
    lift $ newSectionWithPage (numbering <> mergeVText vText) Nothing Nothing currentPage (return ())

    -- Typeset section text.
    typesetContent (Left fullText) font finalSize JustifyLeft 0 beforeSpace afterSpace

-- Typesets the given figure, with it's caption if present.
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
        let (_, marginY) = vertMargin
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

-- Typesets the given list.
typesetList :: [[VText]] -> Maybe ListStyle -> Typesetter ()
typesetList items mStyle = do
    RenderState{..} <- get

    let size = fromJust $ cfgParSize config
    let font = fromJust $ cfgFont config
    let (Spacing (Pt beforeSpace) (Pt afterSpace)) = fromJust $ cfgListSpacing config

    -- Get list style and convert it to a character.
    let styleToken = fromJust $ mStyle <|> cfgListStyle config
    let styleZap = setStyle (Font (PDFFont (zapf loadedFonts) (convertAdjustFontSize size 0.6)) black black)

    -- The style is a function that takes the item's position in the list and returns the element to typeset.
    let styleFunction = case styleToken of
            ListBullet -> \_ -> do
                styleZap
                txt $ "●"
            ListSquare -> \_ -> do
                styleZap
                txt $ "■"
            ListArrow -> \_ -> do
                styleZap
                txt $ "➤"
            ListNumber -> \n -> do
                setStyle (Font (PDFFont (getFont loadedFonts font Normal) (convertFontSize size)) black black)
                txt $ T.pack $ show n ++ "."

    let list = do
            setJustification LeftJustification
            paragraph $ do
                -- Typeset each line.
                forM_ (zip items [1..(length items)]) $ \(line, i) -> do
                    kern 10
                    styleFunction i
                    kern 5
                    -- Convert all text into HPDF paragraphs.
                    forM_ line $ \(VText txtContent style) -> do
                        -- Apply styling to segment.
                        let styledFont = getFont loadedFonts font style
                        setStyle (Font (PDFFont styledFont (convertFontSize size)) black black)
                        txt txtContent
                        forceNewLine

    typesetContent (Right list) font size JustifyLeft 0 beforeSpace afterSpace

    return ()

-- Typesets the given table.
typesetTable :: [[[VText]]] -> TableColumns -> Typesetter ()
typesetTable tableContents (TableColumns columns) = do
    RenderState{..} <- get

    -- Get table spacing and update cursor to reflect it, this is done because the row typesetting function gets the state to get the cursor.
    let (Spacing (Pt beforeSpace) (Pt afterSpace)) = fromJust $ cfgTableSpacing config
    modify $ \s -> s { currentY = currentY - beforeSpace }

    let font = fromJust $ cfgFont config 
    let cSize = convertFontSize (fromJust $ cfgParSize config)

    -- Calculate width of each column
    let marginX = fromPt (fromJust $ cfgHozMargin config)
    let (_, marginY) = vertMargin
    let colWidth = (pageX - marginX * 2) / (int2Double columns) 

    let cellBeforeSpacing = (int2Double cSize) * 0.4
    let cellAfterSpacing = (int2Double cSize) * 0.35

    -- Helper function to layout a single row without committing state changes yet.
    -- Returns: (The combined drawing action, The lowest Y point of the row, Did it fit?)
    let layoutRow rowItems startY = do
            -- Process each cell in the row.
            results <- forM (zip rowItems [0..(columns - 1)]) $ \(cell, index) -> do
                let tText = do
                        setJustification Centered
                        paragraph $ do
                            -- Convert all text into HPDF paragraphs.
                            forM_ cell $ \(VText txtContent style) -> do
                                -- Apply styling to segment.
                                let styledFont = getFont loadedFonts font style
                                setStyle (Font (PDFFont styledFont cSize) black black)
                                txt txtContent

                -- Calculate cell position.
                let cellX = marginX + (int2Double index * colWidth)
                let cellY = startY - cellBeforeSpacing -- Used to lower the text from the table cell.
                let height = cellY - marginY

                -- Create container for this cell.
                let container = mkContainer cellX cellY colWidth height 0
                let verState = defaultVerState NormalParagraph

                -- Make boxes and fill container.
                let boxes = getBoxes NormalParagraph (Font (PDFFont (getFont loadedFonts font Normal) cSize) black black) tText
                let (action, usedContainer, remaining) = fillContainer verState container boxes
                
                -- Determine the bottom Y of this specific cell
                let (Rectangle (bottomX :+ bottomY) (topX :+ _)) = containerContentRectangle usedContainer

                lift $ drawWithPage currentPage $ do
                    strokeColor red
                    stroke $ containerContentRectangle usedContainer
                
                return (action, bottomY, null remaining, (bottomX, topX))

            let allFit = all (\(_, _, fit, _) -> fit) results
            -- The new Y is the minimum of all cell bottom Ys, minus the padding.
            let lowestY = (minimum $ map(\(_, by, _, _) -> by) results) - cellAfterSpacing
            -- Get all draw actions from row.
            let rowActions = sequence_ $ map
                    (\(act, _, _, (bx, tx)) -> do
                        -- Typeset rectangle for cell.
                        strokeColor black
                        stroke $ Rectangle (bx :+ lowestY) (tx :+ startY)
                        -- Typeset cell.
                        act)
                    results
                    
            return (rowActions, lowestY, allFit)

    -- Iterate through the table rows.
    forM_ tableContents $ \row -> do
        RenderState{..} <- get
        -- Attempt to layout the row on the current page.
        (drawAction, nextY, fits) <- layoutRow row currentY

        -- If table didn't fit create a new page and typeset again.
        if fits
            then do
                -- It fits, draw it and update the cursor position.
                lift $ drawWithPage (currentPage) drawAction
                modify $ \s -> s { currentY = nextY - afterSpace}
            else do
                -- It does not fit, create a new page and typeset it there.
                makeNewPage
                -- Get the new state, to have the updated Y cursor.
                RenderState{..} <- get

                -- Layout the row again on the new page.
                (drawActionNew, nextYNew, _) <- layoutRow row currentY
                -- Typeset it on the new page. It's worth noting that there are no guarantees that the table will fit on an empty page, it
                -- could simply be to big.
                lift $ drawWithPage currentPage drawActionNew
                modify $ \s -> s { currentY = nextYNew - afterSpace}