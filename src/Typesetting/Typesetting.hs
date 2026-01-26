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
import Control.Monad.Reader
import Control.Monad.State.Lazy

import Data.Map (Map)
import qualified Data.Map as M
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromJust, fromMaybe)

import GHC.Float (int2Double)

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

------------------------
-- INITIALIZATION FUNCTIONS
------------------------
-- Removes all "Just" from configuration.
toRenderConfig :: VConfig -> RenderConfig
toRenderConfig VConfig{..} = RenderConfig
    { rcPageSize            = fromJust cfgPageSize
    , rcPageNumbering       = fromJust cfgPageNumbering
    , rcSectionSpacing      = fromJust cfgSectionSpacing
    , rcParagraphSpacing    = fromJust cfgParagraphSpacing
    , rcListSpacing         = fromJust cfgListSpacing
    , rcTableSpacing        = fromJust cfgTableSpacing
    , rcFigureSpacing       = fromJust cfgFigureSpacing
    , rcSpacingGlue         = fromJust cfgSpacingGlue
    , rcTextGlue            = fromJust cfgTextGlue
    , rcParIndent           = fromJust cfgParIndent
    , rcFont                = fromJust cfgFont
    , rcTitleSize           = fromJust cfgTitleSize
    , rcParSize             = fromJust cfgParSize
    , rcSectionSize         = fromJust cfgSectionSize
    , rcSubsectionSize      = fromJust cfgSubsectionSize
    , rcJustification       = fromJust cfgJustification
    , rcListStyle           = fromJust cfgListStyle
    , rcVertMargin          = fromJust cfgVertMargin
    , rcHozMargin           = fromJust cfgHozMargin
    , rcSectionNumbering    = fromJust cfgSectionNumbering
    , rcFigureNumbering     = fromJust cfgFigureNumbering
    }

typesetDocument :: ValidatedDocument -> ResourceMap -> LoadedFonts -> FilePath -> Bool -> IO ()
typesetDocument (ValidatedDocument cfg meta cnt) res fonts outPath dbg = do
    let rConfig = toRenderConfig cfg

    let pageRect@(PDFRect _ _ px py) = pageSizeToRect (fromJust $ cfgPageSize cfg)
    let (Pt topMargin) = rcVertMargin rConfig
    let startY = py - topMargin
    
    -- Get bottom margin based on page numbering.
    let bottomMarginRaw = topMargin
    let bottomMargin = bottomMarginRaw + case rcPageNumbering rConfig of
            NumberingNone -> 0.0
            _             -> 16.0

    -- Static environment setup.
    let env = RenderEnv 
            { envConfig = rConfig
            , envFonts = fonts
            , envResources = res
            , envPageWidth = px
            , envPageHeight = py
            , envVertMargin = (topMargin, bottomMargin)
            , envDebug = dbg
            }

    let initialState = RenderState 
            { rsCurrentY = startY
            , rsCurrentPage = error "INTERNAL: First page accessed before creation"
            , rsCounters = DocumentCounters 0 0 0 0
            }

    -- Generate typesetting action based on if there are title elements.
    let action = if hasTitleElems meta
        then do
            makeNewPage False
            typesetTitlepage meta
            makeNewPage True
        else do
            makeNewPage True

    -- Typeset PDF and store result in "outPath".
    runPdf outPath (generateDocInfo meta) pageRect $ do
        evalStateT (runReaderT (action >> typesetElements cnt) env) initialState

  where
    hasTitleElems (ValidatedMetadata Nothing Nothing Nothing) = False
    hasTitleElems _ = True

------------------------
-- TYPESETTING LOOP
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
                makeNewPage True

            VHLine ->
                drawHLine

        -- Check for overflow after typesetting every element.
        cs <- checkSpace
        when cs $ makeNewPage True


------------------------
-- AUXILIARY FUNCTIONS
------------------------
pdfLift :: (MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m)) => m a -> t1 (t2 m) a
pdfLift = lift . lift

-- Checks if the cursor is below the bottom margin.
checkSpace :: Typesetter (Bool)
checkSpace = do
    currentY <- gets rsCurrentY
    (_, bottom) <- asks envVertMargin

    return $ currentY < bottom

-- Creates a new page, if the flag is set adds a number to that page.
makeNewPage :: Bool -> Typesetter ()
makeNewPage makeNumbering = do
    RenderEnv{..} <- ask

    -- Create new page of standard size in PDF monad.
    newPage <- pdfLift $ addPage Nothing

    let numbering = rcPageNumbering envConfig
    let (topMargin, bottomMargin) = envVertMargin
    
    -- Update state.
    modify $ \s -> s { rsCurrentPage = newPage, rsCurrentY = envPageHeight - topMargin }

    -- If appropriate, print debug information.
    when envDebug (pdfLift $ drawWithPage newPage $ do
            strokeColor $ Rgb 1 0.8 0.1
            let (Pt hozMargin) = rcHozMargin envConfig
            stroke $ Rectangle (hozMargin :+ bottomMargin) ((envPageWidth - hozMargin) :+ (envPageHeight - topMargin)))

    -- Typeset line number onto new page.
    unless (numbering == NumberingNone || not makeNumbering) $ typesetPageNumber numbering

  where
    typesetPageNumber numbering = do
        RenderEnv{..} <- ask
        RenderState{..} <- get

        let newPageNumber = dcPage rsCounters + 1
        -- Update page number.
        modify $ \s -> s { rsCounters = rsCounters { dcPage = newPageNumber } }

        let (Pt hMargin) = rcHozMargin envConfig
        let (_, bottomMargin) = envVertMargin
        -- Get the page number centred in the bottom area of the margin.
        let rect = Rectangle (hMargin :+ (bottomMargin * 0.2)) ((envPageWidth - hMargin) :+ (bottomMargin * 0.6))
        let font = getFont envFonts (rcFont envConfig) Normal

        let pnStr = case numbering of
                NumberingArabic -> show newPageNumber
                NumberingRoman  -> toRoman newPageNumber

        -- Whilst this function doesn't handle overflows from the render area well they should never happen; Since a number would have to be
        -- as wide as the page for that to be a problem.
        pdfLift $ drawWithPage rsCurrentPage $ do
            displayFormattedText rect NormalParagraph (Font (PDFFont font 12) black black) $ do
                setJustification Centered
                paragraph $ txt (T.pack pnStr)


------------------------
-- CONTENT TYPESETTING FUNCTIONS
------------------------
-- Typesets the given VText or paragraph, with the given options. Note that justification and indentation only have an effect in VText mode.
typesetContent :: (Either [VText] HPDFParagraph)
                -> Font
                -> Datatypes.ValidatedTokens.FontSize
                -> Datatypes.ValidatedTokens.Justification
                -> Double -> Double -> Double -> Typesetter ()
typesetContent content font size just indent beforeSpace afterSpace = do
    RenderEnv{..} <- ask
    RenderState{..} <- get

    -- Maps VText tokens to PDF typesetting instructions. Defined with in function so we can use "let" defined variables.
    let textGenerator vText = do
            let cSize = convertFontSize size

            -- Set justification and convert to PDF data.
            setJustification $ case just of
                    JustifyLeft   -> LeftJustification
                    JustifyRight  -> RightJustification
                    JustifyCenter -> Centered
                    JustifyFull   -> FullJustification
 
            paragraph $ do
                kern indent
                -- Convert all text into HPDF paragraphs.
                forM_ vText $ \(VText txtContent style) -> do
                    -- Apply styling to segment.
                    let styledFont = getFont envFonts font style
                    setStyle (Font (PDFFont styledFont cSize) black black)
                    txt txtContent

    -- The content is converted into a list of renderable boxes. The style 'NormalParagraph' is used as the baseline context.
    let cnt = case content of
            Left text -> textGenerator text
            Right block -> block
    let boxes = getBoxes NormalParagraph (Font (PDFFont (getFont envFonts font Normal) (convertFontSize size)) black black) cnt

    fillBoxLoop boxes
  where
    -- Fills containers page by page.
    fillBoxLoop [] = return () -- Done
    fillBoxLoop boxes = do
        RenderEnv{..} <- ask
        RenderState{..} <- get
            
        -- Calculate available height from currentY down to the bottom margin and make a container with that size, taking into account the
        -- given spacing.
        let (Pt marginX) = rcHozMargin envConfig
        let (_, bottomMargin) = envVertMargin
        let width   = envPageWidth - (marginX * 2) 
        let height  = rsCurrentY - bottomMargin 

        -- The mkContainer function defines a container based on it's top left point.
        let container = mkContainer marginX (rsCurrentY - beforeSpace) width height 0
        let verState = defaultVerState NormalParagraph

        -- Fill container.
        let (drawAction, usedContainer, remainingBoxes) = fillContainer verState container boxes

        -- Execute the resulting draw action on the current page.
        pdfLift $ drawWithPage rsCurrentPage drawAction

        -- If appropriate, print debug information.
        when envDebug (pdfLift $ drawWithPage rsCurrentPage $ do
            strokeColor blue
            stroke $ containerContentRectangle usedContainer)

        -- Update State, containerContentRectangle tells us the bounds of the text explicitly placed.
        let (Rectangle (_ :+ newBottomY) _) = containerContentRectangle usedContainer
            
        if null remainingBoxes
            then
                -- Text fits on page, update cursor to the bottom of the placed text plus paragraph spacing.
                modify $ \s -> s { rsCurrentY = newBottomY - afterSpace }
            else do
                -- Overflow, force new page and render remaining boxes.
                makeNewPage True
                fillBoxLoop remainingBoxes

-- Generates the title-page.
typesetTitlepage :: ValidatedMetadata -> Typesetter ()
typesetTitlepage meta = do
    RenderEnv{..} <- ask
    RenderState{..} <- get

    -- Move the cursor down to separate any title elements.
    modify $ \s -> s { rsCurrentY = envPageHeight * 0.92 }
    RenderState{..} <- get

    let font = rcFont envConfig
    let (FontSize cSizeTitle) = rcTitleSize envConfig
    let cSizeRest = cSizeTitle * 0.6

    case vmTitle meta of
        Nothing -> return ()
        Just t -> do
            typesetContent (Left t) font (FontSize cSizeTitle) JustifyCenter 0 0 (envPageWidth * 0.15)

    case vmAuthor meta of
        Nothing -> return ()
        Just a -> do
            typesetContent (Left a) font (FontSize cSizeRest) JustifyCenter 0 0 (envPageWidth * 0.025)

    case vmDate meta of
        Nothing -> return ()
        Just d -> do
            typesetContent (Left d) font (FontSize cSizeRest) JustifyCenter 0 0 0

-- Typesets the given paragraph.
typesetParagraph :: [VText] -> Maybe Font ->
                    Maybe Datatypes.ValidatedTokens.FontSize -> Maybe Datatypes.ValidatedTokens.Justification -> Typesetter ()
typesetParagraph vText mFont mSize mJust = do
    cfg <- asks envConfig

    let font = fromMaybe (rcFont cfg) mFont
    let size = fromMaybe (rcParSize cfg) mSize
    let just = fromMaybe (rcJustification cfg) mJust
    let (Pt indent) = rcParIndent cfg
    let (Spacing (Pt beforeSpace) (Pt afterSpace)) = rcParagraphSpacing cfg

    typesetContent (Left vText) font size just indent beforeSpace afterSpace

-- Typesets both sections and subsections. 
typesetHeader :: [VText] -> Maybe Font -> Maybe Datatypes.ValidatedTokens.FontSize -> Bool -> Typesetter ()
typesetHeader vText mFont mSize isSection = do
    RenderState{..} <- get
    cfg <- asks envConfig

    let font = fromMaybe (rcFont cfg) mFont
    let (Spacing (Pt beforeSpace) (Pt afterSpace)) = rcSectionSpacing cfg
    let numberingEnabled = rcSectionNumbering cfg

    let (size, newCounters, label) = if isSection
        then
            let next = dcSection rsCounters + 1
            in ( fromMaybe (rcSectionSize cfg) mSize
                , rsCounters { dcSection = next, dcSubsection = 0}
                , show next )
        else
            let next = dcSection rsCounters + 1
            in ( fromMaybe (rcSubsectionSize cfg) mSize
                , rsCounters { dcSubsection = next}
                , show (dcSection rsCounters) ++ "." ++ show next )

    -- Update counter state.
    modify $ \s -> s { rsCounters = newCounters }

    let numbering = if numberingEnabled then T.pack $ label ++ ". " else ""
    let fullText = if numberingEnabled then (VText numbering Bold):vText else vText

    -- Create bookmark in PDF.
    pdfLift $ newSectionWithPage (numbering <> mergeVText vText) Nothing Nothing rsCurrentPage (return ())

    -- Typeset section text.
    typesetContent (Left fullText) font size JustifyLeft 0 beforeSpace afterSpace

-- Typesets the given figure, with it's caption if present.
typesetFigure :: FilePath -> PageWidth -> Maybe Caption -> Typesetter ()
typesetFigure path (PageWidth givenWidth) mCap = do
    RenderEnv{..} <- ask
    RenderState{..} <- get

    -- Validation guarantees that all paths have a valid resource.
    let (FileInfo bs w h) = fromJust $ M.lookup path envResources
    -- Get figure spacing.
    let (Spacing (Pt beforeSpace) (Pt afterSpace)) = rcFigureSpacing envConfig

    -- This function expects raw pixel values; Or so I think, from reading the source code.
    img <- pdfLift $ createPDFRawImageFromByteString w h True NoFilter bs

    -- Calculate image scale and position.
    let imageRatio = (int2Double w) / (int2Double h)
    let figureWidth = envPageWidth * givenWidth
    let figureHeight = figureWidth / imageRatio

    let figureX = (envPageWidth - figureWidth) / 2
    let figureY = rsCurrentY - figureHeight - beforeSpace

    let figureScaleX = (1 / (int2Double w)) * figureWidth
    let figureScaleY = (1 / (int2Double h)) * figureHeight

    let drawFigure = pdfLift (drawWithPage rsCurrentPage $ do
            withNewContext $ do
                applyMatrix (translate (figureX :+ figureY))
                applyMatrix (scale figureScaleX figureScaleY)
                drawXObject img)

    -- Update cursor by the height of the figure, spacing after it is added later.
    modify $ \s -> s { rsCurrentY = figureY }
    cs <- checkSpace

    -- Check if there's enough space to fit the figure, if there's not create a new page and typeset it there.
    if cs
        then do
            makeNewPage True
            typesetFigure path (PageWidth givenWidth) mCap
        else -- Draw image and potentially caption.
            case mCap of
                Just caption -> do
                    (captionFits, drawCaption) <- makeFigureCaption caption afterSpace
                    -- Check if there's enough space to fit the figure and caption, if there's not create a new page and typeset it there.
                    -- Note that this does not guarantee that the image and caption fit properly, it could by that they just will never
                    -- fit in the page.
                    if captionFits
                        then do
                            drawFigure
                            pdfLift $ drawWithPage rsCurrentPage drawCaption
                        else do
                            makeNewPage True
                            drawFigure 
                            pdfLift $ drawWithPage rsCurrentPage drawCaption

                -- No caption, draw figure and spacing directly.
                Nothing -> do
                    modify $ \s -> s { rsCurrentY = rsCurrentY - afterSpace }
                    drawFigure
  where
    -- The signature is required, otherwise Haskell complains about inferring a concrete type.
    makeFigureCaption :: Text -> Double -> Typesetter (Bool, Draw ())
    makeFigureCaption caption afterSpace = do
        RenderEnv{..} <- ask
        RenderState{..} <- get

        let font = rcFont envConfig

        let numberingEnabled = rcFigureNumbering envConfig
        let newNumber = (dcFigure rsCounters) + 1
        let finalCaption = if numberingEnabled
            then T.pack ("Figure " ++ show newNumber ++ ": ") <> caption
            else caption

        -- Generate and format the text, then put it in a box.
        let boxes = getBoxes NormalParagraph (Font (PDFFont (getFont envFonts font Normal) 12) black black) $ do
                setJustification Centered
                paragraph $ do 
                    txt finalCaption
        
        -- Space between figure and caption.
        let figureCaptionSpacing = 10
        -- Calculate available height from currentY down to the bottom margin and make a container with that size, taking into account the
        -- given spacing.
        let (Pt marginX) = rcHozMargin envConfig
        let (_, bottomMargin) = envVertMargin
        let width = envPageWidth - (marginX * 2) 
        let height = rsCurrentY - bottomMargin

        -- The mkContainer function defines a container based on it's top left point.
        let container = mkContainer marginX (rsCurrentY - figureCaptionSpacing) width height 0
        let verState = defaultVerState NormalParagraph

        -- Fill container.
        let (drawAction, usedContainer, remainingBoxes) = fillContainer verState container boxes

        -- Update state, containerContentRectangle returns the bounds of the text explicitly placed.
        let (Rectangle (_ :+ newBottomY) _) = containerContentRectangle usedContainer
        
        if null remainingBoxes
            then do
                -- Text fits on page, add caption; Then update cursor to the bottom of the placed text plus paragraph spacing.
                modify $ \s -> s { rsCurrentY = newBottomY - afterSpace, rsCounters = rsCounters { dcFigure = newNumber } }

                -- If appropriate, print debug information.
                when envDebug (pdfLift $ drawWithPage rsCurrentPage $ do
                    strokeColor green
                    stroke $ containerContentRectangle usedContainer)

                return (True, drawAction)
            else
                return (False, return ())

-- Typesets the given list.
typesetList :: [[VText]] -> Maybe ListStyle -> Typesetter ()
typesetList items mStyle = do
    cfg <- asks envConfig
    fonts <- asks envFonts

    let size = rcParSize cfg
    let font = rcFont cfg
    let (Spacing (Pt beforeSpace) (Pt afterSpace)) = rcListSpacing cfg

    -- Get list style and convert it to a character.
    let styleToken = fromMaybe (rcListStyle cfg) mStyle
    let styleZap = setStyle (Font (PDFFont (zapf fonts) (convertAdjustFontSize size 0.6)) black black)

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
                setStyle (Font (PDFFont (getFont fonts font Normal) (convertFontSize size)) black black)
                txt $ T.pack $ show n ++ "."

    -- Typeset list.
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
                        let styledFont = getFont fonts font style
                        setStyle (Font (PDFFont styledFont (convertFontSize size)) black black)
                        txt txtContent
                        forceNewLine

    typesetContent (Right list) font size JustifyLeft 0 beforeSpace afterSpace

-- Typesets the given table.
typesetTable :: [[[VText]]] -> TableColumns -> Typesetter ()
typesetTable tableContents columns = do
    RenderState{..} <- get
    RenderEnv{..} <- ask

    -- Get table spacing and update cursor to reflect it, this is done because the row typesetting function gets the state to get the cursor.
    let (Spacing (Pt beforeSpace) (Pt afterSpace)) = rcTableSpacing envConfig
    modify $ \s -> s { rsCurrentY = rsCurrentY - beforeSpace }

    let font = rcFont envConfig
    let cSize = convertFontSize (rcParSize envConfig)

    -- Calculate width of each column
    let (Pt marginX) = rcHozMargin envConfig
    let (_, bottomMargin) = envVertMargin
    let colWidth = (envPageWidth - marginX * 2) / (int2Double columns) 

    let cellBeforeSpacing = (int2Double cSize) * 0.4
    let cellAfterSpacing = (int2Double cSize) * 0.35

    -- Helper function to layout a single row without committing state changes yet.
    let layoutRow rowItems startY = do
            -- Process each cell in the row.
            results <- forM (zip rowItems [0..(columns - 1)]) $ \(cell, index) -> do
                let tText = do
                        setJustification Centered
                        paragraph $ do
                            -- Convert all text into HPDF paragraphs.
                            forM_ cell $ \(VText txtContent style) -> do
                                -- Apply styling to segment.
                                let styledFont = getFont envFonts font style
                                setStyle (Font (PDFFont styledFont cSize) black black)
                                txt txtContent

                -- Calculate cell position.
                let cellX = marginX + (int2Double index * colWidth)
                let cellY = startY - cellBeforeSpacing -- Used to lower the text from the table cell.
                let height = cellY - bottomMargin

                -- Create container for this cell.
                let container = mkContainer cellX cellY colWidth height 0
                let verState = defaultVerState NormalParagraph

                -- Make boxes and fill container.
                let boxes = getBoxes NormalParagraph (Font (PDFFont (getFont envFonts font Normal) cSize) black black) tText
                let (action, usedContainer, remaining) = fillContainer verState container boxes
                
                -- Determine the bottom Y of this specific cell
                let (Rectangle (bottomX :+ bottomY) (topX :+ _)) = containerContentRectangle usedContainer

                -- If appropriate, print debug information.
                when envDebug (lift $ drawWithPage rsCurrentPage $ do
                    strokeColor red
                    stroke $ containerContentRectangle usedContainer)
                
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
        (drawAction, nextY, fits) <- lift $ layoutRow row rsCurrentY

        -- If table didn't fit create a new page and typeset again.
        if fits
            then do
                -- It fits, draw it and update the cursor position.
                pdfLift $ drawWithPage rsCurrentPage drawAction
                modify $ \s -> s { rsCurrentY = nextY }
            else do
                -- It does not fit, create a new page and typeset it there.
                makeNewPage True
                -- Get the new state, to have the updated Y cursor.
                RenderState{..} <- get

                -- Layout the row again on the new page.
                (drawActionNew, nextYNew, _) <- lift $ layoutRow row rsCurrentY
                -- Typeset it on the new page. It's worth noting that there are no guarantees that the table will fit on an empty page, it
                -- could simply be to big.
                pdfLift $ drawWithPage rsCurrentPage drawActionNew
                modify $ \s -> s { rsCurrentY = nextYNew }

    -- Get updated cursor and space after the table.
    RenderState{..} <- get
    modify $ \s -> s { rsCurrentY = rsCurrentY - afterSpace }

-- Draw a horizontal line at the cursors current position.
drawHLine :: Typesetter ()
drawHLine = do
    y <- gets rsCurrentY
    p <- gets rsCurrentPage
    w <- asks envPageWidth

    -- Get horizontal start and end position.
    let lineSpan = 0.9
    let xStart = (1 - lineSpan) * w
    let xEnd = lineSpan * w

    pdfLift $ drawWithPage p $ do
        strokeColor black
        stroke $ Line xStart y xEnd y
