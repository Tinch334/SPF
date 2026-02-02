{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Typesetting.Typesetting (typesetDocument) where

import qualified Datatypes.ValidatedTokens as VT
import Datatypes.Located
import Datatypes.Resources
import Typesetting.Helpers
import Typesetting.Structures
import Typesetting.Styles

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Lazy

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromJust, fromMaybe, isJust)

import GHC.Float (int2Double, double2Int)

import Graphics.PDF
import Graphics.PDF.Typesetting.WritingSystem


------------------------
-- INITIALIZATION FUNCTIONS
------------------------
-- Removes all "Maybe" from configuration.
toRenderConfig :: VT.VConfig -> RenderConfig
toRenderConfig cfg = RenderConfig
    { layout = RCLayout 
        { rcPageSize   = fromJust (VT.pageSize   l)
        , rcNumbering  = fromJust (VT.numbering  l)
        , rcMarginVert = fromJust (VT.marginVert l)
        , rcMarginHoz  = fromJust (VT.marginHoz  l)
        }
    , styles = RCStyles
        { rcFont          = fromJust (VT.font          s)
        , rcJustification = fromJust (VT.justification s)
        , rcListType      = fromJust (VT.listType      s)
        }
    , sizes = RCSizes
        { rcParSize        = fromJust (VT.paragraphSize  sz)
        , rcTitleSize      = fromJust (VT.titleSize      sz)
        , rcSectionSize    = fromJust (VT.sectionSize    sz)
        , rcSubsectionSize = fromJust (VT.subsectionSize sz)
        , rcVerbatimSize   = fromJust (VT.verbatimSize   sz)
        }
    , spacing = RCSpacing
        { rcSectionSp   = fromJust (VT.sectionSp   sp)
        , rcParagraphSp = fromJust (VT.paragraphSp sp)
        , rcListSp      = fromJust (VT.listSp      sp)
        , rcTableSp     = fromJust (VT.tableSp     sp)
        , rcFigureSp    = fromJust (VT.figureSp    sp)
        , rcVerbatimSp  = fromJust (VT.verbatimSp  sp)
        , rcParIndent   = fromJust (VT.parIndent   sp)
        }
    , toggles = RCToggle
        { rcSectionNumbering    = fromJust (VT.sectionNumbering    t)
        , rcFigureNumbering     = fromJust (VT.figureNumbering     t)
        , rcVerbatimNumbering   = fromJust (VT.verbatimNumbering   t)
        }
    }
  where
    l  = VT.layout cfg
    s  = VT.styles cfg
    sz = VT.sizes cfg
    sp = VT.spacing cfg
    t  = VT.toggles cfg

typesetDocument :: VT.ValidatedDocument -> ResourceMap -> LoadedFonts -> FilePath -> Bool -> IO ()
typesetDocument (VT.ValidatedDocument cfg meta cnt) res fonts outPath dbg = do
    let rConfig = toRenderConfig cfg
    let layoutCfg = layout rConfig

    let pageRect@(PDFRect _ _ px py) = pageSizeToRect $ rcPageSize layoutCfg
    let (VT.Pt topMargin) = rcMarginVert layoutCfg
    let startY = py - topMargin
    
    -- Get bottom margin based on page numbering.
    let bottomMarginRaw = topMargin
    let bottomMargin = bottomMarginRaw + case rcNumbering layoutCfg of
            VT.NumberingNone -> 0.0
            _             -> 10.0

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
            -- The first page should never be accessed before it's created. Note that this relies on Haskell having lazy evaluation.
            , rsCurrentPage = error "INTERNAL: First page accessed before creation"
            , rsCounters = DocumentCounters 0 0 0 0
            }

    -- Generate typesetting action based on if there are title elements.
    let action = if hasTitleElems meta
        then do
            makeNewPage Unnumbered
            typesetTitlepage meta
            makeNewPage Numbered
        else do
            makeNewPage Numbered

    -- Typeset PDF and store result in "outPath".
    runPdf outPath (generateDocInfo meta) pageRect $ do
        evalStateT (runReaderT (action >> typesetElements cnt) env) initialState

  where
    hasTitleElems (VT.ValidatedMetadata Nothing Nothing Nothing) = False
    hasTitleElems _ = True

------------------------
-- TYPESETTING LOOP
------------------------
typesetElements :: [Located VT.VComm] -> Typesetter ()
typesetElements elements = do
    forM_ elements $ \(Located _ comm) -> do
        -- Check for overflow before typesetting.
        cs <- checkSpace
        when cs $ makeNewPage Numbered

        case comm of
            VT.VParagraph text font size just -> 
                typesetParagraph text font size just
            VT.VSection text font size -> do
                typesetHeader text font size LevelSection
            VT.VSubsection text font size -> do
                typesetHeader text font size LevelSubsection
            VT.VFigure path width caption -> do
                typesetFigure path width caption
            VT.VTable table columns ->
                typesetTable table columns
            VT.VList list style -> do
                typesetList list style
            VT.VVerbatim code font background ->
                do typesetVerbatim code font background
            VT.VNewpage ->
                makeNewPage Numbered
            VT.VHLine width thick ->
                typesetHLine width thick


------------------------
-- AUXILIARY FUNCTIONS
------------------------
-- Since two monads are being used for typesetting PDF monads have to be lifted twice, helper to avoid repetition.
pdfLift :: (MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m)) => m a -> t1 (t2 m) a
pdfLift = lift . lift

-- Checks if the cursor is below the bottom margin.
checkSpace :: Typesetter (Bool)
checkSpace = do
    currentY <- gets rsCurrentY
    (_, bottom) <- asks envVertMargin

    return $ currentY < bottom

-- Creates a new page, if the flag is set adds a number to that page.
makeNewPage :: NewPageMode -> Typesetter ()
makeNewPage mode = do
    RenderEnv{..} <- ask
    let layoutCfg = layout envConfig

    -- Create new page of standard size in PDF monad.
    newPage <- pdfLift $ addPage Nothing

    let numbering = rcNumbering layoutCfg
    let (topMargin, bottomMargin) = envVertMargin
    
    -- Update state.
    modify $ \s -> s { rsCurrentPage = newPage, rsCurrentY = envPageHeight - topMargin }

    -- If appropriate, print debug information.
    when envDebug (pdfLift $ drawWithPage newPage $ do
            strokeColor $ Rgb 1 0.8 0.1
            let (VT.Pt hozMargin) = rcMarginHoz layoutCfg
            stroke $ Rectangle (hozMargin :+ bottomMargin) ((envPageWidth - hozMargin) :+ (envPageHeight - topMargin)))

    -- Typeset line number onto new page.
    unless (numbering == VT.NumberingNone || mode == Unnumbered) $ typesetPageNumber numbering

  where
    typesetPageNumber numbering = do
        RenderEnv{..} <- ask
        RenderState{..} <- get

        let newPageNumber = dcPage rsCounters + 1
        -- Update page number.
        modify $ \s -> s { rsCounters = rsCounters { dcPage = newPageNumber } }

        let (VT.Pt hMargin) = rcMarginHoz (layout envConfig)
        let (_, bottomMargin) = envVertMargin
        -- Get the page number centred in the bottom area of the margin.
        let rect = Rectangle (hMargin :+ (bottomMargin * 0.2)) ((envPageWidth - hMargin) :+ (bottomMargin * 0.6))
        let font = getFont envFonts (rcFont $ styles envConfig) VT.Normal

        let pnStr = case numbering of
                VT.NumberingArabic -> show newPageNumber
                VT.NumberingRoman  -> toRoman newPageNumber

        -- If appropriate, print debug information.
        when envDebug (pdfLift $ drawWithPage rsCurrentPage $ do
            strokeColor green
            stroke rect)        

        -- Whilst this function doesn't handle overflows from the render area properly they should never happen; Since a number would have to
        -- be as wide as the page area for that to be a problem.
        pdfLift $ drawWithPage rsCurrentPage $ do
            displayFormattedText rect NormalParagraph (Font (PDFFont font 12) black black) $ do
                setJustification Centered
                paragraph $ txt (T.pack pnStr)


------------------------
-- CONTENT TYPESETTING FUNCTIONS
------------------------
-- Wrapper of "typesetContent", hides uncommon options.
typesetContentSimple :: [VT.VText] -> VT.Font -> VT.FontSize -> VT.Justification -> Double -> Double -> Typesetter ()
typesetContentSimple vText font size just beforeSpace afterSpace =
    typesetContent (Left vText) font size just NormalPara 0 beforeSpace afterSpace

-- Typesets the given VText or paragraph, with the given options. Note that justification and indentation only have an effect in VText mode.
typesetContent :: (Either [VT.VText] (TM CustomParaStyle StandardStyle ())) -> VT.Font -> VT.FontSize
                -> VT.Justification -> CustomParaStyle -> Double -> Double -> Double -> Typesetter ()
typesetContent content font size just paraStyle indent beforeSpace afterSpace = do
    RenderEnv{..} <- ask
    RenderState{..} <- get

    -- Maps VText tokens to PDF typesetting instructions. Defined with in function so we can use "let" defined variables.
    let textGenerator vText = do
            let cSize = convertFontSize size

            -- Set justification and convert to PDF data.
            setJustification $ case just of
                    VT.JustifyLeft   -> LeftJustification
                    VT.JustifyRight  -> RightJustification
                    VT.JustifyCenter -> Centered
                    VT.JustifyFull   -> FullJustification
 
            paragraph $ do
                kern indent
                -- Convert all text into HPDF paragraphs.
                forM_ vText $ \(VT.VText txtContent style) -> do
                    -- Apply styling to segment.
                    let styledFont = getFont envFonts font style
                    setStyle (Font (PDFFont styledFont cSize) black black)
                    txt txtContent

    -- The content is converted into a list of renderable boxes. The style 'NormalParagraph' is used as the baseline context.
    let cnt = case content of
            Left text -> textGenerator text
            Right block -> block
    --let styleObj = NormalPara $ PDFFont (getFont envFonts font Normal) (convertFontSize size)
    let boxes = getBoxes paraStyle (Font (PDFFont (getFont envFonts font VT.Normal) (convertFontSize size)) black black) cnt
    
    fillBoxLoop boxes paraStyle
  where
    -- Fills containers page by page.
    fillBoxLoop [] _ = return () -- Done
    fillBoxLoop boxes paraStyle = do
        RenderEnv{..} <- ask
        RenderState{..} <- get
            
        -- Calculate available height from currentY down to the bottom margin and make a container with that size, taking into account the
        -- given spacing.
        let (VT.Pt marginX) = rcMarginHoz (layout envConfig)
        let (_, bottomMargin) = envVertMargin
        let width   = envPageWidth - (marginX * 2) 
        let height  = rsCurrentY - bottomMargin 

        -- The mkContainer function defines a container based on it's top left point.
        let container = mkContainer marginX (rsCurrentY - beforeSpace) width height 0
        -- Generate a vertical style with the given paragraph style.
        let verState = defaultVerState paraStyle
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
                makeNewPage Numbered
                fillBoxLoop remainingBoxes paraStyle

-- Generates the title-page.
typesetTitlepage :: VT.ValidatedMetadata -> Typesetter ()
typesetTitlepage meta = do
    RenderEnv{..} <- ask

    -- Move the cursor down to separate any title elements.
    modify $ \s -> s { rsCurrentY = envPageHeight * 0.85 }

    let font = rcFont (styles envConfig)

    let (VT.FontSize baseSize) = rcTitleSize (sizes envConfig)
    let sizeTitle  = VT.FontSize baseSize
    let sizeAuthor = VT.FontSize (baseSize * 0.6)
    let sizeDate   = VT.FontSize (baseSize * 0.45)

    -- Element spacing
    let largeGap = envPageHeight * 0.05
    let smallGap = envPageHeight * 0.02

    let mTitle = VT.vmTitle meta
    let mAuthor = VT.vmAuthor meta
    let mDate = VT.vmDate meta

    case mTitle of
        Nothing -> return ()
        Just t -> do
            typesetContentSimple t font sizeTitle VT.JustifyCenter 0 0

    -- If title and author are present typeset a separating line.
    when (isJust mTitle && isJust mAuthor) $ do
        -- It's necessary to get the cursor position both times since typesetting, first the title and then the line, changes it.
        cy <- gets rsCurrentY
        modify $ \s -> s { rsCurrentY = cy - smallGap }
        typesetHLine (VT.PageWidth 0.6) (Just $ VT.Pt 2.0)
        cy <- gets rsCurrentY
        modify $ \s -> s { rsCurrentY = cy - smallGap }

    case mAuthor of
        Nothing -> return ()
        Just a -> do
            typesetContentSimple a font sizeAuthor VT.JustifyCenter 0 largeGap

    case mDate of
        Nothing -> return ()
        Just d -> do
            typesetContentSimple d font sizeDate VT.JustifyCenter 0 0

-- Typesets the given paragraph.
typesetParagraph :: [VT.VText] -> Maybe VT.Font -> Maybe VT.FontSize -> Maybe VT.Justification -> Typesetter ()
typesetParagraph vText mFont mSize mJust = do
    cfg <- asks envConfig
    let styleCfg = styles cfg
    let sizeCfg  = sizes cfg
    let spaceCfg = spacing cfg

    let font = fromMaybe (rcFont styleCfg) mFont
    let size = fromMaybe (rcParSize sizeCfg) mSize
    let just = fromMaybe (rcJustification styleCfg) mJust
    let (VT.Pt indent) = rcParIndent spaceCfg
    let (VT.Spacing (VT.Pt beforeSpace) (VT.Pt afterSpace)) = rcParagraphSp spaceCfg

    typesetContent (Left vText) font size just NormalPara indent beforeSpace afterSpace

-- Typesets both sections and subsections. 
typesetHeader :: [VT.VText] -> Maybe VT.Font -> Maybe VT.FontSize -> HeaderLevel -> Typesetter ()
typesetHeader vText mFont mSize level = do
    RenderState{..} <- get
    cfg <- asks envConfig
    let styleCfg = styles cfg
    let sizeCfg  = sizes cfg
    let spaceCfg = spacing cfg

    let font = fromMaybe (rcFont styleCfg) mFont
    let (VT.Spacing (VT.Pt beforeSpace) (VT.Pt afterSpace)) = rcSectionSp spaceCfg
    let numberingEnabled = rcSectionNumbering (toggles cfg)

    let (size, newCounters, label) = case level of
            LevelSection ->
                let next = dcSection rsCounters + 1
                in ( fromMaybe (rcSectionSize sizeCfg) mSize
                   , rsCounters { dcSection = next, dcSubsection = 0 }
                   , show next )
            LevelSubsection ->
                let next = dcSubsection rsCounters + 1
                in ( fromMaybe (rcSubsectionSize sizeCfg) mSize
                   , rsCounters { dcSubsection = next }
                   , show (dcSection rsCounters) ++ "." ++ show next )

    -- Update counter state.
    modify $ \s -> s { rsCounters = newCounters }

    let numbering = if numberingEnabled then T.pack $ label ++ ". " else ""
    let fullText = if numberingEnabled then (VT.VText numbering VT.Bold):vText else vText

    -- Create bookmark in PDF.
    pdfLift $ newSectionWithPage (numbering <> mergeVText vText) Nothing Nothing rsCurrentPage (return ())
    -- Typeset section text.
    typesetContentSimple fullText font size VT.JustifyLeft beforeSpace afterSpace

-- Typesets the given figure, with it's caption if present.
typesetFigure :: FilePath -> VT.PageWidth -> Maybe VT.Caption -> Typesetter ()
typesetFigure path gw mCap = typesetFigureInner path gw mCap True

typesetFigureInner :: FilePath -> VT.PageWidth -> Maybe VT.Caption -> Bool -> Typesetter ()
typesetFigureInner path gw@(VT.PageWidth givenWidth) mCap newpageAllowed = do
    RenderEnv{..} <- ask
    RenderState{..} <- get

    -- Validation guarantees that all paths have a valid resource.
    let (FileInfo bs w h) = fromJust $ M.lookup path envResources
    -- Get figure spacing.
    let (VT.Spacing (VT.Pt beforeSpace) (VT.Pt afterSpace)) = rcFigureSp (spacing envConfig)

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

    -- Adjust image size according to given page-width.
    let drawFigure = withNewContext $ do
            applyMatrix (translate (figureX :+ figureY))
            applyMatrix (scale figureScaleX figureScaleY)
            drawXObject img

    -- Update cursor by the height of the figure, spacing after it is added later.
    modify $ \s -> s { rsCurrentY = figureY }
    cs <- checkSpace

    -- Check if there's enough space to fit the figure, if there's not create a new page and typeset it there.
    (captionFits, drawCaption) <- case mCap of
            Just caption -> makeFigureCaption caption afterSpace
            Nothing -> return (True, return ())

    -- Check if figure and caption fit in page, if not make a new page. Any given figure can only create one new page, to avoid an
    -- infinite loop if a figure is too big to fit even on an empty page.
    if (cs && not captionFits) && newpageAllowed
        then do
            makeNewPage Numbered
            typesetFigureInner path gw mCap False
        else do
            pdfLift $ drawWithPage rsCurrentPage drawFigure
            pdfLift $ drawWithPage rsCurrentPage drawCaption

    -- Get updated state, with figure and possibly caption changes to cursor position.
    RenderState{..} <- get
    modify $ \s -> s { rsCurrentY = rsCurrentY - afterSpace }

  where
    -- The signature is fromJustuired, otherwise Haskell complains about inferring a concrete type.
    makeFigureCaption :: Text -> Double -> Typesetter (Bool, Draw ())
    makeFigureCaption caption afterSpace = do
        RenderEnv{..} <- ask
        RenderState{..} <- get

        let font = rcFont (styles envConfig)
        let numberingEnabled = rcFigureNumbering (toggles envConfig)
        let newNumber = (dcFigure rsCounters) + 1
        let finalCaption = if numberingEnabled then T.pack ("Figure " ++ show newNumber ++ ": ") <> caption else caption

        -- Generate and format the text, then put it in a box.
        let boxes = getBoxes NormalPara (Font (PDFFont (getFont envFonts font VT.Normal) 12) black black) $ do
                setJustification Centered
                paragraph $ do 
                    txt finalCaption
        
        -- Space between figure and caption.
        let figureCaptionSpacing = 10
        -- Calculate available height from currentY down to the bottom margin and make a container with that size, taking into account the
        -- given spacing.
        let (VT.Pt marginX) = rcMarginHoz (layout envConfig)
        let (_, bottomMargin) = envVertMargin
        let width = envPageWidth - (marginX * 2) 
        let height = rsCurrentY - bottomMargin

        -- The mkContainer function defines a container based on it's top left point.
        let container = mkContainer marginX (rsCurrentY - figureCaptionSpacing) width height 0
        -- Fill container.
        let (drawAction, usedContainer, remainingBoxes) = fillContainer (defaultVerState NormalPara) container boxes

        -- Update state, containerContentRectangle returns the bounds of the text explicitly placed.
        let (Rectangle (_ :+ newBottomY) _) = containerContentRectangle usedContainer
        
        if null remainingBoxes
            then do
                -- Text fits on page, add caption; Then update cursor to the bottom of the placed text plus paragraph spacing.
                modify $ \s -> s { rsCurrentY = newBottomY - afterSpace, rsCounters = rsCounters { dcFigure = newNumber } }

                -- If appropriate, print debug information.
                when envDebug (pdfLift $ drawWithPage rsCurrentPage $ do
                    strokeColor red
                    stroke $ containerContentRectangle usedContainer)

                return (True, drawAction)
            else
                return (False, return ())

-- Typesets the given list.
typesetList :: [[VT.VText]] -> Maybe VT.ListStyle -> Typesetter ()
typesetList items mStyle = do
    cfg <- asks envConfig
    fonts <- asks envFonts
    let styleCfg = styles cfg
    let sizeCfg  = sizes cfg

    let size = rcParSize sizeCfg
    let (VT.FontSize sizeValue) = size
    let font = rcFont styleCfg
    let (VT.Spacing (VT.Pt beforeSpace) (VT.Pt afterSpace)) = rcListSp (spacing cfg)

    let styleToken = fromMaybe (rcListType styleCfg) mStyle
    let styleZap = setStyle (Font (PDFFont (zapf fonts) (convertAdjustFontSize size 0.6)) black black)

    -- The style is a function that takes the item's position in the list and returns the element to typeset.
    let styleFunction = case styleToken of
            VT.ListBullet -> \_ -> do
                styleZap
                txt $ "●"
            VT.ListSquare -> \_ -> do
                styleZap
                txt $ "■"
            VT.ListArrow  -> \_ -> do
                styleZap
                txt $ "➤"
            VT.ListNumber -> \n -> do
                setStyle (Font (PDFFont (getFont fonts font VT.Normal) (convertFontSize size)) black black)
                txt $ T.pack $ show n ++ "."

    -- Typeset list.
    forM_ (zip items [1..(length items)]) $ \(line, i) -> do
        let beforeLine = if i == 1 then beforeSpace else 0
        let afterLine = if i == length items then afterSpace else sizeValue * 0.7

        let listElement = do
                setJustification LeftJustification
                paragraph $ do
                    kern sizeValue
                    styleFunction i
                    kern $ sizeValue * 0.5
                    -- Convert all text into HPDF paragraphs.
                    forM_ line $ \(VT.VText txtContent style) -> do
                        -- Apply styling to segment.
                        let styledFont = getFont fonts font style
                        setStyle (Font (PDFFont styledFont (convertFontSize size)) black black)
                        txt txtContent

        -- Typeset each line individually to properly control line spacing.
        typesetContent (Right listElement) font size VT.JustifyLeft NormalPara 0 beforeLine afterLine

-- Typesets the given table.
typesetTable :: [[[VT.VText]]] -> VT.TableColumns -> Typesetter ()
typesetTable tableContents columns = do
    RenderState{..} <- get
    RenderEnv{..} <- ask

    -- Get table spacing and update cursor to reflect it, this is done because the row typesetting function gets the state.
    let (VT.Spacing (VT.Pt beforeSpace) (VT.Pt afterSpace)) = rcTableSp (spacing envConfig)
    modify $ \s -> s { rsCurrentY = rsCurrentY - beforeSpace }

    let font = rcFont (styles envConfig)
    let cSize = convertFontSize (rcParSize (sizes envConfig))

    -- Calculate width of each column.
    let (VT.Pt marginX) = rcMarginHoz (layout envConfig)
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
                            forM_ cell $ \(VT.VText txtContent style) -> do
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
                let boxes = getBoxes NormalParagraph (Font (PDFFont (getFont envFonts font VT.Normal) cSize) black black) tText
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
                makeNewPage Numbered
                -- Get the new state, to have the updated Y cursor.
                RenderState{..} <- get

                -- Layout the row again on the new page.
                (drawActionNew, nextYNew, _) <- lift $ layoutRow row rsCurrentY
                -- Typeset it on the new page. It's worth noting that there are no guarantees that the table will fit on an empty page, it
                -- could simply be too big.
                pdfLift $ drawWithPage rsCurrentPage drawActionNew
                modify $ \s -> s { rsCurrentY = nextYNew }

    -- Get updated cursor and space after the table.
    RenderState{..} <- get
    modify $ \s -> s { rsCurrentY = rsCurrentY - afterSpace }

-- Typeset the given text preserving all spaces and escaping no characters.
typesetVerbatim :: [Text] -> Maybe VT.FontSize -> Maybe Bool -> Typesetter ()
typesetVerbatim code mSize mNumbering = do
    cfg <- asks envConfig
    fonts <- asks envFonts

    let sizeCfg = sizes cfg
    let spaceCfg = spacing cfg
    let layoutCfg = layout cfg

    let codeFontType = VT.Courier
    let (VT.FontSize codeFontSize) = fromMaybe (rcVerbatimSize sizeCfg) mSize
    let styledFont = getFont fonts codeFontType VT.Normal
    let pdfFont = PDFFont styledFont (double2Int codeFontSize)

    let numbering = fromMaybe (rcVerbatimNumbering (toggles cfg)) mNumbering
    let (VT.Spacing (VT.Pt beforeSpace) (VT.Pt afterSpace)) = rcVerbatimSp spaceCfg

    -- Calculate numbering width.
    let maxNumWidth = textWidth pdfFont (T.pack $ show (length code))
    -- Gap between line number and code.
    let gapWidth = 10.0 
    -- Total gutter: Number width + gap
    let totalGutter = maxNumWidth + gapWidth

    -- Line wrapping would be a problem if we simply typeset lines and numbers "normally", instead we use a "hanging indent". The cursor starts
    -- where the code starts (because of linePosition). We move back to the start of the line to draw the line number.
    let paraFormat = if numbering
        then VerbatimPara (Rgb 0.94 0.94 0.94) 5 0 (Just $ codeFontSize * 0.5) totalGutter
        else VerbatimPara (Rgb 0.94 0.94 0.94) 5 0 Nothing 0

    -- In case of no numbering and a line with spaces at the start the typesetting engine sees: "Space Space ....", since this is the start
    -- of the line this is treated as unwanted glue and discarded. To solve this a zero width space is inserted, stopping this behaviour.
    let zws = T.singleton '\x200B'

    let formattedCode = do
            setWritingSystem UnknownWritingSystem
            setJustification LeftJustification
            paragraph $ do
                setStyle (Font pdfFont black black)

                forM_ (zip code [1..(length code)]) $ \(line, i) -> do
                    let currentNumStr = T.pack $ show i
                    let currentNumWidth = textWidth pdfFont currentNumStr

                    -- Align and draw line number.
                    when numbering $ do
                        kern (-totalGutter)
                        let numberPadding = (totalGutter - gapWidth) - currentNumWidth
                        kern numberPadding
                        txt currentNumStr
                        -- Move cursor forward to the code start position. Note that the cursor is at "Padding + NumWidth", we need to reach
                        -- "TotalGutter".
                        kern gapWidth 

                    txt $ zws <> line
                    forceNewLine

    typesetContent (Right formattedCode) codeFontType (VT.FontSize codeFontSize) VT.JustifyLeft paraFormat 0 beforeSpace afterSpace

-- Draw a horizontal line at the cursors current position.
typesetHLine :: VT.PageWidth -> Maybe VT.Pt -> Typesetter ()
typesetHLine (VT.PageWidth width) mThick = do
    y <- gets rsCurrentY
    p <- gets rsCurrentPage
    w <- asks envPageWidth

    -- Get margins on either side of line, note that this calculation does not take margins into account, to allow for full page lines.
    let lineMargin = (w - w * width) / 2

    let xStart = lineMargin
    let xEnd = w - lineMargin

    let thickness = case mThick of
            Just (VT.Pt t) -> t
            Nothing -> 1

    -- Typeset line.
    pdfLift $ drawWithPage p $ do
        setWidth thickness
        strokeColor black
        stroke $ Line xStart y xEnd y

    -- Update cursor taking into account line thickness
    modify $ \s -> s { rsCurrentY = y + thickness}