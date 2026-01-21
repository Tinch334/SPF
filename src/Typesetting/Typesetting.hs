{-# LANGUAGE OverloadedStrings #-}

module Typesetting.Typesetting (typesetDocument) where

import Resources (LoadedFonts(..), getFont)
import Datatypes.ValidatedTokens
import Datatypes.Located
import Typesetting.Helpers

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.Map (Map)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Data.List (intersperse)

import Graphics.PDF
import Graphics.PDF.Typesetting

import qualified Debug.Trace as DT


data RenderState = RenderState
    { currentY      :: Double                   -- Vertical cursor position.
    , currentPage   :: PDFReference PDFPage     -- The page currently being typeset.
    , config        :: VConfig                  -- Configuration.
    , loadedFonts   :: LoadedFonts              -- Loaded fonts.
    , resources     :: Map FilePath ByteString  -- All loaded resources, with their associated path.
    , pageX         :: Double                   -- Page dimensions are needed for state resetting, the PDF monad doesn't provide access.
    , pageY         :: Double
    , remainingText :: [VText]                  -- Any text that cannot fit on the current page gets stored here.
    }

-- Manages the cursor and page, as well as PDF generation.
type Typesetter a = StateT RenderState PDF a

-- Helper function to run the typesetter.
runTypesetter :: RenderState -> Typesetter a -> PDF a
runTypesetter initial action = evalStateT action initial

typesetDocument :: ValidatedDocument -> Map FilePath ByteString -> LoadedFonts -> FilePath -> IO ()
typesetDocument (ValidatedDocument cfg meta cnt) res fonts outPath = do
    let pageRect = pageSizeToRect (fromJust $ cfgPageSize cfg)
    let (PDFRect _ _ px py) = pageRect
    let startY = py - fromPt (fromJust $ cfgVertMargin cfg)

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
        }

        evalStateT (typesetElements cnt) initialState
------------------------
-- COMPLETE COMMANDS; NO LONGER DONE IN Main.hs!!!!!!!!!!
------------------------

typesetElements :: [Located VComm] -> Typesetter ()
typesetElements [] = return ()
typesetElements ((Located _ comm):rest) = do
    case comm of
        VParagraph text font size just -> 
            typesetParagraph text font size just
            
    {-    VSection text font size -> 
            typesetHeader text font size (cfgSectionSize) (cfgSectionSpacing)
            
        VFigure path width caption -> 
            typesetFigure path width caption
            
        VNewpage -> 
            forceNewPage
            
        VHLine -> 
            drawHLine-}

    -- Continue typesetting.
    typesetElements rest

------------------------
-- AUXILIARY FUNCTIONS
------------------------
-- Generates the document information from the metadata.
generateDocInfo :: ValidatedMetadata -> PDFDocumentInfo
generateDocInfo meta = let
    baseInfo = standardDocInfo {compressed = False}
    withAuthor = case vmAuthor meta of
        Just (VAuthor a _ _) -> baseInfo {author = mergeVText a}
        Nothing -> baseInfo
    withSubject = case vmTitle meta of
        Just (VTitle t _ _) -> withAuthor {subject = mergeVText t}
        Nothing -> withAuthor in withSubject

-- Creates a new page.
makeNewPage :: Typesetter ()
makeNewPage = do
    -- Create new page of standard size in PDF Monad.
    newPage <- lift $ addPage Nothing
    
    -- Reset state, since we are in a new page.
    cfg <- gets config
    py <- gets pageY
    
    modify $ \s -> s { 
        currentPage = newPage, 
        currentY = py - fromPt (fromJust $ cfgVertMargin cfg)
    }

typesetParagraph :: [VText] -> Maybe Font -> Maybe Datatypes.ValidatedTokens.FontSize -> Maybe Datatypes.ValidatedTokens.Justification -> Typesetter ()
typesetParagraph vText mFont mSize mJust = do
    cfg <- gets config
    lf <- gets loadedFonts
    
    -- Get font, size and justification
    ---------------
    -- CHANGE STYLE
    ---------------
    let font = fromJust $ mFont <|> cfgFont cfg
    let size = convertFontSize (fromJust $ mSize <|> cfgParSize cfg)
    -- Get justification and convert to PDF data.
    let just = case mJust <|> cfgJustification cfg of
            Just JustifyLeft   -> LeftJustification
            Just JustifyRight  -> RightJustification
            Just JustifyCenter -> Centered
            Just JustifyFull   -> FullJustification

    -- Maps VText tokens to PDF typesetting instructions. Defined with in function so we can use "let" defined variables.
    let textGenerator :: TM StandardParagraphStyle StandardStyle ()
        textGenerator = do
            setJustification just
            paragraph $ do
                forM_ vText $ \(VText txtContent style) -> do
                    -- Apply styling to segment.
                    let styledFont = getFont lf font style
                    setStyle (Font (PDFFont styledFont size) black black)
                    txt txtContent

    -- The text instructions are converted into a list of renderable boxes. StandardParagraphStyle 'NormalParagraph' is used as the baseline
    -- context.
    let boxes = getBoxes NormalParagraph (Font (PDFFont (getFont lf font Normal) size) black black) textGenerator

    -- 4. Enter the Box Filling Loop
    fillBoxLoop boxes

  where
    -- Fills containers page by page.
    fillBoxLoop [] = return () -- Done
    fillBoxLoop boxes = do
        px <- gets pageX
        cfg <- gets config
        yStart <- gets currentY
        page <- gets currentPage
        
        -- Calculate available height from currentY down to the bottom margin and make a container with that size and the corresponding
        -- position.

        let marginX = fromPt (fromJust $ cfgHozMargin cfg)
        let marginY = fromPt (fromJust $ cfgVertMargin cfg)
        let width   = px - (marginX * 2) 
        let height  = yStart - marginY 

        -- The mkContainer function defines a container based on it's top left point.
        let container = mkContainer marginX yStart width height 0

        -- Fit boxes into container.
        let verState = defaultVerState NormalParagraph
        let (drawAction, usedContainer, remainingBoxes) = fillContainer verState container boxes

        -- Execute the resulting draw action on the current page.
        lift $ drawWithPage page drawAction

        lift $ drawWithPage page $ do
            DT.trace (printContainer container) $ strokeColor red
            stroke $ containerContentRectangle container
            strokeColor blue
            stroke $ containerContentRectangle usedContainer

        -- Update State, containerContentRectangle tells us the bounds of the text explicitly placed.
        let (Rectangle (_ :+ newBottomY) _) = containerContentRectangle usedContainer
        
        if null remainingBoxes
            then do
                -- Text fits on page, update cursor to the bottom of the placed text.
                modify $ \s -> s { currentY = newBottomY }
            else do
                -- Overflow, force new page and render remaining boxes.
                makeNewPage
                fillBoxLoop remainingBoxes

printContainer c = show (containerX c) ++ " - " ++ show (containerY c) ++ " - " ++ show (containerWidth c) ++ " - " ++ show (containerHeight c)


-- The title page is generated using a different mechanism that the rest of the document.
{-
typesetMetadata :: VConfig -> LoadedFonts -> ValidatedMetadata -> PDF ()
typesetMetadata cfg fonts meta = do
    -- Get page width and size.
    let (PDFRect _ _ pw ph) = pageSizeToRect (fromJust $ cfgPageSize cfg)
    let (titleHozMargin, titleVertMargin) = (0.15, 0.1)
    let titleRect = Rectangle (pw * titleHozMargin :+ ph * titleVertMargin) (pw * (1 - titleHozMargin) :+ ph * (1 - titleVertMargin))
    --printRect titleRect
    let (VTitle vtitle tf ts) = completeMeta cfg (fromJust $ vmTitle meta)
    let (VAuthor vauth af as) = completeMeta cfg (fromJust $ vmAuthor meta)
    let (VDate vdate df ds) = completeMeta cfg (fromJust $ vmDate meta)

    titlePage <- addPage Nothing

    drawWithPage titlePage $ do
        strokeColor red
        stroke $ titleRect
        displayFormattedText
            titleRect
            NormalParagraph
            (Font (PDFFont (hbi fonts) 10) black black) $ do
                setJustification Centered

                typesetParagraph fonts vtitle tf ts Centered
                typesetParagraph fonts vauth af as Centered
                typesetParagraph fonts vdate df ds Centered


-}