{-# LANGUAGE OverloadedStrings, BangPatterns, FlexibleInstances #-}

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
import Data.IORef

import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (evaluate)
import Control.Concurrent.MVar

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

-- Provides access to all rendering information, additionally it handles the current state of the PDF monad.
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

        evalStateT (typesetBlocks cnt) initialState

------------------------
-- COMPLETE COMMANDS; NO LONGER DONE IN Main.hs!!!!!!!!!!
------------------------
------------------------
-- TYPESETTING BLOCKS (The Tree Walker)
------------------------
-- | Forces an IO action to run immediately within the PDF monad.
{-# NOINLINE unsafeLiftIO #-}
unsafeLiftIO :: IO a -> PDF a
unsafeLiftIO action = do
    let !result = unsafePerformIO action
    return result

typesetBlocks :: [DocBlock] -> Typesetter ()
typesetBlocks [] = return ()
typesetBlocks (block : rest) = do
    case block of
        BlockLeaf locComm -> 
            typesetElement locComm

        BlockSection ((VSection text font size)) children -> do
            typesetSection text font size
            tunnelChildren text children

        BlockSubsection ((VSubsection text font size)) children -> do
            typesetSubsection text font size
            tunnelChildren text children
            
        _ -> return ()

    cb <- checkBrake
    if cb then makeNewPage else return ()
    
    typesetBlocks rest

 where
    tunnelChildren :: [VText] -> [DocBlock] -> Typesetter ()
    tunnelChildren text children = do
        st <- get
        let title = mergeVText text
        
        -- 1. Create a reference to hold the state.
        ref <- lift $ unsafeLiftIO $ newIORef st
        
        -- 2. Run the section.
        lift $ newSection title Nothing Nothing $ do
            -- Run the children logic starting with the current state 'st'
            finalState <- execStateT (typesetBlocks children) st
            
            -- Write the RESULTING state to the ref.
            _ <- unsafeLiftIO $ writeIORef ref finalState
            
            -- CRITICAL FIX: explicit return () to satisfy newSection's type signature.
            return ()
        
        -- 3. Read the state back.
        -- Because we used the strict unsafeLiftIO, this guaranteed to run 
        -- AFTER the writeIORef above.
        newState <- lift $ unsafeLiftIO $ readIORef ref
        put newState


-- Typeset a single non hierarchical element.
typesetElement :: VComm -> Typesetter ()
typesetElement comm = do
    case comm of
        VParagraph text mFont mSize mJust -> 
            typesetParagraph text mFont mSize mJust

        VNewpage ->
            makeNewPage

        VHLine ->
            drawHLine


------------------------
-- AUXILIARY FUNCTIONS
------------------------
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

drawHLine :: Typesetter ()
drawHLine = do
    cy <- gets currentY
    px <- gets pageX
    page <- gets currentPage

    -- Get horizontal start and end position.
    let lineSpan = 0.9
    let xStart = (1 - lineSpan) * px
    let xEnd = lineSpan * px

    lift $ drawWithPage page $ do
        strokeColor black
        stroke $ Line xStart cy xEnd cy

-- Returns True if the cursor has advanced over the bottom margin.
checkBrake :: Typesetter Bool
checkBrake = do
    cfg <- gets config
    cy <- gets currentY

    let marginY = fromPt (fromJust $ cfgVertMargin cfg)

    DT.trace ("Check brake: " ++ show cy ++ " - " ++ show marginY) (return $ cy < marginY)

-- Typesets the given VText, with the given options.
typesetVText :: [VText] -> Font -> Datatypes.ValidatedTokens.FontSize -> Datatypes.ValidatedTokens.Justification -> Double -> Double -> Typesetter ()
typesetVText vText font size just beforeSpace afterSpace = do
    cfg <- gets config
    lf <- gets loadedFonts
    
    let cSize = convertFontSize size
    -- Get justification and convert to PDF data.
    let cJust = case just of
            JustifyLeft   -> LeftJustification
            JustifyRight  -> RightJustification
            JustifyCenter -> Centered
            JustifyFull   -> FullJustification

    -- Maps VText tokens to PDF typesetting instructions. Defined with in function so we can use "let" defined variables.
    let textGenerator :: TM StandardParagraphStyle StandardStyle ()
        textGenerator = do
            setJustification cJust
            paragraph $ do
                -- Convert all text into HPDF paragraphs.
                forM_ vText $ \(VText txtContent style) -> do
                    -- Apply styling to segment.
                    let styledFont = getFont lf font style
                    setStyle (Font (PDFFont styledFont cSize) black black)
                    txt txtContent

    -- The text instructions are converted into a list of renderable boxes. StandardParagraphStyle 'NormalParagraph' is used as the baseline
    -- context.
    let boxes = getBoxes NormalParagraph (Font (PDFFont (getFont lf font Normal) cSize) black black) textGenerator

    fillBoxLoop boxes

  where
    -- Fills containers page by page.
    fillBoxLoop [] = return () -- Done
    fillBoxLoop boxes = do
        px <- gets pageX
        cfg <- gets config
        yStart <- gets currentY
        page <- gets currentPage
        
        -- Calculate available height from currentY down to the bottom margin and make a container with that size, taking into account the
        -- given spacing.
        let marginX = fromPt (fromJust $ cfgHozMargin cfg)
        let marginY = fromPt (fromJust $ cfgVertMargin cfg)
        let width   = px - (marginX * 2) 
        let height  = yStart - marginY 

        -- The mkContainer function defines a container based on it's top left point.
        let container = mkContainer marginX (yStart - beforeSpace) width height 0

        -- Fit boxes into container.
        let verState = defaultVerState NormalParagraph
        let (drawAction, usedContainer, remainingBoxes) = fillContainer verState container boxes

        -- Execute the resulting draw action on the current page.
        lift $ drawWithPage page drawAction

        lift $ drawWithPage page $ do
            strokeColor blue
            stroke $ containerContentRectangle usedContainer

        -- Update State, containerContentRectangle tells us the bounds of the text explicitly placed.
        let (Rectangle (_ :+ newBottomY) _) = containerContentRectangle usedContainer
        
        if null remainingBoxes
            then do
                -- Text fits on page, update cursor to the bottom of the placed text plus paragraph spacing.
                modify $ \s -> s { currentY = newBottomY - afterSpace}
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
    let (Spacing (Pt beforeSpace) (Pt afterSpace)) = fromJust $ cfgParagraphSpacing cfg

    typesetVText vText font size just beforeSpace afterSpace

-- Typesets the given header.
typesetSection :: [VText] -> Maybe Font -> Maybe Datatypes.ValidatedTokens.FontSize -> Typesetter ()
typesetSection vText mFont mSize = do
    cfg <- gets config

    let font = fromJust $ mFont <|> cfgFont cfg
    let size = fromJust $ mSize <|> cfgSectionSize cfg
    let (Spacing (Pt beforeSpace) (Pt afterSpace)) = fromJust $ cfgSectionSpacing cfg

    -- Typeset header
    typesetVText vText font size JustifyLeft beforeSpace afterSpace

-- Typesets the given header.
typesetSubsection :: [VText] -> Maybe Font -> Maybe Datatypes.ValidatedTokens.FontSize -> Typesetter ()
typesetSubsection vText mFont mSize = do
    cfg <- gets config

    let font = fromJust $ mFont <|> cfgFont cfg
    let size = fromJust $ mSize <|> cfgSectionSize cfg
    let (Spacing (Pt beforeSpace) (Pt afterSpace)) = fromJust $ cfgSectionSpacing cfg

    typesetVText vText font size JustifyLeft beforeSpace afterSpace