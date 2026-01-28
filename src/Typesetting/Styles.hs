{-# LANGUAGE MultiParamTypeClasses #-}

module Typesetting.Styles where

import Graphics.PDF
import Graphics.PDF.Typesetting


data CustomParaStyle 
    = ColouredPara Color    -- Style for coloured boxes.
    | NormalPara            -- Style for standard text.


instance ComparableStyle CustomParaStyle where
    isSameStyleAs (ColouredPara _) (ColouredPara _) = True
    isSameStyleAs NormalPara NormalPara = True
    isSameStyleAs _ _ = False


-- We use "StandardStyle" as the second parameter since Paragraph style requires a text style.
instance ParagraphStyle CustomParaStyle StandardStyle where
    -- Width, all paragraphs use full width.
    lineWidth _ w _ = w
    
    -- Position, all paragraphs start at 0.
    linePosition _ _ _ = 0.0

    -- Interline, controls he background drawing logic.
    interline (ColouredPara c) = Just $ \r -> do
        fillColor c
        strokeColor c
        fillAndStroke r
    interline NormalPara = Nothing

    -- Paragraph change, just pass the style along.
    paragraphChange s _ l = (s, l)

    -- Paragraph Style, sets the container styling.
    paragraphStyle (ColouredPara c) = Just $ \(Rectangle (xa :+ ya) (xb :+ yb)) b -> do
        let f = Rectangle ((xa-3) :+ (ya-3)) ((xb+3) :+ (yb+3))
        fillColor c
        fill f
        b
        return ()
    paragraphStyle NormalPara = Nothing