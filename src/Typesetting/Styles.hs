{-# LANGUAGE MultiParamTypeClasses #-}

module Typesetting.Styles where

import Graphics.PDF
import Graphics.PDF.Typesetting


-- HPDF allows definition of custom paragraph styles, this controls how the paragraph is typeset and allows for style changes.
data CustomParaStyle 
    = ColouredPara Color Double Double  -- Style for coloured boxes. Takes colour of the box, extra box margins and text line width adjustment.
    | NormalPara                        -- Style for standard text.

instance ComparableStyle CustomParaStyle where
    isSameStyleAs (ColouredPara _ _ _) (ColouredPara _ _ _) = True
    isSameStyleAs NormalPara NormalPara = True
    isSameStyleAs _ _ = False

-- We use "StandardStyle" as the second parameter since Paragraph style requires a text style.
instance ParagraphStyle CustomParaStyle StandardStyle where
    -- Width with respect to normal value, all paragraphs use full width.
    lineWidth (ColouredPara _ _ a) w _ = w + a
    lineWidth _ w _ = w
    
    -- Position, all paragraphs start at 0.
    linePosition _ _ _ = 0.0

    -- Interline, controls he background drawing logic.
    interline (ColouredPara c _ _) = Just $ \r -> do
        fillColor c
        strokeColor c
        fillAndStroke r
    interline NormalPara = Nothing

    -- Paragraph change, allows for changing the content of a paragraph before the line breaking algorithm is run. Just pass the style along.
    paragraphChange s _ l = (s, l)

    -- Paragraph Style, gets the paragraph bounding box, can be used to apply additional effects.
    paragraphStyle (ColouredPara c m _) = Just $ \(Rectangle (xa :+ ya) (xb :+ yb)) b -> do
        let f = Rectangle ((xa - m) :+ (ya - m)) ((xb + m) :+ (yb + m))
        fillColor c
        fill f
        b
        return ()
    paragraphStyle NormalPara = Nothing