{-# LANGUAGE MultiParamTypeClasses #-}

module Typesetting.Styles where

import Graphics.PDF
import Graphics.PDF.Typesetting


-- HPDF allows definition of custom paragraph styles, this controls how the paragraph is typeset and allows for style changes.
data CustomParaStyle    = VerbatimPara Color Double Double (Maybe Double) Double
                        -- Verbatim arguments: Colour of box, margins of box, right size adjust, code offset off vertical line, gutter width.
                        -- The Maybe in the second to last argument is used to enable/disable the line and line numbering.
                        | QuotedPara
                        | UnderlinedPara
                        | NormalPara

-- We use "StandardStyle" as the second parameter since Paragraph style requires a text style.
instance ComparableStyle CustomParaStyle where
    -- Update pattern match
    isSameStyleAs (VerbatimPara _ _ _ _ _) (VerbatimPara _ _ _ _ _) = True
    isSameStyleAs QuotedPara QuotedPara = True
    isSameStyleAs UnderlinedPara UnderlinedPara = True
    isSameStyleAs NormalPara NormalPara = True
    isSameStyleAs _ _ = False

instance ParagraphStyle CustomParaStyle StandardStyle where
    -- Paragraph width with respect to the normal value.
    lineWidth (VerbatimPara _ _ a _ _) w _ = w - a
    lineWidth _ w _ = w
    
    -- Paragraph start position with respect to the normal value.
    linePosition (VerbatimPara _ _ _ _ gutter) _ _ = gutter
    linePosition _ _ _ = 0.0

    -- Interline, controls the style of interline glue added by the line braking algorithm.
    interline UnderlinedPara = Just $ \(Rectangle (xa :+ ya) (xb :+ yb)) -> do
        strokeColor black
        setWidth 0.5
        stroke $ Line xa yb xb yb
    interline _ = Nothing

    -- Paragraph change, allows for changing the content of a paragraph before the line breaking algorithm is run.
    paragraphChange s _ l = (s, l)

    -- Paragraph Style, gets the paragraph bounding box, can be used to apply additional effects.
    paragraphStyle (VerbatimPara c m _ mLineOffset gutter) = Just $ \(Rectangle (xa :+ ya) (xb :+ yb)) b -> do
        -- Note that "xa" represents the start position of the code, the box needs to be drawn from the left gutter.
        let boxLeft = xa - gutter - m
        let f = Rectangle (boxLeft :+ (ya - m)) ((xb + m) :+ (yb + m))
        
        fillColor c
        fill f

        -- Draw a vertical line relative to xa.
        case mLineOffset of
            Just offset -> do
                strokeColor (Rgb 0.6 0.6 0.6) 
                setWidth 1
                let lineX = xa - offset 
                stroke $ Line lineX (ya - m) lineX (yb + m)
            Nothing -> return ()

        b
        return ()
    paragraphStyle _ = Nothing