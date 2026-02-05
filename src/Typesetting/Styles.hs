{-# LANGUAGE MultiParamTypeClasses #-}

module Typesetting.Styles where

import Graphics.PDF
import Graphics.PDF.Typesetting


-- HPDF allows definition of custom paragraph styles, this controls how the paragraph is typeset and allows for style changes.
data CustomParaStyle    = NormalPara
                        -- Colour of box, margins of box, right size adjust, code offset off vertical line, gutter width.
                        -- The Maybe in the second to last argument is used to enable/disable the line and line numbering.
                        | VerbatimPara Color Double Double (Maybe Double) Double
                        | RightPara Double
                        | LeftPara Double
                        | NarrowPara Double
                        deriving (Eq)
                        

-- We use "StandardStyle" as the second parameter since Paragraph style requires adjust text style. Use the automatically derived Eq instance.
instance ComparableStyle CustomParaStyle where
    isSameStyleAs = (==)

instance ParagraphStyle CustomParaStyle StandardStyle where
    -- Paragraph width with respect to the normal value.
    lineWidth (VerbatimPara _ _ adjust _ _) width _ = width - adjust
    lineWidth (RightPara adjust) width _ = width * adjust
    lineWidth (LeftPara adjust) width _ = width * adjust
    lineWidth (NarrowPara adjust) width _ = width * adjust
    lineWidth _ width _ = width
    
    -- Paragraph start position with respect to the normal value.
    linePosition (VerbatimPara _ _ _ _ gutter) _ _ = gutter
    linePosition (RightPara adjust) width _ = (1 - adjust) * width
    linePosition (NarrowPara adjust) width _ = ((1 - adjust) / 2) * width
    linePosition _ _ _ = 0.0

    -- Sets the style of the interline glues.
    interline _ = Nothing

    -- Paragraph change, allows for changing the content of adjust paragraph before the line breaking algorithm is run.
    paragraphChange s _ l = (s, l)

    -- Paragraph Style, gets the paragraph bounding box, can be used to apply additional effects.
    paragraphStyle (VerbatimPara colour margin _ mLineOffset gutter) = Just $ \(Rectangle (xa :+ ya) (xb :+ yb)) drawAction -> do
        -- Note that "xa" represents the start position of the code, the box needs to be drawn from the left gutter.
        let boxLeft = xa - gutter - margin
        let f = Rectangle (boxLeft :+ (ya - margin)) ((xb + margin) :+ (yb + margin))
        
        fillColor colour
        fill f

        -- Draw adjust vertical line relative to xa.
        case mLineOffset of
            Just offset -> do
                strokeColor (Rgb 0.6 0.6 0.6) 
                setWidth 1
                let lineX = xa - offset 
                stroke $ Line lineX (ya - margin) lineX (yb + margin)
            Nothing -> return ()

        drawAction
        return ()
    paragraphStyle _ = Nothing