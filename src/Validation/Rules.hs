{-# LANGUAGE OverloadedStrings #-}

module Validation.Rules 
    ( validateCommand
    , validateConfig
    , convertMeta
    ) where

import Validation.Schema
import Datatypes.Located
import Datatypes.ParseTokens
import Datatypes.ValidatedTokens
import Common

import Control.Applicative
import Control.Monad

import Data.Text (Text)
import Data.Validation
import Data.Functor ((<&>))
import qualified Data.Text as T
import qualified Data.List as L

import GHC.Float (double2Int)
import System.FilePath (isValid)
import Text.Megaparsec (SourcePos)


type CommandValidationType = Validation [String] VComm


--------------------
-- ENUM LISTS
--------------------
-- Used for validation of options with multiple possible values.
fonts :: [(Text, Font)]
fonts = [("helvetica", Helvetica), ("courier", Courier), ("times", Times)]

pageSizes :: [(Text, PageSize)]
pageSizes = [("a4", SizeA4), ("a3", SizeA3), ("legal", SizeLegal)]

pageNumberings :: [(Text, PageNumbering)]
pageNumberings = [("arabic", NumberingArabic), ("roman", NumberingRoman), ("none", NumberingNone)]

justifications :: [(Text, Justification)]
justifications = [("left", JustifyLeft), ("right", JustifyRight), ("center", JustifyCenter), ("full", JustifyFull)]

listStyles :: [(Text, ListStyle)]
listStyles = [("bullet", ListBullet), ("square", ListSquare), ("arrow", ListArrow), ("number", ListNumber)]


--------------------
-- CONFIGURATION VALIDATION
--------------------
-- Helper setters to modify nested parts of VConfig. The function they take specifies which attribute will be modified.
setLayout :: (LayoutConfig -> LayoutConfig) -> VConfig
setLayout f = emptyVConfig { layout = f (layout emptyVConfig) }

setStyle :: (StyleConfig -> StyleConfig) -> VConfig
setStyle f = emptyVConfig { styles = f (styles emptyVConfig) }

setSize :: (SizeConfig -> SizeConfig) -> VConfig
setSize f = emptyVConfig { sizes = f (sizes emptyVConfig) }

setSpacing :: (SpacingConfig -> SpacingConfig) -> VConfig
setSpacing f = emptyVConfig { spacing = f (spacing emptyVConfig) }

setToggle :: (ToggleConfig -> ToggleConfig) -> VConfig
setToggle f = emptyVConfig { toggles = f (toggles emptyVConfig) }

-- General schemas, there are several options that share the same values, avoids repetition.
schemaSize :: (FontSize -> VConfig) -> Schema VConfig
schemaSize setter = setter <$> requireNumberWith "size" (validateNumInst (> 0) FontSize) "Size must be positive"

schemaSpacing :: (Spacing -> VConfig) -> Schema VConfig
schemaSpacing setter = setter <$> (Spacing <$> (Pt <$> requireNumber "before") <*> (Pt <$> requireNumber "after"))

schemaMargin :: (Pt -> VConfig) -> Schema VConfig
schemaMargin setter = setter <$> requireNumberWith "margin" (validateNumInst (> 0) Pt) "Margin must be positive"

schemaNumbering :: (Bool -> VConfig) -> Schema VConfig
schemaNumbering setter = setter <$> requireBool "numbering"

-- Configuration validator, takes a parsed configuration and returns the corresponding validated configuration. It's worth noting that a
-- certain level of repetition is unavoidable, however it has been minimized as much as possible. 
validateConfig :: Located PConfig -> Validation [LocatedError] (Located VConfig)
validateConfig (Located pos (PConfig arg opt)) = withPos pos $ 
    case arg of
        -- Layout rules.
        PSize -> runSchema (choiceSchema 
             [ ensureValidKeys "Invalid page size" ["size"] 
                (requireTextWith "size" (validateEnum pageSizes) "Unknown page size" <&> \x -> setLayout (\c -> c { pageSize = Just x }))
             , ensureValidKeys "Invalid custom size" ["width", "height"]
                (SizeCustom <$> requireNumberWith "width" (validateNumInst (> 0) Pt) "Width > 0" 
                            <*> requireNumberWith "height" (validateNumInst (> 0) Pt) "Height > 0" 
                            <&> \x -> setLayout (\c -> c { pageSize = Just x }))
             ]) (getOpts opt)

        PPagenumbering -> simpleSchema ["numbering"] 
            (requireTextWith "numbering" (validateEnum pageNumberings) "Unknown numbering" <&> \x -> setLayout (\c -> c { numbering = Just x }))

        PVerMargin  -> simpleSchema ["margin"] (schemaMargin (\x -> setLayout (\c -> c { marginVert = Just x })))
        PHozMargin  -> simpleSchema ["margin"] (schemaMargin (\x -> setLayout (\c -> c { marginHoz = Just x })))

        -- Style rules.
        PFont -> simpleSchema ["font"] 
            (requireTextWith "font" (validateEnum fonts) "Unknown font" <&> \x -> setStyle (\c -> c { font = Just x }))
        
        PJustification -> simpleSchema ["justification"] 
            (requireTextWith "justification" (validateEnum justifications) "Unknown justification" <&> \x -> setStyle (\c -> c { justification = Just x }))

        PListstyle -> simpleSchema ["style"]
            (requireTextWith "style" (validateEnum listStyles) "Unknown list style" <&> \x -> setStyle (\c -> c { listType = Just x }))

        -- Size rules.
        PParsize        -> simpleSchema ["size"] (schemaSize (\x -> setSize (\c -> c { paragraphSize = Just x })))
        PTitleSize      -> simpleSchema ["size"] (schemaSize (\x -> setSize (\c -> c { titleSize = Just x })))
        PSectionSize    -> simpleSchema ["size"] (schemaSize (\x -> setSize (\c -> c { sectionSize = Just x })))
        PSubsectionSize -> simpleSchema ["size"] (schemaSize (\x -> setSize (\c -> c { subsectionSize = Just x })))
        PVerbatimSize   -> simpleSchema ["size"] (schemaSize (\x -> setSize (\c -> c { verbatimSize = Just x })))
        
        PParIndent -> simpleSchema ["indent"] 
            (requireNumberWith "indent" (validateNumInst (> 0) Pt) "Indent > 0" <&> \x -> setSpacing (\c -> c { parIndent = Just x }))

        -- Spacing rules.
        PSectionspacing     -> simpleSchema ["before", "after"] (schemaSpacing (\x -> setSpacing (\c -> c { sectionSp = Just x })))
        PParagraphspacing   -> simpleSchema ["before", "after"] (schemaSpacing (\x -> setSpacing (\c -> c { paragraphSp = Just x })))
        PListspacing        -> simpleSchema ["before", "after"] (schemaSpacing (\x -> setSpacing (\c -> c { listSp = Just x })))
        PTablespacing       -> simpleSchema ["before", "after"] (schemaSpacing (\x -> setSpacing (\c -> c { tableSp = Just x })))
        PFigurespacing      -> simpleSchema ["before", "after"] (schemaSpacing (\x -> setSpacing (\c -> c { figureSp = Just x })))
        PVerbatimSpacing    -> simpleSchema ["before", "after"] (schemaSpacing (\x -> setSpacing (\c -> c { verbatimSp = Just x })))

        -- Toggle rules.
        PSectionNumbering   -> simpleSchema ["numbering"] (schemaNumbering (\x -> setToggle (\c -> c { sectionNumbering = Just x })))
        PFigureNumbering    -> simpleSchema ["numbering"] (schemaNumbering (\x -> setToggle (\c -> c { figureNumbering = Just x })))
        PVerbatimNumbering  -> simpleSchema ["numbering"] (schemaNumbering (\x -> setToggle (\c -> c { verbatimNumbering = Just x })))

  where
    -- If there are no options configuration validation fails.
    getOpts (POptionMap m) = m
    getOpts POptionNone    = []
    
    -- Helper to avoid code duplication, runs the schema and checks the keys.
    simpleSchema keys schema = runSchema (ensureValidKeys ("Expected: " ++ quoteList keys) keys schema) (getOpts opt)


--------------------
-- COMMAND & META VALIDATION 
--------------------
convertMeta :: DocumentMetadata -> Validation [LocatedError] ValidatedMetadata
convertMeta (DocumentMetadata t a d) = Success $ ValidatedMetadata
    (maybe Nothing (Just . convertText) t)
    (maybe Nothing (Just . convertText) a)
    (maybe Nothing (Just . convertText) d)

convertText :: [PText] -> [VText]
convertText = map cnvInner
  where
    cnvInner (PNormal t)     = VText {textCnt = t, style = Normal}
    cnvInner (PBold t)       = VText {textCnt = t, style = Bold}
    cnvInner (PItalic t)     = VText {textCnt = t, style = Italic}
    cnvInner (PEmphasised t) = VText {textCnt = t, style = Emphasised}


validateCommand :: Located PCommOpt -> Validation [LocatedError] (Located VComm)
validateCommand (Located pos comm) = withPos pos $ case comm of
    PCommOpt (PSection text) opts       -> genericFontCmd VSection text opts 
    PCommOpt (PSubsection text) opts    -> genericFontCmd VSubsection text opts
    PCommOpt (PFigure path) opts        -> namedFigure path opts
    PCommOpt (PTable rows) opts         -> namedTable rows opts
    PCommOpt (PList lst) opts           -> namedList lst opts
    PCommOpt (PParagraph txt) opts      -> namedParagraph txt opts
    PCommOpt (PVerbatim code) opts      -> namedVerbatim code opts
    PCommOpt PHLine opts                -> namedHLine opts
    PCommOpt PNewpage POptionNone       -> Success VNewpage
    PCommOpt PNewpage _                 -> Failure ["The command " ++ quote "newpage" ++ " does not accept any options"]
    _                                   -> error "INTERNAL: Attempt to validate unknown command"

-- Generic validator for options that expect font and size.
genericFontCmd cons text (POptionMap o) =
  runSchema (ensureValidKeys ("Expected some of fields " ++ quoteList ["font", "size"]) ["font", "size"]
    (cons (convertText text) 
        <$> tryTextWith "font" (validateEnum fonts) "Unknown font"
        <*> tryNumberWith "size" (validateNumInst (> 0) FontSize) "Font size must be positive"
    )) o
genericFontCmd cons text POptionNone = Success $ cons (convertText text) Nothing Nothing

namedFigure p (POptionMap o) =
    if isValid p then runSchema
        (ensureValidKeys ("Expected some of fields " ++ quoteList ["width", "caption"]) ["width", "caption"]
            (VFigure p 
                    <$> requireNumberWith "width" (validateNumInst (\n -> n > 0 && n <= 1) PageWidth) "Figure width must be between 0 and 1"
                    <*> tryText "caption"
            )) o
    else Failure ["Invalid filepath " ++ quote (T.pack p)]
namedFigure _ POptionNone = Failure ["Expected one numeric value (width)"]

namedTable :: [[[PText]]] -> POption -> CommandValidationType
namedTable cnt (POptionMap o) =
    let columnCount = runSchema (ensureValidKeys "Expected one numeric value (columns)" ["columns"] (requireNumberWith "columns" (validateNumInst (> 0) (\d -> double2Int d)) "Column number must be positive")) o
        in case columnCount of
            Success tc -> VTable <$> validateTableMatrix cnt tc <*> pure tc
            Failure e -> Failure e
  where
    validateTableMatrix :: [[[PText]]] -> Int -> Validation [String] [[[VText]]]
    validateTableMatrix c rLen =
        let cLen = map length c
            in if all (== rLen) cLen
                then Success $ map (map convertText) c
                else Failure ["Rows of different length in table"]
namedTable _ POptionNone = Failure ["Expected one numeric value (columns)"]

namedList :: [[PText]] -> POption -> CommandValidationType
namedList lst (POptionMap o) =
    runSchema
        ( ensureValidKeys
            ("Expected field " ++ quote "style" ++ " to be one of " ++ quoteList ["bullet", "square", "arrow", "number"])
            ["style"]
            ( VList (map convertText lst)
                <$> tryTextWith "style" (validateEnum listStyles) ("Unknown list style. Expected one of " ++ quoteList ["bullet", "square", "arrow", "number"])
            )
        ) o
namedList lst POptionNone = Success $ VList (map convertText lst) Nothing

namedParagraph :: [PText] -> POption -> CommandValidationType
namedParagraph txt (POptionMap o) =
    runSchema
        ( ensureValidKeys
            ("Expected some of fields " ++ quoteList ["font", "size", "justification"])
            ["font", "size", "justification"]
            ( VParagraph (convertText txt)
                <$> tryTextWith "font" (validateEnum fonts) "Unknown font"
                <*> tryNumberWith "size" (validateNumInst (> 0) FontSize) "Font size must be positive"
                <*> tryTextWith "justification" (validateEnum justifications) ("Expected field " ++ quote "justification" ++ " to be one of " ++ quoteList ["left", "right", "centred", "full"])
            )
        ) o
namedParagraph txt POptionNone = Success $ VParagraph (convertText txt) Nothing Nothing Nothing

namedVerbatim :: [Text] -> POption -> CommandValidationType
namedVerbatim code (POptionMap o) =
    runSchema
        ( ensureValidKeys
            ("Expected some of fields" ++ quoteList ["size", "numbering"])
            ["size", "numbering"]
            ( VVerbatim code
                <$> tryNumberWith "size" (validateNumInst (> 0) FontSize) "Font size must be positive")
                <*> tryBool "numbering"
        ) o
namedVerbatim code POptionNone = Success $ VVerbatim code Nothing Nothing

namedHLine :: POption -> CommandValidationType
namedHLine (POptionMap o) =
    runSchema
        ( ensureValidKeys
            ("Expected some of fields" ++ quoteList ["width", "thickness"])
            ["width", "thickness"]
            ( VHLine
                <$> requireNumberWith "width" (validateNumInst (\n -> n > 0 && n <= 1) PageWidth) "HLine width must be between 0 and 1"
                <*> tryNumberWith "thickness" (validateNumInst (> 0) Pt) "HLine thickness must be positive"
            )
        ) o
namedHLine POptionNone = Failure $ ["Expected one numeric value (width)"]