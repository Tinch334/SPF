{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where


import qualified Common                     as C
import qualified Datatypes.Located          as L
import qualified Datatypes.ValidatedTokens  as VT
import qualified Parser                     as P
import qualified Resources                  as R
import qualified Typesetting.Typesetting    as TS
import qualified Validation.Document        as VD

import System.IO.Error (IOError, tryIOError)
import System.FilePath (addExtension, dropExtension)
import GHC.Internal.IO.Exception as IIE

import           Data.Map             (Map)
import qualified Data.Map             as M
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified Data.Validation      as V

import qualified Text.Colour          as TC
import qualified Text.Megaparsec      as MP
import           Options.Applicative
import qualified Options.Applicative.Simple as OPS


data Options = Options
    {   verbose :: Bool
    ,   inFile  :: FilePath
    ,   outFile :: Maybe FilePath
    }


--------------------
-- ARGUMENT PARSER FUNCTIONS
--------------------
verboseParser :: Parser Bool
verboseParser = switch ( long "verbose"
    <> short 'v'
    <> help "Show additional information during file compilation" )

inFileParser :: Parser FilePath
inFileParser = argument str ( metavar "FILENAME"
    <> help "File to be compiled")

outFileParser :: Parser (Maybe FilePath)
outFileParser = optional $ strOption ( metavar "FILENAME"
    <> long "output"
    <> short 'o'
    <> help "File where the compiled PDF will be stored")

optionParser :: Parser Options
optionParser =
  Options
    <$> verboseParser
    <*> inFileParser
    <*> outFileParser


--------------------
-- AUXILIARY FUNCTIONS
--------------------
-- Prints contents based on the verbose flag, the first string is used in the verbose case.
logStep :: Show a => Bool -> a -> String -> String -> IO ()
logStep True items header _ = do
    putStrLn header
    print items
    putStrLn ""
logStep False _ _ msg = putStrLn msg

-- Prints the keys of maps with "String" keys.
logStepMap :: Bool -> Map String b -> String -> String -> IO ()
logStepMap v m sv snv = if v
    then do
        putStrLn sv
        mapM_ (putStrLn . C.quote . T.pack) (M.keys m)
        putStrLn ""
    else putStrLn snv

-- Print text in the given foreground and background colours.
printColourText :: TC.Colour -> TC.Colour -> T.Text -> IO ()
printColourText fg bg t = let ct = TC.fore fg (TC.back bg $ TC.chunk t) in
    TC.putChunksUtf8With TC.With8Colours [ct]

-- Print the text "ERROR" in red
printError :: String -> IO ()
printError e = printColourText TC.red TC.black "ERROR" *> putStrLn (" - " ++ e)

-- Prints a located error nicely.
printLocatedError :: T.Text -> L.LocatedError -> IO ()
printLocatedError fileContents (L.LocatedError pos err) = do
    let lineIdx = MP.unPos (MP.sourceLine pos)
        allLines = T.lines fileContents
        lineStr = show lineIdx
        padding = replicate (length lineStr + 1) ' '
    
    putStrLn $ MP.sourceName pos ++ ":" ++ lineStr
    putStrLn $ padding ++ "|"
    -- Line shouldn't be out of range, check just in case.
    case drop (lineIdx - 1) allLines of
        (line:_) -> putStrLn $ lineStr ++ " | " ++ T.unpack (T.strip line)
        _        -> putStrLn $ lineStr ++ " | [Line out of range]"
    putStrLn $ padding ++ "|"
    putStrLn err    

-- Haskell definition of "show" for "IO error" does not follow the style of the rest of the program.
showIOError :: IOError -> String
showIOError e = let reason = "\nReason: " ++ show (IIE.ioe_type e) in
    case IIE.ioe_filename e of
        Nothing -> "An IO error occurred" ++ reason
        Just f -> "The file " ++ C.quote (T.pack f) ++ " could not be accessed" ++ reason


--------------------
-- MAIN FUNCTION
--------------------
main :: IO ()
main = do
    -- No commands used, second argument can be discarded.
    (opts, ()) <- OPS.simpleOptions "0.1.4.3" "SPF" "A simple document preparation system, using a DSL inspired in LaTeX" optionParser empty 
    runCompiler opts


runCompiler :: Options -> IO ()
runCompiler Options{..} = do
    -- Read file.
    contentRes <- tryIOError $ TIO.readFile inFile
    case contentRes of
        Left err -> printError $ showIOError err
        Right contents -> processContents contents 
        where

    -- Parse file.
    processContents contents = 
        case MP.runParser P.parseLanguage inFile contents of
            Left err -> do
                printError "File could not be parsed:"
                putStr (MP.errorBundlePretty err)
            Right parsed -> do
                logStep verbose parsed "Parsed contents\n===============" "File parsed"
                validateAndRender contents parsed

    -- Validate file.
    validateAndRender contents parsed =
        case VD.validateDocument parsed of
            V.Failure errs -> do
                printError "File contains invalid elements:"
                mapM_ (printLocatedError contents) errs
            V.Success vParsed -> do
                logStep verbose vParsed "Validated contents\n==================" "File validated"
                loadAssets contents vParsed

    -- Load resources and fonts.
    loadAssets contents vParsed = do
        resR <- R.loadResources (VT.vContent vParsed) inFile
        case resR of
            V.Failure errs -> do
                printError "Some resources could not be loaded:"
                mapM_ (printLocatedError contents) errs
            V.Success resources -> do
                fonts <- R.loadFonts
                logStepMap verbose resources "Loaded resources\n================" "Resources loaded"

                let outPath = maybe (addExtension (dropExtension inFile) C.outputExtension) id outFile -- Get output filepath.
                    completePath = C.completePath inFile outPath

                -- Fix output path
                TS.typesetDocument vParsed resources fonts outPath
                
                putStrLn $ "Compilation succeeded, result in " ++ (C.quote $ T.pack completePath)