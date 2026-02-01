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

import System.Exit                          (exitFailure)
import System.IO.Error                      (tryIOError, ioeGetErrorType, ioeGetFileName)
import System.FilePath                      (addExtension, dropExtension)

import           Data.Map                   (Map)
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import qualified Data.Validation            as V

import qualified Text.Colour                as TC
import qualified Text.Megaparsec            as MP
import           Options.Applicative
import qualified Options.Applicative.Simple as OPS


data Options = Options
    { verbose   :: Bool
    , debug     :: Bool
    , inFile    :: FilePath
    , outFile   :: Maybe FilePath
    }


--------------------
-- ARGUMENT PARSER FUNCTIONS
--------------------
verboseParser :: Parser Bool
verboseParser = switch ( long "verbose"
    <> short 'v'
    <> help "Show additional information during file compilation" )

debugParser :: Parser Bool
debugParser = switch ( long "debug"
    <> short 'd'
    <> help "Display element bounds on PDF" )

inFileParser :: Parser FilePath
inFileParser = argument str ( metavar "FILENAME"
    <> help "File to be compiled" )

outFileParser :: Parser (Maybe FilePath)
outFileParser = optional $ strOption ( metavar "FILENAME"
    <> long "output"
    <> short 'o'
    <> help "File where the compiled PDF will be stored" )

optionParser :: Parser Options
optionParser =
  Options
    <$> verboseParser
    <*> debugParser
    <*> inFileParser
    <*> outFileParser


--------------------
-- AUXILIARY FUNCTIONS
--------------------
-- Prints contents based on the verbose flag, the first string is used in the verbose case.
logStep :: Show a => Bool -> a -> String -> String -> IO ()
logStep True m sv _ = do
    putStrLn sv
    print m
    putStrLn ""
logStep False _ _ snv = putStrLn snv

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

-- Print the text "ERROR" in red followed by the given error message.
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
showIOError e =
    let reason = "\nReason: " ++ show (ioeGetErrorType e)
    in case ioeGetFileName e of
        Nothing -> "An IO error occurred" ++ reason
        Just f -> "The file " ++ C.quote (T.pack f) ++ " could not be accessed" ++ reason


--------------------
-- FLOW CONTROL FUNCTIONS
--------------------
handleIO :: Either IOError a -> IO a
handleIO (Left err) = printError (showIOError err) >> exitFailure
handleIO (Right res) = return res

handleParser :: (MP.VisualStream s, MP.TraversableStream s, MP.ShowErrorComponent e) => Either (MP.ParseErrorBundle s e) a -> IO a
handleParser (Left err) = do
        printError "File could not be parsed:"
        putStr (MP.errorBundlePretty err)
        exitFailure
handleParser (Right val) = return val

handleValidation :: T.Text -> V.Validation [L.LocatedError] a -> IO a
handleValidation contents (V.Failure errs) = do
    printError "File contains invalid elements:"
    mapM_ (printLocatedError contents) errs
    exitFailure
handleValidation _ (V.Success val) = return val


--------------------
-- MAIN FUNCTION
--------------------
main :: IO ()
main = do
    -- No commands used, second argument can be discarded.
    (opts, ()) <- OPS.simpleOptions "0.1.5.4" "SPF" "A simple document preparation system, using a DSL inspired by LaTeX" optionParser empty 
    runCompiler opts

runCompiler :: Options -> IO ()
runCompiler Options{..} = do
    -- Read file.
    contents <- (tryIOError $ TIO.readFile inFile) >>= handleIO

    -- Parse file.
    parsedContents <- handleParser $ MP.runParser P.parseLanguage inFile contents
    logStep verbose parsedContents "Parsed contents\n===============" "File parsed"

    -- Validate file.
    validatedContents <- handleValidation contents $ VD.validateDocument parsedContents
    logStep verbose validatedContents "Validated contents\n==================" "File validated"

    -- Load resources
    resources <- R.loadResources (VT.vContent validatedContents) inFile >>= handleValidation contents
    logStepMap verbose resources "Loaded resources\n================" "Resources loaded"
    -- Load fonts, note that font loading cannot fail, save for an internal error.
    fonts <- R.loadFonts

    -- Typeset document.
    let outPath = maybe (addExtension (dropExtension inFile) C.outputExtension) id outFile
    TS.typesetDocument validatedContents resources fonts outPath debug                
    putStrLn $ "Compilation succeeded, result in " ++ (C.quote $ T.pack outPath)