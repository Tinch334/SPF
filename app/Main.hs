module Main (main) where


-- Imports
import Lib
import qualified Parser as P
import qualified Common as C
import qualified Validation.Commands as VC

import System.FilePath
import System.IO.Error

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Validation as V

import Options.Applicative
import qualified Options.Applicative.Simple as OPS

import qualified Text.Megaparsec as M


-- Data structures
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
printCnt :: Show a => Bool -> a -> String -> String -> IO ()
printCnt v c sv snv = if v
    then do
        putStrLn sv
        print c
        putStr "\n"
    else
        putStrLn snv

-- Determines the output filename, based on if it was provided as an argument.
getOutFilename :: FilePath -> Maybe FilePath -> FilePath
getOutFilename _ (Just outPath) = outPath
getOutFilename inPath Nothing = addExtension (dropExtension inPath) C.outputExtension


--------------------
-- MAIN FUNCTION
--------------------
main :: IO ()
main = do
    -- No commands used, second argument can be discarded.
    (opts, ()) <- OPS.simpleOptions "0.1.2.0" "SPF" "A simple document preparation system, using a DSL inspired in LaTeX" optionParser empty
    let ve = verbose opts

    strOrErr <- tryIOError $ TIO.readFile (inFile opts)
    case strOrErr of
        Left _ -> putStr $ "ERROR - File " ++ (C.quote $ T.pack (inFile opts)) ++ " could not be accessed!\n"
        Right contents -> case M.runParser P.parseLanguage (inFile opts) contents of
            Left e -> putStrLn "ERROR - File could not be parsed:" >> putStr (M.errorBundlePretty e)
            Right p -> (printCnt ve p "Parsed file:" "File parsed") >> case traverse VC.validateCommand p of
                V.Success vp -> printCnt ve vp "Validated file:" "File validated"
                V.Failure errs -> putStrLn "ERROR - File contains invalid elements:" <* mapM putStrLn errs