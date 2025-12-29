module Main (main) where


-- Imports
import Lib
import qualified Constants as C
import qualified Parser as P

import System.FilePath
import System.IO.Error

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Options.Applicative
import qualified Options.Applicative.Simple as OPS

import qualified Text.Megaparsec as M


-- Data structures
data Options = Options
    {   verbose :: Bool
    ,   inFile :: FilePath
    ,   outFile :: Maybe FilePath
    }


-- Argument parser.
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


-- Helper functions.
quoteFile :: FilePath -> FilePath
quoteFile p = "\"" ++ p ++ "\""


-- Main body.
main :: IO ()
main = do
    -- No commands used, second argument can be discarded.
    (opts, ()) <- OPS.simpleOptions "0.1.0.0" "SPF" "A simple document preparation system, using a DSL inspired in LaTeX" optionParser empty

    strOrErr <- tryIOError $ TIO.readFile (inFile opts)
    case strOrErr of
        Left _ -> putStr $ "File " ++ (quoteFile $ inFile opts) ++ " could not be accessed!\n"
        --Right contents -> print $ M.runParser P.parsePText (inFile opts) contents
        Right contents -> print $ M.runParser P.parseOptions (inFile opts) contents


-- Determines the output filename, based on if it was provided as an argument.
getOutFilename :: FilePath -> Maybe FilePath -> FilePath
getOutFilename _ (Just outPath) = outPath
getOutFilename inPath Nothing = addExtension (dropExtension inPath) C.outputExtension