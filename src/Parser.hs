{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ExistentialQuantification #-}

module Parser where

import Common
import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

-- \config{paragraphGlue}[0.5, 2, "Hola"]
-- \config{paragraphGlue}[min: 0.5, max: 2]

-- No custom error handling for now.
type Parser = Parsec Void Text


-- Auxiliary functions
-- 


-- Parser functions
parseOptions :: Parser POption
parseOptions = undefined

-- A generic parser for a command, checks for the structure: \<command>{argument}, options and \begin-\end commands are parsed separately.
-- In order to streamline the parser we use a spec for each command.
data CommandSpec = forall a. CommandSpec
    String          -- Name of the command, without backslash.
    (Parser a)      -- The parser for the command's argument, can be "pure ()" if it has none.
    (a -> PComm)    -- The function to construct the command given it's argument.
-- Creates a parser for a command without an argument.
commandNoArg :: String -> PComm -> CommandSpec
commandNoArg n c = CommandSpec n (pure ()) (\_ -> c)

commandTable :: [CommandSpec]
commandTable =
    [ CommandSpec   "config"        parseConfig     PConfig
    , CommandSpec   "title"         parsePText      PTitle
    , CommandSpec   "author"        parsePText      PAuthor
    , CommandSpec   "date"          parsePText      PDate
    , CommandSpec   "section"       parsePText      PSection
    , CommandSpec   "subsection"    parsePText      PSubsection
    , CommandSpec   "figure"        parseFilepath   PFigure
    , commandNoArg  "newpage"       PNewpage
    , commandNoArg  "hline"         PHLine ]

-- Creates a parser by applying the data constructor to the result of parsing the command name string; Followed by the internal parser between
-- brackets.
commandToParser :: CommandSpec -> Parser PComm
commandToParser (CommandSpec n p f) = fmap f (string (T.pack n) *> between (char '}') (char '{') p)

-- Parses a command, we use the "try" in case a command matches partially.
parseCommand :: Parser PComm
parseCommand = (char '\\') *> choice (map (try . commandToParser) commandTable)


-- Similar idea to CommandSpec, simplified to the valid text types. In this case a quantifier isn't necessary, because all constructors take
-- a string as their argument.
data TextType = TextType String (String -> PText)

textTypesTable :: [TextType]
textTypesTable = 
    [ TextType  "bold"      PBold
    , TextType  "italic"    PItalic
    , TextType  "emph"      PEmphasised
    , TextType  "verbatim"  PVerbatim ]

textTypeToParser :: TextType -> Parser PText
textTypeToParser (TextType n c) = do
    void (string $ T.pack n)
    t <- between (char '}') (char '{') (Text.Megaparsec.some alphaNumChar)
    return (c t)

parseSpecialPText :: Parser PText
parseSpecialPText = void (char '\\') *> choice (map (try . textTypeToParser) textTypesTable)

parsePText :: Parser [PText]
parsePText = do
    (do
        t <- parseSpecialPText
        rest <- parsePText
        return $ t:rest)
    <|>
    (do
        t <- (Text.Megaparsec.some alphaNumChar)
        rest <- parsePText
        return ((PNormal t):rest))
    <|>
        return []


parseFilepath :: Parser FilePath
parseFilepath = Text.Megaparsec.some letterChar


parseConfig :: Parser String 
parseConfig = Text.Megaparsec.some letterChar

parseConfigOption :: Parser ConfigOption
parseConfigOption = choice
    [ Size              <$ string "size"
    , Pagenumbering     <$ string "pagenumbering"
    , Titlespacing      <$ string "titlespacing"
    , Paragraphspacing  <$ string "paragraphspacing"
    , Listspacing       <$ string "listspacing"
    , Tablespacing      <$ string "tablespacing"
    , Figurespacing     <$ string "figurespacing"
    , Spacingglue       <$ string "spacingglue"
    , Textglue          <$ string "textglue"
    , Font              <$ string "font"
    , Parsize           <$ string "parsize"
    , Titlesize         <$ string "titlesize"
    , Justification     <$ string "justification" ]


pConfig :: Parser POption
pConfig = undefined
    