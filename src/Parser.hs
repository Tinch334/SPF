{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ExistentialQuantification #-}

module Parser where

import Common

import Control.Applicative
import Control.Monad

import Data.Text (Text)
import qualified Data.Char as DC
import Data.Void

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

import qualified Data.Text as T


-- No custom error handling for now.
type Parser = Parsec Void Text

-- Characters that have special meanings and should not be considered as regular characters for parsing.
specialCharacters :: [Char]
specialCharacters = ['\\', '{', '}']

--------------------
-- PARSER FUNCTIONS
--------------------
-- A generic parser for a command, checks for the structure: \<command>{argument}, \begin-\end commands and options are parsed separately.
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
data TextType = TextType String (T.Text -> PText)

textTypesTable :: [TextType]
textTypesTable = 
    [ TextType  "bold"      PBold
    , TextType  "italic"    PItalic
    , TextType  "emph"      PEmphasised
    , TextType  "verbatim"  PVerbatim
    , TextType  "quoted"    PQuoted ]

-- Creates a parser by applying the data constructor to the result of parsing the command name string; Followed by the parser for raw text.
textTypeToParser :: TextType -> Parser PText
textTypeToParser (TextType n c) = do
    void (string $ T.pack n)
    t <- between (char '{') (char '}') parseRawText
    return (c t)

parseSpecialPText :: Parser PText
parseSpecialPText = void (char '\\') *> choice (map (try . textTypeToParser) textTypesTable)

parsePText :: Parser [PText]
parsePText = do
    (do
        t <- dbg "special" parseSpecialPText
        rest <- parsePText
        return $ t:rest)
    <|>
    (do
        t <- dbg "normal" parseRawText
        rest <- parsePText
        return ((PNormal t):rest))
    <|>
        return []

-- Parsing raw text is done one character at a time, note that multiple spaces are collapsed down to one.
parseRawText :: Parser T.Text
parseRawText = T.pack <$> (Text.Megaparsec.some $ choice
    [ alphaNumChar
    , spaceChar <* space
    , satisfy (\c -> not (elem c specialCharacters) && DC.isPunctuation c) ])

-- Parses configuration options.
parseConfig :: Parser ConfigOption
parseConfig = choice
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

parseFilepath :: Parser FilePath
parseFilepath = Text.Megaparsec.some letterChar


-- Parses the options of a command.
parseOptions :: Parser POption
parseOptions = between (char '[') (char ']') $ choice 
    [ try (POptionDirect <$> parseOptionList parseOptionValue)
    , POptionMap <$> parseOptionList parseOptionsMap ]

-- Both types of option lists follow the same form, a single parser can be used; Takes a parser for the elements of the list..
parseOptionList :: Parser a -> Parser [a]
parseOptionList ip = sepBy1 ip (space *> char ',' <* space)

parseOptionsMap :: Parser OptionPair
parseOptionsMap = do
    k <- Text.Megaparsec.some letterChar
    void space
    void (char ':')
    void space
    v <- parseOptionValue
    return $ (T.pack k, v)

parseOptionValue :: Parser OptionValue
parseOptionValue = choice
    [ OVInt <$> L.decimal
    , OVFloat <$> L.float
    , OVText . T.pack <$> Text.Megaparsec.some letterChar ]
    <* notFollowedBy (char ':') -- To avoid a errors. Otherwise in the case of a map the parser would read the key, commit it as "OVText"
                                -- and then try to read a ":" causing an error.


-- \config{paragraphGlue}[0.5, 2, "Hola"]
-- \config{paragraphGlue}[min: 0.5, max: 2]