{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Parser where

import Common

import Control.Applicative
import Control.Monad

import qualified Data.Text as T
import qualified Data.Char as DC
import Data.Void

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug


-- No custom error handling for now.
type Parser = Parsec Void T.Text

-- Characters that have special meanings and should not be considered as regular characters for parsing.
specialCharacters :: [Char]
specialCharacters = ['\\', '{', '}']

newlineCharacters :: [Char]
newlineCharacters = ['\n', '\r', '\036']

--------------------
-- GENERAL PARSING FUNCTIONS
--------------------
-- 
sc :: Parser ()
sc = L.space
  space1                           -- Consumes at least one whitespace.
  (L.skipLineComment "//")         -- Single line comments.
  (L.skipBlockComment "/*" "*/")   -- Multiline comments.

-- Instantiated versions of lexeme and symbol.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc


parseLanguage :: Parser PLang
parseLanguage = do
    cfg <- sepEndBy (parseCommandOption preDocumentCommandTable) sc
    doc <- parseDocument
    return (cfg ++ doc)

parseDocument :: Parser PLang
parseDocument = sepEndBy1 (choice [lexeme (parseCommandOption (beginEndCommandTable ++ documentCommandTable)), lexeme parseParagraph]) sc where
    -- Necessary to parse standalone blocks of text without commands.
    

--------------------
-- COMAMND PARSING FUNCTIONS
--------------------
-- In order to streamline the command parser we use a spec for each command.
data CommandSpec = forall a. CommandSpec
    T.Text              -- Name of the command, without backslash.
    (Parser PCommOpt)   -- The parser for the command's argument.

-- Creates a CommandSpec, this is then used to parse the commands.
simpleSpecMake :: T.Text -> Parser a -> (a -> PComm) -> CommandSpec
simpleSpecMake n p f = CommandSpec n $ do
    void (char '\\')
    void (string n)
    arg <- between (char '{') (char '}') p
    void sc
    op <- optional parseOptions
    case op of
        Nothing -> return (PCommOpt (f arg) POptionNone)
        Just l -> return (PCommOpt (f arg) l)

simpleNoArgSpecMake :: T.Text -> PComm -> CommandSpec
simpleNoArgSpecMake n f = CommandSpec n $ do
    void (char '\\')
    void (string n)
    void sc
    op <- optional parseOptions
    case op of
        Nothing -> return (PCommOpt f POptionNone)
        Just l -> return (PCommOpt f l)

beginEndSpecMake :: T.Text -> Parser a -> (a -> PComm) -> CommandSpec
beginEndSpecMake n p f = CommandSpec n $ do
    void (string "\\begin")
    void (between (char '{') (char '}') (string n))
    void sc -- There can be space between the closing brace and the opening bracket.
    op <- optional parseOptions
    b <- p
    void (string "\\end")
    void (between (char '{') (char '}') (string n))
    case op of
        Nothing -> return (PCommOpt (f b) POptionNone)
        Just l -> return (PCommOpt (f b) l)

-- Parses a command and it's options.
parseCommandOption :: [CommandSpec] -> Parser PCommOpt
parseCommandOption lst = choice $ map (try . (\(CommandSpec _ p) -> p)) lst

-- Configuration commands are currently the only commands accepted before the document.
preDocumentCommandTable :: [CommandSpec]
preDocumentCommandTable =
    [ simpleSpecMake    "config"     parseConfig    PConfig ]

-- Commands accepted in the document.
documentCommandTable :: [CommandSpec]
documentCommandTable =
    [ simpleSpecMake        "title"         parsePText      PTitle          -- Commands with arguments.
    , simpleSpecMake        "author"        parsePText      PAuthor
    , simpleSpecMake        "date"          parsePText      PDate
    , simpleSpecMake        "section"       parsePText      PSection
    , simpleSpecMake        "subsection"    parsePText      PSubsection
    , simpleSpecMake        "figure"        parseFilepath   PFigure
    , simpleNoArgSpecMake   "newpage"       PNewpage                        -- Commands with no arguments.
    , simpleNoArgSpecMake   "hline"         PHLine ]

beginEndCommandTable :: [CommandSpec]
beginEndCommandTable =
    [ beginEndSpecMake  "paragraph"     parsePText  PTextblock
    , beginEndSpecMake  "table"         parseTable      PTable
    , beginEndSpecMake  "list"          parseList       PList ]


--------------------
-- TEXT PARSING FUNCTIONS
--------------------
parseParagraph :: Parser PCommOpt
parseParagraph = do
        t <- parsePText
        void (optional eol)
        --void (space)
        return (PCommOpt (PParagraph t) POptionNone)

parsePText :: Parser [PText]
parsePText = Text.Megaparsec.some (choice [parseSpecialPText, PNormal <$> parseRawTextLine])

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
    t <- between (char '{') (char '}') parseRawTextLine
    return (c t)

parseSpecialPText :: Parser PText
parseSpecialPText = void (char '\\') *> choice (map (try . textTypeToParser) textTypesTable)

-- Parsing raw text is done one character at a time, this parser stops after hitting a newline.
parseRawTextLine :: Parser T.Text
parseRawTextLine = do
    s <- takeWhile1P (Just "raw text") (\c -> notElem c (specialCharacters ++ newlineCharacters))
    return s -- Additional spaces are not trimmed, done later depending on the type of text.

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

-- The simplest possible filepath definition, will probably need tweaking. Follows POSIX "Fully portable filenames".
parseFilepath :: Parser FilePath
parseFilepath = Text.Megaparsec.some $ choice 
    [ alphaNumChar
    , oneOf ("/\\-_." :: String)]

-- The elements in a table row are separated by "|". A line ending is denoted by a "\\", that is two "\" characters.
parseTable :: Parser [[[PText]]]
parseTable = sepEndBy1 (sepBy1 parsePText (symbol "|"))  (symbol "\\\\")

parseList :: Parser [[PText]]
parseList = sepBy1 parsePText (symbol "\\item{}")


--------------------
-- OPTIONS PARSING FUNCTIONS
--------------------
-- Parses the options of a command. To avoid a errors the map parser must go first, otherwise in the case of a map the value parser would
--read the key, commit it as "OVText" and then try to read a ":" causing an error.
parseOptions :: Parser POption
parseOptions = between (symbol "[") (symbol "]") $ choice 
    [ try (POptionMap <$> parseOptionList parseOptionMap)
    , POptionDirect <$> parseOptionList parseOptionValue ]

-- Both types of option lists follow the same form, a single parser can be used. Takes a parser for the elements of the list.
parseOptionList :: Parser a -> Parser [a]
parseOptionList ip = sepBy1 (lexeme ip) (symbol ",")

-- Parses an option in map form.
parseOptionMap :: Parser OptionPair
parseOptionMap = do
    k <- lexeme $ Text.Megaparsec.some letterChar
    void (symbol ":")
    v <- parseOptionValue
    return $ (T.pack k, v)

-- Parses an option in value form.
parseOptionValue :: Parser OptionValue
parseOptionValue = choice
    [ try $ OVFloat <$> L.float -- Goes first otherwise a float might be interpreted as a decimal number and committed, leaving a ".".
    , OVInt <$> L.decimal
    , OVText . T.pack <$> Text.Megaparsec.some letterChar ]