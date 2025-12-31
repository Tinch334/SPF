{-# LANGUAGE OverloadedStrings #-}

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


--------------------
-- ERROR HANDLING FUNCTIONS
--------------------
data CustomError    = UnknownCommand T.Text         -- Thrown when encountering \command, where command is invalid.
                    | UnknownText T.Text            -- Thrown when encountering \text-type, where text-type is invalid.
                    | IncorrectMapOption T.Text     -- Thrown when options are improperly formatted in a map.
                    | OtherError T.Text             -- Generic constructor for other types of errors.
                    deriving (Eq, Ord, Show)

-- To allow for easy error throwing.
unknownCommand :: T.Text -> Parser a
unknownCommand = customFailure . UnknownCommand

unknownText :: T.Text -> Parser a
unknownText = customFailure . UnknownText

incorrectMapOption :: T.Text -> Parser a
incorrectMapOption = customFailure . IncorrectMapOption

otherError :: T.Text -> Parser a
otherError = customFailure . OtherError

-- Helper functions.
quote :: String -> String
quote t = "\"" ++ t ++ "\""

-- Make error labelling easier.
mkErrStr :: String -> T.Text -> String -> String
mkErrStr b t a = b ++ (T.unpack t) ++ a

-- How each custom error will be shown.
instance ShowErrorComponent CustomError where
    showErrorComponent (UnknownCommand c) = "Unknown command: " ++ quote (T.unpack c)
    showErrorComponent (UnknownText t) = "Unknown text type: " ++ quote (T.unpack t)
    showErrorComponent (IncorrectMapOption o) = "Invalid option shape in map: " ++ quote (T.unpack o)
    showErrorComponent (OtherError t) = "Error during parsing: " ++ quote(T.unpack t)
        

--------------------
-- GENERAL DEFINITIONS
--------------------
-- Parser for Text using custom errors.
type Parser = Parsec CustomError T.Text

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
data CommandSpec = CommandSpec
    T.Text              -- Name of the command, without backslash.
    (Parser PCommOpt)   -- The parser for the command's argument.

-- Creates a CommandSpec, this is then used to parse the commands.
simpleSpecMake :: T.Text -> Parser a -> (a -> PComm) -> CommandSpec
simpleSpecMake n p f = CommandSpec n $ do
    void (char '\\') <?> mkErrStr "backslash before " n ""
    void (string n) <?> T.unpack n
    arg <- between (char '{') (char '}') p <?> mkErrStr "" n " argument"
    void sc
    op <- optional parseOptions
    case op of
        Nothing -> return (PCommOpt (f arg) POptionNone)
        Just l -> return (PCommOpt (f arg) l)

simpleNoArgSpecMake :: T.Text -> PComm -> CommandSpec
simpleNoArgSpecMake n f = CommandSpec n $ do
    void (char '\\') <?> mkErrStr "backslash before " n ""
    void (string n) <?> T.unpack n
    void sc
    op <- optional parseOptions
    case op of
        Nothing -> return (PCommOpt f POptionNone)
        Just l -> return (PCommOpt f l)

beginEndSpecMake :: T.Text -> Parser a -> (a -> PComm) -> CommandSpec
beginEndSpecMake n p f = CommandSpec n $ do
    void (string "\\begin") <?> "\\begin"
    void (between (char '{') (char '}') (string n)) <?> mkErrStr "" n " for begin"
    void sc -- There can be space between the closing brace and the opening bracket.
    op <- optional parseOptions
    b <- p <?> mkErrStr "" n " argument"
    void (string "\\end") <?> "\\end"
    void (between (char '{') (char '}') (string n)) <?> mkErrStr "" n " for end"
    case op of
        Nothing -> return (PCommOpt (f b) POptionNone)
        Just l -> return (PCommOpt (f b) l)

-- Parses a command and it's options.
parseCommandOption :: [CommandSpec] -> Parser PCommOpt
parseCommandOption lst = label "command" $ (choice (map (try . (\(CommandSpec n p) -> p <?> T.unpack n)) lst)) <|> try unknown where
    -- Runs after all other commands are tried, detects invalid commands.
    unknown = do
        void (char '\\')
        c <- Text.Megaparsec.some letterChar
        unknownCommand (T.pack c)

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
parseParagraph = label "paragraph" $ do
        t <- parsePText
        void (optional eol)
        return (PCommOpt (PParagraph t) POptionNone)

parsePText :: Parser [PText]
parsePText = Text.Megaparsec.some (choice [parseSpecialPText, PNormal <$> parseRawTextLine])

-- Similar idea to CommandSpec, simplified to the valid text types. In this case a quantifier isn't necessary, because all constructors take
-- a string as their argument.
data TextType = TextType T.Text (T.Text -> PText)

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
    void (string n) <?> mkErrStr "" n " command"
    t <- between (char '{') (char '}') parseRawTextLine <?> mkErrStr "" n " argument"
    return (c t)

parseSpecialPText :: Parser PText
parseSpecialPText = do
    void (char '\\') <?> "escape for special text"
    choice (map (try . textTypeToParser) textTypesTable) <|> unknown where
        unknown = do
            c <- Text.Megaparsec.some letterChar
            unknownText (T.pack c)

-- Parsing raw text is done one character at a time, this parser stops after hitting a newline.
parseRawTextLine :: Parser T.Text
parseRawTextLine = label "raw text" $ do
    s <- takeWhile1P (Just "raw text") (\c -> notElem c (specialCharacters ++ newlineCharacters))
    return s -- Additional spaces are not trimmed, done later depending on the type of text.

-- Parses configuration options.
parseConfig :: Parser ConfigOption
parseConfig = label "config option" $ choice
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
parseFilepath = label "filepath" $ Text.Megaparsec.some $ choice 
    [ alphaNumChar
    , oneOf ("/\\-_." :: String)]

-- The elements in a table row are separated by "|". A line ending is denoted by a "\\", that is two "\" characters.
parseTable :: Parser [[[PText]]]
parseTable = label "table" $ sepEndBy1 (sepBy1 parsePText (symbol "|"))  (symbol "\\\\")

parseList :: Parser [[PText]]
parseList = label "list" $ sepBy1 parsePText (symbol "\\item{}")


--------------------
-- OPTIONS PARSING FUNCTIONS
--------------------
-- Parses the options of a command. To avoid a errors the map parser must go first, otherwise in the case of a map the value parser would
--read the key, commit it as "OVText" and then try to read a ":" causing an error.
parseOptions :: Parser POption
parseOptions = label "options" $ between (symbol "[") (symbol "]") $ choice 
    [ try (POptionMap <$> parseOptionList parseOptionMap) <?> "map style options"
    , POptionDirect <$> parseOptionList parseOptionValue <?> "value style options"]

-- Both types of option lists follow the same form, a single parser can be used. Takes a parser for the elements of the list.
parseOptionList :: Parser a -> Parser [a]
parseOptionList ip = sepBy1 (lexeme ip) (symbol ",")

-- Parses an option in map form.
parseOptionMap :: Parser OptionPair
parseOptionMap = label "map option pair" $ do
    k <- lexeme $ Text.Megaparsec.some letterChar
    sep <- optional (symbol ":")
    -- Check for absence of separator.
    case sep of
        Nothing -> incorrectMapOption (T.pack k)
        Just _ -> do
            v <- parseOptionValue
            return $ (T.pack k, v)

-- Parses an option in value form.
parseOptionValue :: Parser OptionValue
parseOptionValue = label "option value" $ choice
    [ try $ OVFloat <$> L.float -- Goes first otherwise a float might be interpreted as a decimal number and committed, leaving a ".".
    , OVInt <$> L.decimal
    , OVText . T.pack <$> Text.Megaparsec.some letterChar ]