{-# LANGUAGE OverloadedStrings #-}

module Parser (parseLanguage) where

import Datatypes.ParseTokens
import Datatypes.Located (Located(..))
import Common

import Control.Applicative
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as DC
import Data.Either

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


--------------------
-- ERROR HANDLING FUNCTIONS
--------------------
data CustomError    = UnknownCommand Text   -- Thrown when encountering \command, where command is invalid.
                    | UnknownText Text      -- Thrown when encountering \text-type, where text-type is invalid.
                    | InvalidOptions Text   -- Thrown when a set of options has an invalid format.
                    | OtherError Text       -- Generic constructor for other types of errors.
                    deriving (Eq, Ord, Show)

-- To allow for easy error throwing.
unknownCommand :: Text -> Parser a
unknownCommand = customFailure . UnknownCommand

unknownText :: Text -> Parser a
unknownText = customFailure . UnknownText

invalidOptions :: Text -> Parser a
invalidOptions = customFailure . InvalidOptions

otherError :: Text -> Parser a
otherError = customFailure . OtherError

-- Make error labelling easier.
mkErrStr :: String -> Text -> String -> String
mkErrStr b t a = b ++ (T.unpack t) ++ a

-- How each custom error will be shown.
instance ShowErrorComponent CustomError where
    showErrorComponent (UnknownCommand c) = "Unknown command: " ++ quote c
    showErrorComponent (UnknownText t) = "Unknown text type: " ++ quote t
    showErrorComponent (InvalidOptions o) = "Invalid format for options: " ++ T.unpack o
    showErrorComponent (OtherError t) = "Error during parsing: " ++ quote t
        

--------------------
-- GENERAL DEFINITIONS
--------------------
-- Parser for Text using custom errors.
type Parser = Parsec CustomError Text

-- Characters that have special meanings and should not be considered as regular characters for parsing.
specialCharacters :: [Char]
specialCharacters = ['\\', '{', '}']

newlineCharacters :: [Char]
newlineCharacters = ['\n', '\r', '\036']

sc :: Parser ()
sc = L.space
  space1                           -- Consumes at least one whitespace.
  (L.skipLineComment "//")         -- Single line comments.
  (L.skipBlockComment "/*" "*/")   -- Multiline comments.

-- Instantiated versions of lexeme and symbol.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc


--------------------
-- GENERAL PARSING FUNCTIONS
--------------------
parseLanguage :: Parser PLocatedLang
parseLanguage = do
    cfg <- sepEndBy (parseCommandOption preDocumentCommandTable) sc
    doc <- parseDocument
    return (cfg ++ doc)

parseDocument :: Parser PLocatedLang
parseDocument = sepEndBy1 documentOptions sc where
    documentOptions = choice 
        [ lexeme (parseCommandOption (beginEndCommandTable ++ documentCommandTable))
        , lexeme parseParagraph ]
    

--------------------
-- COMAMND PARSING FUNCTIONS
--------------------
-- In order to streamline the command parser we use a spec for each command.
data CommandSpec = CommandSpec
    Text                -- Name of the command, without backslash.
    (Parser PCommOpt)   -- The parser for the command's argument.

commandBackslash :: Parser ()
commandBackslash = void (char '\\') <?> "backslash before command"

-- Creates a CommandSpec, this is then used to parse the commands.
simpleSpecMake :: Text -> Parser a -> (a -> PComm) -> CommandSpec
simpleSpecMake n p f = CommandSpec n $ do
    commandBackslash
    void (string n) <?> T.unpack n
    arg <- between (char '{') (char '}') p <?> mkErrStr "" n " argument"
    void sc
    op <- lexeme $ optional parseOptions
    case op of
        Nothing -> return (PCommOpt (f arg) POptionNone)
        Just l -> return (PCommOpt (f arg) l)

simpleNoArgSpecMake :: Text -> PComm -> CommandSpec
simpleNoArgSpecMake n f = CommandSpec n $ do
    commandBackslash
    void (string n) <?> T.unpack n
    void sc
    op <- lexeme $ optional parseOptions
    case op of
        Nothing -> return (PCommOpt f POptionNone)
        Just l -> return (PCommOpt f l)

beginEndSpecMake :: Text -> Parser a -> (a -> PComm) -> CommandSpec
beginEndSpecMake n p f = CommandSpec n $ do
    void (string "\\begin") <?> "\\begin"
    void (between (char '{') (char '}') (string n)) <?> mkErrStr "" n " for begin"
    void sc -- There can be space between the closing brace and the opening bracket.
    op <- lexeme $ optional parseOptions
    b <- p <?> mkErrStr "" n " argument"
    void (string "\\end") <?> "\\end"
    void (between (char '{') (char '}') (string n)) <?> mkErrStr "" n " for end"
    case op of
        Nothing -> return (PCommOpt (f b) POptionNone)
        Just l -> return (PCommOpt (f b) l)

-- Parses a command and it's options.
parseCommandOption :: [CommandSpec] -> Parser (Located PCommOpt)
parseCommandOption lst = do
    pos <- getSourcePos
    comm <- label "command" $ (choice (map (try . (\(CommandSpec n p) -> p <?> T.unpack n)) lst)) <|> try unknown
    return (Located pos comm) where
    -- Runs after all other commands are tried, detects invalid commands.
    unknown = do
        commandBackslash
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
    [ beginEndSpecMake  "paragraph"     parsePText  PParagraph
    , beginEndSpecMake  "table"         parseTable      PTable
    , beginEndSpecMake  "list"          parseList       PList ]


--------------------
-- TEXT PARSING FUNCTIONS
--------------------
-- Parses standalone blocks of text without commands, note that text modes are not commands.
parseParagraph :: Parser (Located PCommOpt)
parseParagraph = label "paragraph" $ do
        pos <- getSourcePos
        t <- parsePText
        void (optional eol)
        return $ Located pos (PCommOpt (PParagraph t) POptionNone)

parsePText :: Parser [PText]
parsePText = Text.Megaparsec.some (choice [parseSpecialPText, PNormal <$> parseRawTextParagraph])

-- Similar idea to CommandSpec, simplified to the valid text types. In this case a quantifier isn't necessary, because all constructors take
-- a string as their argument.
data TextType = TextType Text (Text -> PText)

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
    t <- between (char '{') (char '}') parseRawTextParagraph <?> mkErrStr "" n " argument"
    return (c t)

parseSpecialPText :: Parser PText
parseSpecialPText = do
    void (char '\\') <?> "escape for special text"
    choice (map (try . textTypeToParser) textTypesTable) <|> unknown where
        unknown = do
            c <- Text.Megaparsec.some letterChar
            unknownText (T.pack c)

-- Parsing raw text is done one character at a time, this parser stops after hitting an empty line. Follows LaTeX style paragraph separation.
parseRawTextParagraph :: Parser Text
parseRawTextParagraph = label "raw paragraph" $ do
    l <- sepEndBy1 parseRawTextLine singleNewline
    return (foldl mappend "" l) -- Additional spaces are not trimmed, this is done later depending on the type of text.

parseRawTextLine :: Parser Text
parseRawTextLine = takeWhile1P (Just "raw line") (\c -> notElem c (specialCharacters ++ newlineCharacters))

-- Succeeds when a single newline is present.
singleNewline :: Parser ()
singleNewline = try $ parseNewline *> notFollowedBy parseNewline where
    parseNewline = choice
        [ string "\r\n"
        , string "\n\r"
        , string "\n"
        , string "\r" ]

-- Parses configuration options.
parseConfig :: Parser PConfigOption
parseConfig = label "config option" $ choice
    [ PSize             <$ string "size"
    , PPagenumbering    <$ string "pagenumbering"
    , PTitlespacing     <$ string "titlespacing"
    , PParagraphspacing <$ string "paragraphspacing"
    , PListspacing      <$ string "listspacing"
    , PTablespacing     <$ string "tablespacing"
    , PFigurespacing    <$ string "figurespacing"
    , PSpacingglue      <$ string "spacingglue"
    , PTextglue         <$ string "textglue"
    , PFont             <$ string "font"
    , PParsize          <$ string "parsize"
    , PTitleSize        <$ string "titlesize"
    , PSectionSize      <$ string "sectionsize"
    , PSubsectionSize   <$ string "subsectionsize"
    , PJustification    <$ string "justification"
    , PListstyle        <$ string "style" ]

-- Parses a filepath, doesn't check that it's valid. Follows POSIX standard "Fully portable filenames", adding spaces.
parseFilepath :: Parser FilePath
parseFilepath = label "filepath" $ do
    t <- takeWhile1P (Just "filepath char") (\c -> DC.isAlphaNum c || elem c ("/\\-_. " :: String))
    return (T.unpack t)

-- The elements in a table row are separated by "|". A line ending is denoted by a "\\", that is two "\" characters.
parseTable :: Parser [[[PText]]]
parseTable = label "table" $ sepEndBy1 (sepBy1 parsePText (symbol "|")) (symbol "\\\\")

parseList :: Parser [[PText]]
parseList = label "list" $ sepBy1 parsePText (symbol "\\item{}")


--------------------
-- OPTIONS PARSING FUNCTIONS
--------------------
-- Parses the options of a command. To avoid a errors the map parser must go first, otherwise in the case of a map the value parser would
--read the key, commit it as "OVText" and then try to read a ":" causing an error.
parseOptions :: Parser POption
parseOptions = label "options" $ between (symbol "[") (symbol "]") $ do
    l <- parseOptionList
    -- Check that all options are of the same type, otherwise throw an error.
    return $ POptionMap l

-- Parses all elements in the list, regardless of type.
parseOptionList :: Parser [POptionPair]
parseOptionList = label "option" $ sepBy1 (lexeme parseOptionMap) (symbol ",")

-- Parses an option in map form.
parseOptionMap :: Parser POptionPair
parseOptionMap = label "option pair" $ do
    k <- lexeme $ Text.Megaparsec.some letterChar
    -- Check for ":" if not present throw error.
    colon <- optional (symbol ":")
    case colon of
        Just _ -> do
            v <- lexeme parseOptionValue
            return $ (T.pack k, v)
        Nothing -> invalidOptions $ T.pack ("Missing value for key " ++ quote (T.pack k))

-- Parses an option in value form., note that both integers and floats are returned as numbers, the parsing separation is simply
-- because there's not a single parser for integers and floats.
parseOptionValue :: Parser POptionValue
parseOptionValue = label "option value" $ choice
    [ try $ PNumber <$> L.float -- Goes first otherwise a float might be interpreted as a decimal number and committed, leaving a ".".
    , PNumber . fromIntegral <$> L.decimal
    , do
        t <- Text.Megaparsec.some letterChar
        notFollowedBy (symbol ":") -- Avoids situations where there's a value and colon followed by nothing, for example "key:".
        return (PText $ T.pack t) ]