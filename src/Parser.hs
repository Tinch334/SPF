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
import Data.Maybe (isNothing)
import GHC.Float (int2Double)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


--------------------
-- ERROR HANDLING FUNCTIONS
--------------------
data CustomError    = UnknownCommand Text   -- Thrown when encountering \command, where command is invalid.
                    | UnknownText Text      -- Thrown when encountering \text-type, where text-type is invalid.
                    | InvalidOptions Text   -- Thrown when a set of options has an invalid format.
                    deriving (Eq, Ord, Show)

-- To allow for easy error throwing.
unknownCommand :: Text -> Parser a
unknownCommand = customFailure . UnknownCommand

unknownText :: Text -> Parser a
unknownText = customFailure . UnknownText

invalidOptions :: Text -> Parser a
invalidOptions = customFailure . InvalidOptions

-- Make error labelling easier.
mkErrStr :: String -> Text -> String -> String
mkErrStr b t a = b ++ (T.unpack t) ++ a

-- How each custom error will be shown.
instance ShowErrorComponent CustomError where
    showErrorComponent (UnknownCommand c) = "Unknown command: " ++ quote c
    showErrorComponent (UnknownText t) = "Unknown text type: " ++ quote t
    showErrorComponent (InvalidOptions o) = "Invalid format for options: " ++ T.unpack o
        

--------------------
-- GENERAL DEFINITIONS
--------------------
-- Parser for Text using custom errors.
type Parser = Parsec CustomError Text

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

-- Brace wrapper.
braces :: Parser a -> Parser a
braces = between (char '{') (char '}')

-- Bracket wrapper.
brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')

-- Avoid having to manually get position for commands
withPos :: Parser a -> Parser (Located a)
withPos p = Located <$> getSourcePos <*> p


--------------------
-- GENERAL PARSING FUNCTIONS
--------------------
-- Parse the language and store the results in the appropriate structure.
parseLanguage :: Parser ParsedDocument
parseLanguage = do
    -- Retaining the configuration tokens and parsing them normally is done to reduce function definitions and duplicated code.
    cfg <- sepEndBy (withPos parseConfigCommand) sc
    meta <- parseMeta
    doc <- parseDocument

    return $ ParsedDocument
        { pdConfig = cfg
        , pdMetadata = meta
        , pdContent = doc
        }

-- Parse metadata commands, ensuring at most one of each kind is present.
parseMeta :: Parser DocumentMetadata
parseMeta = DocumentMetadata
    <$> meta "title"  PTitle
    <*> meta "author" PAuthor
    <*> meta "date"   PDate where
        meta n c = optional $ withPos $ do
            void (string $ "\\" <> n)
            arg <- braces parsePText
            maybeOp <- optional $ lexeme parseOptions
            void sc -- There might be blank space between metadata definitions.
            case maybeOp of
                Just op -> return $ PMetaOpt (c arg) op
                Nothing -> return $ PMetaOpt (c arg) POptionNone

parseDocument :: Parser [Located PCommOpt]
parseDocument = sepEndBy1 documentOptions sc where
    documentOptions = lexeme $ withPos (parseCommand documentCommandTable <|> parseParagraph)

--------------------
-- COMAMND PARSING FUNCTIONS
--------------------
-- In order to streamline the command parser we use a spec for each command.
data CommandSpec = CommandSpec
    { cmdName   :: Text             -- Name of the command, without backslash.
    , cmdParser :: Parser PCommOpt  -- The parser for the command.
    }

parseBackslash :: Parser ()
parseBackslash = void (char '\\') <?> "backslash before command"

-- Helper for a command with an argument "\comm{arg}[opts].
mkSimpleCommand :: Text -> Parser a -> (a -> PComm) -> CommandSpec
mkSimpleCommand n p c = CommandSpec n $ do
    void (string $ "\\" <> n) <?> "\\" ++ T.unpack n
    arg <- braces p <?> mkErrStr "" n " argument"
    op <- lexeme $ optional parseOptions

    case op of
        Nothing -> return (PCommOpt (c arg) POptionNone)
        Just l -> return (PCommOpt (c arg) l)

-- Helper for a command with no argument "\comm[opts].
mkNoArgCommand :: Text -> PComm -> CommandSpec
mkNoArgCommand n c = CommandSpec n $ do
    void (string $ "\\" <> n) <?> "\\" ++ T.unpack n
    op <- lexeme $ optional parseOptions

    case op of
        Nothing -> return (PCommOpt c POptionNone)
        Just l -> return (PCommOpt c l)

mkBeginEndCommand :: Text -> Parser a -> (a -> PComm) -> CommandSpec
mkBeginEndCommand n p c = CommandSpec n $ do
    void (string "\\begin") <?> "\\begin"
    braces (string n) <?> mkErrStr "" n " for begin"

    op <- lexeme $ optional parseOptions
    b <- p <?> mkErrStr "" n " content"

    void (string "\\end") <?> "\\end"
    braces (string n) <?> mkErrStr "" n " for end"

    case op of
        Nothing -> return (PCommOpt (c b) POptionNone)
        Just l -> return (PCommOpt (c b) l)

-- Parses a command and it's options.
parseCommand :: [CommandSpec] -> Parser PCommOpt
parseCommand lst = do
    label "command" $ choice [try (cmdParser l <?> T.unpack (cmdName l)) | l <- lst] <|> try unknown where
        -- Runs after all other commands are tried, detects invalid commands.
        unknown = do
            void (char '\\')
            c <- Text.Megaparsec.some letterChar
            unknownCommand (T.pack c)


-- Commands accepted in the document.
documentCommandTable :: [CommandSpec]
documentCommandTable =
    [ mkSimpleCommand       "section"       parsePText      PSection        -- Commands with arguments.
    , mkSimpleCommand       "subsection"    parsePText      PSubsection
    , mkSimpleCommand       "figure"        parseFilepath   PFigure
    , mkBeginEndCommand     "paragraph"     parsePText      PParagraph
    , mkBeginEndCommand     "table"         parseTable      PTable
    , mkBeginEndCommand     "list"          parseList       PList
    , mkNoArgCommand        "newpage"       PNewpage                        -- Commands with no arguments.
    , mkNoArgCommand        "hline"         PHLine
    ]


--------------------
-- TEXT PARSING FUNCTIONS
--------------------
-- Parses standalone blocks of text without commands, note that text modes are not commands.
parseParagraph :: Parser PCommOpt
parseParagraph = label "paragraph" $ do
        t <- parsePText
        void (optional eol)
        return $ PCommOpt (PParagraph t) POptionNone

parsePText :: Parser [PText]
parsePText = Text.Megaparsec.some (choice [parseSpecialText, PNormal <$> parseRawText])

-- Similar idea to CommandSpec, simplified to the valid text types. In this case a quantifier isn't necessary, because all constructors take
-- a string as their argument.
data TextType = TextType Text (Text -> PText)

textTypesTable :: [TextType]
textTypesTable = 
    [ TextType  "bold"      PBold
    , TextType  "italic"    PItalic
    , TextType  "emph"      PEmphasised
    ]

-- Creates a parser by applying the data constructor to the result of parsing the command name string; Followed by the parser for raw text.
textTypeToParser :: TextType -> Parser PText
textTypeToParser (TextType n c) = do
    void (string n) <?> mkErrStr "" n " command"
    t <- braces parseRawText <?> mkErrStr "" n " argument"
    return (c t)

parseSpecialText :: Parser PText
parseSpecialText = do
    -- Forces parsePText to stop if a structural command is encountered.
    notFollowedBy (choice [string "\\item", string "\\end"])
    -- Parse text.
    void (char '\\') <?> "escape for special text"
    choice (map (try . textTypeToParser) textTypesTable) <|> unknown where
        unknown = do
            c <- Text.Megaparsec.some letterChar
            unknownText (T.pack c)

-- Parsing raw text is done one character at a time, this parser stops after hitting an empty line. Follows LaTeX style paragraph separation.
parseRawText :: Parser Text
parseRawText = label "raw paragraph" $ do
    l <- sepEndBy1 parseRawTextLine singleNewline
    return (foldl mappend "" l) -- Additional spaces are not trimmed, this is done later depending on the type of text.

parseRawTextLine :: Parser Text
parseRawTextLine = takeWhile1P (Just "raw line") (\c -> notElem c (['\\', '{', '}', '\n', '\r', '\036']))

-- Succeeds when a single newline is present.
singleNewline :: Parser ()
singleNewline = try $ parseNewline *> notFollowedBy parseNewline where
    parseNewline = choice
        [ string "\r\n"
        , string "\n\r"
        , string "\n"
        , string "\r" ]

--------------------
-- CONFIGURATION PARSING FUNCTIONS
--------------------
parseConfigCommand :: Parser PConfig
parseConfigCommand = label "configuration command" $ do
    void (string "\\config") <?> "\\config"
    arg <- braces parseConfigArg <?> "config argument"
    maybeOp <- lexeme $ optional parseOptions
    case maybeOp of
        Just op -> return $ PConfig arg op
        Nothing -> return $ PConfig arg POptionNone


-- Parses configuration arguments.
parseConfigArg :: Parser PConfigArg
parseConfigArg = label "config option" $ choice
    [ PSize             <$ string "size"
    , PPagenumbering    <$ string "pagenumbering"
    , PSectionspacing   <$ string "sectionspacing"
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
parseList = label "list" $ Text.Megaparsec.many $ do
    void $ symbol "\\item{}"
    t <- parsePText
    sc -- Consume trailing whitespace.

    return t


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
    , PNumber . int2Double <$> L.decimal -- The function "fromIntegral" is not used since it performs silent truncation.
    , do
        ic <- letterChar
        t <- Text.Megaparsec.some (alphaNumChar <|> spaceChar)
        notFollowedBy (symbol ":") -- Avoids situations where there's a value and colon followed by nothing, for example "key:".
        return (PText $ T.pack $ ic:t) ]