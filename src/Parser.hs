{-# LANGUAGE OverloadedStrings #-}

module Parser (parseLanguage) where

import Datatypes.ParseTokens
import Datatypes.Located (Located(..))
import Common

import Control.Applicative hiding (many, some)
import Control.Monad (void)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as DC
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
                    | InvalidMeta Text      -- Thrown when metadata has an invalid format.
                    deriving (Eq, Ord, Show)

-- To allow for easy error throwing.
unknownCommand :: Text -> Parser a
unknownCommand = customFailure . UnknownCommand

unknownText :: Text -> Parser a
unknownText = customFailure . UnknownText

invalidOptions :: Text -> Parser a
invalidOptions = customFailure . InvalidOptions

invalidMeta :: Text -> Parser a
invalidMeta = customFailure . InvalidMeta

-- Make error labelling easier.
mkErrStr :: String -> Text -> String -> String
mkErrStr b t a = b ++ (T.unpack t) ++ a

-- How each custom error will be shown.
instance ShowErrorComponent CustomError where
    showErrorComponent (UnknownCommand c) = "Unknown command: " ++ quote c
    showErrorComponent (UnknownText t) = "Unknown text type: " ++ quote t
    showErrorComponent (InvalidOptions o) = "Invalid format for options: " ++ T.unpack o
    showErrorComponent (InvalidMeta o) = "Invalid format for metadata: " ++ T.unpack o


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

-- Succeeds when a single newline is present.
singleNewline :: Parser ()
singleNewline = try $ eol *> notFollowedBy eol

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

    return $ ParsedDocument cfg meta doc

-- Parse metadata commands, ensuring at most one of each kind is present.
parseMeta :: Parser DocumentMetadata
parseMeta = DocumentMetadata
    <$> meta "title"
    <*> meta "author"
    <*> meta "date"
  where
    meta n = optional $ do
        void (string $ "\\" <> n)
        arg <- braces parsePText
        o <- optional $ parseOptions
        case o of
            Nothing -> do
                void sc -- There might be blank space between metadata definitions.
                return arg
            Just _ -> invalidMeta "Metadata does not accept options"

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

-- Helper for a command with an argument "\comm{arg}[opts].
mkSimpleCommand :: Text -> Parser a -> (a -> PComm) -> CommandSpec
mkSimpleCommand n p c = CommandSpec n $ do
    void (string $ "\\" <> n) <?> "\\" ++ T.unpack n
    arg <- braces p <?> mkErrStr "" n " argument"
    op <- optional parseOptions

    return $ PCommOpt (c arg) (maybe POptionNone id op)

-- Helper for a command with no argument "\comm[opts].
mkNoArgCommand :: Text -> PComm -> CommandSpec
mkNoArgCommand n c = CommandSpec n $ do
    void (string $ "\\" <> n) <?> "\\" ++ T.unpack n
    op <- optional parseOptions

    return $ PCommOpt c (maybe POptionNone id op)

mkBeginEndCommand :: Text -> Parser a -> (a -> PComm) -> CommandSpec
mkBeginEndCommand n p c = CommandSpec n $ do
    void (string "\\begin") <?> "\\begin"
    void (braces (string n)) <?> mkErrStr "" n " for begin"

    op <- optional parseOptions
    void (optional eol) -- Consumes a single newline if present, avoids altering the contents inside the begin/end block.
    b <- p <?> mkErrStr "" n " content"

    void (string "\\end") <?> "\\end"
    void (braces (string n)) <?> mkErrStr "" n " for end"

    return $ PCommOpt (c b) (maybe POptionNone id op)

-- Parses a command and it's options.
parseCommand :: [CommandSpec] -> Parser PCommOpt
parseCommand lst = do
    label "command" $ choice [try (cmdParser l <?> T.unpack (cmdName l)) | l <- lst] <|> try unknown
  where
    -- Runs after all other commands are tried, detects invalid commands.
    unknown = do
        void (char '\\')
        c <- some letterChar
        unknownCommand (T.pack c)


-- Commands accepted in the document.
documentCommandTable :: [CommandSpec]
documentCommandTable =
    [ mkSimpleCommand       "section"       parsePText      PSection
    , mkSimpleCommand       "subsection"    parsePText      PSubsection
    , mkSimpleCommand       "figure"        parseFilepath   PFigure
    , mkBeginEndCommand     "paragraph"     parsePText      PParagraph
    , mkBeginEndCommand     "table"         parseTable      PTable
    , mkBeginEndCommand     "list"          parseList       PList
    , mkBeginEndCommand     "verbatim"      parseVerbatim   PVerbatim
    , mkNoArgCommand        "newpage"       PNewpage
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
parsePText = some (choice [parseSpecialText, PNormal <$> parseRawText])

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
    notFollowedBy (choice [string "\\item", string "\\end", string "\\break"])
    -- Parse text.
    void (char '\\') <?> "escape for special text"
    choice (map (try . textTypeToParser) textTypesTable) <|> unknown where
        unknown = do
            c <- some letterChar
            unknownText (T.pack c)

-- Parsing raw text is done one character at a time, this parser stops after hitting an empty line. Follows LaTeX style paragraph separation.
parseRawText :: Parser Text
parseRawText = label "raw paragraph" $ do
    l <- sepEndBy1 parseRawTextLine singleNewline
    return (T.unwords l) -- Space is added to account for the one that's ignored when a newline is consumed.

controlChars :: [Char]
controlChars = ['\\', '{', '}', '[', ']', '"', '/', '|', '\n', '\r', '\036']

parseRawTextLine :: Parser Text
parseRawTextLine = T.concat <$> some (try (T.singleton <$> escapedChar) <|> textBlock)
  where
    -- Consumes blocks of non control characters efficiently.
    textBlock = takeWhile1P (Just "text") (`notElem` controlChars)
    -- Allows for character escaping, matches a '\' then accepts a control character that couldn't have been read normally. Otherwise
    -- commands would never match, since they would be treated as an escaped character.
    escapedChar = char '\\' *> oneOf controlChars <?> "escaped character"

-- Parses lines containing any characters until the specified end string is found.
parseAnyText :: Text -> Parser [Text]
parseAnyText end = label "any text block" $ do
    sepEndBy1 vLine eol

  where
    -- Type is needed, otherwise Haskell can't infer the type of the tokens.
    vLine :: Parser Text
    vLine = do
        -- Stop parsing upon reaching and "\end" command.
        notFollowedBy (string end)
        -- Consume everything but newlines.
        takeWhileP (Just "code text") (`notElem` ['\n', '\r'])

--------------------
-- CONFIGURATION PARSING FUNCTIONS
--------------------
parseConfigCommand :: Parser PConfig
parseConfigCommand = label "configuration command" $ do
    void (string "\\config") <?> "\\config"
    arg <- braces parseConfigArg <?> "config argument"
    maybeOp <- lexeme $ optional parseOptions

    return $ PConfig arg (maybe POptionNone id maybeOp)

-- Parses configuration arguments.
parseConfigArg :: Parser PConfigArg
parseConfigArg = label "config option" $ choice
    [ PSize                 <$ string "size"
    , PPagenumbering        <$ string "pagenumbering"
    , PSectionspacing       <$ string "sectionspacing"
    , PParagraphspacing     <$ string "paragraphspacing"
    , PListspacing          <$ string "listspacing"
    , PTablespacing         <$ string "tablespacing"
    , PFigurespacing        <$ string "figurespacing"
    , PVerbatimSpacing      <$ string "verbatimspacing"
    , PHozMargin            <$ string "parindent"
    , PFont                 <$ string "font"
    , PParsize              <$ string "parsize"
    , PTitleSize            <$ string "titlesize"
    , PSectionSize          <$ string "sectionsize"
    , PSubsectionSize       <$ string "subsectionsize"
    , PVerbatimSize         <$ string "verbatimsize"
    , PJustification        <$ string "justification"
    , PListstyle            <$ string "style"
    , PVerMargin            <$ string "vertmargin"
    , PHozMargin            <$ string "hozmargin"
    , PSectionNumbering     <$ string "sectionnumbering"
    , PFigureNumbering      <$ string "figurenumbering"
    , PVerbatimNumbering    <$ string "verbatimnumbering"
    ]


--------------------
-- AUXILIARY PARSING FUNCTIONS
--------------------
-- Parses a filepath, doesn't check that it's valid. Follows POSIX standard "Fully portable filenames", adding spaces.
parseFilepath :: Parser FilePath
parseFilepath = label "filepath" $ do
    t <- takeWhile1P (Just "filepath char") (\c -> DC.isAlphaNum c || elem c ("/\\-_. " :: String))
    return (T.unpack t)

-- The elements in a table row are separated by "|". A line ending is denoted by a "\\", that is two "\" characters.
parseTable :: Parser [[[PText]]]
parseTable = label "table" $ sepEndBy1
    (sepBy1 parsePText (symbol "|"))
    (do
        void $ symbol "\\break"
        void $ optional (string "{}"))

parseList :: Parser [[PText]]
parseList = label "list" $ many $ do
    void $ symbol "\\item"
    void $ optional (string "{}")
    t <- parsePText
    sc -- Consume trailing whitespace.

    return t

parseVerbatim :: Parser [Text]
parseVerbatim = parseAnyText "\\end"


--------------------
-- OPTIONS PARSING FUNCTIONS
--------------------
-- Parses the options of a command. To avoid a errors the map parser must go first, otherwise in the case of a map the value parser would
--read the key, commit it as "OVText" and then try to read a ":" causing an error.
parseOptions :: Parser POption
parseOptions = label "options" $ brackets $ do
    l <- parseOptionList
    return $ POptionMap l

-- Parses all elements in the list, regardless of type.
parseOptionList :: Parser [POptionPair]
parseOptionList = label "option" $ sepBy1 (lexeme parseOptionMap) (symbol ",")

-- Parses an option in map form.
parseOptionMap :: Parser POptionPair
parseOptionMap = label "option pair" $ do
    k <- lexeme (some letterChar)
    -- Check for ":", if not present throw error.
    colon <- optional (symbol ":")
    case colon of
        Just _ -> (,) (T.pack k) <$> lexeme parseOptionValue
        Nothing -> invalidOptions $ T.pack ("Missing value for key " <> k)

-- Parses an option in value form., note that both integers and floats are returned as numbers, the parsing separation is simply
-- because there's not a single parser for integers and floats.
parseOptionValue :: Parser POptionValue
parseOptionValue = label "option value" $ choice
    [ try $ PNumber <$> L.float -- Goes first otherwise a float might be interpreted as a decimal number and committed, leaving a ".".
    , try $ PNumber . int2Double <$> L.decimal -- The function "fromIntegral" is not used since it performs silent truncation.
    , try $ PBool <$> boolean
    , try $ PText <$> stringLiteral
    , PText . T.pack <$> identifier
    ]

  where
    boolean = string "true" *> return True <|> string "false" *> return False
    stringLiteral = between (char '"') (char '"') parseRawTextLine
    identifier = do
        t <- some letterChar
        notFollowedBy (symbol ":")
        return t