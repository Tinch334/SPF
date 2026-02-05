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
import qualified Data.Scientific as DS
import qualified Data.Set as S

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


--------------------
-- ERROR HANDLING FUNCTIONS
--------------------
data CustomError    = UnknownCommand Text       -- Thrown when encountering \command, where command is invalid.
                    | UnknownText Text          -- Thrown when encountering \text-type, where text-type is invalid.
                    | InvalidOptions Text       -- Thrown when a set of options has an invalid format.
                    | InvalidMeta               -- Thrown when metadata has an invalid format.
                    | InvalidConfiguration Text -- Thrown when encountering an unknown configuration option.
                    | MissingContent            -- Thrown when document has no content.
                    | MissingCloser Text        -- Thrown when a closing element is missing, included to avoid cryptic error messages
                    deriving (Eq, Ord, Show)

-- Helpers to allow for delayed errors, this allows multiple parse errors to be reported at once. The specific values returned by the
-- error handlers is not important, they are simply there because the respective parser has to return something to continue. 
recoverWith :: a -> CustomError -> Parser a
recoverWith fallback err = do
    registerFancyFailure (S.singleton (ErrorCustom err))
    return fallback

unknownCommand :: Text -> Parser PCommOpt
unknownCommand c = recoverWith 
    (PCommOpt (PParagraph [PNormal $ "\\" <> c]) POptionNone) 
    (UnknownCommand c)

unknownText :: Text -> Parser PPara
unknownText c = recoverWith 
    (PNormal ("\\" <> c)) 
    (UnknownText c)

invalidOptions :: Text -> Text -> Parser POptionPair
invalidOptions key msg = recoverWith 
    (key, PText "") 
    (InvalidOptions msg)

invalidMeta :: [PPara] -> Parser [PPara]
invalidMeta arg = recoverWith arg InvalidMeta

unknownConfiguration :: Text -> Parser PConfigArg
unknownConfiguration c = recoverWith PFont (InvalidConfiguration c)

missingContent :: [Located PConfig] -> DocumentMetadata -> Parser ParsedDocument
missingContent cfg meta = recoverWith 
    (ParsedDocument cfg meta []) 
    MissingContent

-- Modified: Returns the content parsed so far.
missingCloser :: a -> Text -> Parser a
missingCloser val name = recoverWith val (MissingCloser name)

-- Make error labelling easier.
mkErrStr :: String -> Text -> String -> String
mkErrStr b t a = b ++ (T.unpack t) ++ a

-- How each custom error will be shown.
instance ShowErrorComponent CustomError where
    showErrorComponent (UnknownCommand c) = "Unknown command: " ++ quote c
    showErrorComponent (UnknownText t) = "Unknown text type: " ++ quote t
    showErrorComponent (InvalidOptions o) = "Invalid format for options: " ++ T.unpack o
    showErrorComponent InvalidMeta = "Invalid format for metadata"
    showErrorComponent (InvalidConfiguration o) = "Unknown configuration option: " ++ quote o
    showErrorComponent MissingContent = "Empty document"
    showErrorComponent (MissingCloser c) = "Missing closing element: " ++ quote c


--------------------
-- GENERAL DEFINITIONS
--------------------
-- Parser for "Text" using custom errors.
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

-- Captures offset before an action that has an opening and closing element; If the closer is missing rewinds to start and throws error.
recoveryBetween :: Parser a -> Parser b -> Parser c -> Text -> Parser c
recoveryBetween op cl p errStr = do
    o <- getOffset
    void op
    res <- p
    mClose <- optional cl
    case mClose of
        Just _ -> return res
        Nothing -> do
            setOffset o
            missingCloser res errStr

-- Brace wrapper.
braces :: Parser a -> Parser a
braces p = recoveryBetween (char '{') (char '}') p "}"

-- Bracket wrapper.
brackets :: Parser a -> Parser a
brackets p = recoveryBetween (char '[') (char ']') p "]"

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
    sc -- Consume leading whitespace.

    cfg <- sepEndBy (withPos parseConfigCommand) sc
    meta <- parseMeta
    mDoc <- optional parseDocument

    sc -- Consume trailing whitespace.
    eof -- Ensure all file contents were parsed.

    case mDoc of
        Just doc -> return $ ParsedDocument cfg meta doc
        Nothing -> missingContent cfg meta

-- Parse metadata commands, ensuring at most one of each kind is present.
parseMeta :: Parser DocumentMetadata
parseMeta = DocumentMetadata
    <$> meta "title"
    <*> meta "author"
    <*> meta "date"
  where
    meta n = optional $ do
        void (string $ "\\" <> n)
        arg <- braces parsePPara
        o <- optional $ parseOptions
        case o of
            Nothing -> do
                void sc -- There might be blank space between metadata definitions.
                return arg
            Just _ -> invalidMeta arg

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

mkNoArgCommand :: Text -> PComm -> CommandSpec
mkNoArgCommand n c = CommandSpec n $ do
    void (string $ "\\" <> n) <?> "\\" ++ T.unpack n
    op <- optional parseOptions

    return $ PCommOpt c (maybe POptionNone id op)

mkBeginEndCommand :: Text -> Parser a -> (a -> PComm) -> CommandSpec
mkBeginEndCommand n p c = CommandSpec n $ recoveryBetween parseStart parseEnd parseBody errStr
  where
    parseStart = do
        void (string "\\begin") <?> "\\begin"
        void (braces (string n)) <?> mkErrStr "" n " for begin"

    parseBody = do
        op <- optional parseOptions
        void (optional eol) -- Consumes a single newline if present
        
        b <- p <?> mkErrStr "" n " content"
        
        return $ PCommOpt (c b) (maybe POptionNone id op)

    parseEnd = void (string "\\end") *> braces (string n)

    errStr = "\\end{" <> n <> "}"

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
    [ mkSimpleCommand       "section"       parsePPara      PSection
    , mkSimpleCommand       "subsection"    parsePPara      PSubsection
    , mkSimpleCommand       "figure"        parseFilepath   PFigure
    , mkBeginEndCommand     "paragraph"     parsePPara      PParagraph
    , mkBeginEndCommand     "table"         parseTable      PTable
    , mkBeginEndCommand     "list"          parseList       PList
    , mkBeginEndCommand     "verbatim"      parseVerbatim   PVerbatim
    , mkNoArgCommand        "newpage"       PNewpage
    , mkNoArgCommand        "hline"         PHLine
    ]


--------------------
-- COMMAND PARSING FUNCTIONS
--------------------
-- Parses a filepath, doesn't check that it's valid. Follows POSIX standard "Fully portable filenames", adding spaces.
parseFilepath :: Parser FilePath
parseFilepath = label "filepath" $ do
    t <- takeWhile1P (Just "filepath char") (\c -> DC.isAlphaNum c || elem c ("/\\-_. " :: String))
    return (T.unpack t)

-- The elements in a table row are separated by "|". A line ending is denoted by a "\\", that is two "\" characters.
parseTable :: Parser [[[PPara]]]
parseTable = label "table" $ sepEndBy1
    (lexeme $ sepBy1 parsePPara (symbol "|"))
    (do
        void $ symbol "\\break"
        void $ optional (string "{}")
        sc ) -- Consume trailing whitespace-

parseList :: Parser [[PPara]]
parseList = label "list" $ many $ do
    sc -- Consume leading whitespace.
    void $ symbol "\\item"
    void $ optional (string "{}")
    t <- parsePPara
    sc -- Consume trailing whitespace.

    return t

parseVerbatim :: Parser [Text]
parseVerbatim = label "verbatim block" $ do
    sepEndBy1 vLine eol

  where
    -- Type is needed, otherwise Haskell can't infer the type of the tokens.
    vLine :: Parser Text
    vLine = do
        -- Stop parsing upon reaching and "\end" command.
        notFollowedBy (string "\\end{verbatim}")
        -- Consume everything but newlines.
        takeWhileP (Just "code text") (`notElem` ['\n', '\r'])


--------------------
-- TEXT PARSING FUNCTIONS
--------------------
-- Parses standalone blocks of text without commands, note that paragraph modes are not commands.
parseParagraph :: Parser PCommOpt
parseParagraph = label "paragraph" $ do
        t <- parsePPara
        void (optional eol)
        return $ PCommOpt (PParagraph t) POptionNone

parsePPara :: Parser [PPara]
parsePPara = some (choice [parseSpecialText, PNormal <$> parseRawText])

-- Stores parsers for paragraph modes.
textTypesTable :: [Parser PPara]
textTypesTable = 
    [ paragraphTypeToParser "bold"      PBold
    , paragraphTypeToParser "italic"    PItalic
    , paragraphTypeToParser "emph"      PEmphasised
    , paragraphTypeToParser "quote"     PQuoted
    , parseVerbatimInline
    , parseSpecialText
    ]

-- Creates a parser for the given paragraph type.
paragraphTypeToParser :: Text -> (Text -> PPara) -> Parser PPara
paragraphTypeToParser n c = do
    void (string n) <?> mkErrStr "" n " command"
    t <- braces parseRawText <?> mkErrStr "" n " argument"
    return (c t)

-- The parsing function for a verbatim environment cannot be used because it accepts newlines.
parseVerbatimInline :: Parser PPara
parseVerbatimInline = do
    void $ string "verbatim"
    verb <- braces $ takeWhile1P (Just "verbatim content") (/= '}')
    return $ PVerbatimPara verb

parseSpecialText :: Parser PPara
parseSpecialText = do
    -- Forces parsePPara to stop if a structural command is encountered.
    notFollowedBy (choice [string "\\item", string "\\end", string "\\break"])
    -- Parse text.
    void (char '\\') <?> "escape for special text"
    choice (map try textTypesTable) <|> unknown where
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
    , PParIndent            <$ string "parindent"
    , PFont                 <$ string "font"
    , PParsize              <$ string "parsize"
    , PTitleSize            <$ string "titlesize"
    , PSectionSize          <$ string "sectionsize"
    , PSubsectionSize       <$ string "subsectionsize"
    , PVerbatimSize         <$ string "verbatimsize"
    , PJustification        <$ string "justification"
    , PListstyle            <$ string "liststyle"
    , PVerMargin            <$ string "vertmargin"
    , PHozMargin            <$ string "hozmargin"
    , PSectionNumbering     <$ string "sectionnumbering"
    , PFigureNumbering      <$ string "figurenumbering"
    , PVerbatimNumbering    <$ string "verbatimnumbering"
    , unknown               -- Detects unknown configuration commands.
    ]
  where
    unknown = do
        c <- some letterChar
        unknownConfiguration (T.pack c)


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
        Nothing -> invalidOptions (T.pack k) (T.pack $ "Missing value for key " <> k)

-- Parses an option in value form., note that both integers and floats are returned as numbers, the parsing separation is simply
-- because there's not a single parser for integers and floats.
parseOptionValue :: Parser POptionValue
parseOptionValue = label "option value" $ choice
    [ PBool <$> boolean
    , number
    , PText <$> stringLiteral
    , PText . T.pack <$> identifier
    ]

  where
    boolean = string "true" *> return True <|> string "false" *> return False

    -- Handles integrals, floating point numbers, etc automatically. This is done to avoid partial parsing errors. 
    number = do
        v <- L.scientific
        case DS.floatingOrInteger v of
            Left f -> return $ PFloat f
            Right i -> return $ PInteger i

    stringLiteral = between (char '"') (char '"') parseRawTextLine
    
    identifier = do
        t <- some alphaNumChar
        notFollowedBy (symbol ":")
        return t