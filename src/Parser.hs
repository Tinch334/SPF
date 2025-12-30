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

import qualified System.FilePath as FP


-- No custom error handling for now.
type Parser = Parsec Void T.Text

-- Characters that have special meanings and should not be considered as regular characters for parsing.
specialCharacters :: [Char]
specialCharacters = ['\\', '{', '}']

--------------------
-- PARSER FUNCTIONS
--------------------
-- Parses blank space and line endings.
parseSpaceAndEnding :: Parser String
parseSpaceAndEnding = Text.Megaparsec.some spaceChar

sc :: Parser ()
sc = L.space space1 empty empty   -- Consumes at least one whitespace.

-- Aliases for instantiated versions of lexeme and symbol.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

-- In order to streamline the command parser we use a spec for each command.
data CommandSpec = forall a. CommandSpec
    String          -- Name of the command, without backslash.
    (Parser a)      -- The parser for the command's argument, can be "pure ()" if it has none.
    (a -> PComm)    -- The function to construct the command given it's argument; That is to say the data constructor.

-- Creates a parser for a command without an argument.
commandNoArg :: String -> PComm -> CommandSpec
commandNoArg n c = CommandSpec n (pure ()) (\_ -> c)

commandTable :: [CommandSpec]
commandTable =
    [ CommandSpec   "config"        parseConfig     PConfig         -- Commands with arguments
    , CommandSpec   "title"         parsePText      PTitle
    , CommandSpec   "author"        parsePText      PAuthor
    , CommandSpec   "date"          parsePText      PDate
    , CommandSpec   "section"       parsePText      PSection
    , CommandSpec   "subsection"    parsePText      PSubsection
    , CommandSpec   "figure"        parseFilepath   PFigure
    , commandNoArg  "newpage"       PNewpage
    , commandNoArg  "hline"         PHLine ]

beginEndCommandTable :: [CommandSpec]
beginEndCommandTable =
    [ CommandSpec   "paragraph"     parsePText  PTextblock
    , CommandSpec   "table"         parseTable      PTable
    , CommandSpec   "list"          parseList       PList ]

-- Creates a parser by applying the data constructor to the result of parsing the command name string; Followed by the internal parser between
-- brackets.
simpleSpecToParser :: CommandSpec -> Parser PComm
simpleSpecToParser (CommandSpec n p f) = (char '\\') *> fmap f (string (T.pack n) *> between (char '{') (char '}') p)

beginEndSpecToParser :: CommandSpec -> Parser PCommOpt
beginEndSpecToParser (CommandSpec n p f) = do
    void (string "\\begin")
    void (between (char '{') (char '}') (string $ T.pack n))
    op <- optional parseOptions
    r <- p
    void (string "\\end")
    void (between (char '{') (char '}') (string $ T.pack n))
    void space -- There can be space between the closing brace and the opening bracket.
    case op of
        Nothing -> return (PCommOpt (f r) POptionNone)
        Just l -> return (PCommOpt (f r) l)

-- Parses a command and it's options. Parsing a begin/end command is attempted first since it's the more restrictive command.
parseCommandOption :: Parser PCommOpt
parseCommandOption = choice 
    [ choice (map (try . beginEndSpecToParser) commandTable)
    , try $ do
            com <- choice (map (try . simpleSpecToParser) commandTable)
            void space -- There can be space between the closing brace and the opening bracket.
            op <- optional parseOptions -- Commands can have no options.
            case op of
                Nothing -> return (PCommOpt com POptionNone)
                Just l -> return (PCommOpt com l) ]

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
        t <- parseSpecialPText
        rest <- parsePText
        return $ t:rest)
    <|>
    (do
        t <- parseRawText
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

-- The simplest possible filepath definition, will probably need tweaking. Follows POSIX "Fully portable filenames".
parseFilepath :: Parser FilePath
parseFilepath = Text.Megaparsec.some $ choice 
    [ alphaNumChar
    , oneOf ("/\\-_." :: String)]

parseTable :: Parser [[[PText]]]
parseTable = sepEndBy1 
    (sepBy1 parsePText (space *> char '|' <* space)) -- The elements in a row are separated by "|".
    (parseSpaceAndEnding *> string "\\\\" <* parseSpaceAndEnding) -- A line ending is denoted by a "\\", that is two "\" characters.

parseList :: Parser [[PText]]
parseList = sepBy1 parsePText (string "\\item{}" <* space)


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
    [ try $ OVFloat <$> L.float -- Goes first otherwise  arfloat might be interpreted as a decimal number and committed, leaving a ".".
    , OVInt <$> L.decimal
    , OVText . T.pack <$> Text.Megaparsec.some letterChar ]