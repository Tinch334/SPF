{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Parser (parseLanguage)
import Validation.Document (validateDocument)
import Datatypes.ParseTokens (ParsedDocument(..), PConfig(..), PCommOpt(..))
import Datatypes.ValidatedTokens (ValidatedDocument(..), VConfig(..))
import Datatypes.Located (Located(..), LocatedError(..))

import Data.Validation (Validation(..))
import qualified Data.Text as T
import Data.Either (isRight, isLeft)

import Text.RawString.QQ
import Text.Megaparsec (runParser, errorBundlePretty)

import Test.Hspec


--------------------
-- HELPER FUNCIONS
--------------------
-- Helper to run the parser on a text input.
parseTest :: T.Text -> Either String ParsedDocument
parseTest input = 
    case runParser parseLanguage "test" input of
        Left err -> Left (errorBundlePretty err)
        Right val -> Right val

-- Helper to run both parser and validator.
validateTest :: T.Text -> Either String ValidatedDocument
validateTest input = 
    case parseTest input of
        Left err -> Left $ "Parse Error: " ++ err
        Right parsedDoc -> 
            case validateDocument parsedDoc of
                -- Extract the message string from the error and join them, this avoids escaping issues.
                Failure errs -> Left $ unlines $ map (\(LocatedError _ msg) -> msg) errs
                Success val -> Right val


--------------------
-- MAIN VALIDATION
--------------------
main :: IO ()
main = hspec $ do
    describe "Parser" $ do
        it "Parses a minimal valid document" $ do
            let input = [r|
\config{pagesize}[size: a4]
\title{Test Doc}
\section{Hello}
|]
            parseTest input `shouldSatisfy` isRight

        it "Parses configuration commands correctly" $ do
            let input = [r|
\config{pagesize}[size: a4]
\config{font}[font: times]
Some content.
|]
            let res = parseTest input
            res `shouldSatisfy` isRight
            case res of
                Right (ParsedDocument configs _ _) -> length configs `shouldBe` 2
                _ -> fail "Expected ParsedDocument"

        describe "Invalid commands" $ do
            it "Fails on unknown configuration tags" $ do
                let input = [r| \config{size}[size: a4] |]
                parseTest input `shouldSatisfy` isLeft

            it "Fails on wrong commands" $ do
                let input = [r| \config{pagesize [size: a4] |]
                parseTest input `shouldSatisfy` isLeft

            it "Fails on unknown commands" $ do
                let input = [r| \draw{/line(1, 3)} |]
                parseTest input `shouldSatisfy` isLeft

        describe "Metadata parsing" $ do
            it "Parses valid metadata" $ do
                let input = [r|
\title{A cool title}
\author{A great author}
\date{32\/13\/2055}

Content
|]
                let res = parseTest input
                res `shouldSatisfy` isRight

            it "Fails on invalid metadata" $ do
                let input = [r|
\title{A cool title}
\abstract{A new approach to typesetting documents}
\date{32\/13\/2055}
|]
                let res = parseTest input
                res `shouldSatisfy` isLeft

        describe "Text Parsing" $ do
            it "Parses inline formatting correctly" $ do
                let input = [r| Normal text \bold{Bold} \italic{Italic} \emph{Emph} |]
                let res = parseTest input
                res `shouldSatisfy` isRight
                
            it "Handles escaped characters correctly" $ do
                let input = [r| This is a brace \{ and a backslash \\ |]
                validateTest input `shouldSatisfy` isRight

            it "Parses special characters in text" $ do
                let input = [r| User: admin_01, Price: 50.00 |]
                validateTest input `shouldSatisfy` isRight

        describe "Comments" $ do
            it "Ignores single line comments" $ do
                let input = [r|
\config{pagesize}[size: a4] // This is a comment
\section{Title}
|]
                parseTest input `shouldSatisfy` isRight

            it "Ignores block comments" $ do
                let input = [r|
\config{pagesize}[size: a4]
/* This is a 
   multiline comment
   \section{Hidden}
*/
\section{Visible}
|]
                let res = parseTest input
                res `shouldSatisfy` isRight

    describe "Validator" $ do
        describe "Configuration Validation" $ do
            it "Validates correct page size configuration" $ do
                let input = [r| \config{pagesize}[size: a4] \section{Content} |]
                validateTest input `shouldSatisfy` isRight

            it "Fails on invalid enum value for page size" $ do
                let input = [r| \config{pagesize}[size: huge] \section{Content} |]
                let res = validateTest input
                res `shouldSatisfy` isLeft
                case res of
                    Left err -> err `shouldContain` "Expected field \"size\" to be one of \"a4\", \"a3\", \"legal\""
                    _ -> return ()

            it "Fails on missing required options" $ do
                let input = [r| \config{pagesize}[width: 100] \section{Content} |] 
                let res = validateTest input
                res `shouldSatisfy` isLeft
                case res of
                     Left err -> err `shouldContain` "Invalid keys"
                     _ -> return ()

            it "Validates numeric types correctly" $ do
                let input = [r| \config{vertmargin}[margin: 50] \section{Content} |]
                validateTest input `shouldSatisfy` isRight

            it "Fails when a number is expected but text is provided" $ do
                let input = [r| \config{vertmargin}[margin: "big"] \section{Content} |]
                validateTest input `shouldSatisfy` isLeft

        describe "Command Validation" $ do
            it "Validates section command with valid options" $ do
                let input = [r| \section{Title}[size: 12] |]
                validateTest input `shouldSatisfy` isRight

            it "Fails section command with invalid option key" $ do
                let input = [r| \section{Title}[color: "red"] |]
                validateTest input `shouldSatisfy` isLeft

            it "Validates figure command with valid width" $ do
                let input = [r| \figure{test.png}[width: 0.5] |]
                validateTest input `shouldSatisfy` isRight

            it "Fails figure command if width is out of range (>1)" $ do
                let input = [r| \figure{test.png}[width: 1.5] |]
                validateTest input `shouldSatisfy` isLeft

        describe "Table Validation" $ do
            it "Validates a table with correct structure" $ do
                let input = [r| 
        \begin{table}[columns: 2]
            Cell 1 | Cell 2 \break
            Cell 3 | Cell 4 \break
        \end{table} 
        |]
                validateTest input `shouldSatisfy` isRight

            it "Fails when row cell count does not match specified columns" $ do
                let input = [r| 
        \begin{table}[columns: 2]
            Cell 1 | Cell 2 | Cell 3 \break
        \end{table} 
        |]
                let res = validateTest input
                res `shouldSatisfy` isLeft
                case res of
                    Left err -> err `shouldContain` "Rows of different length"
                    _ -> return ()
                    
            it "Fails on invalid column count" $ do
                let input = [r| \begin{table}[columns: 0] ... \end{table} |]
                validateTest input `shouldSatisfy` isLeft

        describe "Full Document Flow" $ do
            it "Compiles a complex valid document structure" $ do
                let input = [r|
\config{pagesize}[size: a4]
\config{font}[font: helvetica]
\title{Unit Test}
\author{Tester}

\section{Introduction}
\begin{paragraph}[justification: center]
Hello world.
\end{paragraph}

\begin{list}[style: bullet]
\item Item 1
\item Item 2
\end{list}

\begin{verbatim}[size: 15]
if n >= 5
    then return $ Left n
    else return $ Right ((eval n) >>= empty)
\end{verbatim}
|]
                validateTest input `shouldSatisfy` isRight