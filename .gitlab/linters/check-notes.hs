-- This is a script thant checks whether the `Note [<note name>]` references
-- across the GHC code base point to notes that actually exist.

-- XXX JB specify note format here

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE BlockArguments     #-}

import Debug.Trace -- XXX JB

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict

import           Data.Bifunctor
import           Data.Char
import           Data.Either
import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

import           System.Exit
import           System.IO

import           Text.Parsec hiding (Error)
import           Text.Parsec.Char
import           Text.Parsec.Pos

-- comment delimiters that may appear in front of or after note headers
beginComment, endComment :: [String]
beginComment = ["--", "-- |", "{-", "#", "//", "/*"]
endComment = ["-}", "*/"]

-- XXX JB remove all the Show instances
-- | A two-line passage from a source file that looks like it might contain a
-- note, with source location
data Passage = Passage { location   :: !SourcePos
                       , content    :: !Text
                       } deriving Show

data Note = Note { notePassage :: !Passage -- where is the note from?
                 , noteName    :: !Text
                 } deriving Show

newtype Header = Header { headerNote :: Note } deriving Show

newtype Ref = Ref { refNote :: Note } deriving Show

data Results = Results { numNotes :: !Int
                       , numRefs  :: !Int
                       } deriving Show

type Parser = Parsec Text ()

data Error = Error { errorMsg :: Text
                   , errorPsg :: Maybe Passage -- Where did the error occur
                   } deriving Show

data Errors = Errors
  { errorTypeInfo :: Text
  -- ^ Some info about the type of error and why it might have been encountered
  , errorList :: NonEmpty Error
  -- ^ A list of errors consisting of an error message and the corresponding passage
  } deriving Show

parseError :: Text       -- ^ The original input received via stdin
           -> ParseError -- ^ an error produced by parsing said input
           -> Errors
parseError grepOutput err = Errors {errorTypeInfo , errorList}
  where
    errorList = Error errorMsg Nothing :| []
    errorMsg = T.unlines (T.pack (show err) : lineMsg)
    -- This allows us to display the line in which the parse error occured to
    -- the user
    -- Dropping lines from the input as a safer alternative to (!!)
    lineMsg = case drop (sourceLine (errorPos err) - 1) (T.lines grepOutput) of
      [] -> ["The corresponding line could not be found in the input. This is \
             \likely a bug in the check-notes.hs script."]
      (line:_) -> ["in the line", line]
    errorTypeInfo = "\
\Some of the provided lines could not be parsed. Please make sure that you\n\
\are running this script using the provided check-notes.sh bash script."

-- | Creates a list of malformed reference errors for the given lines
malformedRefs :: NonEmpty Passage -> Errors
malformedRefs = undefined

-- | Creates a list of dangling reference errors for the given notes
danglingRefs :: NonEmpty Note -> Errors
danglingRefs = undefined

printErrors :: Errors -> IO ()
printErrors = undefined

main :: IO ()
main = do
  hSetEncoding stdin utf8
  either handleErrors success =<< runExcept . checkNotes <$> T.getContents

-- Takes raw output from grep and checks the references and header in it
checkNotes :: MonadError Errors m => Text -> m Results
checkNotes grepOutput = do
  parsedPassages <- liftEither . first (parseError grepOutput) $
    parse pPassages "stdin" grepOutput
  -- notes          <- runStage parseNotes   malformedRefs parsedPassages
  undefined

-- runStage :: MonadError Errors m
--          => (t -> Except e a)      -- ^ extract an `a` from a `t`
--          -> (NonEmpty e -> Errors) -- ^ collect errors into an Errors value
--          -> [t]                    -- ^ list of `t`s to extract from
--          -> m [a]
-- runStage stage collect = go . partitionEithers . map (runExcept . stage)
--   where go ([]  , res) = pure res
--         go (e:es, _  ) = throwError $ collect (e :| es)

-- | Parse passages separated by grep's group separation markers (--)
pPassages :: Parser [Passage]
pPassages = pPassage `sepBy` (string "--" *> endOfLine) <* eof

-- | Parse a passage in the format
--     filePath:lineNumber:firstLine
--     filePath-lineNumber-secondLine
-- as is usual for grep output with `-n -A 1`
-- if the first and second lines are surrounded by whitespace or comment
-- delimiter, they are discarded.
pPassage :: Parser Passage
pPassage = do
  let colon = char ':'
      dash  = char '-'
  filePath <- manyTill anyChar colon
  lineNumberStr <- many1 digit <* colon
  let lineNumber = foldl' (\a d -> a * 10 + digitToInt d) 0 lineNumberStr
      restOfLine = manyTill anyChar (try pCommentEnd)
  pCommentStart
  content1 <- restOfLine
  _ <- string filePath *> dash *> string lineNumberStr *> dash
  content2 <- restOfLine
  pure $ Passage (newPos filePath lineNumber 1) (T.pack $ content1 <> " " <> content2)
  where
    nonNewLineSpaces = many (satisfy (\c -> isSpace c && c /= '\n'))
    -- pComment get rid of leading and trailing whitespace and, optionally,
    -- the provided comment delimiters
    pComment commentDelim = do
      nonNewLineSpaces
      optional $ traverse (try . string) commentDelim
      nonNewLineSpaces
    pCommentStart = pComment beginComment
    pCommentEnd = pComment endComment *> endOfLine


-- | Parses a line and produces either a header or a list of note references
-- the line contains
-- pNotes :: Parser (Either Header [Ref])
-- pNotes 

success :: Results -> IO ()
success Results {numNotes, numRefs} = do
  putStrLn $ "OK, found " ++ show numNotes ++ " and " ++ show numRefs ++ " references."
  exitSuccess

handleErrors :: Errors -> IO ()
handleErrors Errors {errorTypeInfo, errorList} = do
  forM_ errorList $ \err -> printError err *> printLnStdErr ""
  printStdErr errorTypeInfo
  where
    printStdErr = T.hPutStr stderr
    printLnStdErr = T.hPutStrLn stderr

    printError Error {errorMsg, errorPsg} = do
      printStdErr (red "error: ")
      printStdErr errorMsg
      case errorPsg of
        Nothing      -> pure ()
        Just passage -> do
          printLnStdErr "in the passage"
          printLnStdErr (content passage)

    -- surround with ANSI escape sequence for red color and back to no color
    red :: Text -> Text
    red text = "\ESC[31m" <> text <> "\ESC[0m"
