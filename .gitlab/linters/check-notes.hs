-- This is a script that checks whether the `Note [<note name>]` references
-- across the GHC code base point to notes that actually exist.

-- XXX JB specify note format here

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ViewPatterns        #-}

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict

import           Data.Bifunctor
import           Data.Either
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

import           System.Exit
import           System.IO

import           Text.Printf

-- comment delimiters
beginComment, endComment :: [Text]
beginComment = ["--", "{-", "#", "//", "/*"]
endComment = ["-}", "*/"]

data FileLocation = FileLocation { filePath   :: !Text
                                 , lineNumber :: !Int
                                 }

newtype RawLine = RawLine { rawLine :: Text }

newtype ErrorMessage = ErrorMessage { msg :: Text }

data Line = Line { location :: !FileLocation
                 , content  :: !Text
                 }

data Note = Note { noteLine :: !Line -- where is the note from?
                 , noteName :: !Text
                 }

newtype Header = Header { headerNote :: Note }

newtype Ref = Ref { refNote :: Note }

data Results = Results { numNotes :: !Int
                       , numRefs  :: !Int
                       }

data Errors = Errors
  { errorTypeInfo :: Text
  -- ^ Some info about the type of error and why it might have been encountered
  , errorList :: NonEmpty (Either RawLine Line, ErrorMessage)
  -- ^ A list of errors consisting of an error message and either a raw or parsed line
  }

-- | Creates a list of parse errors for the given list of lines and error
-- messages
parseErrors :: NonEmpty (RawLine, ErrorMessage) -> Errors
parseErrors errorMsgs = Errors {errorTypeInfo, errorList = first Left <$> errorMsgs}
  where

    errorTypeInfo = "\
\Some of the provided lines could not be parsed. Please make sure that you\n\
\are running this script using the provided check-notes.sh bash script."

-- | Creates a list of malformed reference errors for the given lines
malformedRefs :: NonEmpty Line -> Errors
malformedRefs = undefined

-- | Creates a list of dangling reference errors for the given notes
danglingRefs :: NonEmpty Note -> Errors
danglingRefs = undefined

printErrors :: Errors -> IO ()
printErrors = undefined

main :: IO ()
main = either handleErrors success =<<
  checkLines . map RawLine . T.lines <$> T.getContents

checkLines :: MonadError Errors m => [RawLine] -> m Results
checkLines rawLines = do
  parsedLines <- runStage parseLine   parseErrors   rawLines
  notes       <- runStage parseNotes  malformedRefs parsedLines
  undefined

runStage :: MonadError Errors m
         => (t -> Except e a)      -- ^ extract an `a` from a `t`
         -> (NonEmpty e -> Errors) -- ^ collect errors into an Errors value
         -> [t]                    -- ^ list of `t`s to extract from
         -> m [a]
runStage stage collect = go . partitionEithers . map (runExcept . stage)
  where go ([]  , res) = pure res
        go (e:es, _  ) = throwError $ collect (e :| es)

-- Get the text from the state, parse it, return the result and put the
-- remaining text back into the state
statefully :: (MonadState Text m, MonadError e m, MonadReader (Text -> e) m)
           => T.Reader a -> m a
statefully parser = parser <$> get >>= \case
  Left err        -> ask >>= \errFun -> throwError . errFun . T.pack $ err
  Right (x, rest) -> put rest *> pure x

parseText :: (MonadState Text m, MonadError e m, MonadReader (Text -> e) m)
          => Text -> m ()
parseText text = statefully $ \case
  (T.stripPrefix text -> Just rest) -> Right ((), rest)
  _                                 -> Left $ "Missing " <> show (T.unpack text)

-- | Parses lines in the format `filePath:lineNumber:lineContent`
parseLine :: MonadError (RawLine, ErrorMessage) m => RawLine -> m Line
parseLine line = flip evalStateT (rawLine line) . flip runReaderT parseError $ do
  filePath   <- statefully parseFilePath <* parseText ":"
  lineNumber <- statefully T.decimal     <* parseText ":"
  content    <- get
  pure Line {location = FileLocation {filePath, lineNumber}, content}

  where
    parseFilePath :: T.Reader Text
    parseFilePath = Right . T.break (== ':')

    parseError :: Text -> (RawLine, ErrorMessage)
    parseError msg = (line, ErrorMessage $ "Parse error: " <> msg)

-- | Parses a line and produces either a header or a list of note references
-- the line contains
-- Some references may look like headers, if they are the only text in their
-- line (with the exception of comment delimiters). In those cases, this
-- function will return a Header.
-- This can potentially result in some dangling references going unnoticed, but
-- it will never lead to additional (incorrect) error messages.
parseNotes :: MonadError Line m => Line -> m (Either Header [Ref])
parseNotes Line {content} = flip evalStateT content $ do
  -- We don't care about header errors, just try to interpret the line as
  -- references instead
  (Left <$> parseHeader) `catchError` const (Right <$> parseRefs)
  where
    parseHeader :: (MonadError Line m, MonadState Text m) => m Header
    parseHeader = undefined
    -- XXX JB Why is undefined not defined with errorWithoutStackTrace
    -- also source for error says that popCallStack depends on error, but it
    -- actually depends on errorWithoutStackTrace
    -- maybe open a merge request about that
    -- and first check the commit from git blame to make sure popCallStack
    -- really did contain error at that point

    parseRefs = undefined

    parseNote :: (MonadError Line m, MonadState Text m) => m Note
    parseNote = undefined

success :: Results -> IO ()
success Results {numNotes, numRefs} = do
  printf "OK, found %d notes and %d references.\n" numNotes numRefs
  exitSuccess

handleErrors :: Errors -> IO ()
handleErrors Errors {errorTypeInfo, errorList} = do
  forM_ errorList $ \err -> printError err *> printLnStdErr ""
  printStdErr errorTypeInfo
  where
    printStdErr = T.hPutStr stderr
    printLnStdErr = T.hPutStrLn stderr

    printError (line, ErrorMessage msg) = do
      printStdErr (red msg)
      case line of
        Left RawLine {rawLine} -> do
          printLnStdErr " in the line"
          printLnStdErr rawLine
        Right Line {location, content} -> do
          printLnStdErr (locationText location)
          printLnStdErr content

    -- surround with ANSI escape sequence for red color (and back to normal
    -- color)
    red :: Text -> Text
    red text = "\ESC[31m" <> text <> "\ESC[0m"

    locationText FileLocation {filePath, lineNumber} =
      "  in file " <> filePath <> ", line " <> T.pack (show lineNumber) <> ":"
