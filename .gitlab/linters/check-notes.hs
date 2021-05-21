-- This is a script that checks whether the `Note [<note name>]` references
-- across the GHC code base point to notes that actually exist.

-- XXX JB specify note format here

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase    #-}

import           Control.Category ((>>>))
import           Control.Monad.State.Strict
import           Control.Monad.Except

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

data FileLocation = FileLocation { filePath   :: Text
                                 , lineNumber :: Int
                                 }

data Line = Line { loc     :: FileLocation
                 , content :: Text
                 }

data Note = Note { noteLine :: Text -- where is the note from?
                 , noteName :: Text
                 }

data Results = Results { numNotes :: Int
                       , numRefs  :: Int
                       }

data Errors = Errors
  { errorTypeInfo :: Text
  -- ^ Some info about the type of error and why it might have been encountered
  , errorList :: NonEmpty (Maybe Line, Text)
  -- ^ A list of errors consisting of an error message and possibly a source line
  }

-- | Creates a list of parse errors for the given lines
parseErrors :: NonEmpty Text -> Errors
parseErrors = undefined

-- | Creates a list of malformed reference errors for the given lines
malformedRefs :: NonEmpty Line -> Errors
malformedRefs = undefined

-- | Creates a list of dangling reference errors for the given notes
danglingRefs :: NonEmpty Note -> Errors
danglingRefs = undefined

printErrors :: Errors -> IO ()
printErrors = undefined

main :: IO ()
main = either handleErrors success =<< checkLines . T.lines <$> T.getContents

checkLines :: MonadError Errors m => [Text] -> m Results
checkLines rawLines = do
  parsedLines <- runStage parseLine parseErrors rawLines
  notes       <- runStage parseNote malformedRefs parsedLines
  headers     <- runStage parseHeader danglingRefs parsedLines
  undefined

parseNote = undefined

runStage :: MonadError Errors m
         => (t -> Except e a)      -- ^ extract an `a` from a `t`
         -> (NonEmpty e -> Errors) -- ^ collect errors into an Errors value
         -> [t]                    -- ^ list of `t`s to extract from
         -> m [a]
runStage f collect = map (runExcept . f) >>> partitionEithers >>> \case
  ([]  , res) -> pure res
  (e:es, _  ) -> throwError $ collect (e :| es)

-- | Parses lines in the format `filePath:lineNumber:lineContent`
parseLine :: MonadError Text m => Text -> m Line
parseLine line = flip evalStateT line $ do
  filePath   <- stateful parseFilePath <* colon
  lineNumber <- stateful T.decimal     <* colon
  content    <- get
  pure Line {loc = FileLocation {..}, ..}

  where
    -- get the text from the state, parse it, return the result and put the
    -- remaining text back into the state
    stateful :: (MonadState Text m, MonadError Text m) => T.Reader a -> m a
    stateful reader = get >>=
      either (parseError . T.pack) (\(x, r) -> put r *> pure x) . reader

    colon :: (MonadState Text m, MonadError Text m) => m ()
    colon = get >>=
      maybe (parseError "Missing colon") put . (T.stripPrefix ":")

    parseFilePath :: T.Reader Text
    parseFilePath = Right . T.break (== ':')

    parseError :: MonadError Text m => Text -> m a
    parseError text = throwError $ "Parse error: " <> text <> " in the line\n" <> line

noteErrors :: [(FileLocation, Text)] -- ^ All lines that contain a note
           -> Either Errors Results
noteErrors = undefined

success :: Results -> IO ()
success Results {..} = do
  printf "OK, checked %d notes and %d references.\n" numNotes numRefs
  exitSuccess

handleErrors :: Errors -> IO ()
handleErrors = undefined
