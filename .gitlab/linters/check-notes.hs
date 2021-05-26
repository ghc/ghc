-- This is a script than checks whether the `Note [<note name>]` references
-- across the GHC code base point to notes that look like
-- Note [<note name>]
-- ~~~~~~~~~~~~~~~~~~
-- Example note

-- XXX JB specify note format here

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

import Debug.Trace -- XXX JB

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Bifunctor
import           Data.Char
import           Data.Either
import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           System.Exit
import           System.FilePath
import           System.IO

import           Text.Parsec hiding (Error)
import           Text.Parsec.Pos

-- comment delimiters that may appear in front of or after note headers
beginComment, endComment :: [String]
beginComment = ["--", "-- |", "{-", "#", "//", "/*"]
endComment = ["-}", "*/"]

-- | Used as parameter to some functions to determine whether line breaks
-- should be allowed
data AllowLineBreaks = WithLineBreaks | NoLineBreaks

-- XXX JB remove all the Show instances
-- | A two-line passage from a source file that looks like it might contain a
-- note, with source location
data Passage = Passage { location   :: !SourcePos
                       , content    :: !Text
                       } deriving Show

-- | This refers roughly to a string that looks like "Note [<noteName>]"
data Note = Note { notePassage :: !Passage -- where is the note from?
                 , noteName    :: !Text
                 } deriving Show

newtype Header = Header { headerNote :: Note } deriving Show

-- XXX JB
-- data Ref = Ref { refFile :: FilePath -- The file in which the referenced header is
--                , refNote :: Note } deriving Show
newtype Ref = Ref { refNote :: Note } deriving Show

data Results = Results { numNotes      :: !Int
                       , numRefs       :: !Int
                       , unusedHeaders :: ![Header]
                       } deriving Show

-- Int is used to keep track of how many lines were parsed in some contexts
type Parser m = ParsecT Text Int m

data ErrorType = StdInParseError
               | MalformedRef
               | DuplicateHeader
               | DanglingRef
               deriving (Eq, Show)

data Error = Error { errorPsg :: Maybe Passage -- Where did the error occur
                   , errorMsg :: Text
                   } deriving Show

-- | A set of filepaths arranged in inverse hierarchical order in order to
-- efficiently compare suffixes
data FilePathSet = ParentDirs (Map String FilePathSet)

data Errors = Errors
  { errorType :: ErrorType
  , errorList :: NonEmpty Error
  -- ^ A list of errors consisting of an error message and the corresponding passage
  } deriving Show

parseError :: Text       -- ^ The original input received via stdin
           -> ParseError -- ^ an error produced by parsing said input
           -> Errors
parseError grepOutput err = Errors {errorType = StdInParseError, errorList}
  where
    errorList = Error Nothing errorMsg :| []
    errorMsg = T.unlines (T.pack (show err) : lineMsg)
    -- This allows us to display the line in which the parse error occured to
    -- the user
    -- Dropping lines from the input as a safer alternative to (!!)
    lineMsg = case drop (sourceLine (errorPos err) - 1) (T.lines grepOutput) of
      [] -> ["The corresponding line could not be found in the input. This is \
             \likely a bug in the check-notes.hs script."]
      (line:_) -> ["in the line", line]

-- | Creates a list of malformed reference errors for the given lines
malformedRefs :: NonEmpty (Passage, ParseError) -> Errors
malformedRefs errs = Errors {errorType = MalformedRef, errorList}
  where
    errorList = uncurry Error . bimap Just (T.pack . show) <$> errs

duplicateHeaders :: NonEmpty (NonEmpty Header) -> Errors
duplicateHeaders headers = Errors {errorType = DuplicateHeader, errorList}
  where
    makeError (Header Note {notePassage, noteName}) = Error
      (Just notePassage)
      ("Header was found more than once: [" <> T.pack (show noteName) <> "]")
    errorList = makeError <$> join headers

-- | Creates a list of dangling reference errors for the given notes
danglingRefs :: NonEmpty Note -> Errors
danglingRefs = undefined

main :: IO ()
main = do
  mapM_ (flip hSetEncoding utf8) [stdin, stdout, stderr]
  either handleErrors success =<< checkNotes <$> T.getContents

-- Takes raw output from grep and checks the references and header in it
checkNotes :: MonadError Errors m => Text -> m Results
checkNotes grepOutput = do
  passages <- liftEither . first (parseError grepOutput) $
    runParser pPassages 0 "stdin" grepOutput

  let mHeadersRefs = partitionEithers $
        map (first <$> (,) <*> (runParserT pHeadersRefs 0 "" =<< content)) passages
  (headers, refs) <- case mHeadersRefs of
    ([]  , notes) -> pure $ mconcat notes
    (e:es, _    ) -> throwError . malformedRefs $ e :| es

  -- XXX JB TODO: headers should maybe have a target file (i.e. if they say
  -- Note [some note] in GHC/blablabla) and then there can be one header with
  -- the same name per file
  let (duplicates, fmap NE.head -> uniqueSortedHeaders) =
        partition ((> 1) . length) $ NE.groupAllWith (noteName . headerNote) headers

  case duplicates of
    []   -> pure ()
    h:hs -> throwError . duplicateHeaders $ h :| hs

  -- XXX JB TODO lookup refs in map of headers. Make sure to look in the target
  -- file
  -- XXX JB We also need an additional flat header map. If a header is not
  -- found, this should check whether another header of the same name exists in
  -- different files, and display them. Probably not going to bother doing
  -- string distance checking to check for misspellings, though. I mean, it
  -- would be convenient, I'm not going to lie. Just sounds like too much work.
  -- Then again hnngg it would be nice to have

  traceShow refs undefined

-- XXX JB idea:
-- - is length . splitDirectories == 1? Then it's a path
-- - repeat splitExtensions until you're left with an empty String as
--   extension
-- - Is every "extension uppercase?
--   - If yes, is the first "module" empty?
--     - If yes, it's a path (hidden file)
--     - If not, it's a haskell module
--   - If no, it's a path
-- If it's a filepath, and arrange as [dir1, dir2, filename]
-- If it's a module, add .hs extensions and arrange as [dir1, dir2, modname.hs]


-- runStage :: MonadError Errors m
--          => (t -> Except e a)      -- ^ extract an `a` from a `t`
--          -> (NonEmpty e -> Errors) -- ^ collect errors into an Errors value
--          -> [t]                    -- ^ list of `t`s to extract from
--          -> m [a]
-- runStage stage collect = go . partitionEithers . map (runExcept . stage)
--   where go ([]  , res) = pure res
--         go (e:es, _  ) = throwError $ collect (e :| es)

-- | Parse passages separated by grep's group separation markers (--)
pPassages :: Monad m => Parser m [Passage]
pPassages = pPassage `sepBy` (string "--" *> endOfLine) <* eof

-- | Parse a passage in the format
--     filePath:lineNumber:firstLine
--     filePath-lineNumber-secondLine
-- as is usual for grep output with `-n -A 1`
pPassage :: Monad m => Parser m Passage
pPassage = do
  let colon = char ':'
      dashOrColon = oneOf "-:"
      line = manyTill anyChar endOfLine

  -- Parse the line in which "Note" was found
  filePath <- manyTill anyChar colon
  lineNumber <- foldl' (\a d -> a * 10 + digitToInt d) 0 <$> many1 digit <* colon
  -- We've parsed 1 line so far
  putState 1
  noteLine <- line

  -- Following lines in the passage are ones directly following the noteline
  -- in a source file. If "Note" was found in them, they use the colon
  -- separator, otherwise, the dash separator. The passage ends before the
  -- second consecutive line without "Note".
  followLines <- many do
    lineNumber' <- (lineNumber +) <$> getState
    _ <- string filePath *> dashOrColon
    _ <- string (show lineNumber') *> dashOrColon
    modifyState (+1)
    line

  let content = T.pack . unlines $ noteLine : followLines
  pure $ Passage (newPos filePath lineNumber 1) content

pSpaces :: Monad m => AllowLineBreaks -> Parser m ()
pSpaces allowLineBreaks = do
  skipMany $ satisfy \c -> isSpace c && c /= '\n'
  case allowLineBreaks of
    WithLineBreaks -> optional (try $ pCommentEnd *> pCommentStart)
    NoLineBreaks   -> pure ()

-- | Parses a passage and produces a list of headers and references in that
-- passage
-- XXX JB sometimes, you might want to reference more than one note and write
-- Notes [some note] and [other note]
-- so make sure we can parse this
-- e.g. in ../../compiler/GHC/Tc/Utils/TcType.hs
pHeadersRefs :: MonadReader Passage m => Parser m ([Header], [Ref])
pHeadersRefs = bimap (map Header) (map Ref) . mconcat <$> do
  -- Set the source to be the passage we're parsing
  setPosition =<< location <$> ask
  many do
    pCommentStart
    -- A note on the beginning of the line could be a header, if the next line
    -- begins with ~~~~
    -- otherwise it is a regular Note reference
    mPotentialHeader <- optionMaybe (try $ pNote NoLineBreaks)
    noteSeparator
    case mPotentialHeader of
      Nothing -> ([],) <$> notesUntilEOL WithLineBreaks
      Just potentialHeader -> try headerBranch <|> refBranch
        where
          headerBranch = do
            -- after a header, more references can appear on the same line
            firstRefs <- notesUntilEOL NoLineBreaks
            pCommentStart
            -- headers are underlined by ~~~~
            -- 4 is an arbitrary lower bound on how many tildes you need
            (count 4 $ char '~') *> skipMany (char '~')
            -- more references can also appear immediately after the ~~~~
            restRefs <- notesUntilEOL WithLineBreaks
            pure ([potentialHeader], firstRefs ++ restRefs)
          refBranch = do
            refs <- noteSeparator *> notesUntilEOL WithLineBreaks
            pure ([], potentialHeader : refs)

-- | pComment gets rid of a potential comment delimiter as well as any
-- whitespace around it
pComment :: Monad m => [String] -> Parser m ()
pComment commentDelims = do
  pSpaces NoLineBreaks
  optional . msum $ map (try . string) commentDelims
  pSpaces NoLineBreaks

pCommentStart :: Monad m => Parser m ()
pCommentStart = pComment beginComment

-- | pCommentEnd parses a potential comment ending delimiter as well as a new
-- line
pCommentEnd :: Monad m => Parser m ()
pCommentEnd = pComment endComment <* endOfLine

noteSeparator :: Monad m => Parser m ()
-- Even for single-line notes, we want to stop parsing the separator when we
-- encounter a note start that spans multiple lines, hence `WithLineBreaks`
noteSeparator = skipMany $ notFollowedBy (pNoteStart WithLineBreaks) *> noneOf "\n"

notesUntilEOL :: MonadReader Passage m => AllowLineBreaks -> Parser m [Note]
notesUntilEOL allowLineBreaks =
  (pNote allowLineBreaks `endBy` noteSeparator) <* pCommentEnd

pNoteStart :: Monad m => AllowLineBreaks -> Parser m ()
pNoteStart allowLineBreaks = string "Note" *> pSpaces allowLineBreaks <* char '['

pNote :: MonadReader Passage m => AllowLineBreaks -> Parser m Note
pNote allowLineBreaks = do
  notePassage <- ask
  let withinNote = case allowLineBreaks of
        WithLineBreaks -> anyChar
        NoLineBreaks   -> noneOf "\n"
  noteNameStr <- pNoteStart allowLineBreaks *> manyTill withinNote (char ']')
  pure Note {notePassage, noteName = T.pack noteNameStr}

success :: Results -> IO ()
success Results {numNotes, numRefs} = do
  -- XXX JB TODO: keep track of any headers that aren't referenced and emit a
  -- warning for them, before printing this line (and then say in this line %d
  -- headers were found that aren't referenced anywhere.)
  -- We'll need to add a little boolean in the map values to keep track of
  -- which one has been used, and have an additional map to be able to go back
  -- from Suffixed to an actual header (so we can print out the passage etc.)
  -- We might be able to allow pipeline failure on warnings, with
  -- https://gitlab.com/gitlab-org/gitlab/-/issues/273157
  -- If so, when there are warnings, emit error code 3, since 2 is reserved by
  -- bash commands being used incorrectly.
  -- We could also return different exit codes for all types of
  -- errors/warnings, so they can be individually marked as acceptable or not
  -- in the travis.yml file.
  putStrLn $ "OK, found " ++ show numNotes ++ " and " ++ show numRefs ++ " references."
  exitSuccess

errorTypeInfo :: ErrorType -> Text
errorTypeInfo = \case
  StdInParseError -> "\
\Some of the provided lines could not be parsed. Please make sure that you\n\
\are running this script using the provided check-notes.sh bash script."
  MalformedRef -> "\
\Some Note references or headers were malformed. Please ensure that they\n\
\follow this format: XXX JB TODO"
  DuplicateHeader -> "\
\Some Note headers were found more than once in the same file. Please ensure\n\
\each Note header appears at most once. A Note header can be recognized by\n\
\its underline consisting of tildes, as in\n\
\    Note [some note]\n\
\    ~~~~~~~~~~~~~~~~"
  DanglingRef -> "\
\Some references point to headers that cannot be found. Please XXX JB"

handleErrors :: Errors -> IO ()
handleErrors Errors {errorType, errorList} = do
  forM_ errorList $ \err -> printError err *> printLnStdErr ""
  printStdErr $ errorTypeInfo errorType
  where
    printStdErr = T.hPutStr stderr
    printLnStdErr = T.hPutStrLn stderr

    printError Error {errorPsg, errorMsg} = do
      -- XXX JB print something more specific after this like "malformed
      -- reference" etc.
      printStdErr (red "error: ")
      printStdErr errorMsg
      case errorPsg of
        Nothing      -> pure ()
        Just passage -> do
          printLnStdErr "\n  in the passage\n"
          printPassage passage

    -- surround with ANSI escape sequence for red color and back to no color
    red :: Text -> Text
    red text = "\ESC[31m" <> text <> "\ESC[0m"

    printPassage :: Passage -> IO ()
    printPassage Passage {location, content} = do
      let lineNum = sourceLine location
          passageLines = T.lines content
          lastLineNum = lineNum + length passageLines - 1
          lastLineNumLength = length (show lastLineNum)
          lineNums = T.justifyRight lastLineNumLength ' ' . T.pack . show <$> [lineNum..]
          prependNum num str = num <> " | " <> str
      mapM_ printLnStdErr $ zipWith prependNum lineNums passageLines

      let printFilePath = printLnStdErr $ "\nin " <> T.pack (sourceName location) <> "\n"
      when (errorType == DuplicateHeader) printFilePath
