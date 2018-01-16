module System.Console.Haskeline.Completion(
                            CompletionFunc,
                            Completion(..),
                            noCompletion,
                            simpleCompletion,
                            -- * Word completion
                            completeWord,
                            completeWordWithPrev,
                            completeQuotedWord,
                            -- * Filename completion
                            completeFilename,
                            listFiles,
                            filenameWordBreakChars
                        ) where


import System.FilePath
import Data.List(isPrefixOf)
import Control.Monad(forM)

import System.Console.Haskeline.Directory
import System.Console.Haskeline.Monads

-- | Performs completions from the given line state.
--
-- The first 'String' argument is the contents of the line to the left of the cursor,
-- reversed.
-- The second 'String' argument is the contents of the line to the right of the cursor.
--
-- The output 'String' is the unused portion of the left half of the line, reversed.
type CompletionFunc m = (String,String) -> m (String, [Completion])


data Completion = Completion {replacement  :: String, -- ^ Text to insert in line.
                        display  :: String,
                                -- ^ Text to display when listing
                                -- alternatives.
                        isFinished :: Bool
                            -- ^ Whether this word should be followed by a
                            -- space, end quote, etc.
                            }
                    deriving (Eq, Ord, Show)

-- | Disable completion altogether.
noCompletion :: Monad m => CompletionFunc m
noCompletion (s,_) = return (s,[])

--------------
-- Word break functions

-- | A custom 'CompletionFunc' which completes the word immediately to the left of the cursor.
--
-- A word begins either at the start of the line or after an unescaped whitespace character.
completeWord :: Monad m => Maybe Char
        -- ^ An optional escape character
        -> [Char]-- ^ Characters which count as whitespace
        -> (String -> m [Completion]) -- ^ Function to produce a list of possible completions
        -> CompletionFunc m
completeWord esc ws = completeWordWithPrev esc ws . const

-- | A custom 'CompletionFunc' which completes the word immediately to the left of the cursor,
-- and takes into account the line contents to the left of the word.
--
-- A word begins either at the start of the line or after an unescaped whitespace character.
completeWordWithPrev :: Monad m => Maybe Char
        -- ^ An optional escape character
        -> [Char]-- ^ Characters which count as whitespace
        -> (String ->  String -> m [Completion])
            -- ^ Function to produce a list of possible completions.  The first argument is the
            -- line contents to the left of the word, reversed.  The second argument is the word
            -- to be completed.
        -> CompletionFunc m
completeWordWithPrev esc ws f (line, _) = do
    let (word,rest) = case esc of
                        Nothing -> break (`elem` ws) line
                        Just e -> escapedBreak e line
    completions <- f rest (reverse word)
    return (rest,map (escapeReplacement esc ws) completions)
  where
    escapedBreak e (c:d:cs) | d == e && c `elem` (e:ws)
            = let (xs,ys) = escapedBreak e cs in (c:xs,ys)
    escapedBreak e (c:cs) | notElem c ws
            = let (xs,ys) = escapedBreak e cs in (c:xs,ys)
    escapedBreak _ cs = ("",cs)

-- | Create a finished completion out of the given word.
simpleCompletion :: String -> Completion
simpleCompletion = completion

-- NOTE: this is the same as for readline, except that I took out the '\\'
-- so they can be used as a path separator.
filenameWordBreakChars :: String
filenameWordBreakChars = " \t\n`@$><=;|&{("

-- A completion command for file and folder names.
completeFilename :: MonadIO m => CompletionFunc m
completeFilename  = completeQuotedWord (Just '\\') "\"'" listFiles
                        $ completeWord (Just '\\') ("\"\'" ++ filenameWordBreakChars)
                                listFiles

completion :: String -> Completion
completion str = Completion str str True

setReplacement :: (String -> String) -> Completion -> Completion
setReplacement f c = c {replacement = f $ replacement c}

escapeReplacement :: Maybe Char -> String -> Completion -> Completion
escapeReplacement esc ws f = case esc of
    Nothing -> f
    Just e -> f {replacement = escape e (replacement f)}
  where
    escape e (c:cs) | c `elem` (e:ws)     = e : c : escape e cs
                    | otherwise = c : escape e cs
    escape _ "" = ""


---------
-- Quoted completion
completeQuotedWord :: Monad m => Maybe Char -- ^ An optional escape character
                            -> [Char] -- ^ Characters which set off quotes
                            -> (String -> m [Completion]) -- ^ Function to produce a list of possible completions
                            -> CompletionFunc m -- ^ Alternate completion to perform if the
                                            -- cursor is not at a quoted word
                            -> CompletionFunc m
completeQuotedWord esc qs completer alterative line@(left,_)
  = case splitAtQuote esc qs left of
    Just (w,rest) | isUnquoted esc qs rest -> do
        cs <- completer (reverse w)
        return (rest, map (addQuotes . escapeReplacement esc qs) cs)
    _ -> alterative line

addQuotes :: Completion -> Completion
addQuotes c = if isFinished c
    then c {replacement = "\"" ++ replacement c ++ "\""}
    else c {replacement = "\"" ++ replacement c}

splitAtQuote :: Maybe Char -> String -> String -> Maybe (String,String)
splitAtQuote esc qs line = case line of
    c:e:cs | isEscape e && isEscapable c
                        -> do
                            (w,rest) <- splitAtQuote esc qs cs
                            return (c:w,rest)
    q:cs   | isQuote q  -> Just ("",cs)
    c:cs                -> do
                            (w,rest) <- splitAtQuote esc qs cs
                            return (c:w,rest)
    ""                  -> Nothing
  where
    isQuote = (`elem` qs)
    isEscape c = Just c == esc
    isEscapable c = isEscape c || isQuote c

isUnquoted :: Maybe Char -> String -> String -> Bool
isUnquoted esc qs s = case splitAtQuote esc qs s of
    Just (_,s') -> not (isUnquoted esc qs s')
    _ -> True


-- | List all of the files or folders beginning with this path.
listFiles :: MonadIO m => FilePath -> m [Completion]
listFiles path = liftIO $ do
    fixedDir <- fixPath dir
    dirExists <- doesDirectoryExist fixedDir
    -- get all of the files in that directory, as basenames
    allFiles <- if not dirExists
                    then return []
                    else fmap (map completion . filterPrefix)
                            $ getDirectoryContents fixedDir
    -- The replacement text should include the directory part, and also
    -- have a trailing slash if it's itself a directory.
    forM allFiles $ \c -> do
            isDir <- doesDirectoryExist (fixedDir </> replacement c)
            return $ setReplacement fullName $ alterIfDir isDir c
  where
    (dir, file) = splitFileName path
    filterPrefix = filter (\f -> notElem f [".",".."]
                                        && file `isPrefixOf` f)
    alterIfDir False c = c
    alterIfDir True c = c {replacement = addTrailingPathSeparator (replacement c),
                            isFinished = False}
    fullName = replaceFileName path

-- turn a user-visible path into an internal version useable by System.FilePath.
fixPath :: String -> IO String
-- For versions of filepath < 1.2
fixPath "" = return "."
fixPath ('~':c:path) | isPathSeparator c = do
    home <- getHomeDirectory
    return (home </> path)
fixPath path = return path
