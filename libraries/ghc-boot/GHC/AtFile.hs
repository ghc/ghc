module GHC.AtFile (expandAtFile) where

import Prelude -- See note [Why do we import Prelude here?]
import Control.Exception
import Data.Char
import System.Directory
import System.IO.Error

iterationLimit :: Int
iterationLimit = 2000

-- | Expand command-line options by substituting \@__file__ with the
-- command-line options read from __file__. If file does not exist, or cannot
-- be read, then the option will be treated literally, and not removed.
--
-- Options in file are separated by whitespace. A whitespace character may be
-- included in an option by surrounding the entire option in either single or
-- double quotes. Any character (including a backslash) may be included by
-- prefixing the character to be included with a backslash. The file may itself
-- contain additional \@file options; any such options will be processed
-- recursively.
expandAtFile :: [String] -> IO (Either String [String])
expandAtFile = go iterationLimit []
  where
    go :: Int -> [String] -> [String] -> IO (Either String [String])
    go _ acc [] = return $ Right $ reverse acc
    go iteration acc (arg@('@': path): args)
      | iteration <= 0 = return $ Left "too many @-files encountered"
      | otherwise = flip catchIOError (\_ -> go iteration (arg: acc) args) $ do
          result <- readAtFile path
          case result of
            Left{} -> return result
            Right expanded -> go (iteration - 1) acc (expanded ++ args)
    go iteration acc (arg: args) = go iteration (arg: acc) args

readAtFile :: FilePath -> IO (Either String [String])
readAtFile path = do
  isDirectory <- doesDirectoryExist path
  if isDirectory
    then return $ Left $ "@-file refers to a directory"
    else evaluate . parseAll [] =<< readFile path
  where
    parseAll :: [String] -> String -> Either String [String]
    parseAll acc content = case dropWhile isAsciiSpace content of
      "" -> return $ reverse acc
      content' -> do
        (arg, rest) <- parseOne Nothing "" content'
        parseAll (arg: acc) rest

    parseOne :: Maybe Char -> String -> String -> Either String (String, String)
    parseOne _ _ "\\" = Left $
      "unterminated backslash-escape in @-file: " ++ path
    parseOne maybeQuote acc ('\\': c: rest) = parseOne maybeQuote (c: acc) rest
    parseOne (Just quote) acc "" = Left $
      "unmatched `" ++ quote: "' in @-file: " ++ path ++ ": " ++ acc
    parseOne maybeQuote@(Just quote) acc (c: rest)
      | c == quote = parseOne Nothing acc rest
      | otherwise = parseOne maybeQuote (c: acc) rest
    parseOne Nothing acc "" = return (reverse acc, "")
    parseOne Nothing acc (c: rest)
      | c == '\'' || c == '"' = parseOne (Just c) acc rest
      | isAsciiSpace c = return (reverse acc, rest)
      | otherwise = parseOne Nothing (c: acc) rest

    isAsciiSpace :: Char -> Bool
    isAsciiSpace c = isAscii c && isSpace c
