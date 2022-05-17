{-# LANGUAGE BlockArguments #-}

-------------------------------------------------------------------------------
-- |
-- Description: Export all characters and their properties to a CSV file.
--
-- This is intended to compare Haskell results to other languages.
-------------------------------------------------------------------------------

module Main where

import Data.Char
import Data.Foldable
import Data.Version
import GHC.Unicode (unicodeVersion)
import Numeric

main :: IO ()
main = do
  -- First line is Unicode version
  putStrLn (showVersion unicodeVersion)
  -- Second line is CSV header
  putStrLn header
  -- Then all the supported characters
  traverse_ addEntry [minBound..maxBound]

-- | File header
header :: String
header = "Char,General Category,Lower Case,Upper Case,Title Case"

-- | Convert a character to its (short) hexadecimal Unicode codepoint.
mkCodePointHex :: Char -> String
mkCodePointHex c = showHex (ord c) mempty

-- | Make a CSV entry for a char.
addEntry :: Char -> IO ()
addEntry c = do
  putStr (mkCodePointHex c)
  putChar ','
  putStr (show (generalCategory c))
  putChar ','
  putStr (mkCodePointHex (toLower c))
  putChar ','
  putStr (mkCodePointHex (toUpper c))
  putChar ','
  putStrLn (mkCodePointHex (toTitle c))
