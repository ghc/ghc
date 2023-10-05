module Main where

import Control.Exception
import Data.Char
import System.IO

import GHC.Data.StringBuffer

twoBOMs = "T6016-twoBOMs"

ignoreFirstBOM = do
  -- StringBuffer should not contain initial byte-order mark.
  --
  -- Just skipping over it, but leaving it in the Stringbuffer, is not
  -- sufficient. The Lexer calls prevChar when a regular expression
  -- starts with '^' (which is a shorthand for '\n^'). It would never
  -- match on the first line, since instead of '\n', prevChar would
  -- still return '\xfeff'.
  s <- hGetStringBuffer twoBOMs
  assert (prevChar s '\n' == '\n') return ()

dontIgnoreSecondBOM = do
  -- U+FEFF is considered a BOM only if it appears as the first
  -- character of a file.
  h <- openBinaryFile twoBOMs ReadMode
  hSeek h AbsoluteSeek 3
  s <- hGetStringBufferBlock h 3
  hClose h
  assert (currentChar s == '\xfeff') return ()

main = do
  writeFile twoBOMs "\xfeff\xfeff"
  ignoreFirstBOM
  dontIgnoreSecondBOM
