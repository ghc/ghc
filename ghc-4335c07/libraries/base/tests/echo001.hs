module Main(main) where

import System.IO
import Data.Char

main = do
  isT <- hIsTerminalDevice stdin
  flg <- if not isT then return False else hGetEcho stdin
  print flg
  if not isT then hSetEcho stdin False else return ()
  hSetBuffering stdin NoBuffering
  interact (map toUpper)
  
