module Main where

import System.Directory
import System.Environment

main :: IO ()
main = do
  (source:target:_) <- getArgs
  copyFile source target
