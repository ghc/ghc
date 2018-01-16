module Main where

import System.Environment
import System.IO

main :: IO ()
main = do
  (_:source:target:_) <- getArgs
  let f '0' = '1'
      f c = c
  writeFile target . map f  =<< readFile source
