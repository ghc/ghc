module Main where

import System.Process

main :: IO ()
main = do
  let text = unlines . map show $ [1..10000 :: Int]
  (code, out, _) <- readProcessWithExitCode "head" ["-n", "1"] text
  print code
  putStr out
