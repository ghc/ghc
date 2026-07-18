module Main where

import System.Exit (exitFailure)

main :: IO ()
main = do
  mapM_ print [1 .. 101 :: Int]
  exitFailure
