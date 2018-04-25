
module Main where

import System.Directory
import System.Environment

main :: IO ()
main = do d <- getCurrentDirectory
          putStr $ concatMap f d
    where f '\\' = "/"
          f c    = [c]

