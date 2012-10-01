module Main where

import           Documentation.Haddock (haddock)
import           System.Environment (getArgs)

main :: IO ()
main = getArgs >>= haddock
