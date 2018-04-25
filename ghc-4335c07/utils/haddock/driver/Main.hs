module Main where

import Documentation.Haddock (haddock)
import ResponseFile (expandResponse)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= expandResponse >>= haddock
