module Main where

import Documentation.Haddock (haddock)
import GHC.ResponseFile (getArgsWithResponseFiles)

main :: IO ()
main = getArgsWithResponseFiles >>= haddock
