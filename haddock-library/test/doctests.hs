module Main where

import           Test.DocTest

main :: IO ()
main = doctest [
    "-isrc"
  , "-ivendor/attoparsec-0.10.4.0"
  , "-optP-include", "-optPdist/build/autogen/cabal_macros.h"
  , "src/Documentation/Haddock/Parser.hs"
  ]
