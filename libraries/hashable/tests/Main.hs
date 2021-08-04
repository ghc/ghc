-- | Tests for the 'Data.Hashable' module.  We test functions by
-- comparing the C and Haskell implementations.

module Main (main) where

import Properties (properties)
import Regress (regressions)
import Test.Framework (defaultMain, testGroup)

main :: IO ()
main = defaultMain [
         testGroup "properties" properties
       , testGroup "regressions" regressions
       ]
