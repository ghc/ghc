module Main where

import GHC.Utils.Misc
import ManyQueue

main = do
  runTest testManyQueue'1P3C 
  runTest testManyQueue'1P1C 
