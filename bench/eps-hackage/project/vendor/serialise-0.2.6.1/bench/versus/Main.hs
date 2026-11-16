module Main
  ( main -- :: IO ()
  ) where

import           Criterion.Main (bgroup, defaultMain)

-- Import our benchmark suites
import qualified Mini      as Mini
import qualified Macro     as Macro

import           Utils         (prepBenchmarkFiles)


-- A simple driver, for running every set of benchmarks.
main :: IO ()
main = prepBenchmarkFiles >> defaultMain
  [ bgroup "mini"     Mini.benchmarks
  , bgroup "macro"    Macro.benchmarks
  ]
