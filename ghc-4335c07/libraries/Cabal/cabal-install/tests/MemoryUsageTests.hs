module MemoryUsageTests where

import Test.Tasty

import qualified UnitTests.Distribution.Solver.Modular.MemoryUsage

tests :: TestTree
tests =
  testGroup "Memory Usage"
  [ testGroup "UnitTests.Distribution.Solver.Modular.MemoryUsage"
        UnitTests.Distribution.Solver.Modular.MemoryUsage.tests
  ]

main :: IO ()
main = defaultMain tests
