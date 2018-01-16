module SolverQuickCheck where

import Test.Tasty

import qualified UnitTests.Distribution.Solver.Modular.QuickCheck


tests :: TestTree
tests =
  testGroup "Solver QuickCheck"
  [ testGroup "UnitTests.Distribution.Solver.Modular.QuickCheck"
        UnitTests.Distribution.Solver.Modular.QuickCheck.tests
  ]

main :: IO ()
main = defaultMain tests
