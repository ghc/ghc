import Test.Tasty

import qualified Math.NumberTheory.LogarithmsTests as Logarithms

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All"
    [ Logarithms.testSuite
    ]
