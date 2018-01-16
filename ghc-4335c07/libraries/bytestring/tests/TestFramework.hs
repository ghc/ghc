-- |
-- Copyright   : (c) 2011 Duncan Coutts
--
-- test-framework stub API
--
-- Currently we cannot use the nice test-framework package for this testsuite
-- since test-framework indirectly depends on bytestring and this makes cabal
-- think we've got a circular dependency.
--
-- On the other hand, it's very nice to have the testsuite run automatically
-- rather than being a totally separate package (which would fix).
--
-- So until we can fix that we implement our own trivial layer.
--
module TestFramework where

import Test.QuickCheck (Testable(..))
import Test.QuickCheck.Test

import Text.Printf
import System.Environment
import Control.Monad
import Control.Exception

-- Ideally we'd be using:

--import Test.Framework
--import Test.Framework.Providers.QuickCheck2

type TestName = String
type Test     = [(TestName, Int -> IO (Bool, Int))]

testGroup :: String -> [Test] -> Test
testGroup _ = concat

testProperty :: Testable a => String -> a -> Test
testProperty name p = [(name, runQcTest)]
  where
    runQcTest n = do
        result <- quickCheckWithResult testArgs p
        case result of
          Success {} -> return (True,  numTests result)
          _          -> return (False, numTests result)
      where
        testArgs = stdArgs {
                     maxSuccess = n
                     --chatty   = ... if we want to increase verbosity
                   }

assertBool :: String -> Bool -> Bool
assertBool _ = id

testCase :: String -> Bool -> Test
testCase name tst = [(name, runPlainTest)]
  where
    runPlainTest _ = do
      r <- evaluate tst
      putStrLn "+++ OK, passed test."
      return (r, 1)

defaultMain :: [Test] -> IO ()
defaultMain = runTests . concat

runTests :: [(String, Int -> IO (Bool,Int))] -> IO ()
runTests tests = do
    x <- getArgs
    let n = if null x then 100 else read . head $ x
    (results, passed) <- liftM unzip $ mapM (\(s,a) -> printf "%-40s: " s >> a n) tests
    _ <- printf "Passed %d tests!\n" (sum passed)
    when (not . and $ results) $ fail "Not all tests passed!"
