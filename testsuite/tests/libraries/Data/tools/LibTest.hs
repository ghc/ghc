-- Helper module to run QuickCheck tests.

module LibTest (
                run,
                runTests, 
                module Test.QuickCheck,
                
               ) where

import Test.QuickCheck
import Test.QuickCheck.Batch hiding (runTests)
import Data.List
import System.Exit


-- a type with a non-structural equality.
data Nasty k v = Nasty k v

instance Eq k => Eq (Nasty k v) where
    Nasty k1 _ == Nasty k2 _ = k1 == k2

instance Ord k => Ord (Nasty k v) where
    compare (Nasty k1 _) (Nasty k2 _) = compare k1 k2

options = 
    TestOptions 
    {
     no_of_tests = 100, 
     length_of_tests = 10,
     debug_tests = False
    }

prettyResult (TestOk s n msg) = s ++ " " ++ show n ++ concatMap concat msg
prettyResult (TestExausted s n msg) = s ++ show n ++ concatMap concat msg
prettyResult (TestFailed s n) = "failed with at " ++ show n ++ " with " ++ concat (intersperse " " s)
prettyResult (TestAborted e) = "aborted: " ++ show e

resultOk (TestOk _ _ _) = True
resultOk (TestExausted _ _ _) = True
resultOk (TestFailed _ _) = False
resultOk (TestAborted _) = False

runOneTest (name, test) = 
    do putStr $ name ++ "... "
       result <- test options
       putStrLn $ prettyResult result
       return $ resultOk result
       

runTests fileName propNames propTests =
    do putStrLn $ "Running tests of " ++ fileName
       results <- mapM runOneTest $ zip propNames propTests
       let ok = and results
       putStrLn $ "SUMMARY: " ++ fileName ++ " " ++ if ok then "PASSED" else "FAILED"
       exitWith $ if ok then ExitSuccess else ExitFailure 1
       