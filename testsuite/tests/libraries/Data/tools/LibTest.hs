-- Helper module to run QuickCheck tests.

module LibTest (
                run,
                runTests, 
                module Test.QuickCheck,
                Nasty, Nasty' (..),
                StructEq (..)
               ) where

import Test.QuickCheck
import Test.QuickCheck.Batch hiding (runTests)
import Data.List
import System.Exit
import Control.Monad

class StructEq a where
    structEq :: a -> a -> Bool

instance StructEq Int where
    structEq = (==)

instance StructEq Bool where
    structEq = (==)


instance (StructEq a, StructEq b) => StructEq (a,b) where
    structEq (a1,b1) (a2,b2) = structEq a1 a2 && structEq b1 b2

instance (StructEq a, StructEq b) => StructEq (Nasty' a b) where
    structEq (Nasty' a1 b1) (Nasty' a2 b2) = structEq a1 a2 && structEq b1 b2

instance (StructEq a) => StructEq [a] where
    structEq l1 l2 = and (zipWith structEq l1 l2)

instance (StructEq a) => StructEq (Maybe a) where
    structEq Nothing Nothing = True
    structEq (Just x) (Just y) = structEq x y
    structEq _ _ = False


type Nasty = Nasty' Int Int

-- a type with a non-structural equality.
data Nasty' k v = Nasty' {nastyKey :: k, nastyValue :: v}


instance (Show k, Show v) => Show (Nasty' k v) where
    show (Nasty' k v) = show k ++ ":>" ++ show v

instance Eq k => Eq (Nasty' k v) where
    Nasty' k1 _ == Nasty' k2 _ = k1 == k2

instance Ord k => Ord (Nasty' k v) where
    compare (Nasty' k1 _) (Nasty' k2 _) = compare k1 k2

instance (Arbitrary k, Arbitrary v) => Arbitrary (Nasty' k v) where
    arbitrary = return Nasty' `ap` arbitrary `ap` arbitrary


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
       