{-# LANGUAGE ParallelListComp #-}
module UnitTests.Distribution.Solver.Modular.WeightedPSQ (
  tests
  ) where

import qualified Distribution.Solver.Modular.WeightedPSQ as W

import Data.List (sort)

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Blind(..), testProperty)

tests :: [TestTree]
tests = [
    testProperty "'toList . fromList' preserves elements" $ \xs ->
        sort (xs :: [(Int, Char, Bool)]) == sort (W.toList (W.fromList xs))

  , testProperty "'toList . fromList' sorts stably" $ \xs ->
        let indexAsValue :: [(Int, (), Int)]
            indexAsValue = [(x, (), i) | x <- xs | i <- [0..]]
        in isSorted $ W.toList $ W.fromList indexAsValue

  , testProperty "'mapWeightsWithKey' sorts by weight" $ \xs (Blind f) ->
        isSorted $ W.weights $
        W.mapWeightsWithKey (f :: Int -> Int -> Int) $
        W.fromList (xs :: [(Int, Int, Int)])

  , testCase "applying 'mapWeightsWithKey' twice sorts twice" $
        let indexAsKey :: [((), Int, ())]
            indexAsKey = [((), i, ()) | i <- [0..10]]
            actual = W.toList $
                     W.mapWeightsWithKey (\_ _ -> ()) $
                     W.mapWeightsWithKey (\i _ -> -i) $ -- should not be ignored
                     W.fromList indexAsKey
        in reverse indexAsKey @?= actual

  , testProperty "'union' sorts by weight" $ \xs ys ->
        isSorted $ W.weights $
        W.union (W.fromList xs) (W.fromList (ys :: [(Int, Int, Int)]))

  , testProperty "'union' preserves elements" $ \xs ys ->
        let union = W.union (W.fromList xs)
                            (W.fromList (ys :: [(Int, Int, Int)]))
        in sort (xs ++ ys) == sort (W.toList union)

  , testCase "'lookup' returns first occurrence" $
        let xs = W.fromList [((), False, 'A'), ((), True, 'C'), ((), True, 'B')]
        in Just 'C' @?= W.lookup True xs
  ]

isSorted :: Ord a => [a] -> Bool
isSorted (x1 : xs@(x2 : _)) = x1 <= x2 && isSorted xs
isSorted _ = True
