module Main (main) where

import Prelude hiding (foldl)

import Test.ChasingBottoms.IsBottom
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.IntSet

------------------------------------------------------------------------
-- * Properties

------------------------------------------------------------------------
-- ** Lazy module

pFoldlAccLazy :: Int -> Bool
pFoldlAccLazy k =
  isn'tBottom $ foldl (\_ x -> x) (bottom :: Int) (singleton k)

------------------------------------------------------------------------
-- * Test list

tests :: [Test]
tests =
    [
    -- Basic interface
      testGroup "IntSet"
      [ testProperty "foldl is lazy in accumulator" pFoldlAccLazy
      ]
    ]

------------------------------------------------------------------------
-- * Test harness

main :: IO ()
main = defaultMain tests

------------------------------------------------------------------------
-- * Utilities

isn'tBottom :: a -> Bool
isn'tBottom = not . isBottom
