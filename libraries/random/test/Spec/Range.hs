module Spec.Range
  ( symmetric
  , bounded
  , singleton
  , uniformRangeWithin
  , uniformRangeWithinExcludedF
  , uniformRangeWithinExcludedD
  ) where

import System.Random.Internal
import System.Random.Stateful
import Data.Proxy

symmetric :: (RandomGen g, UniformRange a, Eq a) => Proxy a -> g -> (a, a) -> Bool
symmetric _ g (l, r) = fst (uniformR (l, r) g) == fst (uniformR (r, l) g)

bounded :: (RandomGen g, UniformRange a, Ord a) => Proxy a -> g -> (a, a) -> Bool
bounded _ g (l, r) = bottom <= result && result <= top
  where
    bottom = min l r
    top = max l r
    result = fst (uniformR (l, r) g)

singleton :: (RandomGen g, UniformRange a, Eq a) => Proxy a -> g -> a -> Bool
singleton _ g x = result == x
  where
    result = fst (uniformR (x, x) g)

uniformRangeWithin :: (RandomGen g, UniformRange a, Ord a) => Proxy a -> g -> (a, a) -> Bool
uniformRangeWithin _ gen (l, r) =
  runStateGen_ gen $ \g ->
    (\result -> min l r <= result && result <= max l r) <$> uniformRM (l, r) g

uniformRangeWithinExcludedF :: RandomGen g => g -> Bool
uniformRangeWithinExcludedF gen =
  runStateGen_ gen $ \g ->
    (\result -> 0 < result && result <= 1) <$> uniformFloatPositive01M g

uniformRangeWithinExcludedD :: RandomGen g => g -> Bool
uniformRangeWithinExcludedD gen =
  runStateGen_ gen $ \g ->
    (\result -> 0 < result && result <= 1) <$> uniformDoublePositive01M g
