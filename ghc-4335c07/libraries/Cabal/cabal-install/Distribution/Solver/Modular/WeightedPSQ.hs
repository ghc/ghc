{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Distribution.Solver.Modular.WeightedPSQ (
    WeightedPSQ
  , fromList
  , toList
  , keys
  , weights
  , isZeroOrOne
  , filter
  , lookup
  , mapWithKey
  , mapWeightsWithKey
  , union
  ) where

import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Ord (comparing)
import qualified Data.Traversable as T
import Prelude hiding (filter, lookup)

-- | An association list that is sorted by weight.
--
-- Each element has a key ('k'), value ('v'), and weight ('w'). All operations
-- that add elements or modify weights stably sort the elements by weight.
newtype WeightedPSQ w k v = WeightedPSQ [(w, k, v)]
  deriving (Eq, Show, Functor, F.Foldable, T.Traversable)

-- | /O(N)/.
filter :: (v -> Bool) -> WeightedPSQ k w v -> WeightedPSQ k w v
filter p (WeightedPSQ xs) = WeightedPSQ (L.filter (p . triple_3) xs)

-- | /O(1)/. Return @True@ if the @WeightedPSQ@ contains zero or one elements.
isZeroOrOne :: WeightedPSQ w k v -> Bool
isZeroOrOne (WeightedPSQ [])  = True
isZeroOrOne (WeightedPSQ [_]) = True
isZeroOrOne _                 = False

-- | /O(1)/. Return the elements in order.
toList :: WeightedPSQ w k v -> [(w, k, v)]
toList (WeightedPSQ xs) = xs

-- | /O(N log N)/.
fromList :: Ord w => [(w, k, v)] -> WeightedPSQ w k v
fromList = WeightedPSQ . L.sortBy (comparing triple_1)

-- | /O(N)/. Return the weights in order.
weights :: WeightedPSQ w k v -> [w]
weights (WeightedPSQ xs) = L.map triple_1 xs

-- | /O(N)/. Return the keys in order.
keys :: WeightedPSQ w k v -> [k]
keys (WeightedPSQ xs) = L.map triple_2 xs

-- | /O(N)/. Return the value associated with the first occurrence of the give
-- key, if it exists.
lookup :: Eq k => k -> WeightedPSQ w k v -> Maybe v
lookup k (WeightedPSQ xs) = triple_3 `fmap` L.find ((k ==) . triple_2) xs

-- | /O(N log N)/. Update the weights.
mapWeightsWithKey :: Ord w2
                  => (k -> w1 -> w2)
                  -> WeightedPSQ w1 k v
                  -> WeightedPSQ w2 k v
mapWeightsWithKey f (WeightedPSQ xs) = fromList $
                                       L.map (\ (w, k, v) -> (f k w, k, v)) xs

-- | /O(N)/. Update the values.
mapWithKey :: (k -> v1 -> v2) -> WeightedPSQ w k v1 -> WeightedPSQ w k v2
mapWithKey f (WeightedPSQ xs) = WeightedPSQ $
                                L.map (\ (w, k, v) -> (w, k, f k v)) xs

-- | /O((N + M) log (N + M))/. Combine two @WeightedPSQ@s, preserving all
-- elements. Elements from the first @WeightedPSQ@ come before elements in the
-- second when they have the same weight.
union :: Ord w => WeightedPSQ w k v -> WeightedPSQ w k v -> WeightedPSQ w k v
union (WeightedPSQ xs) (WeightedPSQ ys) = fromList (xs ++ ys)

triple_1 :: (x, y, z) -> x
triple_1 (x, _, _) = x

triple_2 :: (x, y, z) -> y
triple_2 (_, y, _) = y

triple_3 :: (x, y, z) -> z
triple_3 (_, _, z) = z
