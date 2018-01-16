{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Distribution.Solver.Modular.PSQ
    ( PSQ(..)  -- Unit test needs constructor access
    , casePSQ
    , cons
    , length
    , lookup
    , filter
    , filterIfAny
    , filterIfAnyByKeys
    , filterKeys
    , firstOnly
    , fromList
    , isZeroOrOne
    , keys
    , map
    , mapKeys
    , mapWithKey
    , maximumBy
    , minimumBy
    , null
    , prefer
    , preferByKeys
    , snoc
    , sortBy
    , sortByKeys
    , toList
    , union
    ) where

-- Priority search queues.
--
-- I am not yet sure what exactly is needed. But we need a data structure with
-- key-based lookup that can be sorted. We're using a sequence right now with
-- (inefficiently implemented) lookup, because I think that queue-based
-- operations and sorting turn out to be more efficiency-critical in practice.

import Control.Arrow (first, second)

import qualified Data.Foldable as F
import Data.Function
import qualified Data.List as S
import Data.Ord (comparing)
import Data.Traversable
import Prelude hiding (foldr, length, lookup, filter, null, map)

newtype PSQ k v = PSQ [(k, v)]
  deriving (Eq, Show, Functor, F.Foldable, Traversable) -- Qualified Foldable to avoid issues with FTP

keys :: PSQ k v -> [k]
keys (PSQ xs) = fmap fst xs

lookup :: Eq k => k -> PSQ k v -> Maybe v
lookup k (PSQ xs) = S.lookup k xs

map :: (v1 -> v2) -> PSQ k v1 -> PSQ k v2
map f (PSQ xs) = PSQ (fmap (second f) xs)

mapKeys :: (k1 -> k2) -> PSQ k1 v -> PSQ k2 v
mapKeys f (PSQ xs) = PSQ (fmap (first f) xs)

mapWithKey :: (k -> a -> b) -> PSQ k a -> PSQ k b
mapWithKey f (PSQ xs) = PSQ (fmap (\ (k, v) -> (k, f k v)) xs)

fromList :: [(k, a)] -> PSQ k a
fromList = PSQ

cons :: k -> a -> PSQ k a -> PSQ k a
cons k x (PSQ xs) = PSQ ((k, x) : xs)

snoc :: PSQ k a -> k -> a -> PSQ k a
snoc (PSQ xs) k x = PSQ (xs ++ [(k, x)])

casePSQ :: PSQ k a -> r -> (k -> a -> PSQ k a -> r) -> r
casePSQ (PSQ xs) n c =
  case xs of
    []          -> n
    (k, v) : ys -> c k v (PSQ ys)

sortBy :: (a -> a -> Ordering) -> PSQ k a -> PSQ k a
sortBy cmp (PSQ xs) = PSQ (S.sortBy (cmp `on` snd) xs)

sortByKeys :: (k -> k -> Ordering) -> PSQ k a -> PSQ k a
sortByKeys cmp (PSQ xs) = PSQ (S.sortBy (cmp `on` fst) xs)

maximumBy :: (k -> Int) -> PSQ k a -> (k, a)
maximumBy sel (PSQ xs) =
  S.minimumBy (flip (comparing (sel . fst))) xs

minimumBy :: (a -> Int) -> PSQ k a -> PSQ k a
minimumBy sel (PSQ xs) =
  PSQ [snd (S.minimumBy (comparing fst) (S.map (\ x -> (sel (snd x), x)) xs))]

-- | Sort the list so that values satisfying the predicate are first.
prefer :: (a -> Bool) -> PSQ k a -> PSQ k a
prefer p = sortBy $ flip (comparing p)

-- | Sort the list so that keys satisfying the predicate are first.
preferByKeys :: (k -> Bool) -> PSQ k a -> PSQ k a
preferByKeys p = sortByKeys $ flip (comparing p)

-- | Will partition the list according to the predicate. If
-- there is any element that satisfies the precidate, then only
-- the elements satisfying the predicate are returned.
-- Otherwise, the rest is returned.
--
filterIfAny :: (a -> Bool) -> PSQ k a -> PSQ k a
filterIfAny p (PSQ xs) =
  let
    (pro, con) = S.partition (p . snd) xs
  in
    if S.null pro then PSQ con else PSQ pro

-- | Variant of 'filterIfAny' that takes a predicate on the keys
-- rather than on the values.
--
filterIfAnyByKeys :: (k -> Bool) -> PSQ k a -> PSQ k a
filterIfAnyByKeys p (PSQ xs) =
  let
    (pro, con) = S.partition (p . fst) xs
  in
    if S.null pro then PSQ con else PSQ pro

filterKeys :: (k -> Bool) -> PSQ k a -> PSQ k a
filterKeys p (PSQ xs) = PSQ (S.filter (p . fst) xs)

filter :: (a -> Bool) -> PSQ k a -> PSQ k a
filter p (PSQ xs) = PSQ (S.filter (p . snd) xs)

length :: PSQ k a -> Int
length (PSQ xs) = S.length xs

null :: PSQ k a -> Bool
null (PSQ xs) = S.null xs

isZeroOrOne :: PSQ k a -> Bool
isZeroOrOne (PSQ [])  = True
isZeroOrOne (PSQ [_]) = True
isZeroOrOne _         = False

firstOnly :: PSQ k a -> PSQ k a
firstOnly (PSQ [])      = PSQ []
firstOnly (PSQ (x : _)) = PSQ [x]

toList :: PSQ k a -> [(k, a)]
toList (PSQ xs) = xs

union :: PSQ k a -> PSQ k a -> PSQ k a
union (PSQ xs) (PSQ ys) = PSQ (xs ++ ys)
