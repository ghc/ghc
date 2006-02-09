-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Foldable
-- Copyright   :  Ross Paterson 2005
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Class of data structures that can be folded to a summary value.

module Data.Foldable (
	-- * Folds
	Foldable(..),
	-- ** Special biased folds
	foldr',
	foldl',
	foldrM,
	foldlM,
	-- ** Folding actions
	traverse_,
	mapM_,
	sequenceA_,
	sequence_,
	-- ** Specialized folds
	toList,
	concat,
	concatMap,
	and,
	or,
	any,
	all,
	sum,
	product,
	maximum,
	maximumBy,
	minimum,
	minimumBy,
	-- ** Searches
	elem,
	notElem,
	find
	) where

import Prelude hiding (foldl, foldr, foldl1, foldr1, mapM_, sequence_,
		elem, notElem, concat, concatMap, and, or, any, all,
		sum, product, maximum, minimum)
import qualified Prelude (foldl, foldr, foldl1, foldr1)
import Control.Applicative
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid
import Data.Array

#ifdef __GLASGOW_HASKELL__
import GHC.Exts (build)
#endif

-- | Data structures that can be folded.
--
-- Minimal complete definition: 'foldMap' or 'foldr'.
--
-- For example, given a data type
--
-- > data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
--
-- a suitable instance would be
--
-- > instance Foldable Tree
-- >    foldMap f Empty = mempty
-- >    foldMap f (Leaf x) = f x
-- >    foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r
--
-- This is suitable even for abstract types, as the monoid is assumed
-- to satisfy the monoid laws.
--
class Foldable t where
	-- | Combine the elements of a structure using a monoid.
	fold :: Monoid m => t m -> m
	fold = foldMap id

	-- | Map each element of the structure to a monoid,
	-- and combine the results.
	foldMap :: Monoid m => (a -> m) -> t a -> m
	foldMap f = foldr (mappend . f) mempty

	-- | Right-associative fold of a structure.
	--
	-- @'foldr' f z = 'Prelude.foldr' f z . 'toList'@
	foldr :: (a -> b -> b) -> b -> t a -> b
	foldr f z t = appEndo (foldMap (Endo . f) t) z

	-- | Left-associative fold of a structure.
	--
	-- @'foldl' f z = 'Prelude.foldl' f z . 'toList'@
	foldl :: (a -> b -> a) -> a -> t b -> a
	foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z

	-- | A variant of 'foldr' that has no base case,
	-- and thus may only be applied to non-empty structures.
	--
	-- @'foldr1' f = 'Prelude.foldr1' f . 'toList'@
	foldr1 :: (a -> a -> a) -> t a -> a
	foldr1 f xs = fromMaybe (error "foldr1: empty structure")
			(foldr mf Nothing xs)
	  where mf x Nothing = Just x
		mf x (Just y) = Just (f x y)

	-- | A variant of 'foldl' that has no base case,
	-- and thus may only be applied to non-empty structures.
	--
	-- @'foldl1' f = 'Prelude.foldl1' f . 'toList'@
	foldl1 :: (a -> a -> a) -> t a -> a
	foldl1 f xs = fromMaybe (error "foldl1: empty structure")
			(foldl mf Nothing xs)
	  where mf Nothing y = Just y
		mf (Just x) y = Just (f x y)

-- instances for Prelude types

instance Foldable Maybe where
	foldr f z Nothing = z
	foldr f z (Just x) = f x z

	foldl f z Nothing = z
	foldl f z (Just x) = f z x

instance Foldable [] where
	foldr = Prelude.foldr
	foldl = Prelude.foldl
	foldr1 = Prelude.foldr1
	foldl1 = Prelude.foldl1

instance Ix i => Foldable (Array i) where
	foldr f z = Prelude.foldr f z . elems

-- | Fold over the elements of a structure,
-- associating to the right, but strictly.
foldr' :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr' f z xs = foldl f' id xs z
  where f' k x z = k $! f x z

-- | Monadic fold over the elements of a structure,
-- associating to the right, i.e. from right to left.
foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM f z xs = foldl f' return xs z
  where f' k x z = f x z >>= k

-- | Fold over the elements of a structure,
-- associating to the left, but strictly.
foldl' :: Foldable t => (a -> b -> a) -> a -> t b -> a
foldl' f z xs = foldr f' id xs z
  where f' x k z = k $! f z x

-- | Monadic fold over the elements of a structure,
-- associating to the left, i.e. from left to right.
foldlM :: (Foldable t, Monad m) => (a -> b -> m a) -> a -> t b -> m a
foldlM f z xs = foldr f' return xs z
  where f' x k z = f z x >>= k

-- | Map each element of a structure to an action, evaluate
-- these actions from left to right, and ignore the results.
traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ f = foldr ((*>) . f) (pure ())

-- | Map each element of a structure to an monadic action, evaluate
-- these actions from left to right, and ignore the results.
mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_ f = foldr ((>>) . f) (return ())

-- | Evaluate each action in the structure from left to right,
-- and ignore the results.
sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = foldr (*>) (pure ())

-- | Evaluate each monadic action in the structure from left to right,
-- and ignore the results.
sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
sequence_ = foldr (>>) (return ())

-- These use foldr rather than foldMap to avoid repeated concatenation.

-- | List of elements of a structure.
toList :: Foldable t => t a -> [a]
#ifdef __GLASGOW_HASKELL__
toList t = build (\ c n -> foldr c n t)
#else
toList = foldr (:) []
#endif

-- | The concatenation of all the elements of a container of lists.
concat :: Foldable t => t [a] -> [a]
concat = foldr (++) []

concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap f = foldr ((++) . f) []

-- | 'and' returns the conjunction of a container of Bools.  For the
-- result to be 'True', the container must be finite; 'False', however,
-- results from a 'False' value finitely far from the left end.
and :: Foldable t => t Bool -> Bool
and = getAll . foldMap All

-- | 'or' returns the disjunction of a container of Bools.  For the
-- result to be 'False', the container must be finite; 'True', however,
-- results from a 'True' value finitely far from the left end.
or :: Foldable t => t Bool -> Bool
or = getAny . foldMap Any

-- | Determines whether any element of the structure satisfies the predicate.
any :: Foldable t => (a -> Bool) -> t a -> Bool
any p = getAny . foldMap (Any . p)

-- | Determines whether all elements of the structure satisfy the predicate.
all :: Foldable t => (a -> Bool) -> t a -> Bool
all p = getAll . foldMap (All . p)

-- | The 'sum' function computes the sum of the numbers of a structure.
sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

-- | The 'product' function computes the product of the numbers of a structure.
product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

-- | The largest element of the structure.
maximum :: (Foldable t, Ord a) => t a -> a
maximum = foldr1 max

maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
maximumBy cmp = foldr1 max'
  where max' x y = case cmp x y of
			GT -> x
			_  -> y

-- | The least element of the structure.
minimum :: (Foldable t, Ord a) => t a -> a
minimum = foldr1 min

minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
minimumBy cmp = foldr1 min'
  where min' x y = case cmp x y of
			GT -> y
			_  -> x

-- | Does the element occur in the structure?
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem = any . (==)

notElem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem x = not . elem x

-- | The 'find' function takes a predicate and a structure and returns
-- the leftmost element of the structure matching the predicate, or
-- 'Nothing' if there is no such element.
find :: Foldable t => (a -> Bool) -> t a -> Maybe a
find p = listToMaybe . concatMap (\ x -> if p x then [x] else [])
