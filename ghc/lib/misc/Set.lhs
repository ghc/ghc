%
% (c) The AQUA Project, Glasgow University, 1994-1995
%
\section[Set]{An implementation of sets}

This new (94/04) implementation of sets sits squarely upon our
implementation of @FiniteMaps@.  The interface is (roughly?) as
before.

(95/08: This module is no longer part of the GHC compiler proper; it
is now just a GHC library module).

\begin{code}
module Set (
	Set,          -- abstract
	              -- instance of: Eq

	emptySet,     -- :: Set a
	mkSet,        -- :: Ord a => [a]  -> Set a
	setToList,    -- :: Set a -> [a] 
	unitSet,      -- :: a -> Set a
	singletonSet, -- :: a -> Set a

	union,          -- :: Ord a => Set a -> Set a -> Set a
	unionManySets,  -- :: Ord a => [Set a] -> Set a
	minusSet,       -- :: Ord a => Set a -> Set a -> Set a
	mapSet,         -- :: Ord a => (b -> a) -> Set b -> Set a
	intersect,      -- :: Ord a => Set a -> Set a -> Set a

	elementOf,      -- :: Ord a => a -> Set a -> Bool
	isEmptySet,     -- :: Set a -> Bool
	
	cardinality     -- :: Set a -> Int
    ) where

import FiniteMap
import Maybe
\end{code}

\begin{code}
-- This can't be a type synonym if you want to use constructor classes.
newtype Set a = MkSet (FiniteMap a ())

emptySet :: Set a
emptySet = MkSet emptyFM

unitSet :: a -> Set a
unitSet x = MkSet (unitFM x ())
singletonSet = unitSet -- old;deprecated.

setToList :: Set a -> [a]
setToList (MkSet set) = keysFM set

mkSet :: Ord a => [a]  -> Set a
mkSet xs = MkSet (listToFM [ (x, ()) | x <- xs])

union :: Ord a => Set a -> Set a -> Set a
union (MkSet set1) (MkSet set2) = MkSet (plusFM set1 set2)

unionManySets :: Ord a => [Set a] -> Set a
unionManySets ss = foldr union emptySet ss

minusSet  :: Ord a => Set a -> Set a -> Set a
minusSet (MkSet set1) (MkSet set2) = MkSet (minusFM set1 set2)

intersect :: Ord a => Set a -> Set a -> Set a
intersect (MkSet set1) (MkSet set2) = MkSet (intersectFM set1 set2)

elementOf :: Ord a => a -> Set a -> Bool
elementOf x (MkSet set) = isJust (lookupFM set x)

isEmptySet :: Set a -> Bool
isEmptySet (MkSet set) = sizeFM set == 0

mapSet :: Ord a => (b -> a) -> Set b -> Set a
mapSet f (MkSet set) = MkSet (listToFM [ (f key, ()) | key <- keysFM set ])

cardinality :: Set a -> Int
cardinality (MkSet set) = sizeFM set

-- fair enough...
instance (Eq a) => Eq (Set a) where
  (MkSet set_1) == (MkSet set_2) = set_1 == set_2
  (MkSet set_1) /= (MkSet set_2) = set_1 /= set_2

-- but not so clear what the right thing to do is:
{- NO:
instance (Ord a) => Ord (Set a) where
  (MkSet set_1) <= (MkSet set_2) = set_1 <= set_2
-}
\end{code}
