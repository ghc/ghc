%
% (c) The AQUA Project, Glasgow University, 1994-1995
%
\section[Set]{An implementation of sets}

This new (94/04) implementation of sets sits squarely upon our
implementation of @FiniteMaps@.  The interface is (roughly?) as
before.

(95/08: This module is no longer part of the GHC compiler proper; it
is a GHC library module only, now.)

\begin{code}
module Set (
	-- not a synonym so we can make it abstract
	Set,

	mkSet, setToList, emptySet, singletonSet,
	union, unionManySets, minusSet,
	elementOf, mapSet,
	intersect, isEmptySet,
	cardinality
	
	-- to make the interface self-sufficient
#if defined(__GLASGOW_HASKELL__)
	, FiniteMap   -- abstract

	-- for pragmas
	, keysFM, sizeFM
#endif
    ) where

import FiniteMap
import Maybes		( maybeToBool
#if __HASKELL1__ < 3
                          , Maybe(..)
#endif
			)
\end{code}

\begin{code}
-- This can't be a type synonym if you want to use constructor classes.
data Set a = MkSet (FiniteMap a ()) {-# STRICT #-}

emptySet :: Set a
emptySet = MkSet emptyFM

singletonSet :: a -> Set a
singletonSet x = MkSet (singletonFM x ())

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
elementOf x (MkSet set) = maybeToBool(lookupFM set x)

isEmptySet :: Set a -> Bool
isEmptySet (MkSet set) = sizeFM set == 0

mapSet :: Ord a => (b -> a) -> Set b -> Set a
mapSet f (MkSet set) = MkSet (listToFM [ (f key, ()) | key <- keysFM set ])

cardinality :: Set a -> Int
cardinality (MkSet set) = sizeFM set

-- fair enough...
instance (Eq a) => Eq (Set a) where
  (MkSet set_1) == (MkSet set_2) = set_1 == set_2

-- but not so clear what the right thing to do is:
{- NO:
instance (Ord a) => Ord (Set a) where
  (MkSet set_1) <= (MkSet set_2) = set_1 <= set_2
-}
\end{code}
