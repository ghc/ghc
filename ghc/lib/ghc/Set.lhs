%
% (c) The AQUA Project, Glasgow University, 1994
%
\section[Set]{An implementation of sets}

This new (94/04) implementation of sets sits squarely upon our
implementation of @FiniteMaps@.  The interface is (roughly?) as
before.

See also the @UniqSet@ module (sets of things from which you can
extract a @Unique@).

\begin{code}
#if defined(COMPILING_GHC) && defined(DEBUG_FINITEMAPS)
#define OUTPUTABLE_a , Outputable a
#else
#define OUTPUTABLE_a {--}
#endif

module Set (
#if defined(__GLASGOW_HASKELL__)
	Set(..),    -- abstract type: NOT
#else
	-- not a synonym so we can make it abstract
	Set,
#endif

	mkSet, setToList, emptySet, singletonSet,
	union, unionManySets, minusSet,
	elementOf, mapSet,
	intersect, isEmptySet
	
	-- to make the interface self-sufficient
#if defined(__GLASGOW_HASKELL__)
	, FiniteMap   -- abstract

	-- for pragmas
	, intersectFM, minusFM, keysFM, plusFM
#endif
    ) where

import FiniteMap
import Maybes		( maybeToBool
#if __HASKELL1__ < 3
                          , Maybe(..)
#endif
			)
#if defined(__GLASGOW_HASKELL__)
-- I guess this is here so that our friend USE_ATTACK_PRAGMAS can
-- do his job of seeking out and destroying information hiding. ADR
import Util		--OLD: hiding ( Set(..), emptySet )
#endif

#if defined(COMPILING_GHC)
import Outputable
#endif
\end{code}

\begin{code}
#if defined(__GLASGOW_HASKELL__)

type Set a = FiniteMap a ()

#define MkSet {--}

#else
-- This can't be a type synonym if you want to use constructor classes.
data Set a = MkSet (FiniteMap a ()) {-# STRICT #-}
#endif

emptySet :: Set a
emptySet = MkSet emptyFM

singletonSet :: a -> Set a
singletonSet x = MkSet (singletonFM x ())

setToList :: Set a -> [a]
setToList (MkSet set) = keysFM set

mkSet :: (Ord a OUTPUTABLE_a) => [a]  -> Set a
mkSet xs = MkSet (listToFM [ (x, ()) | x <- xs])

union :: (Ord a OUTPUTABLE_a) => Set a -> Set a -> Set a
union (MkSet set1) (MkSet set2) = MkSet (plusFM set1 set2)

unionManySets :: (Ord a OUTPUTABLE_a) => [Set a] -> Set a
unionManySets ss = foldr union emptySet ss

minusSet  :: (Ord a OUTPUTABLE_a) => Set a -> Set a -> Set a
minusSet (MkSet set1) (MkSet set2) = MkSet (minusFM set1 set2)

intersect :: (Ord a OUTPUTABLE_a) => Set a -> Set a -> Set a
intersect (MkSet set1) (MkSet set2) = MkSet (intersectFM set1 set2)

elementOf :: (Ord a OUTPUTABLE_a) => a -> Set a -> Bool
elementOf x (MkSet set) = maybeToBool(lookupFM set x)

isEmptySet :: Set a -> Bool
isEmptySet (MkSet set) = sizeFM set == 0

mapSet :: (Ord a OUTPUTABLE_a) => (b -> a) -> Set b -> Set a
mapSet f (MkSet set) = MkSet (listToFM [ (f key, ()) | key <- keysFM set ])
\end{code}
