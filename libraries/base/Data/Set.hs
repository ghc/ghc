-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Set
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- This implementation of sets sits squarely upon Data.FiniteMap.
--
-----------------------------------------------------------------------------

module Data.Set (
	Set,            -- abstract, instance of: Eq

	emptySet,       -- :: Set a
	mkSet,          -- :: Ord a => [a]  -> Set a
	setToList,      -- :: Set a -> [a] 
	unitSet,        -- :: a -> Set a

	union,          -- :: Ord a => Set a -> Set a -> Set a
	unionManySets,  -- :: Ord a => [Set a] -> Set a
	minusSet,       -- :: Ord a => Set a -> Set a -> Set a
	mapSet,         -- :: Ord a => (b -> a) -> Set b -> Set a
	intersect,      -- :: Ord a => Set a -> Set a -> Set a
	addToSet,      	-- :: Ord a => Set a -> a -> Set a
	delFromSet,    	-- :: Ord a => Set a -> a -> Set a

	elementOf,      -- :: Ord a => a -> Set a -> Bool
	isEmptySet,     -- :: Set a -> Bool
	
	cardinality     -- :: Set a -> Int
    ) where

import Prelude

import Data.FiniteMap
import Data.Maybe

-- This can't be a type synonym if you want to use constructor classes.
newtype Set a = MkSet (FiniteMap a ())

emptySet :: Set a
emptySet = MkSet emptyFM

unitSet :: a -> Set a
unitSet x = MkSet (unitFM x ())

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

addToSet :: Ord a => Set a -> a -> Set a
addToSet (MkSet set) a = MkSet (addToFM set a ())

delFromSet :: Ord a => Set a -> a -> Set a
delFromSet (MkSet set) a = MkSet (delFromFM set a)

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
