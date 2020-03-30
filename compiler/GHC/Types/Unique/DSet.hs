-- (c) Bartosz Nitka, Facebook, 2015

-- |
-- Specialised deterministic sets, for things with @Uniques@
--
-- Based on 'UniqDFM's (as you would expect).
-- See Note [Deterministic UniqFM] in GHC.Types.Unique.DFM for explanation why we need it.
--
-- Basically, the things need to be in class 'Uniquable'.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module GHC.Types.Unique.DSet (
        -- * Unique set type
        UniqDSet,    -- type synonym for UniqFM a
        getUniqDSet,
        pprUniqDSet,

        -- ** Manipulating these sets
        delOneFromUniqDSet, delListFromUniqDSet,
        emptyUniqDSet,
        unitUniqDSet,
        mkUniqDSet,
        addOneToUniqDSet, addListToUniqDSet,
        unionUniqDSets, unionManyUniqDSets,
        minusUniqDSet, uniqDSetMinusUniqSet,
        intersectUniqDSets, uniqDSetIntersectUniqSet,
        uniqDSetDisjointUniqSet,
        foldUniqDSet,
        elementOfUniqDSet,
        filterUniqDSet,
        sizeUniqDSet,
        isEmptyUniqDSet,
        lookupUniqDSet,
        uniqDSetToList,
        partitionUniqDSet,
        mapUniqDSet
    ) where

import GhcPrelude

import Outputable
import GHC.Types.Unique.DFM
import GHC.Types.Unique.Set
import GHC.Types.Unique

import Data.Coerce
import Data.Data
import qualified Data.Semigroup as Semi

-- See Note [UniqSet invariant] in GHC.Types.Unique.Set for why we want a newtype here.
-- Beyond preserving invariants, we may also want to 'override' typeclass
-- instances.

newtype UniqDSet a = UniqDSet {getUniqDSet' :: UniqDFM a}
                   deriving (Data, Semi.Semigroup, Monoid)

emptyUniqDSet :: UniqDSet a
emptyUniqDSet = UniqDSet emptyUDFM

unitUniqDSet :: Uniquable a => a -> UniqDSet a
unitUniqDSet x = UniqDSet (unitUDFM x x)

mkUniqDSet :: Uniquable a => [a] -> UniqDSet a
mkUniqDSet = foldl' addOneToUniqDSet emptyUniqDSet

-- The new element always goes to the right of existing ones.
addOneToUniqDSet :: Uniquable a => UniqDSet a -> a -> UniqDSet a
addOneToUniqDSet (UniqDSet set) x = UniqDSet (addToUDFM set x x)

addListToUniqDSet :: Uniquable a => UniqDSet a -> [a] -> UniqDSet a
addListToUniqDSet = foldl' addOneToUniqDSet

delOneFromUniqDSet :: Uniquable a => UniqDSet a -> a -> UniqDSet a
delOneFromUniqDSet (UniqDSet s) = UniqDSet . delFromUDFM s

delListFromUniqDSet :: Uniquable a => UniqDSet a -> [a] -> UniqDSet a
delListFromUniqDSet (UniqDSet s) = UniqDSet . delListFromUDFM s

unionUniqDSets :: UniqDSet a -> UniqDSet a -> UniqDSet a
unionUniqDSets (UniqDSet s) (UniqDSet t) = UniqDSet (plusUDFM s t)

unionManyUniqDSets :: [UniqDSet a] -> UniqDSet a
unionManyUniqDSets [] = emptyUniqDSet
unionManyUniqDSets sets = foldr1 unionUniqDSets sets

minusUniqDSet :: UniqDSet a -> UniqDSet a -> UniqDSet a
minusUniqDSet (UniqDSet s) (UniqDSet t) = UniqDSet (minusUDFM s t)

uniqDSetMinusUniqSet :: UniqDSet a -> UniqSet b -> UniqDSet a
uniqDSetMinusUniqSet xs ys
  = UniqDSet (udfmMinusUFM (getUniqDSet xs) (getUniqSet ys))

intersectUniqDSets :: UniqDSet a -> UniqDSet a -> UniqDSet a
intersectUniqDSets (UniqDSet s) (UniqDSet t) = UniqDSet (intersectUDFM s t)

uniqDSetIntersectUniqSet :: UniqDSet a -> UniqSet b -> UniqDSet a
uniqDSetIntersectUniqSet xs ys
  = UniqDSet (udfmIntersectUFM (getUniqDSet xs) (getUniqSet ys))

uniqDSetDisjointUniqSet :: UniqDSet a -> UniqSet b -> Bool
uniqDSetDisjointUniqSet xs ys
  = disjointUdfmUfm (getUniqDSet xs) (getUniqSet ys)

foldUniqDSet :: (a -> b -> b) -> b -> UniqDSet a -> b
foldUniqDSet c n (UniqDSet s) = foldUDFM c n s

elementOfUniqDSet :: Uniquable a => a -> UniqDSet a -> Bool
elementOfUniqDSet k = elemUDFM k . getUniqDSet

filterUniqDSet :: (a -> Bool) -> UniqDSet a -> UniqDSet a
filterUniqDSet p (UniqDSet s) = UniqDSet (filterUDFM p s)

sizeUniqDSet :: UniqDSet a -> Int
sizeUniqDSet = sizeUDFM . getUniqDSet

isEmptyUniqDSet :: UniqDSet a -> Bool
isEmptyUniqDSet = isNullUDFM . getUniqDSet

lookupUniqDSet :: Uniquable a => UniqDSet a -> a -> Maybe a
lookupUniqDSet = lookupUDFM . getUniqDSet

uniqDSetToList :: UniqDSet a -> [a]
uniqDSetToList = eltsUDFM . getUniqDSet

partitionUniqDSet :: (a -> Bool) -> UniqDSet a -> (UniqDSet a, UniqDSet a)
partitionUniqDSet p = coerce . partitionUDFM p . getUniqDSet

-- See Note [UniqSet invariant] in GHC.Types.Unique.Set
mapUniqDSet :: Uniquable b => (a -> b) -> UniqDSet a -> UniqDSet b
mapUniqDSet f = mkUniqDSet . map f . uniqDSetToList

-- Two 'UniqDSet's are considered equal if they contain the same
-- uniques.
instance Eq (UniqDSet a) where
  UniqDSet a == UniqDSet b = equalKeysUDFM a b

getUniqDSet :: UniqDSet a -> UniqDFM a
getUniqDSet = getUniqDSet'

instance Outputable a => Outputable (UniqDSet a) where
  ppr = pprUniqDSet ppr

pprUniqDSet :: (a -> SDoc) -> UniqDSet a -> SDoc
pprUniqDSet f = braces . pprWithCommas f . uniqDSetToList
