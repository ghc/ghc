{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1994-1998

\section[UniqSet]{Specialised sets, for things with @Uniques@}

Based on @UniqFMs@ (as you would expect).

Basically, the things need to be in class @Uniquable@.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module GHC.Types.Unique.Set (
        -- * Unique set type
        UniqSet,    -- type synonym for UniqFM a
        getUniqSet,
        pprUniqSet,

        -- ** Manipulating these sets
        emptyUniqSet,
        unitUniqSet,
        mkUniqSet,
        addOneToUniqSet, addListToUniqSet,
        delOneFromUniqSet, delOneFromUniqSet_Directly, delListFromUniqSet,
        delListFromUniqSet_Directly,
        unionUniqSets, unionManyUniqSets,
        minusUniqSet, uniqSetMinusUFM,
        intersectUniqSets,
        disjointUniqSets,
        restrictUniqSetToUFM,
        uniqSetAny, uniqSetAll,
        elementOfUniqSet,
        elemUniqSet_Directly,
        filterUniqSet,
        filterUniqSet_Directly,
        sizeUniqSet,
        isEmptyUniqSet,
        lookupUniqSet,
        lookupUniqSet_Directly,
        partitionUniqSet,
        mapUniqSet,
        unsafeUFMToUniqSet,
        nonDetEltsUniqSet,
        nonDetKeysUniqSet,
        nonDetFoldUniqSet,
        nonDetFoldUniqSet_Directly
    ) where

import GhcPrelude

import GHC.Types.Unique.FM
import GHC.Types.Unique
import Data.Coerce
import Outputable
import Data.Data
import qualified Data.Semigroup as Semi

-- Note [UniqSet invariant]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- UniqSet has the following invariant:
--   The keys in the map are the uniques of the values
-- It means that to implement mapUniqSet you have to update
-- both the keys and the values.

newtype UniqSet a = UniqSet {getUniqSet' :: UniqFM a}
                  deriving (Data, Semi.Semigroup, Monoid)

emptyUniqSet :: UniqSet a
emptyUniqSet = UniqSet emptyUFM

unitUniqSet :: Uniquable a => a -> UniqSet a
unitUniqSet x = UniqSet $ unitUFM x x

mkUniqSet :: Uniquable a => [a]  -> UniqSet a
mkUniqSet = foldl' addOneToUniqSet emptyUniqSet

addOneToUniqSet :: Uniquable a => UniqSet a -> a -> UniqSet a
addOneToUniqSet (UniqSet set) x = UniqSet (addToUFM set x x)

addListToUniqSet :: Uniquable a => UniqSet a -> [a] -> UniqSet a
addListToUniqSet = foldl' addOneToUniqSet

delOneFromUniqSet :: Uniquable a => UniqSet a -> a -> UniqSet a
delOneFromUniqSet (UniqSet s) a = UniqSet (delFromUFM s a)

delOneFromUniqSet_Directly :: UniqSet a -> Unique -> UniqSet a
delOneFromUniqSet_Directly (UniqSet s) u = UniqSet (delFromUFM_Directly s u)

delListFromUniqSet :: Uniquable a => UniqSet a -> [a] -> UniqSet a
delListFromUniqSet (UniqSet s) l = UniqSet (delListFromUFM s l)

delListFromUniqSet_Directly :: UniqSet a -> [Unique] -> UniqSet a
delListFromUniqSet_Directly (UniqSet s) l =
    UniqSet (delListFromUFM_Directly s l)

unionUniqSets :: UniqSet a -> UniqSet a -> UniqSet a
unionUniqSets (UniqSet s) (UniqSet t) = UniqSet (plusUFM s t)

unionManyUniqSets :: [UniqSet a] -> UniqSet a
unionManyUniqSets = foldl' (flip unionUniqSets) emptyUniqSet

minusUniqSet  :: UniqSet a -> UniqSet a -> UniqSet a
minusUniqSet (UniqSet s) (UniqSet t) = UniqSet (minusUFM s t)

intersectUniqSets :: UniqSet a -> UniqSet a -> UniqSet a
intersectUniqSets (UniqSet s) (UniqSet t) = UniqSet (intersectUFM s t)

disjointUniqSets :: UniqSet a -> UniqSet a -> Bool
disjointUniqSets (UniqSet s) (UniqSet t) = disjointUFM s t

restrictUniqSetToUFM :: UniqSet a -> UniqFM b -> UniqSet a
restrictUniqSetToUFM (UniqSet s) m = UniqSet (intersectUFM s m)

uniqSetMinusUFM :: UniqSet a -> UniqFM b -> UniqSet a
uniqSetMinusUFM (UniqSet s) t = UniqSet (minusUFM s t)

elementOfUniqSet :: Uniquable a => a -> UniqSet a -> Bool
elementOfUniqSet a (UniqSet s) = elemUFM a s

elemUniqSet_Directly :: Unique -> UniqSet a -> Bool
elemUniqSet_Directly a (UniqSet s) = elemUFM_Directly a s

filterUniqSet :: (a -> Bool) -> UniqSet a -> UniqSet a
filterUniqSet p (UniqSet s) = UniqSet (filterUFM p s)

filterUniqSet_Directly :: (Unique -> elt -> Bool) -> UniqSet elt -> UniqSet elt
filterUniqSet_Directly f (UniqSet s) = UniqSet (filterUFM_Directly f s)

partitionUniqSet :: (a -> Bool) -> UniqSet a -> (UniqSet a, UniqSet a)
partitionUniqSet p (UniqSet s) = coerce (partitionUFM p s)

uniqSetAny :: (a -> Bool) -> UniqSet a -> Bool
uniqSetAny p (UniqSet s) = anyUFM p s

uniqSetAll :: (a -> Bool) -> UniqSet a -> Bool
uniqSetAll p (UniqSet s) = allUFM p s

sizeUniqSet :: UniqSet a -> Int
sizeUniqSet (UniqSet s) = sizeUFM s

isEmptyUniqSet :: UniqSet a -> Bool
isEmptyUniqSet (UniqSet s) = isNullUFM s

lookupUniqSet :: Uniquable a => UniqSet b -> a -> Maybe b
lookupUniqSet (UniqSet s) k = lookupUFM s k

lookupUniqSet_Directly :: UniqSet a -> Unique -> Maybe a
lookupUniqSet_Directly (UniqSet s) k = lookupUFM_Directly s k

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetEltsUniqSet :: UniqSet elt -> [elt]
nonDetEltsUniqSet = nonDetEltsUFM . getUniqSet'

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetKeysUniqSet :: UniqSet elt -> [Unique]
nonDetKeysUniqSet = nonDetKeysUFM . getUniqSet'

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetFoldUniqSet :: (elt -> a -> a) -> a -> UniqSet elt -> a
nonDetFoldUniqSet c n (UniqSet s) = nonDetFoldUFM c n s

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetFoldUniqSet_Directly:: (Unique -> elt -> a -> a) -> a -> UniqSet elt -> a
nonDetFoldUniqSet_Directly f n (UniqSet s) = nonDetFoldUFM_Directly f n s

-- See Note [UniqSet invariant]
mapUniqSet :: Uniquable b => (a -> b) -> UniqSet a -> UniqSet b
mapUniqSet f = mkUniqSet . map f . nonDetEltsUniqSet

-- Two 'UniqSet's are considered equal if they contain the same
-- uniques.
instance Eq (UniqSet a) where
  UniqSet a == UniqSet b = equalKeysUFM a b

getUniqSet :: UniqSet a -> UniqFM a
getUniqSet = getUniqSet'

-- | 'unsafeUFMToUniqSet' converts a @'UniqFM' a@ into a @'UniqSet' a@
-- assuming, without checking, that it maps each 'Unique' to a value
-- that has that 'Unique'. See Note [UniqSet invariant].
unsafeUFMToUniqSet :: UniqFM a -> UniqSet a
unsafeUFMToUniqSet = UniqSet

instance Outputable a => Outputable (UniqSet a) where
    ppr = pprUniqSet ppr

pprUniqSet :: (a -> SDoc) -> UniqSet a -> SDoc
-- It's OK to use nonDetUFMToList here because we only use it for
-- pretty-printing.
pprUniqSet f = braces . pprWithCommas f . nonDetEltsUniqSet
