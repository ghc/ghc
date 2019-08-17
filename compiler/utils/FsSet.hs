{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1994-1998

\section[FsSet]{Specialised sets, for things with @Uniques@}

Based on @FastStringEnvs@ (as you would expect).

Basically, the things need to be in class @Uniquable@.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module FsSet (
        -- * Unique set type
        FsSet,    -- type synonym for FastStringEnv a
        getFsSet,
        pprFsSet,

        -- ** Manipulating these sets
        emptyFsSet,
        unitFsSet,
        mkFsSet,
        addOneToFsSet, addListToFsSet,
        delOneFromFsSet, delListFromFsSet,
        unionFsSets, unionManyFsSets,
        minusFsSet, uniqSetMinusUFM,
        intersectFsSets,
        restrictFsSetToUFM,
        uniqSetAny, uniqSetAll,
        elementOfFsSet,
--        elemFsSet_Directly,
        filterFsSet,
--        filterFsSet_Directly,
        sizeFsSet,
        isEmptyFsSet,
        lookupFsSet,
        lookupFsSet_Directly,
        partitionFsSet,
--        mapFsSet,
        unsafeUFMToFsSet,
        nonDetEltsFsSet
    ) where

import GhcPrelude

import FastStringEnv
import Data.Coerce
import Outputable
import Data.Data
import FastString
import UniqMap
import qualified Data.Semigroup as Semi

-- Note [FsSet invariant]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- FsSet has the following invariant:
--   The keys in the map are the uniques of the values
-- It means that to implement mapFsSet you have to update
-- both the keys and the values.

newtype FsSet a = FsSet {getFsSet' :: FastStringEnv a}
                  deriving (Data, Semi.Semigroup, Monoid)

emptyFsSet :: FsSet a
emptyFsSet = FsSet emptyFsEnv

unitFsSet :: HasFastString a => a -> FsSet a
unitFsSet x = FsSet $ unitFsEnv x x

mkFsSet :: HasFastString a => [a]  -> FsSet a
mkFsSet = foldl' addOneToFsSet emptyFsSet

addOneToFsSet :: HasFastString a => FsSet a -> a -> FsSet a
addOneToFsSet (FsSet set) x = FsSet (addToUniqMap set (coerce $ getFastString x) x)

addListToFsSet :: HasFastString a => FsSet a -> [a] -> FsSet a
addListToFsSet = foldl' addOneToFsSet

delOneFromFsSet :: HasFastString a => FsSet a -> a -> FsSet a
delOneFromFsSet (FsSet s) a = FsSet (delFromUniqMap s (coerce $ getFastString a))

--delOneFromFsSet_Directly :: FsSet a -> Unique -> FsSet a
--delOneFromFsSet_Directly (FsSet s) u = FsSet (delFromUFM_Directly s u)

delListFromFsSet :: HasFastString a => FsSet a -> [a] -> FsSet a
delListFromFsSet (FsSet s) l = FsSet (delListFromUniqMap s (coerce (map getFastString l)))

--delListFromFsSet_Directly :: FsSet a -> [Unique] -> FsSet a
--delListFromFsSet_Directly (FsSet s) l =
--    FsSet (delListFromUFM_Directly s l)

unionFsSets :: FsSet a -> FsSet a -> FsSet a
unionFsSets (FsSet s) (FsSet t) = FsSet (plusUniqMap s t)

unionManyFsSets :: [FsSet a] -> FsSet a
unionManyFsSets = foldl' (flip unionFsSets) emptyFsSet

minusFsSet  :: FsSet a -> FsSet a -> FsSet a
minusFsSet (FsSet s) (FsSet t) = FsSet (minusUniqMap s t)

intersectFsSets :: FsSet a -> FsSet a -> FsSet a
intersectFsSets (FsSet s) (FsSet t) = FsSet (intersectUniqMap s t)

restrictFsSetToUFM :: FsSet a -> FastStringEnv b -> FsSet a
restrictFsSetToUFM (FsSet s) m = FsSet (intersectUniqMap s m)

uniqSetMinusUFM :: FsSet a -> FastStringEnv b -> FsSet a
uniqSetMinusUFM (FsSet s) t = FsSet (minusUniqMap s t)

elementOfFsSet :: HasFastString a => a -> FsSet a -> Bool
elementOfFsSet a (FsSet s) = elemUniqMap (coerce $ getFastString a) s

--elemFsSet_Directly :: FastStringU -> FsSet a -> Bool
--elemFsSet_Directly a (FsSet s) = elemUniq a s

filterFsSet :: (a -> Bool) -> FsSet a -> FsSet a
filterFsSet p (FsSet s) = FsSet (filterUniqMap p s)

--filterFsSet_Directly :: (Unique -> elt -> Bool) -> FsSet elt -> FsSet elt
--filterFsSet_Directly f (FsSet s) = FsSet (filterUniqMap f s)

partitionFsSet :: (a -> Bool) -> FsSet a -> (FsSet a, FsSet a)
partitionFsSet p (FsSet s) = coerce (partitionUniqMap p s)

uniqSetAny :: (a -> Bool) -> FsSet a -> Bool
uniqSetAny p (FsSet s) = anyUniqMap p s

uniqSetAll :: (a -> Bool) -> FsSet a -> Bool
uniqSetAll p (FsSet s) = allUniqMap p s

sizeFsSet :: FsSet a -> Int
sizeFsSet (FsSet s) = sizeUniqMap s

isEmptyFsSet :: FsSet a -> Bool
isEmptyFsSet (FsSet s) = isNullUniqMap s

lookupFsSet :: HasFastString a => FsSet b -> a -> Maybe b
lookupFsSet (FsSet s) k = lookupFsEnv s k

lookupFsSet_Directly :: FsSet a -> FastString -> Maybe a
lookupFsSet_Directly (FsSet s) k = lookupFsEnv s k

-- See Note [Deterministic FastStringEnv] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetEltsFsSet :: FsSet elt -> [elt]
nonDetEltsFsSet = nonDetEltsUniqMap . getFsSet'

{-
-- See Note [Deterministic FastStringEnv] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetKeysFsSet :: FsSet elt -> [Unique]
nonDetKeysFsSet = nonDetKeysUFM . getFsSet'

-- See Note [Deterministic FastStringEnv] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetFoldFsSet :: (elt -> a -> a) -> a -> FsSet elt -> a
nonDetFoldFsSet c n (FsSet s) = nonDetFoldUFM c n s
-}


{-
-- See Note [Deterministic FastStringEnv] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetFoldFsSet_Directly:: (Unique -> elt -> a -> a) -> a -> FsSet elt -> a
nonDetFoldFsSet_Directly f n (FsSet s) = nonDetFoldUFM_Directly f n s

-- See Note [FsSet invariant]
mapFsSet :: HasFastString b => (a -> b) -> FsSet a -> FsSet b
mapFsSet f = mkFsSet . map f . nonDetEltsFsSet

-}

getFsSet :: FsSet a -> FastStringEnv a
getFsSet = getFsSet'

-- | 'unsafeUFMToFsSet' converts a @'FastStringEnv' a@ into a @'FsSet' a@
-- assuming, without checking, that it maps each 'Unique' to a value
-- that has that 'Unique'. See Note [FsSet invariant].
unsafeUFMToFsSet :: FastStringEnv a -> FsSet a
unsafeUFMToFsSet = FsSet

instance Outputable a => Outputable (FsSet a) where
    ppr = pprFsSet ppr

pprFsSet :: (a -> SDoc) -> FsSet a -> SDoc
-- It's OK to use nonDetUFMToList here because we only use it for
-- pretty-printing.
pprFsSet f (FsSet s) = pprUniqMap f  s
