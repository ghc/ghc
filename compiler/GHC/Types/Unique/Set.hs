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
        minusUniqSet, uniqSetMinusUFM, uniqSetMinusUDFM,
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
        nonDetStrictFoldUniqSet,

        -- UniqueSet
        UniqueSet(..),
        nullUniqueSet,
        sizeUniqueSet,
        memberUniqueSet,
        emptyUniqueSet,
        singletonUniqueSet,
        insertUniqueSet,
        deleteUniqueSet,
        differenceUniqueSet,
        unionUniqueSet,
        unionsUniqueSet,
        intersectionUniqueSet,
        isSubsetOfUniqueSet,
        filterUniqueSet,
        foldlUniqueSet,
        foldrUniqueSet,
        elemsUniqueSet,
        fromListUniqueSet,
    ) where

import GHC.Prelude

import GHC.Types.Unique.DFM
import GHC.Types.Unique.FM
import GHC.Types.Unique
import Data.Coerce
import GHC.Utils.Outputable
import Data.Data
import qualified Data.Semigroup as Semi
import Control.DeepSeq
import qualified GHC.Data.Word64Set as S

-- Note [UniqSet invariant]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- UniqSet has the following invariant:
--   The keys in the map are the uniques of the values
-- It means that to implement mapUniqSet you have to update
-- both the keys and the values.

-- | Set of Uniquable values
newtype UniqSet a = UniqSet {getUniqSet' :: UniqFM a a}
                  deriving (Data, Semi.Semigroup, Monoid)

instance NFData a => NFData (UniqSet a) where
  rnf = forceUniqSet rnf

emptyUniqSet :: UniqSet a
emptyUniqSet = UniqSet emptyUFM

unitUniqSet :: Uniquable a => a -> UniqSet a
unitUniqSet x = UniqSet $ unitUFM x x

mkUniqSet :: Uniquable a => [a] -> UniqSet a
mkUniqSet = foldl' addOneToUniqSet emptyUniqSet
{-# INLINEABLE mkUniqSet #-}

addOneToUniqSet :: Uniquable a => UniqSet a -> a -> UniqSet a
addOneToUniqSet (UniqSet set) x = UniqSet (addToUFM set x x)

addListToUniqSet :: Uniquable a => UniqSet a -> [a] -> UniqSet a
addListToUniqSet = foldl' addOneToUniqSet
{-# INLINEABLE addListToUniqSet #-}

delOneFromUniqSet :: Uniquable a => UniqSet a -> a -> UniqSet a
delOneFromUniqSet (UniqSet s) a = UniqSet (delFromUFM s a)

delOneFromUniqSet_Directly :: UniqSet a -> Unique -> UniqSet a
delOneFromUniqSet_Directly (UniqSet s) u = UniqSet (delFromUFM_Directly s u)

delListFromUniqSet :: Uniquable a => UniqSet a -> [a] -> UniqSet a
delListFromUniqSet (UniqSet s) l = UniqSet (delListFromUFM s l)
{-# INLINEABLE delListFromUniqSet #-}

delListFromUniqSet_Directly :: UniqSet a -> [Unique] -> UniqSet a
delListFromUniqSet_Directly (UniqSet s) l =
    UniqSet (delListFromUFM_Directly s l)
{-# INLINEABLE delListFromUniqSet_Directly #-}

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

restrictUniqSetToUFM :: UniqSet key -> UniqFM key b -> UniqSet key
restrictUniqSetToUFM (UniqSet s) m = UniqSet (intersectUFM s m)

uniqSetMinusUFM :: UniqSet key -> UniqFM key b -> UniqSet key
uniqSetMinusUFM (UniqSet s) t = UniqSet (minusUFM s t)

uniqSetMinusUDFM :: UniqSet key -> UniqDFM key b -> UniqSet key
uniqSetMinusUDFM (UniqSet s) t = UniqSet (ufmMinusUDFM s t)

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

-- | What's the point you might ask? We might have changed an object
-- without it's key changing. In which case this lookup makes sense.
lookupUniqSet :: Uniquable key => UniqSet key -> key -> Maybe key
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
nonDetStrictFoldUniqSet :: (elt -> a -> a) -> a -> UniqSet elt -> a
nonDetStrictFoldUniqSet c n (UniqSet s) = nonDetStrictFoldUFM c n s

-- See Note [UniqSet invariant]
mapUniqSet :: Uniquable b => (a -> b) -> UniqSet a -> UniqSet b
mapUniqSet f = mkUniqSet . map f . nonDetEltsUniqSet

-- Two 'UniqSet's are considered equal if they contain the same
-- uniques.
instance Eq (UniqSet a) where
  UniqSet a == UniqSet b = equalKeysUFM a b

getUniqSet :: UniqSet a -> UniqFM a a
getUniqSet = getUniqSet'

-- | 'unsafeUFMToUniqSet' converts a @'UniqFM' a@ into a @'UniqSet' a@
-- assuming, without checking, that it maps each 'Unique' to a value
-- that has that 'Unique'. See Note [UniqSet invariant].
unsafeUFMToUniqSet :: UniqFM a a -> UniqSet a
unsafeUFMToUniqSet = UniqSet

instance Outputable a => Outputable (UniqSet a) where
    ppr = pprUniqSet ppr

pprUniqSet :: (a -> SDoc) -> UniqSet a -> SDoc
-- It's OK to use nonDetUFMToList here because we only use it for
-- pretty-printing.
pprUniqSet f = braces . pprWithCommas f . nonDetEltsUniqSet

forceUniqSet :: (a -> ()) -> UniqSet a -> ()
forceUniqSet f (UniqSet fm) = seqEltsUFM f fm

--------------------------------------------------------
-- UniqueSet
--------------------------------------------------------

-- | Set of Unique values
--
-- Similar to 'UniqSet Unique' but with a more compact representation.
newtype UniqueSet = US { unUniqueSet :: S.Word64Set }
  deriving (Eq, Ord, Show, Semigroup, Monoid)

{-# INLINE nullUniqueSet #-}
nullUniqueSet :: UniqueSet -> Bool
nullUniqueSet (US s) = S.null s

{-# INLINE sizeUniqueSet #-}
sizeUniqueSet :: UniqueSet -> Int
sizeUniqueSet (US s) = S.size s

{-# INLINE memberUniqueSet #-}
memberUniqueSet :: Unique -> UniqueSet -> Bool
memberUniqueSet k (US s) = S.member (getKey k) s

{-# INLINE emptyUniqueSet #-}
emptyUniqueSet :: UniqueSet
emptyUniqueSet = US S.empty

{-# INLINE singletonUniqueSet #-}
singletonUniqueSet :: Unique -> UniqueSet
singletonUniqueSet k = US (S.singleton (getKey k))

{-# INLINE insertUniqueSet #-}
insertUniqueSet :: Unique -> UniqueSet -> UniqueSet
insertUniqueSet k (US s) = US (S.insert (getKey k) s)

{-# INLINE deleteUniqueSet #-}
deleteUniqueSet :: Unique -> UniqueSet -> UniqueSet
deleteUniqueSet k (US s) = US (S.delete (getKey k) s)

{-# INLINE unionUniqueSet #-}
unionUniqueSet :: UniqueSet -> UniqueSet -> UniqueSet
unionUniqueSet (US x) (US y) = US (S.union x y)

{-# INLINE unionsUniqueSet #-}
unionsUniqueSet :: [UniqueSet] -> UniqueSet
unionsUniqueSet xs = US (S.unions (map unUniqueSet xs))

{-# INLINE differenceUniqueSet #-}
differenceUniqueSet :: UniqueSet -> UniqueSet -> UniqueSet
differenceUniqueSet (US x) (US y) = US (S.difference x y)

{-# INLINE intersectionUniqueSet #-}
intersectionUniqueSet :: UniqueSet -> UniqueSet -> UniqueSet
intersectionUniqueSet (US x) (US y) = US (S.intersection x y)

{-# INLINE isSubsetOfUniqueSet #-}
isSubsetOfUniqueSet :: UniqueSet -> UniqueSet -> Bool
isSubsetOfUniqueSet (US x) (US y) = S.isSubsetOf x y

{-# INLINE filterUniqueSet #-}
filterUniqueSet :: (Unique -> Bool) -> UniqueSet -> UniqueSet
filterUniqueSet f (US s) = US (S.filter (f . mkUniqueGrimily) s)

{-# INLINE foldlUniqueSet #-}
foldlUniqueSet :: (a -> Unique -> a) -> a -> UniqueSet -> a
foldlUniqueSet k z (US s) = S.foldl' (\a b -> k a (mkUniqueGrimily b)) z s

{-# INLINE foldrUniqueSet #-}
foldrUniqueSet :: (Unique -> b -> b) -> b -> UniqueSet -> b
foldrUniqueSet k z (US s) = S.foldr (k . mkUniqueGrimily) z s

{-# INLINE elemsUniqueSet #-}
elemsUniqueSet :: UniqueSet -> [Unique]
elemsUniqueSet (US s) = map mkUniqueGrimily (S.elems s)

{-# INLINE fromListUniqueSet #-}
fromListUniqueSet :: [Unique] -> UniqueSet
fromListUniqueSet ks = US (S.fromList (map getKey ks))
