{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1994-1998

\section[UniqSet]{Specialised sets, for things with @Uniques@}

Based on @UniqFMs@ (as you would expect).

Basically, the things need to be in class @Uniquable@.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

module UniqSet (
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

import UniqFM
import Unique
import Data.Coerce
import Outputable
import Data.Foldable (foldl')
import Data.Data
#if __GLASGOW_HASKELL__ >= 801
import qualified Data.Semigroup
#endif

{-
************************************************************************
*                                                                      *
\subsection{The signature of the module}
*                                                                      *
************************************************************************
-}

emptyUniqSet :: UniqSet a
unitUniqSet :: Uniquable a => a -> UniqSet a
mkUniqSet :: Uniquable a => [a]  -> UniqSet a

addOneToUniqSet :: Uniquable a => UniqSet a -> a -> UniqSet a
addListToUniqSet :: Uniquable a => UniqSet a -> [a] -> UniqSet a

delOneFromUniqSet :: Uniquable a => UniqSet a -> a -> UniqSet a
delOneFromUniqSet_Directly :: UniqSet a -> Unique -> UniqSet a
delListFromUniqSet :: Uniquable a => UniqSet a -> [a] -> UniqSet a
delListFromUniqSet_Directly :: UniqSet a -> [Unique] -> UniqSet a

unionUniqSets :: UniqSet a -> UniqSet a -> UniqSet a
unionManyUniqSets :: [UniqSet a] -> UniqSet a
minusUniqSet  :: UniqSet a -> UniqSet a -> UniqSet a
intersectUniqSets :: UniqSet a -> UniqSet a -> UniqSet a
restrictUniqSetToUFM :: UniqSet a -> UniqFM b -> UniqSet a
uniqSetMinusUFM :: UniqSet a -> UniqFM b -> UniqSet a

elementOfUniqSet :: Uniquable a => a -> UniqSet a -> Bool
elemUniqSet_Directly :: Unique -> UniqSet a -> Bool
filterUniqSet :: (a -> Bool) -> UniqSet a -> UniqSet a
filterUniqSet_Directly :: (Unique -> elt -> Bool) -> UniqSet elt -> UniqSet elt
partitionUniqSet :: (a -> Bool) -> UniqSet a -> (UniqSet a, UniqSet a)

sizeUniqSet :: UniqSet a -> Int
isEmptyUniqSet :: UniqSet a -> Bool
lookupUniqSet :: Uniquable a => UniqSet b -> a -> Maybe b
lookupUniqSet_Directly :: UniqSet a -> Unique -> Maybe a

nonDetEltsUniqSet :: UniqSet elt -> [elt]
nonDetKeysUniqSet :: UniqSet elt -> [Unique]

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetFoldUniqSet :: (elt -> a -> a) -> a -> UniqSet elt -> a

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetFoldUniqSet_Directly:: (Unique -> elt -> a -> a) -> a -> UniqSet elt -> a

mapUniqSet :: Uniquable b => (a -> b) -> UniqSet a -> UniqSet b

{-
************************************************************************
*                                                                      *
\subsection{Implementation using ``UniqFM''}
*                                                                      *
************************************************************************
-}

-- Note [Unsound mapUniqSet]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- UniqSet has the following invariant:
--   The keys in the map are the uniques of the values
-- It means that to implement mapUniqSet you'd have to update
-- both the keys and the values. There used to be an implementation
-- that only updated the values and it's been removed, because it broke
-- the invariant.

newtype UniqSet a = UniqSet {getUniqSet' :: UniqFM a} deriving Data

-- Two 'UniqSet's are considered equal if they contain the same
-- uniques.
instance Eq (UniqSet a) where
  UniqSet a == UniqSet b = equalKeysUFM a b

getUniqSet :: UniqSet a -> UniqFM a
getUniqSet = getUniqSet'

-- | 'unsafeUFMToUniqSet' converts a @'UniqFM' a@ into a @'UniqSet' a@
-- assuming, without checking, that it maps each 'Unique' to a value
-- that has that 'Unique'. See Note [Unsound mapUniqSet].
unsafeUFMToUniqSet :: UniqFM a -> UniqSet a
unsafeUFMToUniqSet = UniqSet

instance Outputable a => Outputable (UniqSet a) where
    ppr = pprUniqSet ppr
#if __GLASGOW_HASKELL__ >= 801
instance Data.Semigroup.Semigroup (UniqSet a) where
  (<>) = mappend
#endif
instance Monoid (UniqSet a) where
  mempty = UniqSet mempty
  UniqSet s `mappend` UniqSet t = UniqSet (s `mappend` t)

pprUniqSet :: (a -> SDoc) -> UniqSet a -> SDoc
pprUniqSet f (UniqSet s) = pprUniqFM f s

emptyUniqSet = UniqSet emptyUFM
unitUniqSet x = UniqSet $ unitUFM x x
mkUniqSet = foldl' addOneToUniqSet emptyUniqSet

addOneToUniqSet (UniqSet set) x = UniqSet (addToUFM set x x)
addListToUniqSet = foldl' addOneToUniqSet

delOneFromUniqSet (UniqSet s) a = UniqSet (delFromUFM s a)
delOneFromUniqSet_Directly (UniqSet s) u = UniqSet (delFromUFM_Directly s u)
delListFromUniqSet (UniqSet s) l = UniqSet (delListFromUFM s l)
delListFromUniqSet_Directly (UniqSet s) l =
    UniqSet (delListFromUFM_Directly s l)

unionUniqSets (UniqSet s) (UniqSet t) = UniqSet (plusUFM s t)

unionManyUniqSets = foldl' (flip unionUniqSets) emptyUniqSet

minusUniqSet (UniqSet s) (UniqSet t) = UniqSet (minusUFM s t)
uniqSetMinusUFM (UniqSet s) t = UniqSet (minusUFM s t)


intersectUniqSets (UniqSet s) (UniqSet t) = UniqSet (intersectUFM s t)
restrictUniqSetToUFM (UniqSet s) m = UniqSet (intersectUFM s m)

elementOfUniqSet a (UniqSet s) = elemUFM a s
elemUniqSet_Directly a (UniqSet s) = elemUFM_Directly a s
filterUniqSet p (UniqSet s) = UniqSet (filterUFM p s)
filterUniqSet_Directly f (UniqSet s) = UniqSet (filterUFM_Directly f s)

partitionUniqSet p (UniqSet s) = coerce (partitionUFM p s)

sizeUniqSet (UniqSet s) = sizeUFM s
isEmptyUniqSet (UniqSet s) = isNullUFM s
lookupUniqSet (UniqSet s) k = lookupUFM s k
lookupUniqSet_Directly (UniqSet s) k = lookupUFM_Directly s k

uniqSetAny :: (a -> Bool) -> UniqSet a -> Bool
uniqSetAny p (UniqSet s) = anyUFM p s

uniqSetAll :: (a -> Bool) -> UniqSet a -> Bool
uniqSetAll p (UniqSet s) = allUFM p s

nonDetFoldUniqSet c n (UniqSet s) = nonDetFoldUFM c n s
nonDetFoldUniqSet_Directly f n (UniqSet s) = nonDetFoldUFM_Directly f n s
nonDetEltsUniqSet = nonDetEltsUFM . getUniqSet'
nonDetKeysUniqSet = nonDetKeysUFM . getUniqSet'

mapUniqSet f = mkUniqSet . map f . nonDetEltsUniqSet
