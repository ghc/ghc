{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1994-1998

\section[UniqSet]{Specialised sets, for things with @Uniques@}

Based on @UniqFMs@ (as you would expect).

Basically, the things need to be in class @Uniquable@.
-}

module UniqSet (
        -- * Unique set type
        UniqSet,    -- type synonym for UniqFM a

        -- ** Manipulating these sets
        emptyUniqSet,
        unitUniqSet,
        mkUniqSet,
        addOneToUniqSet, addOneToUniqSet_C, addListToUniqSet,
        delOneFromUniqSet, delOneFromUniqSet_Directly, delListFromUniqSet,
        unionUniqSets, unionManyUniqSets,
        minusUniqSet,
        intersectUniqSets,
        uniqSetAny, uniqSetAll,
        elementOfUniqSet,
        elemUniqSet_Directly,
        filterUniqSet,
        sizeUniqSet,
        isEmptyUniqSet,
        lookupUniqSet,
        partitionUniqSet
    ) where

import UniqFM
import Unique

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
addOneToUniqSet_C :: Uniquable a => (a -> a -> a) -> UniqSet a -> a -> UniqSet a
addListToUniqSet :: Uniquable a => UniqSet a -> [a] -> UniqSet a

delOneFromUniqSet :: Uniquable a => UniqSet a -> a -> UniqSet a
delOneFromUniqSet_Directly :: UniqSet a -> Unique -> UniqSet a
delListFromUniqSet :: Uniquable a => UniqSet a -> [a] -> UniqSet a

unionUniqSets :: UniqSet a -> UniqSet a -> UniqSet a
unionManyUniqSets :: [UniqSet a] -> UniqSet a
minusUniqSet  :: UniqSet a -> UniqSet a -> UniqSet a
intersectUniqSets :: UniqSet a -> UniqSet a -> UniqSet a

elementOfUniqSet :: Uniquable a => a -> UniqSet a -> Bool
elemUniqSet_Directly :: Unique -> UniqSet a -> Bool
filterUniqSet :: (a -> Bool) -> UniqSet a -> UniqSet a
partitionUniqSet :: (a -> Bool) -> UniqSet a -> (UniqSet a, UniqSet a)

sizeUniqSet :: UniqSet a -> Int
isEmptyUniqSet :: UniqSet a -> Bool
lookupUniqSet :: Uniquable a => UniqSet b -> a -> Maybe b

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

type UniqSet a = UniqFM a

emptyUniqSet = emptyUFM
unitUniqSet x = unitUFM x x
mkUniqSet = foldl addOneToUniqSet emptyUniqSet

addOneToUniqSet set x = addToUFM set x x
addOneToUniqSet_C f set x = addToUFM_C f set x x
addListToUniqSet = foldl addOneToUniqSet

delOneFromUniqSet = delFromUFM
delOneFromUniqSet_Directly = delFromUFM_Directly
delListFromUniqSet = delListFromUFM

unionUniqSets = plusUFM
unionManyUniqSets [] = emptyUniqSet
unionManyUniqSets sets = foldr1 unionUniqSets sets
minusUniqSet = minusUFM
intersectUniqSets = intersectUFM

elementOfUniqSet = elemUFM
elemUniqSet_Directly = elemUFM_Directly
filterUniqSet = filterUFM
partitionUniqSet = partitionUFM

sizeUniqSet = sizeUFM
isEmptyUniqSet = isNullUFM
lookupUniqSet = lookupUFM

uniqSetAny :: (a -> Bool) -> UniqSet a -> Bool
uniqSetAny = anyUFM

uniqSetAll :: (a -> Bool) -> UniqSet a -> Bool
uniqSetAll = allUFM
