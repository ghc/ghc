%
% (c) The University of Glasgow 2006
% (c) The AQUA Project, Glasgow University, 1994-1998
%
\section[UniqSet]{Specialised sets, for things with @Uniques@}

Based on @UniqFMs@ (as you would expect).

Basically, the things need to be in class @Uniquable@.

\begin{code}
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
        foldUniqSet,
        mapUniqSet,
        elementOfUniqSet,
        elemUniqSet_Directly,
        filterUniqSet,
        sizeUniqSet,
        isEmptyUniqSet,
        lookupUniqSet,
        uniqSetToList,
        partitionUniqSet
    ) where

import UniqFM
import Unique

\end{code}

%************************************************************************
%*                                                                      *
\subsection{The signature of the module}
%*                                                                      *
%************************************************************************

\begin{code}
emptyUniqSet :: UniqSet a
unitUniqSet :: Uniquable a => a -> UniqSet a
mkUniqSet :: Uniquable a => [a]  -> UniqSet a

addOneToUniqSet :: Uniquable a => UniqSet a -> a -> UniqSet a
addOneToUniqSet_C :: Uniquable a => (a -> a -> a) -> UniqSet a -> a -> UniqSet a
addListToUniqSet :: Uniquable a => UniqSet a -> [a] -> UniqSet a

delOneFromUniqSet :: Uniquable a => UniqSet a -> a -> UniqSet a
delOneFromUniqSet_Directly :: Uniquable a => UniqSet a -> Unique -> UniqSet a
delListFromUniqSet :: Uniquable a => UniqSet a -> [a] -> UniqSet a

unionUniqSets :: UniqSet a -> UniqSet a -> UniqSet a
unionManyUniqSets :: [UniqSet a] -> UniqSet a
minusUniqSet  :: UniqSet a -> UniqSet a -> UniqSet a
intersectUniqSets :: UniqSet a -> UniqSet a -> UniqSet a

foldUniqSet :: (a -> b -> b) -> b -> UniqSet a -> b
mapUniqSet :: (a -> b) -> UniqSet a -> UniqSet b
elementOfUniqSet :: Uniquable a => a -> UniqSet a -> Bool
elemUniqSet_Directly :: Unique -> UniqSet a -> Bool
filterUniqSet :: (a -> Bool) -> UniqSet a -> UniqSet a
partitionUniqSet :: (a -> Bool) -> UniqSet a -> (UniqSet a, UniqSet a)

sizeUniqSet :: UniqSet a -> Int
isEmptyUniqSet :: UniqSet a -> Bool
lookupUniqSet :: Uniquable a => UniqSet a -> a -> Maybe a
uniqSetToList :: UniqSet a -> [a]
\end{code}
%************************************************************************
%*                                                                      *
\subsection{Implementation using ``UniqFM''}
%*                                                                      *
%************************************************************************

\begin{code}

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

foldUniqSet = foldUFM
mapUniqSet = mapUFM
elementOfUniqSet = elemUFM
elemUniqSet_Directly = elemUFM_Directly
filterUniqSet = filterUFM
partitionUniqSet = partitionUFM

sizeUniqSet = sizeUFM
isEmptyUniqSet = isNullUFM
lookupUniqSet = lookupUFM
uniqSetToList = eltsUFM

\end{code}
