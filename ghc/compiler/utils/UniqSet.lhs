%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[UniqSet]{Specialised sets, for things with @Uniques@}

Based on @UniqFMs@ (as you would expect).

Basically, the things need to be in class @Uniquable@.

\begin{code}
module UniqSet (
	UniqSet,    -- abstract type: NOT

	mkUniqSet, uniqSetToList, emptyUniqSet, unitUniqSet,
	addOneToUniqSet, addListToUniqSet,
	unionUniqSets, unionManyUniqSets, minusUniqSet,
	elementOfUniqSet, mapUniqSet, intersectUniqSets,
	isEmptyUniqSet, filterUniqSet, sizeUniqSet
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} Name

import Maybes		( maybeToBool )
import UniqFM
import Unique		( Unique, Uniquable(..) )
import SrcLoc		( SrcLoc )
import Outputable	( Outputable(..) )

#if ! OMIT_NATIVE_CODEGEN
#define IF_NCG(a) a
#else
#define IF_NCG(a) {--}
#endif
\end{code}

%************************************************************************
%*									*
\subsection{The @UniqSet@ type}
%*									*
%************************************************************************

We use @UniqFM@, with a (@uniqueOf@-able) @Unique@ as ``key''
and the thing itself as the ``value'' (for later retrieval).

\begin{code}
--data UniqSet a = MkUniqSet (FiniteMap Unique a) : NOT

type UniqSet a = UniqFM a
#define MkUniqSet {--}

emptyUniqSet :: UniqSet a
emptyUniqSet = MkUniqSet emptyUFM

unitUniqSet :: Uniquable a => a -> UniqSet a
unitUniqSet x = MkUniqSet (unitUFM x x)

uniqSetToList :: UniqSet a -> [a]
uniqSetToList (MkUniqSet set) = eltsUFM set

mkUniqSet :: Uniquable a => [a]  -> UniqSet a
mkUniqSet xs = MkUniqSet (listToUFM [ (x, x) | x <- xs])

addOneToUniqSet :: Uniquable a => UniqSet a -> a -> UniqSet a
addOneToUniqSet (MkUniqSet set) x = MkUniqSet (addToUFM set x x)

addListToUniqSet :: Uniquable a => UniqSet a -> [a] -> UniqSet a
addListToUniqSet (MkUniqSet set) xs = MkUniqSet (addListToUFM set [(x,x) | x<-xs])

unionUniqSets :: UniqSet a -> UniqSet a -> UniqSet a
unionUniqSets (MkUniqSet set1) (MkUniqSet set2) = MkUniqSet (plusUFM set1 set2)

unionManyUniqSets :: [UniqSet a] -> UniqSet a
	-- = foldr unionUniqSets emptyUniqSet ss
unionManyUniqSets []	 = emptyUniqSet
unionManyUniqSets [s]	 = s
unionManyUniqSets (s:ss) = s `unionUniqSets` unionManyUniqSets ss

minusUniqSet  :: UniqSet a -> UniqSet a -> UniqSet a
minusUniqSet (MkUniqSet set1) (MkUniqSet set2) = MkUniqSet (minusUFM set1 set2)

filterUniqSet :: (a -> Bool) -> UniqSet a -> UniqSet a
filterUniqSet pred (MkUniqSet set) = MkUniqSet (filterUFM pred set)

intersectUniqSets :: UniqSet a -> UniqSet a -> UniqSet a
intersectUniqSets (MkUniqSet set1) (MkUniqSet set2) = MkUniqSet (intersectUFM set1 set2)

elementOfUniqSet :: Uniquable a => a -> UniqSet a -> Bool
elementOfUniqSet x (MkUniqSet set) = maybeToBool (lookupUFM set x)

sizeUniqSet :: UniqSet a -> Int
sizeUniqSet (MkUniqSet set) = sizeUFM set

isEmptyUniqSet :: UniqSet a -> Bool
isEmptyUniqSet (MkUniqSet set) = isNullUFM set {-SLOW: sizeUFM set == 0-}

mapUniqSet :: Uniquable b => (a -> b) -> UniqSet a -> UniqSet b
mapUniqSet f (MkUniqSet set)
  = MkUniqSet (listToUFM [ let
			     mapped_thing = f thing
			  in
			  (mapped_thing, mapped_thing)
			| thing <- eltsUFM set ])
\end{code}

\begin{code}
#if __GLASGOW_HASKELL__
{-# SPECIALIZE
    addOneToUniqSet :: UniqSet Unique -> Unique -> UniqSet Unique
    #-}
{-# SPECIALIZE
    elementOfUniqSet :: Name -> UniqSet Name -> Bool
		      , Unique -> UniqSet Unique -> Bool
    #-}
{-# SPECIALIZE
    mkUniqSet :: [Name] -> UniqSet Name
    #-}

{-# SPECIALIZE
    unitUniqSet :: Name -> UniqSet Name
		 , Unique -> UniqSet Unique
    #-}
#endif
\end{code}
