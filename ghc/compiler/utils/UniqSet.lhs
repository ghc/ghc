%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[UniqSet]{Specialised sets, for things with @Uniques@}

Based on @UniqFMs@ (as you would expect).

Basically, the things need to be in class @Uniquable@.

\begin{code}
#include "HsVersions.h"

module UniqSet (
	UniqSet(..),    -- abstract type: NOT

	mkUniqSet, uniqSetToList, emptyUniqSet, unitUniqSet,
	addOneToUniqSet,
	unionUniqSets, unionManyUniqSets, minusUniqSet,
	elementOfUniqSet, mapUniqSet, intersectUniqSets,
	isEmptyUniqSet
    ) where

IMP_Ubiq(){-uitous-}

import Maybes		( maybeToBool, Maybe )
import UniqFM
import Unique		( Unique )
--import Outputable	( Outputable(..), ExportFlag )
import SrcLoc		( SrcLoc )
import Pretty		( Pretty(..), PrettyRep )
import PprStyle		( PprStyle )
import Util		( Ord3(..) )

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
addOneToUniqSet set x = set `unionUniqSets` unitUniqSet x

unionUniqSets :: UniqSet a -> UniqSet a -> UniqSet a
unionUniqSets (MkUniqSet set1) (MkUniqSet set2) = MkUniqSet (plusUFM set1 set2)

unionManyUniqSets :: [UniqSet a] -> UniqSet a
	-- = foldr unionUniqSets emptyUniqSet ss
unionManyUniqSets []	 = emptyUniqSet
unionManyUniqSets [s]	 = s
unionManyUniqSets (s:ss) = s `unionUniqSets` unionManyUniqSets ss

minusUniqSet  :: UniqSet a -> UniqSet a -> UniqSet a
minusUniqSet (MkUniqSet set1) (MkUniqSet set2) = MkUniqSet (minusUFM set1 set2)

intersectUniqSets :: UniqSet a -> UniqSet a -> UniqSet a
intersectUniqSets (MkUniqSet set1) (MkUniqSet set2) = MkUniqSet (intersectUFM set1 set2)

elementOfUniqSet :: Uniquable a => a -> UniqSet a -> Bool
elementOfUniqSet x (MkUniqSet set) = maybeToBool (lookupUFM set x)

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

%************************************************************************
%*									*
\subsection{The @IdSet@ and @TyVarSet@ specialisations for sets of Ids/TyVars}
%*									*
%************************************************************************

@IdSet@ is a specialised version, optimised for sets of Ids.

\begin{code}
--type NameSet           = UniqSet Name
--type GenTyVarSet flexi = UniqSet (GenTyVar flexi)
--type GenIdSet ty       = UniqSet (GenId ty)

#if ! OMIT_NATIVE_CODEGEN
--type RegSet   = UniqSet Reg
#endif

#if 0
#if __GLASGOW_HASKELL__
{-# SPECIALIZE
    unitUniqSet :: GenId ty       -> GenIdSet ty,
			GenTyVar flexi -> GenTyVarSet flexi,
			Name  -> NameSet
    IF_NCG(COMMA	Reg   -> RegSet)
    #-}

{-# SPECIALIZE
    mkUniqSet :: [GenId ty]    -> GenIdSet ty,
		 [GenTyVar flexi] -> GenTyVarSet flexi,
		 [Name]  -> NameSet
    IF_NCG(COMMA [Reg]   -> RegSet)
    #-}

{-# SPECIALIZE
    elementOfUniqSet :: GenId ty       -> GenIdSet ty       -> Bool,
		    	GenTyVar flexi -> GenTyVarSet flexi -> Bool,
			Name  -> NameSet  -> Bool
    IF_NCG(COMMA	Reg   -> RegSet   -> Bool)
    #-}

{-# SPECIALIZE
    mapUniqSet :: (GenId ty       -> GenId ty)       -> GenIdSet ty        -> GenIdSet ty,
		  (GenTyVar flexi -> GenTyVar flexi) -> GenTyVarSet flexi -> GenTyVarSet flexi,
		  (Name  -> Name)  -> NameSet  -> NameSet
    IF_NCG(COMMA  (Reg  -> Reg)    -> RegSet   -> RegSet)
    #-}
#endif
#endif
\end{code}
