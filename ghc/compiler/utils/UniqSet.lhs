%
% (c) The AQUA Project, Glasgow University, 1994-1995
%
\section[UniqSet]{Specialised sets, for things with @Uniques@}

Based on @UniqFMs@ (as you would expect).

Basically, the things need to be in class @NamedThing@.

We also export specialisations for @Ids@ and @TyVars@.

\begin{code}
#include "HsVersions.h"

module UniqSet (
	UniqSet(..),    -- abstract type: NOT

	mkUniqSet, uniqSetToList, emptyUniqSet, singletonUniqSet,
	unionUniqSets, unionManyUniqSets, minusUniqSet,
	elementOfUniqSet, mapUniqSet,
	intersectUniqSets, isEmptyUniqSet,
	
	-- specalised for Ids:
	IdSet(..),

	-- specalised for TyVars:
	TyVarSet(..),

	-- specalised for Names:
	NameSet(..),

	-- to make the interface self-sufficient
	Id, TyVar, Name,

	UniqFM, Unique

	-- and to be pragma friendly
#ifdef USE_ATTACK_PRAGMAS
	, emptyUFM, intersectUFM, isNullUFM, minusUFM, singletonUFM,
	plusUFM, eltsUFM,
	u2i
#endif
    ) where

import UniqFM
import Id		-- for specialisation to Ids
import IdInfo		-- sigh
import Maybes		( maybeToBool, Maybe(..) )
import Name
import Outputable
import AbsUniType	-- for specialisation to TyVars
import Util
#if ! OMIT_NATIVE_CODEGEN
import AsmRegAlloc	( Reg )
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

We use @UniqFM@, with a (@getTheUnique@-able) @Unique@ as ``key''
and the thing itself as the ``value'' (for later retrieval).

\begin{code}
--data UniqSet a = MkUniqSet (FiniteMap Unique a) : NOT

type UniqSet a = UniqFM a
#define MkUniqSet {--}

emptyUniqSet :: UniqSet a
emptyUniqSet = MkUniqSet emptyUFM

singletonUniqSet :: NamedThing a => a -> UniqSet a
singletonUniqSet x = MkUniqSet (singletonUFM x x)

uniqSetToList :: UniqSet a -> [a]
uniqSetToList (MkUniqSet set) = BSCC("uniqSetToList") eltsUFM set ESCC

mkUniqSet :: NamedThing a => [a]  -> UniqSet a
mkUniqSet xs = MkUniqSet (listToUFM [ (x, x) | x <- xs])

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

elementOfUniqSet :: NamedThing a => a -> UniqSet a -> Bool
elementOfUniqSet x (MkUniqSet set) = maybeToBool (lookupUFM set x)

isEmptyUniqSet :: UniqSet a -> Bool
isEmptyUniqSet (MkUniqSet set) = isNullUFM set {-SLOW: sizeUFM set == 0-}

mapUniqSet :: NamedThing b => (a -> b) -> UniqSet a -> UniqSet b
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
type IdSet    = UniqSet Id
type TyVarSet = UniqSet TyVar
type NameSet  = UniqSet Name
#if ! OMIT_NATIVE_CODEGEN
type RegSet   = UniqSet Reg
#endif

#if __GLASGOW_HASKELL__
    -- avoid hbc bug (0.999.7)
{-# SPECIALIZE
    singletonUniqSet :: Id    -> IdSet,
			TyVar -> TyVarSet,
			Name  -> NameSet
    IF_NCG(COMMA	Reg   -> RegSet)
    #-}

{-# SPECIALIZE
    mkUniqSet :: [Id]    -> IdSet,
		 [TyVar] -> TyVarSet,
		 [Name]  -> NameSet
    IF_NCG(COMMA [Reg]   -> RegSet)
    #-}

{-# SPECIALIZE
    elementOfUniqSet :: Id    -> IdSet    -> Bool,
		    	TyVar -> TyVarSet -> Bool,
			Name  -> NameSet  -> Bool
    IF_NCG(COMMA	Reg   -> RegSet   -> Bool)
    #-}

{-# SPECIALIZE
    mapUniqSet :: (Id    -> Id)    -> IdSet    -> IdSet,
		  (TyVar -> TyVar) -> TyVarSet -> TyVarSet,
		  (Name  -> Name)  -> NameSet  -> NameSet
    IF_NCG(COMMA  (Reg  -> Reg)    -> RegSet   -> RegSet)
    #-}
#endif
\end{code}
