%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{@VarSet@: Variable sets}

\begin{code}
module VarSet (
	VarSet, IdSet, GenIdSet, TyVarSet, GenTyVarSet, IdOrTyVarSet,
	emptyVarSet, unitVarSet, mkVarSet,
	extendVarSet,
	elemVarSet, varSetElems,
	unionVarSet, unionVarSets,
	intersectVarSet, intersectsVarSet,
	isEmptyVarSet, delVarSet,
	minusVarSet, foldVarSet, filterVarSet,
	lookupVarSet, mapVarSet,

	uniqAway
    ) where

#include "HsVersions.h"

import Var		( Var, Id, GenId, TyVar, GenTyVar, IdOrTyVar, setVarUnique )
import Unique		( Uniquable(..), incrUnique )
import UniqSet
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection{@VarSet@s}
%*									*
%************************************************************************

\begin{code}
type VarSet fs ft      = UniqSet (Var fs ft)
type IdSet 	       = UniqSet Id
type GenIdSet flexi    = UniqSet (GenId flexi)
type TyVarSet	       = UniqSet TyVar
type GenTyVarSet flexi = UniqSet (GenTyVar flexi)
type IdOrTyVarSet      = UniqSet IdOrTyVar

emptyVarSet	:: VarSet fs ft
intersectVarSet	:: VarSet fs ft -> VarSet fs ft -> VarSet fs ft
intersectsVarSet:: VarSet fs ft -> VarSet fs ft -> Bool 	-- True if non-empty intersection
unionVarSet	:: VarSet fs ft -> VarSet fs ft -> VarSet fs ft
unionVarSets	:: [VarSet fs ft] -> VarSet fs ft
varSetElems	:: VarSet fs ft -> [Var fs ft]
unitVarSet	:: Var fs ft -> VarSet fs ft
extendVarSet	:: VarSet fs ft -> Var fs ft -> VarSet fs ft
elemVarSet	:: Var fs ft -> VarSet fs ft -> Bool
delVarSet	:: VarSet fs ft -> Var fs ft -> VarSet fs ft
minusVarSet	:: VarSet fs ft -> VarSet fs ft -> VarSet fs ft
isEmptyVarSet	:: VarSet fs ft -> Bool
mkVarSet	:: [Var fs ft] -> VarSet fs ft
foldVarSet	:: (Var fs ft -> a -> a) -> a -> VarSet fs ft -> a
lookupVarSet	:: VarSet fs ft -> Var fs ft -> Maybe (Var fs ft)
			-- Returns the set element, which may be
			-- (==) to the argument, but not the same as
mapVarSet 	:: (Var fs ft -> Var fs ft) -> VarSet fs ft -> VarSet fs ft
filterVarSet	:: (Var fs ft -> Bool) -> VarSet fs ft -> VarSet fs ft

emptyVarSet	= emptyUniqSet
unitVarSet	= unitUniqSet
extendVarSet	= addOneToUniqSet
intersectVarSet	= intersectUniqSets
intersectsVarSet s1 s2 = not (isEmptyVarSet (s1 `intersectVarSet` s2))
unionVarSet	= unionUniqSets
unionVarSets	= unionManyUniqSets
varSetElems	= uniqSetToList
elemVarSet	= elementOfUniqSet
minusVarSet	= minusUniqSet
delVarSet	= delOneFromUniqSet
isEmptyVarSet	= isEmptyUniqSet
mkVarSet	= mkUniqSet
foldVarSet	= foldUniqSet
lookupVarSet	= lookupUniqSet
mapVarSet	= mapUniqSet
filterVarSet	= filterUniqSet
\end{code}

\begin{code}
uniqAway :: VarSet fs ft -> Var fs ft -> Var fs ft
-- Give the Var a new unique, different to any in the VarSet
uniqAway set var
  = try 1 (incrUnique (getUnique var))
  where
    try n uniq | uniq `elemUniqSet_Directly` set = try ((n+1)::Int) (incrUnique uniq)
	       | otherwise = {- pprTrace "uniqAway:" (ppr n <+> text "tries") -}
			     setVarUnique var uniq
\end{code}
