%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{@VarSet@: Variable sets}

\begin{code}
module VarSet (
	VarSet, IdSet, TyVarSet, UVarSet,
	emptyVarSet, unitVarSet, mkVarSet,
	extendVarSet, extendVarSet_C,
	elemVarSet, varSetElems, subVarSet,
	unionVarSet, unionVarSets,
	intersectVarSet, intersectsVarSet,
	isEmptyVarSet, delVarSet, delVarSetByKey,
	minusVarSet, foldVarSet, filterVarSet,
	lookupVarSet, mapVarSet, sizeVarSet, seqVarSet
    ) where

#include "HsVersions.h"

import Var		( Var, Id, TyVar, UVar )
import Unique		( Unique )
import UniqSet
import UniqFM		( delFromUFM_Directly, addToUFM_C )
\end{code}

%************************************************************************
%*									*
\subsection{@VarSet@s}
%*									*
%************************************************************************

\begin{code}
type VarSet       = UniqSet Var
type IdSet 	  = UniqSet Id
type TyVarSet	  = UniqSet TyVar
type UVarSet      = UniqSet UVar

emptyVarSet	:: VarSet
intersectVarSet	:: VarSet -> VarSet -> VarSet
intersectsVarSet:: VarSet -> VarSet -> Bool 	-- True if non-empty intersection
unionVarSet	:: VarSet -> VarSet -> VarSet
unionVarSets	:: [VarSet] -> VarSet
varSetElems	:: VarSet -> [Var]
unitVarSet	:: Var -> VarSet
extendVarSet	:: VarSet -> Var -> VarSet
elemVarSet	:: Var -> VarSet -> Bool
delVarSet	:: VarSet -> Var -> VarSet
minusVarSet	:: VarSet -> VarSet -> VarSet
isEmptyVarSet	:: VarSet -> Bool
mkVarSet	:: [Var] -> VarSet
foldVarSet	:: (Var -> a -> a) -> a -> VarSet -> a
lookupVarSet	:: VarSet -> Var -> Maybe Var
			-- Returns the set element, which may be
			-- (==) to the argument, but not the same as
mapVarSet 	:: (Var -> Var) -> VarSet -> VarSet
sizeVarSet	:: VarSet -> Int
filterVarSet	:: (Var -> Bool) -> VarSet -> VarSet
subVarSet	:: VarSet -> VarSet -> Bool
extendVarSet_C  :: (Var->Var->Var) -> VarSet -> Var -> VarSet

delVarSetByKey	:: VarSet -> Unique -> VarSet

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
sizeVarSet	= sizeUniqSet
filterVarSet	= filterUniqSet
extendVarSet_C combine s x = addToUFM_C combine s x x
a `subVarSet` b = isEmptyVarSet (a `minusVarSet` b)
delVarSetByKey	= delFromUFM_Directly	-- Can't be bothered to add this to UniqSet
\end{code}

\begin{code}
seqVarSet :: VarSet -> ()
seqVarSet s = sizeVarSet s `seq` ()
\end{code}

