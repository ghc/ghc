%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{@VarSet@: Variable sets}

\begin{code}
module VarSet (
	VarSet, IdSet, TyVarSet, IdOrTyVarSet,
	emptyVarSet, unitVarSet, mkVarSet,
	extendVarSet,
	elemVarSet, varSetElems, subVarSet,
	unionVarSet, unionVarSets,
	intersectVarSet, intersectsVarSet,
	isEmptyVarSet, delVarSet, delVarSetByKey,
	minusVarSet, foldVarSet, filterVarSet,
	lookupVarSet, mapVarSet,

	uniqAway
    ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_PprStyle_Debug )
import Var		( Var, Id, TyVar, IdOrTyVar, setVarUnique )
import Unique		( Unique, Uniquable(..), incrUnique, deriveUnique )
import UniqSet
import UniqFM		( delFromUFM_Directly )
import Outputable
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
type IdOrTyVarSet = UniqSet IdOrTyVar

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
filterVarSet	:: (Var -> Bool) -> VarSet -> VarSet
subVarSet	:: VarSet -> VarSet -> Bool

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
filterVarSet	= filterUniqSet
a `subVarSet` b = isEmptyVarSet (a `minusVarSet` b)
delVarSetByKey	= delFromUFM_Directly	-- Can't be bothered to add this to UniqSet
\end{code}

\begin{code}
uniqAway :: VarSet -> Var -> Var
-- Give the Var a new unique, different to any in the VarSet
uniqAway set var
  | not (var `elemVarSet` set) = var	-- Nothing to do

  | otherwise
  = try 1 (deriveUnique (getUnique var) (hashUniqSet set))
  where
    try n uniq | uniq `elemUniqSet_Directly` set = try ((n+1)::Int) (incrUnique uniq)
#ifdef DEBUG
	       | opt_PprStyle_Debug && n > 3
	       = pprTrace "uniqAway:" (ppr n <+> text "tries" <+> ppr var) 
		 setVarUnique var uniq
#endif			    
	       | otherwise = setVarUnique var uniq
\end{code}
