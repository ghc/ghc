
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{@VarEnvs@: Variable environments}

\begin{code}
module VarEnv (
	VarEnv, IdEnv, TyVarEnv,
	emptyVarEnv, unitVarEnv, mkVarEnv,
	elemVarEnv, varEnvElts,
	extendVarEnv, extendVarEnv_C, extendVarEnvList,
	plusVarEnv, plusVarEnv_C,
	delVarEnvList, delVarEnv,
	lookupVarEnv, lookupVarEnv_NF, lookupWithDefaultVarEnv,
	mapVarEnv, zipVarEnv,
	modifyVarEnv, modifyVarEnv_Directly,
	isEmptyVarEnv, foldVarEnv, 
	lookupVarEnv_Directly,
	filterVarEnv_Directly,

	-- InScopeSet
	InScopeSet, emptyInScopeSet, mkInScopeSet, delInScopeSet,
	extendInScopeSet, extendInScopeSetList, modifyInScopeSet,
	getInScopeVars, lookupInScope, elemInScopeSet, uniqAway, 

	-- TidyEnvs
	TidyEnv, emptyTidyEnv
    ) where

#include "HsVersions.h"

import OccName	  ( TidyOccEnv, emptyTidyOccEnv )
import Var	  ( Var, setVarUnique )
import VarSet
import UniqFM  
import Unique	  ( Unique, deriveUnique, getUnique )
import Util	  ( zipEqual )
import CmdLineOpts	( opt_PprStyle_Debug )
import Outputable
import FastTypes
\end{code}


%************************************************************************
%*									*
		In-scope sets
%*									*
%************************************************************************

\begin{code}
data InScopeSet = InScope (VarEnv Var) FastInt
	-- The Int# is a kind of hash-value used by uniqAway
	-- For example, it might be the size of the set
	-- INVARIANT: it's not zero; we use it as a multiplier in uniqAway

instance Outputable InScopeSet where
  ppr (InScope s i) = ptext SLIT("InScope") <+> ppr s

emptyInScopeSet :: InScopeSet
emptyInScopeSet = InScope emptyVarSet 1#

getInScopeVars ::  InScopeSet -> VarEnv Var
getInScopeVars (InScope vs _) = vs

mkInScopeSet :: VarEnv Var -> InScopeSet
mkInScopeSet in_scope = InScope in_scope 1#

extendInScopeSet :: InScopeSet -> Var -> InScopeSet
extendInScopeSet (InScope in_scope n) v = InScope (extendVarEnv in_scope v v) (n +# 1#)

extendInScopeSetList :: InScopeSet -> [Var] -> InScopeSet
extendInScopeSetList (InScope in_scope n) vs
   = InScope (foldl (\s v -> extendVarEnv s v v) in_scope vs)
		    (n +# iUnbox (length vs))

modifyInScopeSet :: InScopeSet -> Var -> Var -> InScopeSet
-- Exploit the fact that the in-scope "set" is really a map
-- 	Make old_v map to new_v
modifyInScopeSet (InScope in_scope n) old_v new_v = InScope (extendVarEnv in_scope old_v new_v) (n +# 1#)

delInScopeSet :: InScopeSet -> Var -> InScopeSet
delInScopeSet (InScope in_scope n) v = InScope (in_scope `delVarEnv` v) n

elemInScopeSet :: Var -> InScopeSet -> Bool
elemInScopeSet v (InScope in_scope n) = v `elemVarEnv` in_scope

lookupInScope :: InScopeSet -> Var -> Maybe Var
-- It's important to look for a fixed point
-- When we see (case x of y { I# v -> ... })
-- we add  [x -> y] to the in-scope set (Simplify.simplCaseBinder).
-- When we lookup up an occurrence of x, we map to y, but then
-- we want to look up y in case it has acquired more evaluation information by now.
lookupInScope (InScope in_scope n) v 
  = go v
  where
    go v = case lookupVarEnv in_scope v of
		Just v' | v == v'   -> Just v'	-- Reached a fixed point
			| otherwise -> go v'
		Nothing		    -> Nothing
\end{code}

\begin{code}
uniqAway :: InScopeSet -> Var -> Var
-- (uniqAway in_scope v) finds a unique that is not used in the
-- in-scope set, and gives that to v.  It starts with v's current unique, of course,
-- in the hope that it won't have to change it, and thereafter uses a combination
-- of that and the hash-code found in the in-scope set
uniqAway (InScope set n) var
  | not (var `elemVarSet` set) = var				-- Nothing to do
  | otherwise		       = try 1#
  where
    orig_unique = getUnique var
    try k 
#ifdef DEBUG
	  | k ># 1000#
	  = pprPanic "uniqAway loop:" (ppr (iBox k) <+> text "tries" <+> ppr var <+> int (iBox n)) 
#endif			    
	  | uniq `elemVarSetByKey` set = try (k +# 1#)
#ifdef DEBUG
	  | opt_PprStyle_Debug && k ># 3#
	  = pprTrace "uniqAway:" (ppr (iBox k) <+> text "tries" <+> ppr var <+> int (iBox n)) 
	    setVarUnique var uniq
#endif			    
	  | otherwise = setVarUnique var uniq
	  where
	    uniq = deriveUnique orig_unique (iBox (n *# k))
\end{code}


%************************************************************************
%*									*
		Tidying
%*									*
%************************************************************************

When tidying up print names, we keep a mapping of in-scope occ-names
(the TidyOccEnv) and a Var-to-Var of the current renamings.

\begin{code}
type TidyEnv = (TidyOccEnv, VarEnv Var)

emptyTidyEnv :: TidyEnv
emptyTidyEnv = (emptyTidyOccEnv, emptyVarEnv)
\end{code}


%************************************************************************
%*									*
\subsection{@VarEnv@s}
%*									*
%************************************************************************

\begin{code}
type VarEnv elt   = UniqFM elt
type IdEnv elt    = VarEnv elt
type TyVarEnv elt = VarEnv elt

emptyVarEnv	  :: VarEnv a
mkVarEnv	  :: [(Var, a)] -> VarEnv a
zipVarEnv	  :: [Var] -> [a] -> VarEnv a
unitVarEnv	  :: Var -> a -> VarEnv a
extendVarEnv	  :: VarEnv a -> Var -> a -> VarEnv a
extendVarEnv_C	  :: (a->a->a) -> VarEnv a -> Var -> a -> VarEnv a
plusVarEnv	  :: VarEnv a -> VarEnv a -> VarEnv a
extendVarEnvList  :: VarEnv a -> [(Var, a)] -> VarEnv a
		  
lookupVarEnv_Directly :: VarEnv a -> Unique -> Maybe a
filterVarEnv_Directly :: (Unique -> a -> Bool) -> VarEnv a -> VarEnv a
delVarEnvList     :: VarEnv a -> [Var] -> VarEnv a
delVarEnv	  :: VarEnv a -> Var -> VarEnv a
plusVarEnv_C	  :: (a -> a -> a) -> VarEnv a -> VarEnv a -> VarEnv a
mapVarEnv	  :: (a -> b) -> VarEnv a -> VarEnv b
modifyVarEnv	  :: (a -> a) -> VarEnv a -> Var -> VarEnv a
varEnvElts	  :: VarEnv a -> [a]
		  
isEmptyVarEnv	  :: VarEnv a -> Bool
lookupVarEnv	  :: VarEnv a -> Var -> Maybe a
lookupVarEnv_NF   :: VarEnv a -> Var -> a
lookupWithDefaultVarEnv :: VarEnv a -> a -> Var -> a
elemVarEnv	  :: Var -> VarEnv a -> Bool
foldVarEnv	  :: (a -> b -> b) -> b -> VarEnv a -> b
\end{code}

\begin{code}
elemVarEnv       = elemUFM
extendVarEnv	 = addToUFM
extendVarEnv_C	 = addToUFM_C
extendVarEnvList = addListToUFM
plusVarEnv_C	 = plusUFM_C
delVarEnvList	 = delListFromUFM
delVarEnv	 = delFromUFM
plusVarEnv	 = plusUFM
lookupVarEnv	 = lookupUFM
lookupWithDefaultVarEnv = lookupWithDefaultUFM
mapVarEnv	 = mapUFM
mkVarEnv	 = listToUFM
emptyVarEnv	 = emptyUFM
varEnvElts	 = eltsUFM
unitVarEnv	 = unitUFM
isEmptyVarEnv	 = isNullUFM
foldVarEnv	 = foldUFM
lookupVarEnv_Directly = lookupUFM_Directly
filterVarEnv_Directly = filterUFM_Directly

zipVarEnv tyvars tys       = mkVarEnv (zipEqual "zipVarEnv" tyvars tys)
lookupVarEnv_NF env id     = case (lookupVarEnv env id) of { Just xx -> xx }
\end{code}

@modifyVarEnv@: Look up a thing in the VarEnv, 
then mash it with the modify function, and put it back.

\begin{code}
modifyVarEnv mangle_fn env key
  = case (lookupVarEnv env key) of
      Nothing -> env
      Just xx -> extendVarEnv env key (mangle_fn xx)

modifyVarEnv_Directly mangle_fn env key
  = case (lookupUFM_Directly env key) of
      Nothing -> env
      Just xx -> addToUFM_Directly env key (mangle_fn xx)
\end{code}
