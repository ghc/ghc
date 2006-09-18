
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{@VarEnvs@: Variable environments}

\begin{code}
module VarEnv (
	VarEnv, IdEnv, TyVarEnv,
	emptyVarEnv, unitVarEnv, mkVarEnv,
	elemVarEnv, varEnvElts, varEnvKeys,
	extendVarEnv, extendVarEnv_C, extendVarEnvList,
	plusVarEnv, plusVarEnv_C,
	delVarEnvList, delVarEnv,
	lookupVarEnv, lookupVarEnv_NF, lookupWithDefaultVarEnv,
	mapVarEnv, zipVarEnv,
	modifyVarEnv, modifyVarEnv_Directly,
	isEmptyVarEnv, foldVarEnv, 
	elemVarEnvByKey, lookupVarEnv_Directly,
	filterVarEnv_Directly,

	-- InScopeSet
	InScopeSet, emptyInScopeSet, mkInScopeSet, delInScopeSet,
	extendInScopeSet, extendInScopeSetList, extendInScopeSetSet, 
	modifyInScopeSet,
	getInScopeVars, lookupInScope, elemInScopeSet, uniqAway, 
	mapInScopeSet,

	-- RnEnv2 and its operations
	RnEnv2, mkRnEnv2, rnBndr2, rnBndrs2, rnOccL, rnOccR, inRnEnvL, inRnEnvR,
		rnBndrL, rnBndrR, nukeRnEnvL, nukeRnEnvR, extendRnInScopeList,
		rnInScope, lookupRnInScope,

	-- TidyEnvs
	TidyEnv, emptyTidyEnv
    ) where

#include "HsVersions.h"

import OccName	  ( TidyOccEnv, emptyTidyOccEnv )
import Var	  ( Var, setVarUnique )
import VarSet
import UniqFM  
import Unique	  ( Unique, deriveUnique, getUnique )
import Util	  ( zipEqual, foldl2 )
import Maybes	  ( orElse )
import StaticFlags( opt_PprStyle_Debug )
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

extendInScopeSetSet :: InScopeSet -> VarEnv Var -> InScopeSet
extendInScopeSetSet (InScope in_scope n) vs
   = InScope (in_scope `plusVarEnv` vs) (n +# iUnbox (sizeUFM vs))

modifyInScopeSet :: InScopeSet -> Var -> Var -> InScopeSet
-- Exploit the fact that the in-scope "set" is really a map
-- 	Make old_v map to new_v
modifyInScopeSet (InScope in_scope n) old_v new_v = InScope (extendVarEnv in_scope old_v new_v) (n +# 1#)

delInScopeSet :: InScopeSet -> Var -> InScopeSet
delInScopeSet (InScope in_scope n) v = InScope (in_scope `delVarEnv` v) n

mapInScopeSet :: (Var -> Var) -> InScopeSet -> InScopeSet
mapInScopeSet f (InScope in_scope n) = InScope (mapVarEnv f in_scope) n

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
uniqAway in_scope var
  | var `elemInScopeSet` in_scope = uniqAway' in_scope var	-- Make a new one
  | otherwise 			  = var				-- Nothing to do

uniqAway' :: InScopeSet -> Var -> Var
-- This one *always* makes up a new variable
uniqAway' (InScope set n) var
  = try 1#
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
		Dual renaming
%*									*
%************************************************************************

When we are comparing (or matching) types or terms, we are faced with 
"going under" corresponding binders.  E.g. when comparing
	\x. e1	~   \y. e2

Basically we want to rename [x->y] or [y->x], but there are lots of 
things we must be careful of.  In particular, x might be free in e2, or
y in e1.  So the idea is that we come up with a fresh binder that is free
in neither, and rename x and y respectively.  That means we must maintain
	a) a renaming for the left-hand expression
	b) a renaming for the right-hand expressions
	c) an in-scope set

Furthermore, when matching, we want to be able to have an 'occurs check',
to prevent
	\x. f   ~   \y. y
matching with f->y.  So for each expression we want to know that set of
locally-bound variables. That is precisely the domain of the mappings (a)
and (b), but we must ensure that we always extend the mappings as we go in.


\begin{code}
data RnEnv2 
  = RV2 { envL 	   :: VarEnv Var	-- Renaming for Left term
	, envR 	   :: VarEnv Var	-- Renaming for Right term
	, in_scope :: InScopeSet }	-- In scope in left or right terms

-- The renamings envL and envR are *guaranteed* to contain a binding
-- for every variable bound as we go into the term, even if it is not
-- renamed.  That way we can ask what variables are locally bound
-- (inRnEnvL, inRnEnvR)

mkRnEnv2 :: InScopeSet -> RnEnv2
mkRnEnv2 vars = RV2	{ envL 	   = emptyVarEnv 
			, envR 	   = emptyVarEnv
			, in_scope = vars }

extendRnInScopeList :: RnEnv2 -> [Var] -> RnEnv2
extendRnInScopeList env vs
  = env { in_scope = extendInScopeSetList (in_scope env) vs }

rnInScope :: Var -> RnEnv2 -> Bool
rnInScope x env = x `elemInScopeSet` in_scope env

rnBndrs2 :: RnEnv2 -> [Var] -> [Var] -> RnEnv2
-- Arg lists must be of equal length
rnBndrs2 env bsL bsR = foldl2 rnBndr2 env bsL bsR 

rnBndr2 :: RnEnv2 -> Var -> Var -> RnEnv2
-- (rnBndr2 env bL bR) go under a binder bL in the Left term 1, 
-- 		       and binder bR in the Right term
-- It finds a new binder, new_b,
-- and returns an environment mapping bL->new_b and bR->new_b resp.
rnBndr2 (RV2 { envL = envL, envR = envR, in_scope = in_scope }) bL bR
  = RV2 { envL 	   = extendVarEnv envL bL new_b	  -- See Note
	, envR 	   = extendVarEnv envR bR new_b	  -- [Rebinding]
	, in_scope = extendInScopeSet in_scope new_b }
  where
	-- Find a new binder not in scope in either term
    new_b | not (bL `elemInScopeSet` in_scope) = bL
      	  | not (bR `elemInScopeSet` in_scope) = bR
      	  | otherwise			       = uniqAway' in_scope bL

	-- Note [Rebinding]
	-- If the new var is the same as the old one, note that
	-- the extendVarEnv *deletes* any current renaming
	-- E.g.	  (\x. \x. ...)	 ~  (\y. \z. ...)
	--
	--   Inside \x  \y	{ [x->y], [y->y],       {y} }
	-- 	 \x  \z	  	{ [x->x], [y->y, z->x], {y,x} }

rnBndrL, rnBndrR :: RnEnv2 -> Var -> (RnEnv2, Var)
-- Used when there's a binder on one side or the other only
-- Useful when eta-expanding
rnBndrL (RV2 { envL = envL, envR = envR, in_scope = in_scope }) bL
  = (RV2 { envL     = extendVarEnv envL bL new_b
	 , envR     = envR
	 , in_scope = extendInScopeSet in_scope new_b }, new_b)
  where
    new_b | not (bL `elemInScopeSet` in_scope) = bL
      	  | otherwise			       = uniqAway' in_scope bL

rnBndrR (RV2 { envL = envL, envR = envR, in_scope = in_scope }) bR
  = (RV2 { envL     = envL
	 , envR     = extendVarEnv envR bR new_b
	 , in_scope = extendInScopeSet in_scope new_b }, new_b)
  where
    new_b | not (bR `elemInScopeSet` in_scope) = bR
      	  | otherwise			       = uniqAway' in_scope bR

rnOccL, rnOccR :: RnEnv2 -> Var -> Var
-- Look up the renaming of an occurrence in the left or right term
rnOccL (RV2 { envL = env }) v = lookupVarEnv env v `orElse` v
rnOccR (RV2 { envR = env }) v = lookupVarEnv env v `orElse` v

inRnEnvL, inRnEnvR :: RnEnv2 -> Var -> Bool
-- Tells whether a variable is locally bound
inRnEnvL (RV2 { envL = env }) v = v `elemVarEnv` env
inRnEnvR (RV2 { envR = env }) v = v `elemVarEnv` env

lookupRnInScope :: RnEnv2 -> Var -> Var
lookupRnInScope env v = lookupInScope (in_scope env) v `orElse` v

nukeRnEnvL, nukeRnEnvR :: RnEnv2 -> RnEnv2
nukeRnEnvL env = env { envL = emptyVarEnv }
nukeRnEnvR env = env { envR = emptyVarEnv }
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
varEnvKeys	  :: VarEnv a -> [Unique]
		  
isEmptyVarEnv	  :: VarEnv a -> Bool
lookupVarEnv	  :: VarEnv a -> Var -> Maybe a
lookupVarEnv_NF   :: VarEnv a -> Var -> a
lookupWithDefaultVarEnv :: VarEnv a -> a -> Var -> a
elemVarEnv	  :: Var -> VarEnv a -> Bool
elemVarEnvByKey   :: Unique -> VarEnv a -> Bool
foldVarEnv	  :: (a -> b -> b) -> b -> VarEnv a -> b
\end{code}

\begin{code}
elemVarEnv       = elemUFM
elemVarEnvByKey  = elemUFM_Directly
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
varEnvKeys	 = keysUFM
unitVarEnv	 = unitUFM
isEmptyVarEnv	 = isNullUFM
foldVarEnv	 = foldUFM
lookupVarEnv_Directly = lookupUFM_Directly
filterVarEnv_Directly = filterUFM_Directly

zipVarEnv tyvars tys   = mkVarEnv (zipEqual "zipVarEnv" tyvars tys)
lookupVarEnv_NF env id = case (lookupVarEnv env id) of { Just xx -> xx }
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
