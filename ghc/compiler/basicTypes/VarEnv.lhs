
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{@VarEnvs@: Variable environments}

\begin{code}
module VarEnv (
	VarEnv, IdEnv, TyVarEnv,
	emptyVarEnv, unitVarEnv, mkVarEnv,
	elemVarEnv, rngVarEnv,
	extendVarEnv, extendVarEnv_C, extendVarEnvList,
	plusVarEnv, plusVarEnv_C,
	delVarEnvList, delVarEnv,
	lookupVarEnv, lookupVarEnv_NF, lookupWithDefaultVarEnv,
	mapVarEnv, zipVarEnv,
	modifyVarEnv, modifyVarEnv_Directly,
	isEmptyVarEnv, foldVarEnv,

	-- TidyEnvs
	TidyEnv, emptyTidyEnv,

	-- SubstEnvs
	SubstEnv, TyVarSubstEnv, SubstResult(..),
	emptySubstEnv, substEnvEnv, elemSubstEnv,
	mkSubstEnv, lookupSubstEnv, extendSubstEnv, extendSubstEnvList,
	delSubstEnv, delSubstEnvList, noTypeSubst, isEmptySubstEnv
    ) where

#include "HsVersions.h"

import {-# SOURCE #-}	CoreSyn( CoreExpr )
import {-# SOURCE #-}	TypeRep( Type )

import BasicTypes ( OccInfo )
import OccName	  ( TidyOccEnv, emptyTidyOccEnv )
import Var	  ( Var, Id )
import UniqFM  
import Util	  ( zipEqual )
\end{code}


%************************************************************************
%*									*
\subsection{Tidying}
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
\subsection{Substitution environments}
%*									*
%************************************************************************

\begin{code}

noTys :: SubstResult -> Bool -> Bool
noTys (DoneTy ty) no_tys = False
noTys other	  no_tys = no_tys

data SubstEnv      = SE (VarEnv SubstResult)
			Bool		-- True => definitely no type substitutions in the env

noTypeSubst :: SubstEnv -> Bool
noTypeSubst (SE _ nt) = nt

substEnvEnv :: SubstEnv -> VarEnv SubstResult
substEnvEnv (SE env _) = env

type TyVarSubstEnv = SubstEnv	-- of the form (DoneTy ty) *only*

data SubstResult
  = DoneEx CoreExpr		-- Completed term
  | DoneId Id OccInfo		-- Completed term variable, with occurrence info; only 
				-- used by the simplifier
  | DoneTy Type			-- Completed type
  | ContEx SubstEnv CoreExpr  	-- A suspended substitution

emptySubstEnv :: SubstEnv
emptySubstEnv = SE emptyVarEnv True

isEmptySubstEnv :: SubstEnv -> Bool
isEmptySubstEnv (SE s _) = isEmptyVarEnv s

lookupSubstEnv :: SubstEnv -> Var -> Maybe SubstResult
lookupSubstEnv (SE s _) v = lookupVarEnv s v

elemSubstEnv :: Var -> SubstEnv -> Bool
elemSubstEnv v (SE s _) = elemVarEnv v s

extendSubstEnv :: SubstEnv -> Var -> SubstResult -> SubstEnv
extendSubstEnv (SE s nt) v r = SE (extendVarEnv s v r) (noTys r nt)

mkSubstEnv :: [Var] -> [SubstResult] -> SubstEnv
mkSubstEnv bs vs = extendSubstEnvList emptySubstEnv bs vs

extendSubstEnvList :: SubstEnv -> [Var] -> [SubstResult] -> SubstEnv
extendSubstEnvList env	       []     []     = env
extendSubstEnvList (SE env nt) (b:bs) (r:rs) = extendSubstEnvList (SE (extendVarEnv env b r) (noTys r nt)) bs rs

delSubstEnv :: SubstEnv -> Var -> SubstEnv
delSubstEnv (SE s nt) v = SE (delVarEnv s v) nt

delSubstEnvList :: SubstEnv -> [Var] -> SubstEnv
delSubstEnvList (SE s nt) vs = SE (delVarEnvList s vs) nt
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
		  
delVarEnvList     :: VarEnv a -> [Var] -> VarEnv a
delVarEnv	  :: VarEnv a -> Var -> VarEnv a
plusVarEnv_C	  :: (a -> a -> a) -> VarEnv a -> VarEnv a -> VarEnv a
mapVarEnv	  :: (a -> b) -> VarEnv a -> VarEnv b
modifyVarEnv	  :: (a -> a) -> VarEnv a -> Var -> VarEnv a
rngVarEnv	  :: VarEnv a -> [a]
		  
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
rngVarEnv	 = eltsUFM
unitVarEnv	 = unitUFM
isEmptyVarEnv	 = isNullUFM
foldVarEnv	 = foldUFM

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
