
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{@VarEnvs@: Variable environments}

\begin{code}
module VarEnv (
	VarEnv, IdEnv, TyVarEnv,
	emptyVarEnv, unitVarEnv, mkVarEnv,
	elemVarEnv, rngVarEnv,
	extendVarEnv, extendVarEnvList,
	plusVarEnv, plusVarEnv_C,
	delVarEnvList, delVarEnv,
	lookupVarEnv, lookupVarEnv_NF,
	mapVarEnv, zipVarEnv,
	modifyVarEnv, modifyVarEnv_Directly,
	isEmptyVarEnv, foldVarEnv,

	TidyEnv, emptyTidyEnv
    ) where

#include "HsVersions.h"

import OccName	( TidyOccEnv, emptyTidyOccEnv )
import Var	( Var, Id, IdOrTyVar )
import UniqFM
import Util	( zipEqual )
\end{code}


%************************************************************************
%*									*
\subsection{Tidying}
%*									*
%************************************************************************

When tidying up print names, we keep a mapping of in-scope occ-names
(the TidyOccEnv) and a Var-to-Var of the current renamings.

\begin{code}
type TidyEnv = (TidyOccEnv, VarEnv IdOrTyVar)
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
elemVarEnv	  :: Var -> VarEnv a -> Bool
foldVarEnv	  :: (a -> b -> b) -> b -> VarEnv a -> b
\end{code}

\begin{code}
elemVarEnv       = elemUFM
extendVarEnv	 = addToUFM
plusVarEnv_C	 = plusUFM_C
delVarEnvList	 = delListFromUFM
delVarEnv	 = delFromUFM
plusVarEnv	 = plusUFM
lookupVarEnv	 = lookupUFM
mapVarEnv	 = mapUFM
mkVarEnv	 = listToUFM
emptyVarEnv	 = emptyUFM
rngVarEnv	 = eltsUFM
unitVarEnv	 = unitUFM
isEmptyVarEnv	 = isNullUFM
foldVarEnv	 = foldUFM

zipVarEnv tyvars tys       = listToUFM (zipEqual "zipVarEnv" tyvars tys)
extendVarEnvList env pairs = plusUFM env (listToUFM pairs)
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
