%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
%************************************************************************
%*									*
\section[Spec]{Specialisation of variables}
%*									*
%************************************************************************

One thing which happens {\em a lot} is the instantiation of a type scheme
caused by the occurrence of a variable.  It is so important that it
is written below in a very ``open-code'' fashion.  All the modular monadery
is discarded, and we work directly in terms of the underlying representations.
In particular, this function knows about

	- the TcM monad
	- the representation of UniTypes

\begin{code}
#include "HsVersions.h"

module Spec ( specId, specTy ) where

import AbsSyn
import TcMonadFns	( copyTyVars, newDicts )
import TcMonad

import AbsUniType	{- ( instantiateTauTy, instantiateThetaTy,
			  cloneTyVarFromTemplate, splitType
			) -} -- pragmas want to see it all!
import Id		( getIdUniType, mkInstId, DictVar(..) )
import Inst		-- ( mkMethod, InstOrigin(..), Inst, InstTemplate, SpecInfo )
import LIE
import Subst		( getSubstTyVarUnique )
import UniType		-- known **GRIEVOUS** violation of UniType abstractness!!!
import SplitUniq
import Unique
import Util
\end{code}

%************************************************************************
%*									*
\subsection[Spec-specId]{Instantiating an Id}
%*									*
%************************************************************************

@specId@ takes an @Id@ and implements the SPEC and REL rules
returning
	- the id applied to suitable types and dictionaries
	- the LIE
	- its instantiated tau type

For efficiency, it knows about the TcM implementation.

\begin{code}
specId :: Id -> NF_TcM (TypecheckedExpr, LIE, TauType)

specId id sw_chkr dtys subst uniq errs src_loc
  = case (spec_sigma subst uniq src_loc id (getIdUniType id)) of
      (result, subst2) -> (result, subst2, errs)
\end{code}

\begin{code}
spec_sigma :: Subst		-- TyVar unique supply inside *here*
	   -> SplitUniqSupply	-- "normal" unique supply
	   -> SrcLoc
	   -> Id
	   -> UniType
	   -> ((TypecheckedExpr, LIE, TauType), Subst)

spec_sigma subst uniq src_loc id (UniSyn _ _ ty)
  = spec_sigma subst uniq src_loc id ty

spec_sigma subst uniq src_loc id ty@(UniForall _ _)
  = collect [] [] subst ty
  where
    collect tenv tyvar_tys subst (UniForall tyvar ty)
      = case (getSubstTyVarUnique subst) of
	  (subst', u) ->
	      collect ((tyvar, new_tyvar_ty) : tenv)
		      (new_tyvar_ty : tyvar_tys)
		      subst' ty
	      where
		new_tyvar_ty = UniTyVar (cloneTyVarFromTemplate tyvar u)

    collect tenv tyvar_tys subst ty
      = spec_rho tenv (reverse tyvar_tys) subst uniq src_loc id ty

spec_sigma subst uniq src_loc id tau_ty
	-- Not polymorphic => cannot be overloaded
  = ((Var id, nullLIE, tau_ty), subst)
\end{code}

\begin{code}
spec_rho :: [(TyVarTemplate, UniType)] -> [UniType]
	 -> Subst -> SplitUniqSupply -> SrcLoc
	 -> Id -> UniType
	 -> ((TypecheckedExpr, LIE, TauType), Subst)

spec_rho tenv tys subst uniqs src_loc id (UniSyn _ _ ty)
  = spec_rho tenv tys subst uniqs src_loc id ty

spec_rho tenv tys subst uniqs src_loc id (UniFun (UniDict _ _) ty)
  = ((Var inst_id, unitLIE method, instantiateTauTy tenv tau_ty),
     subst)
  where
    method  = mkMethod u id tys (OccurrenceOf id src_loc)
    inst_id = mkInstId method
    u	    = getSUnique uniqs
    tau_ty  = discard_dicts ty

    discard_dicts (UniFun (UniDict _ _) ty) = discard_dicts ty
    discard_dicts other_ty                  = other_ty

spec_rho tenv tys subst uniqs src_loc id tau_ty
  = ((TyApp (Var id) tys, nullLIE, instantiateTauTy tenv tau_ty),
     subst)
\end{code}


%************************************************************************
%*									*
\subsection[Spec-specTy]{Instantiating a type}
%*									*
%************************************************************************

@specTy@ takes a polymorphic type, and instantiates it with fresh type
variables. It strips off the context part, gets fresh dictionary
variables for each predicate in the context.  It returns

	- a list of the dictionary variables (remember they contain
	  their types)
	- an instantiated tau-type

The returned values are fixed points of the current substitution
though the arguments may not be.

\begin{code}
specTy :: InstOrigin -> SigmaType -> NF_TcM ([TyVar], [Inst], TauType)

specTy origin sigma_ty
  = let
	(old_tyvars, theta, tau_ty) = splitType sigma_ty
    in
	 -- make new tyvars for each of the universally quantified type vars
    copyTyVars old_tyvars	    `thenNF_Tc` \ (inst_env, new_tyvars, _) ->

	 -- instantiate the tau type
    let
	tau_ty' = instantiateTauTy inst_env tau_ty
    in
	 -- instantiate the dictionary types
    newDicts origin (instantiateThetaTy inst_env theta)	`thenNF_Tc` \ dicts ->

	 -- return the list of tyvars, the list of dicts and the tau type
    returnNF_Tc ( new_tyvars, dicts, tau_ty' )
\end{code}

