%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Unify}

This module contains a unifier and a matcher, both of which
use an explicit substitution

\begin{code}
module Unify ( unifyTysX, unifyTyListsX,
	       match, matchTy, matchTys
  ) where 

import TypeRep	( Type(..), funTyCon
		)  -- friend
import Type	( typeKind, tyVarsOfType, splitAppTy_maybe
		)

import PprType	()	-- Instances
			-- This import isn't strictly necessary, but it makes sure that
			-- PprType is below Unify in the hierarchy, which in turn makes
			-- fewer modules boot-import PprType

import Var	( TyVar, tyVarKind )
import VarSet
import VarEnv	( TyVarSubstEnv, emptySubstEnv, lookupSubstEnv, extendSubstEnv, 
		  SubstResult(..)
		)

import Unique	( Uniquable(..) )
import Outputable( panic )
import Util	( snocView )
\end{code}

%************************************************************************
%*									*
\subsection{Unification wih a explicit substitution}
%*									*
%************************************************************************

Unify types with an explicit substitution and no monad.

\begin{code}
type MySubst
   = (TyVarSet,		-- Set of template tyvars
      TyVarSubstEnv)	-- Not necessarily idempotent

unifyTysX :: TyVarSet		-- Template tyvars
	  -> Type
          -> Type
          -> Maybe TyVarSubstEnv
unifyTysX tmpl_tyvars ty1 ty2
  = uTysX ty1 ty2 (\(_,s) -> Just s) (tmpl_tyvars, emptySubstEnv)

unifyTyListsX :: TyVarSet -> [Type] -> [Type]
              -> Maybe TyVarSubstEnv
unifyTyListsX tmpl_tyvars tys1 tys2
  = uTyListsX tys1 tys2 (\(_,s) -> Just s) (tmpl_tyvars, emptySubstEnv)


uTysX :: Type
      -> Type
      -> (MySubst -> Maybe result)
      -> MySubst
      -> Maybe result

uTysX (NoteTy _ ty1) ty2 k subst = uTysX ty1 ty2 k subst
uTysX ty1 (NoteTy _ ty2) k subst = uTysX ty1 ty2 k subst

	-- Variables; go for uVar
uTysX (TyVarTy tyvar1) (TyVarTy tyvar2) k subst 
  | tyvar1 == tyvar2
  = k subst
uTysX (TyVarTy tyvar1) ty2 k subst@(tmpls,_)
  | tyvar1 `elemVarSet` tmpls
  = uVarX tyvar1 ty2 k subst
uTysX ty1 (TyVarTy tyvar2) k subst@(tmpls,_)
  | tyvar2 `elemVarSet` tmpls
  = uVarX tyvar2 ty1 k subst

	-- Functions; just check the two parts
uTysX (FunTy fun1 arg1) (FunTy fun2 arg2) k subst
  = uTysX fun1 fun2 (uTysX arg1 arg2 k) subst

	-- Type constructors must match
uTysX (TyConApp con1 tys1) (TyConApp con2 tys2) k subst
  | (con1 == con2 && length tys1 == length tys2)
  = uTyListsX tys1 tys2 k subst

	-- Applications need a bit of care!
	-- They can match FunTy and TyConApp, so use splitAppTy_maybe
	-- NB: we've already dealt with type variables and Notes,
	-- so if one type is an App the other one jolly well better be too
uTysX (AppTy s1 t1) ty2 k subst
  = case splitAppTy_maybe ty2 of
      Just (s2, t2) -> uTysX s1 s2 (uTysX t1 t2 k) subst
      Nothing       -> Nothing    -- Fail

uTysX ty1 (AppTy s2 t2) k subst
  = case splitAppTy_maybe ty1 of
      Just (s1, t1) -> uTysX s1 s2 (uTysX t1 t2 k) subst
      Nothing       -> Nothing    -- Fail

	-- Not expecting for-alls in unification
#ifdef DEBUG
uTysX (ForAllTy _ _) ty2 k subst = panic "Unify.uTysX subst:ForAllTy (1st arg)"
uTysX ty1 (ForAllTy _ _) k subst = panic "Unify.uTysX subst:ForAllTy (2nd arg)"
#endif

	-- Anything else fails
uTysX ty1 ty2 k subst = Nothing


uTyListsX []         []         k subst = k subst
uTyListsX (ty1:tys1) (ty2:tys2) k subst = uTysX ty1 ty2 (uTyListsX tys1 tys2 k) subst
uTyListsX tys1	     tys2       k subst = Nothing   -- Fail if the lists are different lengths
\end{code}

\begin{code}
-- Invariant: tv1 is a unifiable variable
uVarX tv1 ty2 k subst@(tmpls, env)
  = case lookupSubstEnv env tv1 of
      Just (DoneTy ty1) ->    -- Already bound
		     uTysX ty1 ty2 k subst

      Nothing	     -- Not already bound
	       |  typeKind ty2 == tyVarKind tv1
	       && occur_check_ok ty2
	       ->     -- No kind mismatch nor occur check
	          k (tmpls, extendSubstEnv env tv1 (DoneTy ty2))

	       | otherwise -> Nothing	-- Fail if kind mis-match or occur check
  where
    occur_check_ok ty = all occur_check_ok_tv (varSetElems (tyVarsOfType ty))
    occur_check_ok_tv tv | tv1 == tv = False
			 | otherwise = case lookupSubstEnv env tv of
				         Nothing	   -> True
					 Just (DoneTy ty)  -> occur_check_ok ty
\end{code}



%************************************************************************
%*									*
\subsection{Matching on types}
%*									*
%************************************************************************

Matching is a {\em unidirectional} process, matching a type against a
template (which is just a type with type variables in it).  The
matcher assumes that there are no repeated type variables in the
template, so that it simply returns a mapping of type variables to
types.  It also fails on nested foralls.

@matchTys@ matches corresponding elements of a list of templates and
types.

\begin{code}
matchTy :: TyVarSet			-- Template tyvars
	-> Type  			-- Template
	-> Type				-- Proposed instance of template
	-> Maybe TyVarSubstEnv		-- Matching substitution
					

matchTys :: TyVarSet			-- Template tyvars
	 -> [Type]			-- Templates
	 -> [Type]			-- Proposed instance of template
	 -> Maybe (TyVarSubstEnv,		-- Matching substitution
		   [Type])		-- Left over instance types

matchTy tmpls ty1 ty2 = match ty1 ty2 tmpls (\ senv -> Just senv) emptySubstEnv

matchTys tmpls tys1 tys2 = match_list tys1 tys2 tmpls 
				      (\ (senv,tys) -> Just (senv,tys))
				      emptySubstEnv
\end{code}

@match@ is the main function.

\begin{code}
match :: Type -> Type    	    		-- Current match pair
      -> TyVarSet				-- Template vars
      -> (TyVarSubstEnv -> Maybe result)	-- Continuation
      -> TyVarSubstEnv				-- Current subst
      -> Maybe result

-- When matching against a type variable, see if the variable
-- has already been bound.  If so, check that what it's bound to
-- is the same as ty; if not, bind it and carry on.

match (TyVarTy v) ty tmpls k senv
  | v `elemVarSet` tmpls
  =     -- v is a template variable
    case lookupSubstEnv senv v of
	Nothing -> k (extendSubstEnv senv v (DoneTy ty))
	Just (DoneTy ty')  | ty' == ty	       -> k senv   -- Succeeds
			   | otherwise	       -> Nothing  -- Fails

  | otherwise
  =     -- v is not a template variable; ty had better match
        -- Can't use (==) because types differ
    case ty of
        TyVarTy v' | v == v' -> k senv    -- Success
        other    	     -> Nothing   -- Failure

match (FunTy arg1 res1) (FunTy arg2 res2) tmpls k senv
  = match arg1 arg2 tmpls (match res1 res2 tmpls k) senv

match (AppTy fun1 arg1) ty2 tmpls k senv 
  = case splitAppTy_maybe ty2 of
	Just (fun2,arg2) -> match fun1 fun2 tmpls (match arg1 arg2 tmpls k) senv
	Nothing 	 -> Nothing	-- Fail

match (TyConApp tc1 tys1) (TyConApp tc2 tys2) tmpls k senv
  | tc1 == tc2
  = match_list tys1 tys2 tmpls k' senv
  where
    k' (senv', tys2') | null tys2' = k senv'	-- Succeed
		      | otherwise  = Nothing	-- Fail	

	-- With type synonyms, we have to be careful for the exact
	-- same reasons as in the unifier.  Please see the
	-- considerable commentary there before changing anything
	-- here! (WDP 95/05)
match (NoteTy _ ty1) ty2            tmpls k senv = match ty1 ty2 tmpls k senv
match ty1	     (NoteTy _ ty2) tmpls k senv = match ty1 ty2 tmpls k senv

-- Catch-all fails
match _ _ _ _ _ = Nothing

match_list []         tys2       tmpls k senv = k (senv, tys2)
match_list (ty1:tys1) []         tmpls k senv = Nothing	-- Not enough arg tys => failure
match_list (ty1:tys1) (ty2:tys2) tmpls k senv = match ty1 ty2 tmpls (match_list tys1 tys2 tmpls k) senv
\end{code}

