%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Unify}

This module contains a unifier and a matcher, both of which
use an explicit substitution

\begin{code}
module Unify ( Subst,
	       unifyTysX, unifyTyListsX,
	       matchTy, matchTys
  ) where 

import Var	( TyVar, tyVarKind )
import VarEnv
import VarSet	( varSetElems )
import Type	( Type(..), funTyCon, typeKind, tyVarsOfType,
		  splitAppTy_maybe
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
type Subst
   = ([TyVar],		-- Set of template tyvars
      TyVarEnv Type)	-- Not necessarily idempotent

unifyTysX :: [TyVar]		-- Template tyvars
	  -> Type
          -> Type
          -> Maybe (TyVarEnv Type)
unifyTysX tmpl_tyvars ty1 ty2
  = uTysX ty1 ty2 (\(_,s) -> Just s) (tmpl_tyvars, emptyVarEnv)

unifyTyListsX :: [TyVar] -> [Type] -> [Type]
              -> Maybe (TyVarEnv Type)
unifyTyListsX tmpl_tyvars tys1 tys2
  = uTyListsX tys1 tys2 (\(_,s) -> Just s) (tmpl_tyvars, emptyVarEnv)


uTysX :: Type
      -> Type
      -> (Subst -> Maybe result)
      -> Subst
      -> Maybe result

uTysX (NoteTy _ ty1) ty2 k subst = uTysX ty1 ty2 k subst
uTysX ty1 (NoteTy _ ty2) k subst = uTysX ty1 ty2 k subst

	-- Variables; go for uVar
uTysX (TyVarTy tyvar1) (TyVarTy tyvar2) k subst 
  | tyvar1 == tyvar2
  = k subst
uTysX (TyVarTy tyvar1) ty2 k subst@(tmpls,_)
  | tyvar1 `elem` tmpls
  = uVarX tyvar1 ty2 k subst
uTysX ty1 (TyVarTy tyvar2) k subst@(tmpls,_)
  | tyvar2 `elem` tmpls
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
  = case lookupVarEnv env tv1 of
      Just ty1 ->    -- Already bound
		     uTysX ty1 ty2 k subst

      Nothing	     -- Not already bound
	       |  typeKind ty2 == tyVarKind tv1
	       && occur_check_ok ty2
	       ->     -- No kind mismatch nor occur check
	          k (tmpls, extendVarEnv env tv1 ty2)

	       | otherwise -> Nothing	-- Fail if kind mis-match or occur check
  where
    occur_check_ok ty = all occur_check_ok_tv (varSetElems (tyVarsOfType ty))
    occur_check_ok_tv tv | tv1 == tv = False
			 | otherwise = case lookupVarEnv env tv of
				         Nothing -> True
					 Just ty -> occur_check_ok ty
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
matchTy :: [TyVar]			-- Template tyvars
	-> Type  			-- Template
	-> Type				-- Proposed instance of template
	-> Maybe (TyVarEnv Type)	-- Matching substitution
					

matchTys :: [TyVar]			-- Template tyvars
	 -> [Type]			-- Templates
	 -> [Type]			-- Proposed instance of template
	 -> Maybe (TyVarEnv Type,	-- Matching substitution
		   [Type])		-- Left over instance types

matchTy  tmpls ty1  ty2  = match      ty1  ty2  (\(_,env)       -> Just env)
						(tmpls, emptyVarEnv)

matchTys tmpls tys1 tys2 = match_list tys1 tys2 (\((_,env),tys) -> Just (env,tys))
						(tmpls, emptyVarEnv)
\end{code}

@match@ is the main function.

\begin{code}
match :: Type -> Type    	    -- Current match pair
      -> (Subst -> Maybe result)    -- Continuation
      -> Subst 		 	    -- Current substitution
      -> Maybe result

-- When matching against a type variable, see if the variable
-- has already been bound.  If so, check that what it's bound to
-- is the same as ty; if not, bind it and carry on.

match (TyVarTy v) ty k = \  s@(tmpls,env) ->
			 if v `elem` tmpls then
                               -- v is a template variable
                               case lookupVarEnv env v of
				  Nothing  -> k (tmpls, extendVarEnv env v ty)
				  Just ty' | ty' == ty -> k s      -- Succeeds
				           | otherwise -> Nothing  -- Fails
                         else
                                     -- v is not a template variable; ty had better match
                                     -- Can't use (==) because types differ
                               case ty of
                                  TyVarTy v' | getUnique v == getUnique v'
                                             -> k s       -- Success
                                  other      -> Nothing   -- Failure

match (FunTy arg1 res1)   (FunTy arg2 res2)   k = match arg1 arg2 (match res1 res2 k)
match (AppTy fun1 arg1)   ty2		      k = case splitAppTy_maybe ty2 of
							Just (fun2,arg2) -> match fun1 fun2 (match arg1 arg2 k)
							Nothing 	 -> \ _ -> Nothing	-- Fail
match (TyConApp tc1 tys1) (TyConApp tc2 tys2) k | tc1 == tc2
					        = match_list tys1 tys2 ( \(s,tys2') ->
						  if null tys2' then 
							k s	-- Succeed
						  else
							Nothing	-- Fail	
						  )

	-- With type synonyms, we have to be careful for the exact
	-- same reasons as in the unifier.  Please see the
	-- considerable commentary there before changing anything
	-- here! (WDP 95/05)
match (NoteTy _ ty1) ty2           k = match ty1 ty2 k
match ty1	    (NoteTy _ ty2) k = match ty1 ty2 k

-- Catch-all fails
match _ _ _ = \s -> Nothing

match_list []         tys2       k = \s -> k (s, tys2)
match_list (ty1:tys1) []         k = \s -> Nothing	-- Not enough arg tys => failure
match_list (ty1:tys1) (ty2:tys2) k = match ty1 ty2 (match_list tys1 tys2 k)
\end{code}

