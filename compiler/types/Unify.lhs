\begin{code}
module Unify ( 
	-- Matching of types: 
	--	the "tc" prefix indicates that matching always
	--	respects newtypes (rather than looking through them)
	tcMatchTys, tcMatchTyX, ruleMatchTyX, tcMatchPreds, MatchEnv(..)
   ) where

#include "HsVersions.h"

import Var		( Var, TyVar, tyVarKind )
import VarEnv
import VarSet
import Type		( typeKind, tyVarsOfType, tyVarsOfTypes, tyVarsOfTheta, mkTyVarTys,
			  TvSubstEnv, emptyTvSubstEnv, TvSubst(..), substTy, tcEqTypeX,
			  mkOpenTvSubst, tcView, isSubKind, eqKind, repSplitAppTy_maybe )
import TypeRep          ( Type(..), PredType(..), funTyCon )
import DataCon 		( DataCon, dataConResTys )
import Util		( snocView )
import ErrUtils		( Message )
import Outputable
import Maybes
\end{code}


%************************************************************************
%*									*
		Matching
%*									*
%************************************************************************


Matching is much tricker than you might think.

1. The substitution we generate binds the *template type variables*
   which are given to us explicitly.

2. We want to match in the presence of foralls; 
	e.g 	(forall a. t1) ~ (forall b. t2)

   That is what the RnEnv2 is for; it does the alpha-renaming
   that makes it as if a and b were the same variable.
   Initialising the RnEnv2, so that it can generate a fresh
   binder when necessary, entails knowing the free variables of
   both types.

3. We must be careful not to bind a template type variable to a
   locally bound variable.  E.g.
	(forall a. x) ~ (forall b. b)
   where x is the template type variable.  Then we do not want to
   bind x to a/b!  This is a kind of occurs check.
   The necessary locals accumulate in the RnEnv2.


\begin{code}
data MatchEnv
  = ME	{ me_tmpls :: VarSet	-- Template tyvars
 	, me_env   :: RnEnv2	-- Renaming envt for nested foralls
	}			--   In-scope set includes template tyvars

tcMatchTys :: TyVarSet		-- Template tyvars
	 -> [Type]		-- Template
	 -> [Type]		-- Target
	 -> Maybe TvSubst	-- One-shot; in principle the template
				-- variables could be free in the target

tcMatchTys tmpls tys1 tys2
  = case match_tys menv emptyTvSubstEnv tys1 tys2 of
	Just subst_env -> Just (TvSubst in_scope subst_env)
	Nothing	       -> Nothing
  where
    menv     = ME { me_tmpls = tmpls, me_env = mkRnEnv2 in_scope }
    in_scope = mkInScopeSet (tmpls `unionVarSet` tyVarsOfTypes tys2)
	-- We're assuming that all the interesting 
	-- tyvars in tys1 are in tmpls

-- This is similar, but extends a substitution
tcMatchTyX :: TyVarSet 		-- Template tyvars
	   -> TvSubst		-- Substitution to extend
	   -> Type		-- Template
	   -> Type		-- Target
	   -> Maybe TvSubst
tcMatchTyX tmpls (TvSubst in_scope subst_env) ty1 ty2
  = case match menv subst_env ty1 ty2 of
	Just subst_env -> Just (TvSubst in_scope subst_env)
	Nothing	       -> Nothing
  where
    menv = ME {me_tmpls = tmpls, me_env = mkRnEnv2 in_scope}

tcMatchPreds
	:: [TyVar]			-- Bind these
	-> [PredType] -> [PredType]
   	-> Maybe TvSubstEnv
tcMatchPreds tmpls ps1 ps2
  = match_list (match_pred menv) emptyTvSubstEnv ps1 ps2
  where
    menv = ME { me_tmpls = mkVarSet tmpls, me_env = mkRnEnv2 in_scope_tyvars }
    in_scope_tyvars = mkInScopeSet (tyVarsOfTheta ps1 `unionVarSet` tyVarsOfTheta ps2)

-- This one is called from the expression matcher, which already has a MatchEnv in hand
ruleMatchTyX :: MatchEnv 
	 -> TvSubstEnv		-- Substitution to extend
	 -> Type		-- Template
	 -> Type		-- Target
	 -> Maybe TvSubstEnv

ruleMatchTyX menv subst ty1 ty2 = match menv subst ty1 ty2	-- Rename for export
\end{code}

Now the internals of matching

\begin{code}
match :: MatchEnv	-- For the most part this is pushed downwards
      -> TvSubstEnv 	-- Substitution so far:
			--   Domain is subset of template tyvars
			--   Free vars of range is subset of 
			--	in-scope set of the RnEnv2
      -> Type -> Type	-- Template and target respectively
      -> Maybe TvSubstEnv
-- This matcher works on source types; that is, 
-- it respects NewTypes and PredType

match menv subst ty1 ty2 | Just ty1' <- tcView ty1 = match menv subst ty1' ty2
match menv subst ty1 ty2 | Just ty2' <- tcView ty2 = match menv subst ty1 ty2'

match menv subst (TyVarTy tv1) ty2
  | tv1 `elemVarSet` me_tmpls menv
  = case lookupVarEnv subst tv1' of
	Nothing | any (inRnEnvR rn_env) (varSetElems (tyVarsOfType ty2))
		-> Nothing	-- Occurs check
		| not (typeKind ty2 `isSubKind` tyVarKind tv1)
		-> Nothing	-- Kind mis-match
		| otherwise
		-> Just (extendVarEnv subst tv1 ty2)

	Just ty1' | tcEqTypeX (nukeRnEnvL rn_env) ty1' ty2
		-- ty1 has no locally-bound variables, hence nukeRnEnvL
		-- Note tcEqType...we are doing source-type matching here
		  -> Just subst

	other -> Nothing

   | otherwise	-- tv1 is not a template tyvar
   = case ty2 of
	TyVarTy tv2 | tv1' == rnOccR rn_env tv2 -> Just subst
	other					-> Nothing
  where
    rn_env = me_env menv
    tv1' = rnOccL rn_env tv1

match menv subst (ForAllTy tv1 ty1) (ForAllTy tv2 ty2) 
  = match menv' subst ty1 ty2
  where		-- Use the magic of rnBndr2 to go under the binders
    menv' = menv { me_env = rnBndr2 (me_env menv) tv1 tv2 }

match menv subst (PredTy p1) (PredTy p2) 
  = match_pred menv subst p1 p2
match menv subst (TyConApp tc1 tys1) (TyConApp tc2 tys2) 
  | tc1 == tc2 = match_tys menv subst tys1 tys2
match menv subst (FunTy ty1a ty1b) (FunTy ty2a ty2b) 
  = do { subst' <- match menv subst ty1a ty2a
       ; match menv subst' ty1b ty2b }
match menv subst (AppTy ty1a ty1b) ty2
  | Just (ty2a, ty2b) <- repSplitAppTy_maybe ty2
  = do { subst' <- match menv subst ty1a ty2a
       ; match menv subst' ty1b ty2b }

match menv subst ty1 ty2
  = Nothing

--------------
match_tys menv subst tys1 tys2 = match_list (match menv) subst tys1 tys2

--------------
match_list :: (TvSubstEnv -> a -> a -> Maybe TvSubstEnv)
	   -> TvSubstEnv -> [a] -> [a] -> Maybe TvSubstEnv
match_list fn subst []         []	  = Just subst
match_list fn subst (ty1:tys1) (ty2:tys2) = do	{ subst' <- fn subst ty1 ty2
						; match_list fn subst' tys1 tys2 }
match_list fn subst tys1       tys2 	  = Nothing	

--------------
match_pred menv subst (ClassP c1 tys1) (ClassP c2 tys2)
  | c1 == c2 = match_tys menv subst tys1 tys2
match_pred menv subst (IParam n1 t1) (IParam n2 t2)
  | n1 == n2 = match menv subst t1 t2
match_pred menv subst p1 p2 = Nothing
\end{code}


