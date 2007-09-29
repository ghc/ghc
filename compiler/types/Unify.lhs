%
% (c) The University of Glasgow 2006
%

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module Unify ( 
	-- Matching of types: 
	--	the "tc" prefix indicates that matching always
	--	respects newtypes (rather than looking through them)
	tcMatchTy, tcMatchTys, tcMatchTyX, 
	ruleMatchTyX, tcMatchPreds, MatchEnv(..),
	
	dataConCannotMatch
   ) where

#include "HsVersions.h"

import Var
import VarEnv
import VarSet
import Type
import Coercion
import TyCon
import DataCon
import TypeRep
import Outputable
import Util
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

tcMatchTy :: TyVarSet		-- Template tyvars
	  -> Type		-- Template
	  -> Type		-- Target
	  -> Maybe TvSubst	-- One-shot; in principle the template
				-- variables could be free in the target

tcMatchTy tmpls ty1 ty2
  = case match menv emptyTvSubstEnv ty1 ty2 of
	Just subst_env -> Just (TvSubst in_scope subst_env)
	Nothing	       -> Nothing
  where
    menv     = ME { me_tmpls = tmpls, me_env = mkRnEnv2 in_scope }
    in_scope = mkInScopeSet (tmpls `unionVarSet` tyVarsOfType ty2)
	-- We're assuming that all the interesting 
	-- tyvars in tys1 are in tmpls

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
			 | Just ty2' <- tcView ty2 = match menv subst ty1 ty2'

match menv subst (TyVarTy tv1) ty2
  | tv1' `elemVarSet` me_tmpls menv
  = case lookupVarEnv subst tv1' of
	Nothing 	-- No existing binding
	    | any (inRnEnvR rn_env) (varSetElems (tyVarsOfType ty2))
	    -> Nothing	-- Occurs check
	    | otherwise	
	    -> do { subst1 <- match_kind menv subst tv1 ty2
		  ; return (extendVarEnv subst tv1' ty2) }

	Just ty1' 	-- There is an existing binding; check whether ty2 matches it
	    | tcEqTypeX (nukeRnEnvL rn_env) ty1' ty2
		-- ty1 has no locally-bound variables, hence nukeRnEnvL
		-- Note tcEqType...we are doing source-type matching here
	    -> Just subst
	    | otherwise	-> Nothing	-- ty2 doesn't match
	    

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
	-- 'repSplit' used because the tcView stuff is done above
  = do { subst' <- match menv subst ty1a ty2a
       ; match menv subst' ty1b ty2b }

match menv subst ty1 ty2
  = Nothing

--------------
match_kind :: MatchEnv -> TvSubstEnv -> TyVar -> Type -> Maybe TvSubstEnv
-- Match the kind of the template tyvar with the kind of Type
-- Note [Matching kinds]
match_kind menv subst tv ty
  | isCoVar tv = do { let (ty1,ty2) = splitCoercionKind (tyVarKind tv)
			  (ty3,ty4) = coercionKind ty
		    ; subst1 <- match menv subst ty1 ty3
		    ; match menv subst1 ty2 ty4 }
  | otherwise  = if typeKind ty `isSubKind` tyVarKind tv
		 then Just subst
		 else Nothing

-- Note [Matching kinds]
-- ~~~~~~~~~~~~~~~~~~~~~
-- For ordinary type variables, we don't want (m a) to match (n b) 
-- if say (a::*) and (b::*->*).  This is just a yes/no issue. 
--
-- For coercion kinds matters are more complicated.  If we have a 
-- coercion template variable co::a~[b], where a,b are presumably also
-- template type variables, then we must match co's kind against the 
-- kind of the actual argument, so as to give bindings to a,b.  
--
-- In fact I have no example in mind that *requires* this kind-matching
-- to instantiate template type variables, but it seems like the right
-- thing to do.  C.f. Note [Matching variable types] in Rules.lhs

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


%************************************************************************
%*									*
		GADTs
%*									*
%************************************************************************

Note [Pruning dead case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider	data T a where
		   T1 :: T Int
		   T2 :: T a

		newtype X = MkX Int
		newtype Y = MkY Char

		type family F a
		type instance F Bool = Int

Now consider	case x of { T1 -> e1; T2 -> e2 }

The question before the house is this: if I know something about the type
of x, can I prune away the T1 alternative?

Suppose x::T Char.  It's impossible to construct a (T Char) using T1, 
	Answer = YES (clearly)

Suppose x::T (F a), where 'a' is in scope.  Then 'a' might be instantiated
to 'Bool', in which case x::T Int, so
	ANSWER = NO (clearly)

Suppose x::T X.  Then *in Haskell* it's impossible to construct a (non-bottom) 
value of type (T X) using T1.  But *in FC* it's quite possible.  The newtype
gives a coercion
	CoX :: X ~ Int
So (T CoX) :: T X ~ T Int; hence (T1 `cast` sym (T CoX)) is a non-bottom value
of type (T X) constructed with T1.  Hence
	ANSWER = NO (surprisingly)

Furthermore, this can even happen; see Trac #1251.  GHC's newtype-deriving
mechanism uses a cast, just as above, to move from one dictionary to another,
in effect giving the programmer access to CoX.

Finally, suppose x::T Y.  Then *even in FC* we can't construct a
non-bottom value of type (T Y) using T1.  That's because we can get
from Y to Char, but not to Int.


Here's a related question.  	data Eq a b where EQ :: Eq a a
Consider
	case x of { EQ -> ... }

Suppose x::Eq Int Char.  Is the alternative dead?  Clearly yes.

What about x::Eq Int a, in a context where we have evidence that a~Char.
Then again the alternative is dead.   


			Summary

We are really doing a test for unsatisfiability of the type
constraints implied by the match. And that is clearly, in general, a
hard thing to do.  

However, since we are simply dropping dead code, a conservative test
suffices.  There is a continuum of tests, ranging from easy to hard, that
drop more and more dead code.

For now we implement a very simple test: type variables match
anything, type functions (incl newtypes) match anything, and only
distinct data types fail to match.  We can elaborate later.

\begin{code}
dataConCannotMatch :: [Type] -> DataCon -> Bool
-- Returns True iff the data con *definitely cannot* match a 
--		    scrutinee of type (T tys)
--		    where T is the type constructor for the data con
--
dataConCannotMatch tys con
  | null eq_spec      = False	-- Common
  | all isTyVarTy tys = False	-- Also common
  | otherwise
  = cant_match_s (map (substTyVar subst . fst) eq_spec)
	         (map snd eq_spec)
  where
    dc_tvs  = dataConUnivTyVars con
    eq_spec = dataConEqSpec con
    subst   = zipTopTvSubst dc_tvs tys

    cant_match_s :: [Type] -> [Type] -> Bool
    cant_match_s tys1 tys2 = ASSERT( equalLength tys1 tys2 )
			     or (zipWith cant_match tys1 tys2)

    cant_match :: Type -> Type -> Bool
    cant_match t1 t2
	| Just t1' <- coreView t1 = cant_match t1' t2
	| Just t2' <- coreView t2 = cant_match t1 t2'

    cant_match (FunTy a1 r1) (FunTy a2 r2)
	= cant_match a1 a2 || cant_match r1 r2

    cant_match (TyConApp tc1 tys1) (TyConApp tc2 tys2)
	| isDataTyCon tc1 && isDataTyCon tc2
	= tc1 /= tc2 || cant_match_s tys1 tys2

    cant_match (FunTy {}) (TyConApp tc _) = isDataTyCon tc
    cant_match (TyConApp tc _) (FunTy {}) = isDataTyCon tc
	-- tc can't be FunTyCon by invariant

    cant_match (AppTy f1 a1) ty2
	| Just (f2, a2) <- repSplitAppTy_maybe ty2
	= cant_match f1 f2 || cant_match a1 a2
    cant_match ty1 (AppTy f2 a2)
	| Just (f1, a1) <- repSplitAppTy_maybe ty1
	= cant_match f1 f2 || cant_match a1 a2

    cant_match ty1 ty2 = False	-- Safe!

-- Things we could add;
--	foralls
--	look through newtypes
--	take account of tyvar bindings (EQ example above)
\end{code}