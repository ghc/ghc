\begin{code}
module Unify ( 
	-- Matching and unification
	matchTys, matchTyX, tcMatchPreds, MatchEnv(..), 

	unifyTys, unifyTysX,

	tcRefineTys, tcMatchTys, coreRefineTys,

	-- Re-export
	MaybeErr(..)
   ) where

#include "HsVersions.h"

import Var		( Var, TyVar, tyVarKind )
import VarEnv
import VarSet
import Kind		( isSubKind )
import Type		( typeKind, tyVarsOfType, tyVarsOfTypes, tyVarsOfTheta, 
			  TvSubstEnv, TvSubst(..), substTy, tcEqTypeX )
import TypeRep          ( Type(..), PredType(..), funTyCon )
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

matchTys :: TyVarSet		-- Template tyvars
	 -> [Type]		-- Template
	 -> [Type]		-- Target
	 -> Maybe TvSubstEnv	-- One-shot; in principle the template
				-- variables could be free in the target

matchTys tmpls tys1 tys2
  = match_tys (ME { me_tmpls = tmpls, me_env = mkRnEnv2 in_scope_tyvars})
	      emptyTvSubstEnv 
	      tys1 tys2
  where
    in_scope_tyvars = mkInScopeSet (tmpls `unionVarSet` tyVarsOfTypes tys2)
	-- We're assuming that all the interesting 
	-- tyvars in tys1 are in tmpls

tcMatchPreds
	:: [TyVar]			-- Bind these
	-> [PredType] -> [PredType]
   	-> Maybe TvSubstEnv
tcMatchPreds tmpls ps1 ps2
  = match_list (match_pred menv) emptyTvSubstEnv ps1 ps2
  where
    menv = ME { me_tmpls = mkVarSet tmpls, me_env = mkRnEnv2 in_scope_tyvars }
    in_scope_tyvars = mkInScopeSet (tyVarsOfTheta ps1 `unionVarSet` tyVarsOfTheta ps2)

matchTyX :: MatchEnv 
	 -> TvSubstEnv		-- Substitution to extend
	 -> Type		-- Template
	 -> Type		-- Target
	 -> Maybe TvSubstEnv

matchTyX menv subst ty1 ty2 = match menv subst ty1 ty2	-- Rename for export
\end{code}

Now the internals of matching

\begin{code}
match :: MatchEnv	-- For the ost part this is pushed downwards
      -> TvSubstEnv 	-- Substitution so far:
			--   Domain is subset of template tyvars
			--   Free vars of range is subset of 
			--	in-scope set of the RnEnv2
      -> Type -> Type	-- Template and target respectively
      -> Maybe TvSubstEnv
-- This matcher works on source types; that is, 
-- it respects NewTypes and PredType

match menv subst (NoteTy _ ty1) ty2 = match menv subst ty1 ty2
match menv subst ty1 (NoteTy _ ty2) = match menv subst ty1 ty2

match menv subst (TyVarTy tv1) ty2
  | tv1 `elemVarSet` me_tmpls menv
  = case lookupVarEnv subst tv1' of
	Nothing | any (inRnEnvR rn_env) (varSetElems (tyVarsOfType ty2))
		-> Nothing	-- Occurs check
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


%************************************************************************
%*									*
		The workhorse
%*									*
%************************************************************************

\begin{code}
tcRefineTys, tcMatchTys 
	:: [TyVar]			-- Try to unify these
	-> TvSubstEnv			-- Not idempotent
	-> [Type] -> [Type]
   	-> MaybeErr Message TvSubstEnv	-- Not idempotent
-- This one is used by the type checker.  Neither the input nor result
-- substitition is idempotent
tcRefineTys ex_tvs subst tys1 tys2
  = initUM (tryToBind (mkVarSet ex_tvs)) (unify_tys subst tys1 tys2)

tcMatchTys ex_tvs subst tys1 tys2
  = initUM (bindOnly (mkVarSet ex_tvs)) (unify_tys subst tys1 tys2)

----------------------------
coreRefineTys :: [TyVar]	-- Try to unify these
	      -> TvSubst	-- A full-blown apply-once substitition
	      -> Type		-- A fixed point of the incoming substitution
	      -> Type
	      -> Maybe TvSubstEnv	-- In-scope set is unaffected
-- Used by Core Lint and the simplifier.  Takes a full apply-once substitution.
-- The incoming substitution's in-scope set should mention all the variables free 
-- in the incoming types
coreRefineTys ex_tvs subst@(TvSubst in_scope orig_env) ty1 ty2
  = maybeErrToMaybe $ initUM (tryToBind (mkVarSet ex_tvs)) $
    do	{ 	-- Apply the input substitution; nothing int ty2
	  let ty1' = substTy subst ty1	
		-- Run the unifier, starting with an empty env
	; extra_env <- unify emptyTvSubstEnv ty1' ty2

		-- Find the fixed point of the resulting non-idempotent
		-- substitution, and apply it to the 
	; let extra_subst     = TvSubst in_scope extra_env_fixpt
	      extra_env_fixpt = mapVarEnv (substTy extra_subst) extra_env
	      orig_env'	      = mapVarEnv (substTy extra_subst) orig_env
	; return (orig_env' `plusVarEnv` extra_env_fixpt) }
    

----------------------------
unifyTys :: TyVarSet -> [Type] -> [Type] -> Maybe TvSubstEnv
unifyTys bind_these tys1 tys2
  = maybeErrToMaybe $ initUM (bindOnly bind_these) $
    unify_tys emptyTvSubstEnv tys1 tys2

unifyTysX :: TyVarSet -> TvSubstEnv -> [Type] -> [Type] -> Maybe TvSubstEnv
unifyTysX bind_these subst tys1 tys2
  = maybeErrToMaybe $ initUM (bindOnly bind_these) $
    unify_tys subst tys1 tys2

----------------------------
tryToBind, bindOnly :: TyVarSet -> TyVar -> BindFlag
tryToBind tv_set tv | tv `elemVarSet` tv_set = BindMe
		    | otherwise	             = AvoidMe

bindOnly tv_set tv | tv `elemVarSet` tv_set = BindMe
		   | otherwise	            = DontBindMe

emptyTvSubstEnv :: TvSubstEnv
emptyTvSubstEnv = emptyVarEnv
\end{code}


%************************************************************************
%*									*
		The workhorse
%*									*
%************************************************************************

\begin{code}
unify :: TvSubstEnv		-- An existing substitution to extend
      -> Type -> Type           -- Types to be unified
      -> UM TvSubstEnv		-- Just the extended substitution, 
				-- Nothing if unification failed
-- We do not require the incoming substitution to be idempotent,
-- nor guarantee that the outgoing one is.  That's fixed up by
-- the wrappers.

unify subst ty1 ty2 = -- pprTrace "unify" (ppr subst <+> pprParendType ty1 <+> pprParendType ty2) $
			unify_ subst ty1 ty2

-- in unify_, any NewTcApps/Preds should be taken at face value
unify_ subst (TyVarTy tv1) ty2  = uVar False subst tv1 ty2
unify_ subst ty1 (TyVarTy tv2)  = uVar True  subst tv2 ty1

unify_ subst (PredTy p1) (PredTy p2) = unify_pred subst p1 p2

unify_ subst t1@(TyConApp tyc1 tys1) t2@(TyConApp tyc2 tys2) 
  | tyc1 == tyc2 = unify_tys subst tys1 tys2

unify_ subst (FunTy ty1a ty1b) (FunTy ty2a ty2b) 
  = do { subst' <- unify subst ty1a ty2a
       ; unify subst' ty1b ty2b }

	-- Applications need a bit of care!
	-- They can match FunTy and TyConApp, so use splitAppTy_maybe
	-- NB: we've already dealt with type variables and Notes,
	-- so if one type is an App the other one jolly well better be too
unify_ subst (AppTy ty1a ty1b) ty2
  | Just (ty2a, ty2b) <- repSplitAppTy_maybe ty2
  = do	{ subst' <- unify subst ty1a ty2a
        ; unify subst' ty1b ty2b }

unify_ subst ty1 (AppTy ty2a ty2b)
  | Just (ty1a, ty1b) <- repSplitAppTy_maybe ty1
  = do	{ subst' <- unify subst ty1a ty2a
        ; unify subst' ty1b ty2b }

unify_ subst ty1 ty2 = failWith (misMatch ty1 ty2)

------------------------------
unify_pred subst (ClassP c1 tys1) (ClassP c2 tys2)
  | c1 == c2 = unify_tys subst tys1 tys2
unify_pred subst (IParam n1 t1) (IParam n2 t2)
  | n1 == n2 = unify subst t1 t2
unify_pred subst p1 p2 = failWith (misMatch (PredTy p1) (PredTy p2))
 
------------------------------
unify_tys = unifyList unify

unifyList :: Outputable a 
	  => (TvSubstEnv -> a -> a -> UM TvSubstEnv)
	  -> TvSubstEnv -> [a] -> [a] -> UM TvSubstEnv
unifyList unifier subst orig_xs orig_ys
  = go subst orig_xs orig_ys
  where
    go subst []     []     = return subst
    go subst (x:xs) (y:ys) = do { subst' <- unifier subst x y
			        ; go subst' xs ys }
    go subst _      _      = failWith (lengthMisMatch orig_xs orig_ys)

------------------------------
uVar :: Bool            -- Swapped
     -> TvSubstEnv	-- An existing substitution to extend
     -> TyVar           -- Type variable to be unified
     -> Type            -- with this type
     -> UM TvSubstEnv

uVar swap subst tv1 ty
 = -- check to see whether tv1 is refined
   case (lookupVarEnv subst tv1) of
     -- yes, call back into unify'
     Just ty' | swap      -> unify subst ty ty' 
              | otherwise -> unify subst ty' ty
     -- No, continue
     Nothing          -> uUnrefined subst tv1 ty


uUnrefined :: TvSubstEnv          -- An existing substitution to extend
           -> TyVar               -- Type variable to be unified
           -> Type                -- with this type
           -> UM TvSubstEnv

-- We know that tv1 isn't refined
uUnrefined subst tv1 ty2@(TyVarTy tv2)
  | tv1 == tv2    -- Same, do nothing
  = return subst

    -- Check to see whether tv2 is refined
  | Just ty' <- lookupVarEnv subst tv2
  = uUnrefined subst tv1 ty'

  -- So both are unrefined; next, see if the kinds force the direction
  | k1 == k2	-- Can update either; so check the bind-flags
  = do	{ b1 <- tvBindFlag tv1
	; b2 <- tvBindFlag tv2
	; case (b1,b2) of
	    (DontBindMe, DontBindMe) -> failWith (misMatch ty1 ty2)
	    (DontBindMe, _) 	     -> bindTv subst tv2 ty1
	    (BindMe, _) 	     -> bindTv subst tv1 ty2
	    (AvoidMe, BindMe)	     -> bindTv subst tv2 ty1
	    (AvoidMe, _)	     -> bindTv subst tv1 ty2
	}

  | k1 `isSubKind` k2	-- Must update tv2
  = do	{ b2 <- tvBindFlag tv2
	; case b2 of
	    DontBindMe -> failWith (misMatch ty1 ty2)
	    other      -> bindTv subst tv2 ty1
	}

  | k2 `isSubKind` k1	-- Must update tv1
  = do	{ b1 <- tvBindFlag tv1
	; case b1 of
	    DontBindMe -> failWith (misMatch ty1 ty2)
	    other      -> bindTv subst tv1 ty2
	}

  | otherwise = failWith (kindMisMatch tv1 ty2)
  where
    ty1 = TyVarTy tv1
    k1 = tyVarKind tv1
    k2 = tyVarKind tv2

uUnrefined subst tv1 ty2	-- ty2 is not a type variable
	-- Do occurs check...
  | tv1 `elemVarSet` substTvSet subst (tyVarsOfType ty2)
  = failWith (occursCheck tv1 ty2)
	-- And a kind check...
  | k2 `isSubKind` k1
  = do	{ b1 <- tvBindFlag tv1
	; case b1 of		-- And  check that tv1 is bindable
	    DontBindMe -> failWith (misMatch ty1 ty2)
	    other      -> bindTv subst tv1 ty2
	}
  | otherwise
  = pprTrace "kind" (ppr tv1 <+> ppr k1 $$ ppr ty2 <+> ppr k2) $
    failWith (kindMisMatch tv1 ty2)
  where
    ty1 = TyVarTy tv1
    k1 = tyVarKind tv1
    k2 = typeKind ty2

substTvSet :: TvSubstEnv -> TyVarSet -> TyVarSet
-- Apply the non-idempotent substitution to a set of type variables,
-- remembering that the substitution isn't necessarily idempotent
substTvSet subst tvs
  = foldVarSet (unionVarSet . get) emptyVarSet tvs
  where
    get tv = case lookupVarEnv subst tv of
		Nothing -> unitVarSet tv
		Just ty -> substTvSet subst (tyVarsOfType ty)

bindTv subst tv ty = return (extendVarEnv subst tv ty)
\end{code}

%************************************************************************
%*									*
		Unification monad
%*									*
%************************************************************************

\begin{code}
data BindFlag = BindMe | AvoidMe | DontBindMe

newtype UM a = UM { unUM :: (TyVar -> BindFlag)
		         -> MaybeErr Message a }

instance Monad UM where
  return a = UM (\tvs -> Succeeded a)
  fail s   = UM (\tvs -> Failed (text s))
  m >>= k  = UM (\tvs -> case unUM m tvs of
			   Failed err -> Failed err
			   Succeeded v  -> unUM (k v) tvs)

initUM :: (TyVar -> BindFlag) -> UM a -> MaybeErr Message a
initUM badtvs um = unUM um badtvs

tvBindFlag :: TyVar -> UM BindFlag
tvBindFlag tv = UM (\tv_fn -> Succeeded (tv_fn tv))

failWith :: Message -> UM a
failWith msg = UM (\tv_fn -> Failed msg)

maybeErrToMaybe :: MaybeErr fail succ -> Maybe succ
maybeErrToMaybe (Succeeded a) = Just a
maybeErrToMaybe (Failed m)    = Nothing

------------------------------
repSplitAppTy_maybe :: Type -> Maybe (Type,Type)
-- Like Type.splitAppTy_maybe, but any coreView stuff is already done
repSplitAppTy_maybe (FunTy ty1 ty2)   = Just (TyConApp funTyCon [ty1], ty2)
repSplitAppTy_maybe (AppTy ty1 ty2)   = Just (ty1, ty2)
repSplitAppTy_maybe (TyConApp tc tys) = case snocView tys of
						Just (tys', ty') -> Just (TyConApp tc tys', ty')
						Nothing		 -> Nothing
repSplitAppTy_maybe other = Nothing
\end{code}


%************************************************************************
%*									*
		Error reporting
	We go to a lot more trouble to tidy the types
	in TcUnify.  Maybe we'll end up having to do that
	here too, but I'll leave it for now.
%*									*
%************************************************************************

\begin{code}
misMatch t1 t2
  = ptext SLIT("Can't match types") <+> quotes (ppr t1) <+> 
    ptext SLIT("and") <+> quotes (ppr t2)

lengthMisMatch tys1 tys2
  = sep [ptext SLIT("Can't match unequal length lists"), 
	 nest 2 (ppr tys1), nest 2 (ppr tys2) ]

kindMisMatch tv1 t2
  = vcat [ptext SLIT("Can't match kinds") <+> quotes (ppr (tyVarKind tv1)) <+> 
	    ptext SLIT("and") <+> quotes (ppr (typeKind t2)),
	  ptext SLIT("when matching") <+> quotes (ppr tv1) <+> 
		ptext SLIT("with") <+> quotes (ppr t2)]

occursCheck tv ty
  = hang (ptext SLIT("Can't construct the infinite type"))
       2 (ppr tv <+> equals <+> ppr ty)
\end{code}