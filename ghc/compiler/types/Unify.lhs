\begin{code}
module Unify ( 
	-- Matching and unification
	matchTys, matchTyX, matchTysX,
	unifyTys, unifyTysX,

	tcRefineTys, tcMatchTys, tcMatchPreds, coreRefineTys,

	-- Re-export
	MaybeErr(..)
   ) where

#include "HsVersions.h"

import Var		( Var, TyVar, tyVarKind )
import VarEnv
import VarSet
import Kind		( isSubKind )
import Type		( predTypeRep, newTypeRep, typeKind, 
			  tyVarsOfType, tyVarsOfTypes, 
			  TvSubstEnv, TvSubst(..), substTy )
import TypeRep          ( Type(..), PredType(..), funTyCon )
import Util		( snocView )
import ErrUtils		( Message )
import Outputable
import Maybes
\end{code}


%************************************************************************
%*									*
		External interface
%*									*
%************************************************************************

\begin{code}
----------------------------
tcRefineTys, tcMatchTys 
	:: [TyVar]			-- Try to unify these
	-> TvSubstEnv			-- Not idempotent
	-> [Type] -> [Type]
   	-> MaybeErr TvSubstEnv Message	-- Not idempotent
-- This one is used by the type checker.  Neither the input nor result
-- substitition is idempotent
tcRefineTys ex_tvs subst tys1 tys2
  = initUM (tryToBind (mkVarSet ex_tvs)) (unify_tys Src subst tys1 tys2)

tcMatchTys ex_tvs subst tys1 tys2
  = initUM (bindOnly (mkVarSet ex_tvs)) (unify_tys Src subst tys1 tys2)

tcMatchPreds
	:: [TyVar]			-- Bind these
	-> [PredType] -> [PredType]
   	-> Maybe TvSubstEnv
tcMatchPreds tvs preds1 preds2
  = maybeErrToMaybe $ initUM (bindOnly (mkVarSet tvs)) $
    unify_preds Src emptyVarEnv preds1 preds2

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
	; extra_env <- unify Src emptyTvSubstEnv ty1' ty2

		-- Find the fixed point of the resulting non-idempotent
		-- substitution, and apply it to the 
	; let extra_subst     = TvSubst in_scope extra_env_fixpt
	      extra_env_fixpt = mapVarEnv (substTy extra_subst) extra_env
	      orig_env'	      = mapVarEnv (substTy extra_subst) orig_env
	; return (orig_env' `plusVarEnv` extra_env_fixpt) }
    

----------------------------
matchTys :: TyVarSet		-- Template tyvars
	 -> [Type]		-- Template
	 -> [Type]		-- Target
	 -> Maybe TvSubstEnv	-- Idempotent, because when matching
				--	the range and domain are distinct

-- PRE-CONDITION for matching: template variables are not free in the target

matchTys tmpls tys1 tys2
  = ASSERT2( not (intersectsVarSet tmpls (tyVarsOfTypes tys2)),
	     ppr tmpls $$ ppr tys1 $$ ppr tys2 )
    maybeErrToMaybe $ initUM (bindOnly tmpls)
			     (unify_tys Src emptyTvSubstEnv tys1 tys2)

matchTyX :: TyVarSet		-- Template tyvars
	 -> TvSubstEnv		-- Idempotent substitution to extend
	 -> Type		-- Template
	 -> Type		-- Target
	 -> Maybe TvSubstEnv	-- Idempotent

matchTyX tmpls env ty1 ty2
  = ASSERT( not (intersectsVarSet tmpls (tyVarsOfType ty2)) )
    maybeErrToMaybe $ initUM (bindOnly tmpls)
			     (unify Src env ty1 ty2)

matchTysX :: TyVarSet		-- Template tyvars
	  -> TvSubstEnv		-- Idempotent substitution to extend
	  -> [Type]		-- Template
	  -> [Type]		-- Target
	  -> Maybe TvSubstEnv	-- Idempotent

matchTysX tmpls env tys1 tys2
  = ASSERT( not (intersectsVarSet tmpls (tyVarsOfTypes tys2)) )
    maybeErrToMaybe $ initUM (bindOnly tmpls) 
			     (unify_tys Src env tys1 tys2)


----------------------------
unifyTys :: TyVarSet -> [Type] -> [Type] -> Maybe TvSubstEnv
unifyTys bind_these tys1 tys2
  = maybeErrToMaybe $ initUM (bindOnly bind_these) $
    unify_tys Src emptyTvSubstEnv tys1 tys2

unifyTysX :: TyVarSet -> TvSubstEnv -> [Type] -> [Type] -> Maybe TvSubstEnv
unifyTysX bind_these subst tys1 tys2
  = maybeErrToMaybe $ initUM (bindOnly bind_these) $
    unify_tys Src subst tys1 tys2

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
unify :: SrcFlag                -- True, unifying source types, false core types.
      -> TvSubstEnv		-- An existing substitution to extend
      -> Type -> Type           -- Types to be unified
      -> UM TvSubstEnv		-- Just the extended substitution, 
				-- Nothing if unification failed
-- We do not require the incoming substitution to be idempotent,
-- nor guarantee that the outgoing one is.  That's fixed up by
-- the wrappers.

-- ToDo: remove debugging junk
unify s subst ty1 ty2 = -- pprTrace "unify" (ppr subst <+> pprParendType ty1 <+> pprParendType ty2) $
			unify_ s subst ty1 ty2

-- Look through NoteTy in the obvious fashion
unify_ s subst (NoteTy _ ty1) ty2  = unify s subst ty1 ty2
unify_ s subst ty1 (NoteTy _ ty2)  = unify s subst ty1 ty2

-- In Core mode, look through NewTcApps and Preds
unify_ Core subst (NewTcApp tc tys) ty2 = unify Core subst (newTypeRep tc tys) ty2
unify_ Core subst ty1 (NewTcApp tc tys) = unify Core subst ty1 (newTypeRep tc tys)

unify_ Core subst (PredTy p) ty2 = unify Core subst (predTypeRep p) ty2
unify_ Core subst ty1 (PredTy p) = unify Core subst ty1 (predTypeRep p)

-- From now on, any NewTcApps/Preds should be taken at face value

unify_ s subst (TyVarTy tv1) ty2  = uVar s False subst tv1 ty2
unify_ s subst ty1 (TyVarTy tv2)  = uVar s True  subst tv2 ty1

unify_ s subst (PredTy p1) (PredTy p2) = unify_pred s subst p1 p2

unify_ s subst t1@(TyConApp tyc1 tys1) t2@(TyConApp tyc2 tys2) 
  | tyc1 == tyc2 = unify_tys s subst tys1 tys2
unify_ Src subst t1@(NewTcApp tc1 tys1) t2@(NewTcApp tc2 tys2)  
  | tc1 == tc2 = unify_tys Src subst tys1 tys2
unify_ s subst (FunTy ty1a ty1b) (FunTy ty2a ty2b) 
  = do { subst' <- unify s subst ty1a ty2a
       ; unify s subst' ty1b ty2b }

	-- Applications need a bit of care!
	-- They can match FunTy and TyConApp, so use splitAppTy_maybe
	-- NB: we've already dealt with type variables and Notes,
	-- so if one type is an App the other one jolly well better be too
unify_ s subst (AppTy ty1a ty1b) ty2
  | Just (ty2a, ty2b) <- unifySplitAppTy_maybe ty2
  = do	{ subst' <- unify s subst ty1a ty2a
        ; unify s subst' ty1b ty2b }

unify_ s subst ty1 (AppTy ty2a ty2b)
  | Just (ty1a, ty1b) <- unifySplitAppTy_maybe ty1
  = do	{ subst' <- unify s subst ty1a ty2a
        ; unify s subst' ty1b ty2b }

unify_ s subst ty1 ty2 = failWith (misMatch ty1 ty2)

------------------------------
unify_pred s subst (ClassP c1 tys1) (ClassP c2 tys2)
  | c1 == c2 = unify_tys s subst tys1 tys2
unify_pred s subst (IParam n1 t1) (IParam n2 t2)
  | n1 == n2 = unify s subst t1 t2
 
------------------------------
unifySplitAppTy_maybe :: Type -> Maybe (Type,Type)
-- NoteTy is already dealt with; take NewTcApps at face value
unifySplitAppTy_maybe (FunTy ty1 ty2)   = Just (TyConApp funTyCon [ty1], ty2)
unifySplitAppTy_maybe (AppTy ty1 ty2)   = Just (ty1, ty2)
unifySplitAppTy_maybe (TyConApp tc tys) = case snocView tys of
						Just (tys', ty') -> Just (TyConApp tc tys', ty')
						Nothing		 -> Nothing
unifySplitAppTy_maybe (NewTcApp tc tys) = case snocView tys of
						Just (tys', ty') -> Just (NewTcApp tc tys', ty')
						Nothing		 -> Nothing
unifySplitAppTy_maybe other = Nothing

------------------------------
unify_tys s   = unifyList (unify s)

unify_preds :: SrcFlag -> TvSubstEnv -> [PredType] -> [PredType] -> UM TvSubstEnv
unify_preds s = unifyList (unify_pred s)

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
uVar :: SrcFlag         -- True, unifying source types, false core types.
     -> Bool            -- Swapped
     -> TvSubstEnv	-- An existing substitution to extend
     -> TyVar           -- Type variable to be unified
     -> Type            -- with this type
     -> UM TvSubstEnv

uVar s swap subst tv1 ty
 = -- check to see whether tv1 is refined
   case (lookupVarEnv subst tv1) of
     -- yes, call back into unify'
     Just ty' | swap      -> unify s subst ty ty' 
              | otherwise -> unify s subst ty' ty
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
data SrcFlag = Src | Core	-- Unifying at the source level, or core level?

data BindFlag = BindMe | AvoidMe | DontBindMe

isCore Core = True
isCore Src  = False

newtype UM a = UM { unUM :: (TyVar -> BindFlag)
		         -> MaybeErr a Message }

instance Monad UM where
  return a = UM (\tvs -> Succeeded a)
  fail s   = UM (\tvs -> Failed (text s))
  m >>= k  = UM (\tvs -> case unUM m tvs of
			   Failed err -> Failed err
			   Succeeded v  -> unUM (k v) tvs)

initUM :: (TyVar -> BindFlag) -> UM a -> MaybeErr a Message
initUM badtvs um = unUM um badtvs

tvBindFlag :: TyVar -> UM BindFlag
tvBindFlag tv = UM (\tv_fn -> Succeeded (tv_fn tv))

failWith :: Message -> UM a
failWith msg = UM (\tv_fn -> Failed msg)

maybeErrToMaybe :: MaybeErr succ fail -> Maybe succ
maybeErrToMaybe (Succeeded a) = Just a
maybeErrToMaybe (Failed m)    = Nothing
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