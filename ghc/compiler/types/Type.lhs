\begin{code}
module Type (
	GenType(..), Type, 

	mkTyVarTy, mkTyVarTys, getTyVar, getTyVar_maybe, isTyVarTy,

	mkAppTy, mkAppTys, splitAppTy, splitAppTys,

	mkFunTy, mkFunTys, splitFunTy_maybe, splitFunTys,

	mkTyConApp, mkTyConTy, splitTyConApp_maybe,
	splitAlgTyConApp_maybe, splitAlgTyConApp,
	mkDictTy, splitDictTy_maybe, isDictTy,

	mkSynTy, isSynTy,

	mkForAllTy, mkForAllTys, splitForAllTy_maybe, splitForAllTys, 
	applyTy, applyTys,

	TauType, RhoType, SigmaType, ThetaType,
	isTauTy,
	mkRhoTy, splitRhoTy,
	mkSigmaTy, splitSigmaTy,

	isUnpointedType, isUnboxedType, typePrimRep,

	matchTy, matchTys, 

	tyVarsOfType, tyVarsOfTypes, namesOfType, typeKind,

	instantiateTy, instantiateTauTy, instantiateThetaTy,

	showTypeCategory
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} Id	( Id )

-- friends:
import Class	( classTyCon, Class )
import Kind	( mkBoxedTypeKind, resultKind, Kind )
import TyCon	( mkFunTyCon, isFunTyCon, isEnumerationTyCon, isTupleTyCon, maybeTyConSingleCon,
		  isPrimTyCon, isAlgTyCon, isSynTyCon, tyConArity,
		  tyConKind, tyConDataCons, getSynTyConDefn, 
		  tyConPrimRep, tyConClass_maybe, TyCon )
import TyVar	( GenTyVarSet, TyVarEnv, GenTyVar, TyVar,
		  tyVarKind, emptyTyVarSet, unionTyVarSets, minusTyVarSet,
		  unitTyVarSet, lookupTyVarEnv, delFromTyVarEnv, zipTyVarEnv, mkTyVarEnv,
		  emptyTyVarEnv, isEmptyTyVarEnv, addToTyVarEnv )
import Name	( NamedThing(..), 
		  NameSet(..), unionNameSets, emptyNameSet, unitNameSet, minusNameSet
		)

-- others
import BasicTypes ( Unused )
import Maybes	( maybeToBool, assocMaybe )
import PrimRep	( PrimRep(..) )
import Unique	-- quite a few *Keys
import Util	( thenCmp, panic, assertPanic )
\end{code}



%************************************************************************
%*									*
\subsection{The data type}
%*									*
%************************************************************************


\begin{code}
type Type  = GenType Unused	-- Used after typechecker

data GenType flexi			-- Parameterised over the "flexi" part of a type variable
  = TyVarTy (GenTyVar flexi)

  | AppTy
	(GenType flexi)		-- Function is *not* a TyConApp
	(GenType flexi)

  | TyConApp			-- Application of a TyCon
	TyCon			-- *Invariant* saturated appliations of FunTyCon and
				-- 	synonyms have their own constructors, below.
	[GenType flexi]		-- Might not be saturated.

  | FunTy			-- Special case of TyConApp: TyConApp FunTyCon [t1,t2]
	(GenType flexi)
	(GenType flexi)

  | SynTy 			-- Saturated application of a type synonym
	(GenType flexi)		-- The unexpanded version; always a TyConTy
	(GenType flexi)		-- The expanded version

  | ForAllTy
	(GenTyVar flexi)
	(GenType flexi)		-- TypeKind
\end{code}


%************************************************************************
%*									*
\subsection{Constructor-specific functions}
%*									*
%************************************************************************


---------------------------------------------------------------------
				TyVarTy
				~~~~~~~
\begin{code}
mkTyVarTy  :: GenTyVar flexi   -> GenType flexi
mkTyVarTy  = TyVarTy

mkTyVarTys :: [GenTyVar flexi] -> [GenType flexi]
mkTyVarTys = map mkTyVarTy -- a common use of mkTyVarTy

getTyVar :: String -> GenType flexi -> GenTyVar flexi
getTyVar msg (TyVarTy tv) = tv
getTyVar msg (SynTy _ t)  = getTyVar msg t
getTyVar msg other	  = panic ("getTyVar: " ++ msg)

getTyVar_maybe :: GenType flexi -> Maybe (GenTyVar flexi)
getTyVar_maybe (TyVarTy tv) = Just tv
getTyVar_maybe (SynTy _ t)  = getTyVar_maybe t
getTyVar_maybe other	    = Nothing

isTyVarTy :: GenType flexi -> Bool
isTyVarTy (TyVarTy tv) = True
isTyVarTy (SynTy _ ty) = isTyVarTy ty
isTyVarTy other        = False
\end{code}


---------------------------------------------------------------------
				AppTy
				~~~~~
We need to be pretty careful with AppTy to make sure we obey the 
invariant that a TyConApp is always visibly so.  mkAppTy maintains the
invariant: use it.

\begin{code}
mkAppTy orig_ty1 orig_ty2 = mk_app orig_ty1
  where
    mk_app (SynTy _ ty1)     = mk_app ty1
    mk_app (TyConApp tc tys) = mkTyConApp tc (tys ++ [orig_ty2])
    mk_app ty1		     = AppTy orig_ty1 orig_ty2

mkAppTys :: GenType flexi -> [GenType flexi] -> GenType flexi
mkAppTys orig_ty1 []	    = orig_ty1
	-- This check for an empty list of type arguments
	-- avoids the needless of a type synonym constructor.
	-- For example: mkAppTys Rational []
	--   returns to (Ratio Integer), which has needlessly lost
	--   the Rational part.
mkAppTys orig_ty1 orig_tys2 = mk_app orig_ty1
  where
    mk_app (SynTy _ ty1)     = mk_app ty1
    mk_app (TyConApp tc tys) = mkTyConApp tc (tys ++ orig_tys2)
    mk_app ty1		     = foldl AppTy orig_ty1 orig_tys2

splitAppTy :: GenType flexi -> (GenType flexi, GenType flexi)
splitAppTy (FunTy ty1 ty2)   = (TyConApp mkFunTyCon [ty1], ty2)
splitAppTy (AppTy ty1 ty2)   = (ty1, ty2)
splitAppTy (SynTy _ ty)      = splitAppTy ty
splitAppTy (TyConApp tc tys) = split tys []
			    where
			       split [ty2]    acc = (TyConApp tc (reverse acc), ty2)
			       split (ty:tys) acc = split tys (ty:acc)
splitAppTy other	     = panic "splitAppTy"

splitAppTys :: GenType flexi -> (GenType flexi, [GenType flexi])
splitAppTys ty = split ty ty []
  where
    split orig_ty (AppTy ty arg)        args = split ty ty (arg:args)
    split orig_ty (SynTy _ ty)          args = split orig_ty ty args
    split orig_ty (FunTy ty1 ty2)       args = ASSERT( null args )
					       (TyConApp mkFunTyCon [], [ty1,ty2])
    split orig_ty (TyConApp tc tc_args) args = (TyConApp tc [], tc_args ++ args)
    split orig_ty ty		        args = (orig_ty, args)
\end{code}


---------------------------------------------------------------------
				FunTy
				~~~~~

\begin{code}
mkFunTy :: GenType flexi -> GenType flexi -> GenType flexi
mkFunTy arg res = FunTy arg res

mkFunTys :: [GenType flexi] -> GenType flexi -> GenType flexi
mkFunTys tys ty = foldr FunTy ty tys

splitFunTy_maybe :: GenType flexi -> Maybe (GenType flexi, GenType flexi)
splitFunTy_maybe (FunTy arg res) = Just (arg, res)
splitFunTy_maybe (SynTy _ ty)    = splitFunTy_maybe ty
splitFunTy_maybe other	         = Nothing


splitFunTys :: GenType flexi -> ([GenType flexi], GenType flexi)
splitFunTys ty = split [] ty ty
  where
    split args orig_ty (FunTy arg res) = split (arg:args) res res
    split args orig_ty (SynTy _ ty)    = split args orig_ty ty
    split args orig_ty ty              = (reverse args, orig_ty)
\end{code}



---------------------------------------------------------------------
				TyConApp
				~~~~~~~~

\begin{code}
mkTyConApp :: TyCon -> [GenType flexi] -> GenType flexi
mkTyConApp tycon tys
  | isFunTyCon tycon && length tys == 2
  = case tys of 
	(ty1:ty2:_) -> FunTy ty1 ty2

  | otherwise
  = ASSERT(not (isSynTyCon tycon))
    TyConApp tycon tys

mkTyConTy :: TyCon -> GenType flexi
mkTyConTy tycon = ASSERT( not (isSynTyCon tycon) ) 
		  TyConApp tycon []

-- splitTyConApp "looks through" synonyms, because they don't
-- mean a distinct type, but all other type-constructor applications
-- including functions are returned as Just ..

splitTyConApp_maybe :: GenType flexi -> Maybe (TyCon, [GenType flexi])
splitTyConApp_maybe (TyConApp tc tys) = Just (tc, tys)
splitTyConApp_maybe (FunTy arg res)   = Just (mkFunTyCon, [arg,res])
splitTyConApp_maybe (SynTy _ ty)      = splitTyConApp_maybe ty
splitTyConApp_maybe other	      = Nothing

-- splitAlgTyConApp_maybe looks for 
--	*saturated* applications of *algebraic* data types
-- "Algebraic" => newtype, data type, or dictionary (not function types)
-- We return the constructors too.

splitAlgTyConApp_maybe :: GenType flexi -> Maybe (TyCon, [GenType flexi], [Id])
splitAlgTyConApp_maybe (TyConApp tc tys) 
  | isAlgTyCon tc &&
    tyConArity tc == length tys   = Just (tc, tys, tyConDataCons tc)
splitAlgTyConApp_maybe (SynTy _ ty) = splitAlgTyConApp_maybe ty
splitAlgTyConApp_maybe other	  = Nothing

splitAlgTyConApp :: GenType flexi -> (TyCon, [GenType flexi], [Id])
	-- Here the "algebraic" property is an *assertion*
splitAlgTyConApp (TyConApp tc tys) = ASSERT( isAlgTyCon tc && tyConArity tc == length tys )
	      			     (tc, tys, tyConDataCons tc)
splitAlgTyConApp (SynTy _ ty)      = splitAlgTyConApp ty
\end{code}

"Dictionary" types are just ordinary data types, but you can
tell from the type constructor whether it's a dictionary or not.

\begin{code}
mkDictTy :: Class -> [GenType flexi] -> GenType flexi
mkDictTy clas tys = TyConApp (classTyCon clas) tys

splitDictTy_maybe :: GenType flexi -> Maybe (Class, [GenType flexi])
splitDictTy_maybe (TyConApp tc tys) 
  |  maybeToBool maybe_class
  && tyConArity tc == length tys = Just (clas, tys)
  where
     maybe_class = tyConClass_maybe tc
     Just clas   = maybe_class

splitDictTy_maybe (SynTy _ ty) 	= splitDictTy_maybe ty
splitDictTy_maybe other		= Nothing

isDictTy :: GenType flexi -> Bool
	-- This version is slightly more efficient than (maybeToBool . splitDictTy)
isDictTy (TyConApp tc tys) 
  |  maybeToBool (tyConClass_maybe tc)
  && tyConArity tc == length tys
  = True
isDictTy (SynTy _ ty) 		= isDictTy ty
isDictTy other			= False
\end{code}


---------------------------------------------------------------------
				SynTy
				~~~~~

\begin{code}
mkSynTy syn_tycon tys
  = ASSERT(isSynTyCon syn_tycon)
    SynTy (TyConApp syn_tycon tys)
	  (instantiateTauTy (zipTyVarEnv tyvars tys) body)
  where
    (tyvars, body) = getSynTyConDefn syn_tycon

isSynTy (SynTy _ _) = True
isSynTy other       = False
\end{code}

Notes on type synonyms
~~~~~~~~~~~~~~~~~~~~~~
The various "split" functions (splitFunTy, splitRhoTy, splitForAllTy) try
to return type synonyms whereever possible. Thus

	type Foo a = a -> a

we want 
	splitFunTys (a -> Foo a) = ([a], Foo a)
not			           ([a], a -> a)

The reason is that we then get better (shorter) type signatures in 
interfaces.  Notably this plays a role in tcTySigs in TcBinds.lhs.




---------------------------------------------------------------------
				ForAllTy
				~~~~~~~~

\begin{code}
mkForAllTy = ForAllTy

mkForAllTys :: [GenTyVar flexi] -> GenType flexi -> GenType flexi
mkForAllTys tyvars ty = foldr ForAllTy ty tyvars

splitForAllTy_maybe :: GenType flexi -> Maybe (GenTyVar flexi, GenType flexi)
splitForAllTy_maybe (SynTy _ ty)        = splitForAllTy_maybe ty
splitForAllTy_maybe (ForAllTy tyvar ty) = Just(tyvar, ty)
splitForAllTy_maybe _		        = Nothing

splitForAllTys :: GenType flexi -> ([GenTyVar flexi], GenType flexi)
splitForAllTys ty = split ty ty []
   where
     split orig_ty (ForAllTy tv ty) tvs = split ty ty (tv:tvs)
     split orig_ty (SynTy _ ty)     tvs = split orig_ty ty tvs
     split orig_ty t	            tvs = (reverse tvs, orig_ty)
\end{code}


\begin{code}
applyTy :: GenType flexi -> GenType flexi -> GenType flexi
applyTy (SynTy _ fun)    arg = applyTy fun arg
applyTy (ForAllTy tv ty) arg = instantiateTy (mkTyVarEnv [(tv,arg)]) ty
applyTy other		 arg = panic "applyTy"

applyTys :: GenType flexi -> [GenType flexi] -> GenType flexi
applyTys fun_ty arg_tys
 = go [] fun_ty arg_tys
 where
   go env ty               []         = instantiateTy (mkTyVarEnv env) ty
   go env (SynTy _ fun)    args       = go env fun args
   go env (ForAllTy tv ty) (arg:args) = go ((tv,arg):env) ty args
   go env other            args       = panic "applyTys"
\end{code}


%************************************************************************
%*									*
\subsection{Stuff to do with the source-language types}
%*									*
%************************************************************************

\begin{code}
type RhoType   = Type
type TauType   = Type
type ThetaType = [(Class, [Type])]
type SigmaType = Type
\end{code}

@isTauTy@ tests for nested for-alls.

\begin{code}
isTauTy :: GenType flexi -> Bool
isTauTy (TyVarTy v)      = True
isTauTy (TyConApp _ tys) = all isTauTy tys
isTauTy (AppTy a b)    	 = isTauTy a && isTauTy b
isTauTy (FunTy a b)  	 = isTauTy a && isTauTy b
isTauTy (SynTy _ ty)   	 = isTauTy ty
isTauTy other	       	 = False
\end{code}

\begin{code}
mkRhoTy :: [(Class, [GenType flexi])] -> GenType flexi -> GenType flexi
mkRhoTy theta ty = foldr (\(c,t) r -> FunTy (mkDictTy c t) r) ty theta

splitRhoTy :: GenType flexi -> ([(Class, [GenType flexi])], GenType flexi)
splitRhoTy ty = split ty ty []
 where
  split orig_ty (FunTy arg res) ts = case splitDictTy_maybe arg of
					Just pair -> split res res (pair:ts)
					Nothing   -> (reverse ts, orig_ty)
  split orig_ty (SynTy _ ty) ts    = split orig_ty ty ts
  split orig_ty ty ts		   = (reverse ts, orig_ty)
\end{code}



\begin{code}
mkSigmaTy tyvars theta tau = mkForAllTys tyvars (mkRhoTy theta tau)

splitSigmaTy :: GenType flexi -> ([GenTyVar flexi], [(Class, [GenType flexi])], GenType flexi)
splitSigmaTy ty =
  (tyvars, theta, tau)
 where
  (tyvars,rho) = splitForAllTys ty
  (theta,tau)  = splitRhoTy rho
\end{code}


%************************************************************************
%*									*
\subsection{Kinds and free variables}
%*									*
%************************************************************************

---------------------------------------------------------------------
		Finding the kind of a type
		~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
typeKind :: GenType flexi -> Kind

typeKind (TyVarTy tyvar) 	= tyVarKind tyvar
typeKind (TyConApp tycon tys)	= foldr (\_ k -> resultKind k) (tyConKind tycon) tys
typeKind (SynTy _ ty)		= typeKind ty
typeKind (FunTy fun arg)	= mkBoxedTypeKind
typeKind (AppTy fun arg)	= resultKind (typeKind fun)
typeKind (ForAllTy _ _)		= mkBoxedTypeKind
\end{code}


---------------------------------------------------------------------
		Free variables of a type
		~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
tyVarsOfType :: GenType flexi -> GenTyVarSet flexi

tyVarsOfType (TyVarTy tv)		= unitTyVarSet tv
tyVarsOfType (TyConApp tycon tys)	= tyVarsOfTypes tys
tyVarsOfType (SynTy ty1 ty2)		= tyVarsOfType ty1
tyVarsOfType (FunTy arg res)		= tyVarsOfType arg `unionTyVarSets` tyVarsOfType res
tyVarsOfType (AppTy fun arg)		= tyVarsOfType fun `unionTyVarSets` tyVarsOfType arg
tyVarsOfType (ForAllTy tyvar ty)	= tyVarsOfType ty `minusTyVarSet` unitTyVarSet tyvar

tyVarsOfTypes :: [GenType flexi] -> GenTyVarSet flexi
tyVarsOfTypes tys = foldr (unionTyVarSets.tyVarsOfType) emptyTyVarSet tys

-- Find the free names of a type, including the type constructors and classes it mentions
namesOfType :: GenType flexi -> NameSet
namesOfType (TyVarTy tv)		= unitNameSet (getName tv)
namesOfType (TyConApp tycon tys)	= unitNameSet (getName tycon) `unionNameSets`
					  namesOfTypes tys
namesOfType (SynTy ty1 ty2)		= namesOfType ty1
namesOfType (FunTy arg res)		= namesOfType arg `unionNameSets` namesOfType res
namesOfType (AppTy fun arg)		= namesOfType fun `unionNameSets` namesOfType arg
namesOfType (ForAllTy tyvar ty)		= namesOfType ty `minusNameSet` unitNameSet (getName tyvar)

namesOfTypes tys = foldr (unionNameSets . namesOfType) emptyNameSet tys
\end{code}


%************************************************************************
%*									*
\subsection{Instantiating a type}
%*									*
%************************************************************************

\begin{code}
instantiateTy	 :: TyVarEnv (GenType flexi)  -> GenType flexi  -> GenType flexi
instantiateTauTy :: TyVarEnv (GenType flexi2) -> GenType flexi1 -> GenType flexi2


-- instantiateTy applies a type environment to a type.
-- It can handle shadowing; for example:
--	f = /\ t1 t2 -> \ d ->
--	   letrec f' = /\ t1 -> \x -> ...(f' t1 x')...
--         in f' t1
-- Here, when we clone t1 to t1', say, we'll come across shadowing
-- when applying the clone environment to the type of f'.
--
-- As a sanity check, we should also check that name capture 
-- doesn't occur, but that means keeping track of the free variables of the
-- range of the TyVarEnv, which I don't do just yet.

instantiateTy tenv ty
  | isEmptyTyVarEnv tenv
  = ty

  | otherwise
  = go tenv ty
  where
    go tenv ty@(TyVarTy tv)   = case (lookupTyVarEnv tenv tv) of
				      Nothing -> ty
				      Just ty -> ty
    go tenv (TyConApp tc tys) = TyConApp tc (map (go tenv) tys)
    go tenv (SynTy ty1 ty2)   = SynTy (go tenv ty1) (go tenv ty2)
    go tenv (FunTy arg res)   = FunTy (go tenv arg) (go tenv res)
    go tenv (AppTy fun arg)   = mkAppTy (go tenv fun) (go tenv arg)
    go tenv (ForAllTy tv ty)  = ForAllTy tv (go tenv' ty)
			      where
				tenv' = case lookupTyVarEnv tenv tv of
					    Nothing -> tenv
					    Just _  -> delFromTyVarEnv tenv tv

-- instantiateTauTy works only (a) on types with no ForAlls,
-- 	and when	       (b) all the type variables are being instantiated
-- In return it is more polymorphic than instantiateTy

instantiateTauTy tenv ty = go ty
  where
    go ty@(TyVarTy tv)   = case (lookupTyVarEnv tenv tv) of
				      Just ty -> ty  -- Must succeed
    go (TyConApp tc tys) = TyConApp tc (map go tys)
    go (SynTy ty1 ty2)	 = SynTy (go ty1) (go ty2)
    go (FunTy arg res)	 = FunTy (go arg) (go res)
    go (AppTy fun arg)	 = mkAppTy (go fun) (go arg)
    go (ForAllTy tv ty)  = panic "instantiateTauTy"


instantiateThetaTy :: TyVarEnv Type -> ThetaType -> ThetaType
instantiateThetaTy tenv theta
 = [(clas, map (instantiateTauTy tenv) tys) | (clas, tys) <- theta]
\end{code}


%************************************************************************
%*									*
\subsection{Boxedness and pointedness}
%*									*
%************************************************************************

A type is
	*unboxed*	iff its representation is other than a pointer
			Unboxed types cannot instantiate a type variable
			Unboxed types are always unpointed.

	*unpointed*	iff it can't be a thunk, and cannot have value bottom
			An unpointed type may or may not be unboxed.
				(E.g. Array# is unpointed, but boxed.)
			An unpointed type *can* instantiate a type variable,
			provided it is boxed.

	*primitive*	iff it is a built-in type that can't be expressed
				in Haskell

Currently, all primitive types are unpointed, but that's not necessarily
the case.  (E.g. Int could be primitive.)

\begin{code}
isUnboxedType :: Type -> Bool
isUnboxedType ty = case typePrimRep ty of
			PtrRep -> False
			other  -> True

-- Danger!  Currently the unpointed types are precisely
-- the primitive ones, but that might not always be the case
isUnpointedType :: Type -> Bool
isUnpointedType ty = case splitTyConApp_maybe ty of
			   Just (tc, ty_args) -> isPrimTyCon tc
			   other	      -> False

typePrimRep :: Type -> PrimRep
typePrimRep ty = case splitTyConApp_maybe ty of
		   Just (tc, ty_args) -> tyConPrimRep tc
		   other	      -> PtrRep
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
matchTy :: GenType flexi1			-- Template
	-> GenType flexi2			-- Proposed instance of template
	-> Maybe (TyVarEnv (GenType flexi2))	-- Matching substitution
					

matchTys :: [GenType flexi1]			-- Templates
	 -> [GenType flexi2]			-- Proposed instance of template
	 -> Maybe (TyVarEnv (GenType flexi2),	-- Matching substitution
		   [GenType flexi2])		-- Left over instance types

matchTy  ty1  ty2  = match      ty1  ty2  (\s  -> Just s)  emptyTyVarEnv
matchTys tys1 tys2 = match_list tys1 tys2 (\pr -> Just pr) emptyTyVarEnv
\end{code}

@match@ is the main function.

\begin{code}
match :: GenType flexi1 -> GenType flexi2		-- Current match pair
      -> (TyVarEnv (GenType flexi2) -> Maybe result)	-- Continuation
      -> TyVarEnv (GenType flexi2)			-- Current substitution
      -> Maybe result

-- When matching against a type variable, see if the variable
-- has already been bound.  If so, check that what it's bound to
-- is the same as ty; if not, bind it and carry on.

match (TyVarTy v) ty k = \s -> case lookupTyVarEnv s v of
				 Nothing  -> k (addToTyVarEnv s v ty)
				 Just ty' | ty' == ty -> k s	  -- Succeeds
					  | otherwise -> Nothing  -- Fails

match (FunTy arg1 res1)   (FunTy arg2 res2)  k = match arg1 arg2 (match res1 res2 k)
match (AppTy fun1 arg1)   (AppTy fun2 arg2)  k = match fun1 fun2 (match arg1 arg2 k)
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
match (SynTy _ ty1)       ty2		     k = match ty1 ty2 k
match ty1		  (SynTy _ ty2)      k = match ty1 ty2 k

-- Catch-all fails
match _ _ _ = \s -> Nothing

match_list []         tys2       k = \s -> k (s, tys2)
match_list (ty1:tys1) []         k = panic "match_list"
match_list (ty1:tys1) (ty2:tys2) k = match ty1 ty2 (match_list tys1 tys2 k)
\end{code}

%************************************************************************
%*									*
\subsection{Equality on types}
%*									*
%************************************************************************

For the moment at least, type comparisons don't work if 
there are embedded for-alls.

\begin{code}
instance Eq (GenType flexi) where
  ty1 == ty2 = case ty1 `cmpTy` ty2 of { EQ -> True; other -> False }

instance Ord (GenType flexi) where
  compare ty1 ty2 = cmpTy ty1 ty2

cmpTy :: GenType flexi -> GenType flexi -> Ordering
cmpTy ty1 ty2
  = cmp emptyTyVarEnv ty1 ty2
  where
  -- The "env" maps type variables in ty1 to type variables in ty2
  -- So when comparing for-alls.. (forall tv1 . t1) (forall tv2 . t2)
  -- we in effect substitute tv2 for tv1 in t1 before continuing
    lookup env tv1 = case lookupTyVarEnv env tv1 of
			  Just tv2 -> tv2
			  Nothing  -> tv1

    -- Get rid of SynTy
    cmp env (SynTy _ ty1) ty2 = cmp env ty1 ty2
    cmp env ty1 (SynTy _ ty2) = cmp env ty1 ty2
    
    -- Deal with equal constructors
    cmp env (TyVarTy tv1) (TyVarTy tv2) = lookup env tv1 `compare` tv2
    cmp env (AppTy f1 a1) (AppTy f2 a2) = cmp env f1 f2 `thenCmp` cmp env a1 a2
    cmp env (FunTy f1 a1) (FunTy f2 a2) = cmp env f1 f2 `thenCmp` cmp env a1 a2
    cmp env (TyConApp tc1 tys1) (TyConApp tc2 tys2) = (tc1 `compare` tc2) `thenCmp` (cmps env tys1 tys2)
    cmp env (ForAllTy tv1 t1)   (ForAllTy tv2 t2)   = cmp (addToTyVarEnv env tv1 tv2) t1 t2
    
    -- Deal with the rest: TyVarTy < AppTy < FunTy < TyConApp < ForAllTy
    cmp env (AppTy _ _) (TyVarTy _) = GT
    
    cmp env (FunTy _ _) (TyVarTy _) = GT
    cmp env (FunTy _ _) (AppTy _ _) = GT
    
    cmp env (TyConApp _ _) (TyVarTy _) = GT
    cmp env (TyConApp _ _) (AppTy _ _) = GT
    cmp env (TyConApp _ _) (FunTy _ _) = GT
    
    cmp env (ForAllTy _ _) other       = GT
    
    cmp env _ _		               = LT

    cmps env []     [] = EQ
    cmps env (t:ts) [] = GT
    cmps env [] (t:ts) = LT
    cmps env (t1:t1s) (t2:t2s) = cmp env t1 t2 `thenCmp` cmps env t1s t2s
\end{code}



%************************************************************************
%*									*
\subsection{Grime}
%*									*
%************************************************************************



\begin{code}
showTypeCategory :: Type -> Char
  {-
	{C,I,F,D}   char, int, float, double
	T	    tuple
	S	    other single-constructor type
	{c,i,f,d}   unboxed ditto
	t	    *unpacked* tuple
	s	    *unpacked" single-cons...

	v	    void#
	a	    primitive array

	E	    enumeration type
	+	    dictionary, unless it's a ...
	L	    List
	>	    function
	M	    other (multi-constructor) data-con type
	.	    other type
	-	    reserved for others to mark as "uninteresting"
    -}
showTypeCategory ty
  = if isDictTy ty
    then '+'
    else
      case splitTyConApp_maybe ty of
	Nothing -> if maybeToBool (splitFunTy_maybe ty)
		   then '>'
		   else '.'

	Just (tycon, _) ->
          let utc = uniqueOf tycon in
	  if	  utc == charDataConKey    then 'C'
	  else if utc == intDataConKey     then 'I'
	  else if utc == floatDataConKey   then 'F'
	  else if utc == doubleDataConKey  then 'D'
	  else if utc == integerDataConKey then 'J'
	  else if utc == charPrimTyConKey  then 'c'
	  else if (utc == intPrimTyConKey || utc == wordPrimTyConKey
		|| utc == addrPrimTyConKey)		   then 'i'
	  else if utc  == floatPrimTyConKey		   then 'f'
	  else if utc  == doublePrimTyConKey		   then 'd'
	  else if isPrimTyCon tycon {- array, we hope -}   then 'A'
	  else if isEnumerationTyCon tycon		   then 'E'
	  else if isTupleTyCon tycon			   then 'T'
	  else if maybeToBool (maybeTyConSingleCon tycon)  then 'S'
	  else if utc == listTyConKey			   then 'L'
	  else 'M' -- oh, well...
\end{code}
