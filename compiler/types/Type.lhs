%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[Type]{Type - public interface}


\begin{code}
module Type (
        -- re-exports from TypeRep
	TyThing(..), Type, PredType(..), ThetaType, 
	funTyCon,

	-- Kinds
        Kind, SimpleKind, KindVar,
        kindFunResult, splitKindFunTys, 

        liftedTypeKindTyCon, openTypeKindTyCon, unliftedTypeKindTyCon,
        argTypeKindTyCon, ubxTupleKindTyCon,

        liftedTypeKind, unliftedTypeKind, openTypeKind,
        argTypeKind, ubxTupleKind,

        tySuperKind, coSuperKind, 

        isLiftedTypeKind, isUnliftedTypeKind, isOpenTypeKind,
        isUbxTupleKind, isArgTypeKind, isKind, isTySuperKind, 
        isCoSuperKind, isSuperKind, isCoercionKind,
	mkArrowKind, mkArrowKinds,

        isSubArgTypeKind, isSubOpenTypeKind, isSubKind, defaultKind, eqKind,
        isSubKindCon,

	-- Re-exports from TyCon
	PrimRep(..),

	mkTyVarTy, mkTyVarTys, getTyVar, getTyVar_maybe, isTyVarTy,

	mkAppTy, mkAppTys, splitAppTy, splitAppTys, 
	splitAppTy_maybe, repSplitAppTy_maybe,

	mkFunTy, mkFunTys, splitFunTy, splitFunTy_maybe, 
	splitFunTys, splitFunTysN,
	funResultTy, funArgTy, zipFunTys, isFunTy,

	mkTyConApp, mkTyConTy, 
	tyConAppTyCon, tyConAppArgs, 
	splitTyConApp_maybe, splitTyConApp, 
        splitNewTyConApp_maybe, splitNewTyConApp,

	repType, typePrimRep, coreView, tcView, kindView,

	mkForAllTy, mkForAllTys, splitForAllTy_maybe, splitForAllTys, 
	applyTy, applyTys, isForAllTy, dropForAlls,

	-- Source types
	predTypeRep, mkPredTy, mkPredTys,

	-- Newtypes
	splitRecNewType_maybe, newTyConInstRhs,

	-- Lifting and boxity
	isUnLiftedType, isUnboxedTupleType, isAlgType, isPrimitiveType,
	isStrictType, isStrictPred, 

	-- Free variables
	tyVarsOfType, tyVarsOfTypes, tyVarsOfPred, tyVarsOfTheta,
	typeKind, addFreeTyVars,

	-- Tidying up for printing
	tidyType,      tidyTypes,
	tidyOpenType,  tidyOpenTypes,
	tidyTyVarBndr, tidyFreeTyVars,
	tidyOpenTyVar, tidyOpenTyVars,
	tidyTopType,   tidyPred,
	tidyKind,

	-- Comparison
	coreEqType, tcEqType, tcEqTypes, tcCmpType, tcCmpTypes, 
	tcEqPred, tcCmpPred, tcEqTypeX, 

	-- Seq
	seqType, seqTypes,

	-- Type substitutions
	TvSubstEnv, emptyTvSubstEnv,	-- Representation widely visible
	TvSubst(..), emptyTvSubst,	-- Representation visible to a few friends
	mkTvSubst, mkOpenTvSubst, zipOpenTvSubst, zipTopTvSubst, mkTopTvSubst, notElemTvSubst,
	getTvSubstEnv, setTvSubstEnv, getTvInScope, extendTvInScope,
 	extendTvSubst, extendTvSubstList, isInScope, composeTvSubst, zipTyEnv,

	-- Performing substitution on types
	substTy, substTys, substTyWith, substTheta, 
	substPred, substTyVar, substTyVarBndr, deShadowTy, lookupTyVar,

	-- Pretty-printing
	pprType, pprParendType, pprTyThingCategory,
	pprPred, pprTheta, pprThetaArrow, pprClassPred, pprKind, pprParendKind
    ) where

#include "HsVersions.h"

-- We import the representation and primitive functions from TypeRep.
-- Many things are reexported, but not the representation!

import TypeRep

-- friends:
import Var	( Var, TyVar, tyVarKind, tyVarName, 
		  setTyVarName, setTyVarKind )
import VarEnv
import VarSet

import OccName	( tidyOccName )
import Name	( NamedThing(..), tidyNameOcc )
import Class	( Class, classTyCon )
import PrelNames( openTypeKindTyConKey, unliftedTypeKindTyConKey, 
                  ubxTupleKindTyConKey, argTypeKindTyConKey )
import TyCon	( TyCon, isRecursiveTyCon, isPrimTyCon,
		  isUnboxedTupleTyCon, isUnLiftedTyCon,
		  isFunTyCon, isNewTyCon, newTyConRep, newTyConRhs,
		  isAlgTyCon, tyConArity, isSuperKindTyCon,
		  tcExpandTyCon_maybe, coreExpandTyCon_maybe,
	          tyConKind, PrimRep(..), tyConPrimRep, tyConUnique,
                  isCoercionTyCon_maybe, isCoercionTyCon
		)

-- others
import StaticFlags	( opt_DictsStrict )
import Util		( mapAccumL, seqList, lengthIs, snocView, thenCmp, isEqual, all2 )
import Outputable
import UniqSet		( sizeUniqSet )		-- Should come via VarSet
import Maybe		( isJust )
\end{code}


%************************************************************************
%*									*
		Type representation
%*									*
%************************************************************************

In Core, we "look through" non-recursive newtypes and PredTypes.

\begin{code}
{-# INLINE coreView #-}
coreView :: Type -> Maybe Type
-- Strips off the *top layer only* of a type to give 
-- its underlying representation type. 
-- Returns Nothing if there is nothing to look through.
--
-- In the case of newtypes, it returns
--	*either* a vanilla TyConApp (recursive newtype, or non-saturated)
--	*or*     the newtype representation (otherwise), meaning the
--			type written in the RHS of the newtype decl,
--			which may itself be a newtype
--
-- Example: newtype R = MkR S
--	    newtype S = MkS T
--	    newtype T = MkT (T -> T)
--   expandNewTcApp on R gives Just S
--	            on S gives Just T
--		    on T gives Nothing	 (no expansion)

-- By being non-recursive and inlined, this case analysis gets efficiently
-- joined onto the case analysis that the caller is already doing
coreView (NoteTy _ ty) 	   = Just ty
coreView (PredTy p)    	   = Just (predTypeRep p)
coreView (TyConApp tc tys) | Just (tenv, rhs, tys') <- coreExpandTyCon_maybe tc tys 
			   = Just (mkAppTys (substTy (mkTopTvSubst tenv) rhs) tys')
				-- Its important to use mkAppTys, rather than (foldl AppTy),
				-- because the function part might well return a 
				-- partially-applied type constructor; indeed, usually will!
coreView ty		   = Nothing



-----------------------------------------------
{-# INLINE tcView #-}
tcView :: Type -> Maybe Type
-- Same, but for the type checker, which just looks through synonyms
tcView (NoteTy _ ty) 	 = Just ty
tcView (TyConApp tc tys) | Just (tenv, rhs, tys') <- tcExpandTyCon_maybe tc tys 
			 = Just (mkAppTys (substTy (mkTopTvSubst tenv) rhs) tys')
tcView ty		 = Nothing

-----------------------------------------------
{-# INLINE kindView #-}
kindView :: Kind -> Maybe Kind
-- C.f. coreView, tcView
-- For the moment, we don't even handle synonyms in kinds
kindView (NoteTy _ k) = Just k
kindView other	      = Nothing
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
mkTyVarTy  :: TyVar   -> Type
mkTyVarTy  = TyVarTy

mkTyVarTys :: [TyVar] -> [Type]
mkTyVarTys = map mkTyVarTy -- a common use of mkTyVarTy

getTyVar :: String -> Type -> TyVar
getTyVar msg ty = case getTyVar_maybe ty of
		    Just tv -> tv
		    Nothing -> panic ("getTyVar: " ++ msg)

isTyVarTy :: Type -> Bool
isTyVarTy ty = isJust (getTyVar_maybe ty)

getTyVar_maybe :: Type -> Maybe TyVar
getTyVar_maybe ty | Just ty' <- coreView ty = getTyVar_maybe ty'
getTyVar_maybe (TyVarTy tv) 	 	    = Just tv  
getTyVar_maybe other	         	    = Nothing

\end{code}


---------------------------------------------------------------------
				AppTy
				~~~~~
We need to be pretty careful with AppTy to make sure we obey the 
invariant that a TyConApp is always visibly so.  mkAppTy maintains the
invariant: use it.

\begin{code}
mkAppTy orig_ty1 orig_ty2
  = mk_app orig_ty1
  where
    mk_app (NoteTy _ ty1)    = mk_app ty1
    mk_app (TyConApp tc tys) = mkTyConApp tc (tys ++ [orig_ty2])
    mk_app ty1		     = AppTy orig_ty1 orig_ty2
	-- Note that the TyConApp could be an 
	-- under-saturated type synonym.  GHC allows that; e.g.
	--	type Foo k = k a -> k a
	--	type Id x = x
	--	foo :: Foo Id -> Foo Id
	--
	-- Here Id is partially applied in the type sig for Foo,
	-- but once the type synonyms are expanded all is well

mkAppTys :: Type -> [Type] -> Type
mkAppTys orig_ty1 []	    = orig_ty1
	-- This check for an empty list of type arguments
	-- avoids the needless loss of a type synonym constructor.
	-- For example: mkAppTys Rational []
	--   returns to (Ratio Integer), which has needlessly lost
	--   the Rational part.
mkAppTys orig_ty1 orig_tys2
  = mk_app orig_ty1
  where
    mk_app (NoteTy _ ty1)    = mk_app ty1
    mk_app (TyConApp tc tys) = mkTyConApp tc (tys ++ orig_tys2)
				-- mkTyConApp: see notes with mkAppTy
    mk_app ty1		     = foldl AppTy orig_ty1 orig_tys2

-------------
splitAppTy_maybe :: Type -> Maybe (Type, Type)
splitAppTy_maybe ty | Just ty' <- coreView ty
		    = splitAppTy_maybe ty'
splitAppTy_maybe ty = repSplitAppTy_maybe ty

-------------
repSplitAppTy_maybe :: Type -> Maybe (Type,Type)
-- Does the AppTy split, but assumes that any view stuff is already done
repSplitAppTy_maybe (FunTy ty1 ty2)   = Just (TyConApp funTyCon [ty1], ty2)
repSplitAppTy_maybe (AppTy ty1 ty2)   = Just (ty1, ty2)
repSplitAppTy_maybe (TyConApp tc tys) = case snocView tys of
						Just (tys', ty') -> Just (TyConApp tc tys', ty')
						Nothing		 -> Nothing
repSplitAppTy_maybe other = Nothing
-------------
splitAppTy :: Type -> (Type, Type)
splitAppTy ty = case splitAppTy_maybe ty of
			Just pr -> pr
			Nothing -> panic "splitAppTy"

-------------
splitAppTys :: Type -> (Type, [Type])
splitAppTys ty = split ty ty []
  where
    split orig_ty ty args | Just ty' <- coreView ty = split orig_ty ty' args
    split orig_ty (AppTy ty arg)        args = split ty ty (arg:args)
    split orig_ty (TyConApp tc tc_args) args = (TyConApp tc [], tc_args ++ args)
    split orig_ty (FunTy ty1 ty2)       args = ASSERT( null args )
					       (TyConApp funTyCon [], [ty1,ty2])
    split orig_ty ty		        args = (orig_ty, args)

\end{code}


---------------------------------------------------------------------
				FunTy
				~~~~~

\begin{code}
mkFunTy :: Type -> Type -> Type
mkFunTy arg res = FunTy arg res

mkFunTys :: [Type] -> Type -> Type
mkFunTys tys ty = foldr FunTy ty tys

isFunTy :: Type -> Bool 
isFunTy ty = isJust (splitFunTy_maybe ty)

splitFunTy :: Type -> (Type, Type)
splitFunTy ty | Just ty' <- coreView ty = splitFunTy ty'
splitFunTy (FunTy arg res)   = (arg, res)
splitFunTy other	     = pprPanic "splitFunTy" (ppr other)

splitFunTy_maybe :: Type -> Maybe (Type, Type)
splitFunTy_maybe ty | Just ty' <- coreView ty = splitFunTy_maybe ty'
splitFunTy_maybe (FunTy arg res)   = Just (arg, res)
splitFunTy_maybe other	           = Nothing

splitFunTys :: Type -> ([Type], Type)
splitFunTys ty = split [] ty ty
  where
    split args orig_ty ty | Just ty' <- coreView ty = split args orig_ty ty'
    split args orig_ty (FunTy arg res) 	 = split (arg:args) res res
    split args orig_ty ty                = (reverse args, orig_ty)

splitFunTysN :: Int -> Type -> ([Type], Type)
-- Split off exactly n arg tys
splitFunTysN 0 ty = ([], ty)
splitFunTysN n ty = case splitFunTy ty of { (arg, res) ->
		    case splitFunTysN (n-1) res of { (args, res) ->
		    (arg:args, res) }}

zipFunTys :: Outputable a => [a] -> Type -> ([(a,Type)], Type)
zipFunTys orig_xs orig_ty = split [] orig_xs orig_ty orig_ty
  where
    split acc []     nty ty  	           = (reverse acc, nty)
    split acc xs     nty ty 
	  | Just ty' <- coreView ty 	   = split acc xs nty ty'
    split acc (x:xs) nty (FunTy arg res)   = split ((x,arg):acc) xs res res
    split acc (x:xs) nty ty                = pprPanic "zipFunTys" (ppr orig_xs <+> ppr orig_ty)
    
funResultTy :: Type -> Type
funResultTy ty | Just ty' <- coreView ty = funResultTy ty'
funResultTy (FunTy arg res)   = res
funResultTy ty		      = pprPanic "funResultTy" (ppr ty)

funArgTy :: Type -> Type
funArgTy ty | Just ty' <- coreView ty = funArgTy ty'
funArgTy (FunTy arg res)   = arg
funArgTy ty		   = pprPanic "funArgTy" (ppr ty)
\end{code}


---------------------------------------------------------------------
				TyConApp
				~~~~~~~~
@mkTyConApp@ is a key function, because it builds a TyConApp, FunTy or PredTy,
as apppropriate.

\begin{code}
mkTyConApp :: TyCon -> [Type] -> Type
mkTyConApp tycon tys
  | isFunTyCon tycon, [ty1,ty2] <- tys
  = FunTy ty1 ty2

  | otherwise
  = TyConApp tycon tys

mkTyConTy :: TyCon -> Type
mkTyConTy tycon = mkTyConApp tycon []

-- splitTyConApp "looks through" synonyms, because they don't
-- mean a distinct type, but all other type-constructor applications
-- including functions are returned as Just ..

tyConAppTyCon :: Type -> TyCon
tyConAppTyCon ty = fst (splitTyConApp ty)

tyConAppArgs :: Type -> [Type]
tyConAppArgs ty = snd (splitTyConApp ty)

splitTyConApp :: Type -> (TyCon, [Type])
splitTyConApp ty = case splitTyConApp_maybe ty of
			Just stuff -> stuff
			Nothing	   -> pprPanic "splitTyConApp" (ppr ty)

splitTyConApp_maybe :: Type -> Maybe (TyCon, [Type])
splitTyConApp_maybe ty | Just ty' <- coreView ty = splitTyConApp_maybe ty'
splitTyConApp_maybe (TyConApp tc tys) = Just (tc, tys)
splitTyConApp_maybe (FunTy arg res)   = Just (funTyCon, [arg,res])
splitTyConApp_maybe other	      = Nothing

-- Sometimes we do NOT want to look throught a newtype.  When case matching
-- on a newtype we want a convenient way to access the arguments of a newty
-- constructor so as to properly form a coercion.
splitNewTyConApp :: Type -> (TyCon, [Type])
splitNewTyConApp ty = case splitNewTyConApp_maybe ty of
			Just stuff -> stuff
			Nothing	   -> pprPanic "splitNewTyConApp" (ppr ty)
splitNewTyConApp_maybe :: Type -> Maybe (TyCon, [Type])
splitNewTyConApp_maybe ty | Just ty' <- tcView ty = splitNewTyConApp_maybe ty'
splitNewTyConApp_maybe (TyConApp tc tys) = Just (tc, tys)
splitNewTyConApp_maybe (FunTy arg res)   = Just (funTyCon, [arg,res])
splitNewTyConApp_maybe other	      = Nothing

-- get instantiated newtype rhs, the arguments had better saturate 
-- the constructor
newTyConInstRhs :: TyCon -> [Type] -> Type
newTyConInstRhs tycon tys =
    let (tvs, ty) = newTyConRhs tycon in substTyWith tvs tys ty

\end{code}


---------------------------------------------------------------------
				SynTy
				~~~~~

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


		Representation types
		~~~~~~~~~~~~~~~~~~~~
repType looks through 
	(a) for-alls, and
	(b) synonyms
	(c) predicates
	(d) usage annotations
	(e) all newtypes, including recursive ones
It's useful in the back end.

\begin{code}
repType :: Type -> Type
-- Only applied to types of kind *; hence tycons are saturated
repType ty | Just ty' <- coreView ty = repType ty'
repType (ForAllTy _ ty)  = repType ty
repType (TyConApp tc tys)
  | isNewTyCon tc 	 = -- Recursive newtypes are opaque to coreView
			   -- but we must expand them here.  Sure to
			   -- be saturated because repType is only applied
			   -- to types of kind *
			   ASSERT( {- isRecursiveTyCon tc && -} tys `lengthIs` tyConArity tc )
			   repType (new_type_rep tc tys)
repType ty = ty

-- new_type_rep doesn't ask any questions: 
-- it just expands newtype, whether recursive or not
new_type_rep new_tycon tys = ASSERT( tys `lengthIs` tyConArity new_tycon )
			     case newTyConRep new_tycon of
				 (tvs, rep_ty) -> substTyWith tvs tys rep_ty

-- ToDo: this could be moved to the code generator, using splitTyConApp instead
-- of inspecting the type directly.
typePrimRep :: Type -> PrimRep
typePrimRep ty = case repType ty of
		   TyConApp tc _ -> tyConPrimRep tc
		   FunTy _ _	 -> PtrRep
		   AppTy _ _	 -> PtrRep	-- See note below
		   TyVarTy _	 -> PtrRep
		   other	 -> pprPanic "typePrimRep" (ppr ty)
	-- Types of the form 'f a' must be of kind *, not *#, so
	-- we are guaranteed that they are represented by pointers.
	-- The reason is that f must have kind *->*, not *->*#, because
	-- (we claim) there is no way to constrain f's kind any other
	-- way.

\end{code}


---------------------------------------------------------------------
				ForAllTy
				~~~~~~~~

\begin{code}
mkForAllTy :: TyVar -> Type -> Type
mkForAllTy tyvar ty
  = mkForAllTys [tyvar] ty

mkForAllTys :: [TyVar] -> Type -> Type
mkForAllTys tyvars ty = foldr ForAllTy ty tyvars

isForAllTy :: Type -> Bool
isForAllTy (NoteTy _ ty)  = isForAllTy ty
isForAllTy (ForAllTy _ _) = True
isForAllTy other_ty	  = False

splitForAllTy_maybe :: Type -> Maybe (TyVar, Type)
splitForAllTy_maybe ty = splitFAT_m ty
  where
    splitFAT_m ty | Just ty' <- coreView ty = splitFAT_m ty'
    splitFAT_m (ForAllTy tyvar ty)	    = Just(tyvar, ty)
    splitFAT_m _			    = Nothing

splitForAllTys :: Type -> ([TyVar], Type)
splitForAllTys ty = split ty ty []
   where
     split orig_ty ty tvs | Just ty' <- coreView ty = split orig_ty ty' tvs
     split orig_ty (ForAllTy tv ty)  tvs = split ty ty (tv:tvs)
     split orig_ty t		     tvs = (reverse tvs, orig_ty)

dropForAlls :: Type -> Type
dropForAlls ty = snd (splitForAllTys ty)
\end{code}

-- (mkPiType now in CoreUtils)

applyTy, applyTys
~~~~~~~~~~~~~~~~~
Instantiate a for-all type with one or more type arguments.
Used when we have a polymorphic function applied to type args:
	f t1 t2
Then we use (applyTys type-of-f [t1,t2]) to compute the type of
the expression. 

\begin{code}
applyTy :: Type -> Type -> Type
applyTy ty arg | Just ty' <- coreView ty = applyTy ty' arg
applyTy (ForAllTy tv ty) arg = substTyWith [tv] [arg] ty
applyTy other		 arg = panic "applyTy"

applyTys :: Type -> [Type] -> Type
-- This function is interesting because 
--	a) the function may have more for-alls than there are args
--	b) less obviously, it may have fewer for-alls
-- For case (b) think of 
--	applyTys (forall a.a) [forall b.b, Int]
-- This really can happen, via dressing up polymorphic types with newtype
-- clothing.  Here's an example:
--	newtype R = R (forall a. a->a)
--	foo = case undefined :: R of
--		R f -> f ()

applyTys orig_fun_ty []      = orig_fun_ty
applyTys orig_fun_ty arg_tys 
  | n_tvs == n_args 	-- The vastly common case
  = substTyWith tvs arg_tys rho_ty
  | n_tvs > n_args 	-- Too many for-alls
  = substTyWith (take n_args tvs) arg_tys 
		(mkForAllTys (drop n_args tvs) rho_ty)
  | otherwise		-- Too many type args
  = ASSERT2( n_tvs > 0, ppr orig_fun_ty )	-- Zero case gives infnite loop!
    applyTys (substTyWith tvs (take n_tvs arg_tys) rho_ty)
	     (drop n_tvs arg_tys)
  where
    (tvs, rho_ty) = splitForAllTys orig_fun_ty 
    n_tvs = length tvs
    n_args = length arg_tys     
\end{code}


%************************************************************************
%*									*
\subsection{Source types}
%*									*
%************************************************************************

A "source type" is a type that is a separate type as far as the type checker is
concerned, but which has low-level representation as far as the back end is concerned.

Source types are always lifted.

The key function is predTypeRep which gives the representation of a source type:

\begin{code}
mkPredTy :: PredType -> Type
mkPredTy pred = PredTy pred

mkPredTys :: ThetaType -> [Type]
mkPredTys preds = map PredTy preds

predTypeRep :: PredType -> Type
-- Convert a PredType to its "representation type";
-- the post-type-checking type used by all the Core passes of GHC.
-- Unwraps only the outermost level; for example, the result might
-- be a newtype application
predTypeRep (IParam _ ty)     = ty
predTypeRep (ClassP clas tys) = mkTyConApp (classTyCon clas) tys
	-- Result might be a newtype application, but the consumer will
	-- look through that too if necessary
\end{code}


%************************************************************************
%*									*
		NewTypes
%*									*
%************************************************************************

\begin{code}
splitRecNewType_maybe :: Type -> Maybe Type
-- Sometimes we want to look through a recursive newtype, and that's what happens here
-- It only strips *one layer* off, so the caller will usually call itself recursively
-- Only applied to types of kind *, hence the newtype is always saturated
splitRecNewType_maybe ty | Just ty' <- coreView ty = splitRecNewType_maybe ty'
splitRecNewType_maybe (TyConApp tc tys)
  | isNewTyCon tc
  = ASSERT( tys `lengthIs` tyConArity tc )	-- splitRecNewType_maybe only be applied 
						-- 	to *types* (of kind *)
    ASSERT( isRecursiveTyCon tc ) 		-- Guaranteed by coreView
    case newTyConRhs tc of
	(tvs, rep_ty) -> ASSERT( length tvs == length tys )
			 Just (substTyWith tvs tys rep_ty)
	
splitRecNewType_maybe other = Nothing



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
typeKind :: Type -> Kind
typeKind (TyConApp tycon tys) = ASSERT( not (isCoercionTyCon tycon) )
				   -- We should be looking for the coercion kind,
				   -- not the type kind
				foldr (\_ k -> kindFunResult k) (tyConKind tycon) tys
typeKind (NoteTy _ ty)	      = typeKind ty
typeKind (PredTy pred)	      = predKind pred
typeKind (AppTy fun arg)      = kindFunResult (typeKind fun)
typeKind (ForAllTy tv ty)     = typeKind ty
typeKind (TyVarTy tyvar)      = tyVarKind tyvar
typeKind (FunTy arg res)
    -- Hack alert.  The kind of (Int -> Int#) is liftedTypeKind (*), 
    --              not unliftedTypKind (#)
    -- The only things that can be after a function arrow are
    --   (a) types (of kind openTypeKind or its sub-kinds)
    --   (b) kinds (of super-kind TY) (e.g. * -> (* -> *))
    | isTySuperKind k         = k
    | otherwise               = ASSERT( isSubOpenTypeKind k) liftedTypeKind 
    where
      k = typeKind res

predKind :: PredType -> Kind
predKind (EqPred {}) = coSuperKind	-- A coercion kind!
predKind (ClassP {}) = liftedTypeKind	-- Class and implicitPredicates are
predKind (IParam {}) = liftedTypeKind 	-- always represented by lifted types
\end{code}


---------------------------------------------------------------------
		Free variables of a type
		~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
tyVarsOfType :: Type -> TyVarSet
-- NB: for type synonyms tyVarsOfType does *not* expand the synonym
tyVarsOfType (TyVarTy tv)		= unitVarSet tv
tyVarsOfType (TyConApp tycon tys)	= tyVarsOfTypes tys
tyVarsOfType (NoteTy (FTVNote tvs) ty2) = tvs
tyVarsOfType (PredTy sty)		= tyVarsOfPred sty
tyVarsOfType (FunTy arg res)		= tyVarsOfType arg `unionVarSet` tyVarsOfType res
tyVarsOfType (AppTy fun arg)		= tyVarsOfType fun `unionVarSet` tyVarsOfType arg
tyVarsOfType (ForAllTy tyvar ty)	= delVarSet (tyVarsOfType ty) tyvar

tyVarsOfTypes :: [Type] -> TyVarSet
tyVarsOfTypes tys = foldr (unionVarSet.tyVarsOfType) emptyVarSet tys

tyVarsOfPred :: PredType -> TyVarSet
tyVarsOfPred (IParam _ ty)    = tyVarsOfType ty
tyVarsOfPred (ClassP _ tys)   = tyVarsOfTypes tys
tyVarsOfPred (EqPred ty1 ty2) = tyVarsOfType ty1 `unionVarSet` tyVarsOfType ty2

tyVarsOfTheta :: ThetaType -> TyVarSet
tyVarsOfTheta = foldr (unionVarSet . tyVarsOfPred) emptyVarSet

-- Add a Note with the free tyvars to the top of the type
addFreeTyVars :: Type -> Type
addFreeTyVars ty@(NoteTy (FTVNote _) _)      = ty
addFreeTyVars ty			     = NoteTy (FTVNote (tyVarsOfType ty)) ty
\end{code}


%************************************************************************
%*									*
\subsection{TidyType}
%*									*
%************************************************************************

tidyTy tidies up a type for printing in an error message, or in
an interface file.

It doesn't change the uniques at all, just the print names.

\begin{code}
tidyTyVarBndr :: TidyEnv -> TyVar -> (TidyEnv, TyVar)
tidyTyVarBndr (tidy_env, subst) tyvar
  = case tidyOccName tidy_env (getOccName name) of
      (tidy', occ') -> 	((tidy', subst'), tyvar')
		    where
			subst' = extendVarEnv subst tyvar tyvar'
			tyvar' = setTyVarName tyvar name'
			name'  = tidyNameOcc name occ'
  where
    name = tyVarName tyvar

tidyFreeTyVars :: TidyEnv -> TyVarSet -> TidyEnv
-- Add the free tyvars to the env in tidy form,
-- so that we can tidy the type they are free in
tidyFreeTyVars env tyvars = fst (tidyOpenTyVars env (varSetElems tyvars))

tidyOpenTyVars :: TidyEnv -> [TyVar] -> (TidyEnv, [TyVar])
tidyOpenTyVars env tyvars = mapAccumL tidyOpenTyVar env tyvars

tidyOpenTyVar :: TidyEnv -> TyVar -> (TidyEnv, TyVar)
-- Treat a new tyvar as a binder, and give it a fresh tidy name
tidyOpenTyVar env@(tidy_env, subst) tyvar
  = case lookupVarEnv subst tyvar of
	Just tyvar' -> (env, tyvar')		-- Already substituted
	Nothing	    -> tidyTyVarBndr env tyvar	-- Treat it as a binder

tidyType :: TidyEnv -> Type -> Type
tidyType env@(tidy_env, subst) ty
  = go ty
  where
    go (TyVarTy tv)	    = case lookupVarEnv subst tv of
				Nothing  -> TyVarTy tv
				Just tv' -> TyVarTy tv'
    go (TyConApp tycon tys) = let args = map go tys
			      in args `seqList` TyConApp tycon args
    go (NoteTy note ty)     = (NoteTy $! (go_note note)) $! (go ty)
    go (PredTy sty)	    = PredTy (tidyPred env sty)
    go (AppTy fun arg)	    = (AppTy $! (go fun)) $! (go arg)
    go (FunTy fun arg)	    = (FunTy $! (go fun)) $! (go arg)
    go (ForAllTy tv ty)	    = ForAllTy tvp $! (tidyType envp ty)
			      where
			        (envp, tvp) = tidyTyVarBndr env tv

    go_note note@(FTVNote ftvs) = note	-- No need to tidy the free tyvars

tidyTypes env tys = map (tidyType env) tys

tidyPred :: TidyEnv -> PredType -> PredType
tidyPred env (IParam n ty)     = IParam n (tidyType env ty)
tidyPred env (ClassP clas tys) = ClassP clas (tidyTypes env tys)
tidyPred env (EqPred ty1 ty2)  = EqPred (tidyType env ty1) (tidyType env ty2)
\end{code}


@tidyOpenType@ grabs the free type variables, tidies them
and then uses @tidyType@ to work over the type itself

\begin{code}
tidyOpenType :: TidyEnv -> Type -> (TidyEnv, Type)
tidyOpenType env ty
  = (env', tidyType env' ty)
  where
    env' = tidyFreeTyVars env (tyVarsOfType ty)

tidyOpenTypes :: TidyEnv -> [Type] -> (TidyEnv, [Type])
tidyOpenTypes env tys = mapAccumL tidyOpenType env tys

tidyTopType :: Type -> Type
tidyTopType ty = tidyType emptyTidyEnv ty
\end{code}

\begin{code}

tidyKind :: TidyEnv -> Kind -> (TidyEnv, Kind)
tidyKind env k = tidyOpenType env k

\end{code}


%************************************************************************
%*									*
\subsection{Liftedness}
%*									*
%************************************************************************

\begin{code}
isUnLiftedType :: Type -> Bool
	-- isUnLiftedType returns True for forall'd unlifted types:
	--	x :: forall a. Int#
	-- I found bindings like these were getting floated to the top level.
	-- They are pretty bogus types, mind you.  It would be better never to
	-- construct them

isUnLiftedType ty | Just ty' <- coreView ty = isUnLiftedType ty'
isUnLiftedType (ForAllTy tv ty)  = isUnLiftedType ty
isUnLiftedType (TyConApp tc _)   = isUnLiftedTyCon tc
isUnLiftedType other		 = False	

isUnboxedTupleType :: Type -> Bool
isUnboxedTupleType ty = case splitTyConApp_maybe ty of
			   Just (tc, ty_args) -> isUnboxedTupleTyCon tc
			   other	      -> False

-- Should only be applied to *types*; hence the assert
isAlgType :: Type -> Bool
isAlgType ty = case splitTyConApp_maybe ty of
			Just (tc, ty_args) -> ASSERT( ty_args `lengthIs` tyConArity tc )
					      isAlgTyCon tc
			other		   -> False
\end{code}

@isStrictType@ computes whether an argument (or let RHS) should
be computed strictly or lazily, based only on its type.
Works just like isUnLiftedType, except that it has a special case 
for dictionaries.  Since it takes account of ClassP, you might think
this function should be in TcType, but isStrictType is used by DataCon,
which is below TcType in the hierarchy, so it's convenient to put it here.

\begin{code}
isStrictType (PredTy pred)     = isStrictPred pred
isStrictType ty | Just ty' <- coreView ty = isStrictType ty'
isStrictType (ForAllTy tv ty)  = isStrictType ty
isStrictType (TyConApp tc _)   = isUnLiftedTyCon tc
isStrictType other	       = False	

isStrictPred (ClassP clas _) = opt_DictsStrict && not (isNewTyCon (classTyCon clas))
isStrictPred other	     = False
	-- We may be strict in dictionary types, but only if it 
	-- has more than one component.
	-- [Being strict in a single-component dictionary risks
	--  poking the dictionary component, which is wrong.]
\end{code}

\begin{code}
isPrimitiveType :: Type -> Bool
-- Returns types that are opaque to Haskell.
-- Most of these are unlifted, but now that we interact with .NET, we
-- may have primtive (foreign-imported) types that are lifted
isPrimitiveType ty = case splitTyConApp_maybe ty of
			Just (tc, ty_args) -> ASSERT( ty_args `lengthIs` tyConArity tc )
					      isPrimTyCon tc
			other		   -> False
\end{code}


%************************************************************************
%*									*
\subsection{Sequencing on types
%*									*
%************************************************************************

\begin{code}
seqType :: Type -> ()
seqType (TyVarTy tv) 	  = tv `seq` ()
seqType (AppTy t1 t2) 	  = seqType t1 `seq` seqType t2
seqType (FunTy t1 t2) 	  = seqType t1 `seq` seqType t2
seqType (NoteTy note t2)  = seqNote note `seq` seqType t2
seqType (PredTy p) 	  = seqPred p
seqType (TyConApp tc tys) = tc `seq` seqTypes tys
seqType (ForAllTy tv ty)  = tv `seq` seqType ty

seqTypes :: [Type] -> ()
seqTypes []       = ()
seqTypes (ty:tys) = seqType ty `seq` seqTypes tys

seqNote :: TyNote -> ()
seqNote (FTVNote set) = sizeUniqSet set `seq` ()

seqPred :: PredType -> ()
seqPred (ClassP c tys)   = c `seq` seqTypes tys
seqPred (IParam n ty)    = n `seq` seqType ty
seqPred (EqPred ty1 ty2) = seqType ty1 `seq` seqType ty2
\end{code}


%************************************************************************
%*									*
		Equality for Core types 
	(We don't use instances so that we know where it happens)
%*									*
%************************************************************************

Note that eqType works right even for partial applications of newtypes.
See Note [Newtype eta] in TyCon.lhs

\begin{code}
coreEqType :: Type -> Type -> Bool
coreEqType t1 t2
  = eq rn_env t1 t2
  where
    rn_env = mkRnEnv2 (mkInScopeSet (tyVarsOfType t1 `unionVarSet` tyVarsOfType t2))

    eq env (TyVarTy tv1)       (TyVarTy tv2)     = rnOccL env tv1 == rnOccR env tv2
    eq env (ForAllTy tv1 t1)   (ForAllTy tv2 t2) = eq (rnBndr2 env tv1 tv2) t1 t2
    eq env (AppTy s1 t1)       (AppTy s2 t2)     = eq env s1 s2 && eq env t1 t2
    eq env (FunTy s1 t1)       (FunTy s2 t2)     = eq env s1 s2 && eq env t1 t2
    eq env (TyConApp tc1 tys1) (TyConApp tc2 tys2) 
	| tc1 == tc2, all2 (eq env) tys1 tys2 = True
			-- The lengths should be equal because
			-- the two types have the same kind
	-- NB: if the type constructors differ that does not 
	--     necessarily mean that the types aren't equal
	--     (synonyms, newtypes)
	-- Even if the type constructors are the same, but the arguments
	-- differ, the two types could be the same (e.g. if the arg is just
	-- ignored in the RHS).  In both these cases we fall through to an 
	-- attempt to expand one side or the other.

	-- Now deal with newtypes, synonyms, pred-tys
    eq env t1 t2 | Just t1' <- coreView t1 = eq env t1' t2 
		 | Just t2' <- coreView t2 = eq env t1 t2' 

	-- Fall through case; not equal!
    eq env t1 t2 = False
\end{code}


%************************************************************************
%*									*
		Comparision for source types 
	(We don't use instances so that we know where it happens)
%*									*
%************************************************************************

Note that 
	tcEqType, tcCmpType 
do *not* look through newtypes, PredTypes

\begin{code}
tcEqType :: Type -> Type -> Bool
tcEqType t1 t2 = isEqual $ cmpType t1 t2

tcEqTypes :: [Type] -> [Type] -> Bool
tcEqTypes tys1 tys2 = isEqual $ cmpTypes tys1 tys2

tcCmpType :: Type -> Type -> Ordering
tcCmpType t1 t2 = cmpType t1 t2

tcCmpTypes :: [Type] -> [Type] -> Ordering
tcCmpTypes tys1 tys2 = cmpTypes tys1 tys2

tcEqPred :: PredType -> PredType -> Bool
tcEqPred p1 p2 = isEqual $ cmpPred p1 p2

tcCmpPred :: PredType -> PredType -> Ordering
tcCmpPred p1 p2 = cmpPred p1 p2

tcEqTypeX :: RnEnv2 -> Type -> Type -> Bool
tcEqTypeX env t1 t2 = isEqual $ cmpTypeX env t1 t2
\end{code}

Now here comes the real worker

\begin{code}
cmpType :: Type -> Type -> Ordering
cmpType t1 t2 = cmpTypeX rn_env t1 t2
  where
    rn_env = mkRnEnv2 (mkInScopeSet (tyVarsOfType t1 `unionVarSet` tyVarsOfType t2))

cmpTypes :: [Type] -> [Type] -> Ordering
cmpTypes ts1 ts2 = cmpTypesX rn_env ts1 ts2
  where
    rn_env = mkRnEnv2 (mkInScopeSet (tyVarsOfTypes ts1 `unionVarSet` tyVarsOfTypes ts2))

cmpPred :: PredType -> PredType -> Ordering
cmpPred p1 p2 = cmpPredX rn_env p1 p2
  where
    rn_env = mkRnEnv2 (mkInScopeSet (tyVarsOfPred p1 `unionVarSet` tyVarsOfPred p2))

cmpTypeX :: RnEnv2 -> Type -> Type -> Ordering	-- Main workhorse
cmpTypeX env t1 t2 | Just t1' <- tcView t1 = cmpTypeX env t1' t2
		   | Just t2' <- tcView t2 = cmpTypeX env t1 t2'

cmpTypeX env (TyVarTy tv1)       (TyVarTy tv2)       = rnOccL env tv1 `compare` rnOccR env tv2
cmpTypeX env (ForAllTy tv1 t1)   (ForAllTy tv2 t2)   = cmpTypeX (rnBndr2 env tv1 tv2) t1 t2
cmpTypeX env (AppTy s1 t1)       (AppTy s2 t2)       = cmpTypeX env s1 s2 `thenCmp` cmpTypeX env t1 t2
cmpTypeX env (FunTy s1 t1)       (FunTy s2 t2)       = cmpTypeX env s1 s2 `thenCmp` cmpTypeX env t1 t2
cmpTypeX env (PredTy p1)         (PredTy p2)         = cmpPredX env p1 p2
cmpTypeX env (TyConApp tc1 tys1) (TyConApp tc2 tys2) = (tc1 `compare` tc2) `thenCmp` cmpTypesX env tys1 tys2
cmpTypeX env t1			(NoteTy _ t2)	     = cmpTypeX env t1 t2

    -- Deal with the rest: TyVarTy < AppTy < FunTy < TyConApp < ForAllTy < PredTy
cmpTypeX env (AppTy _ _) (TyVarTy _) = GT
    
cmpTypeX env (FunTy _ _) (TyVarTy _) = GT
cmpTypeX env (FunTy _ _) (AppTy _ _) = GT
    
cmpTypeX env (TyConApp _ _) (TyVarTy _) = GT
cmpTypeX env (TyConApp _ _) (AppTy _ _) = GT
cmpTypeX env (TyConApp _ _) (FunTy _ _) = GT
    
cmpTypeX env (ForAllTy _ _) (TyVarTy _)    = GT
cmpTypeX env (ForAllTy _ _) (AppTy _ _)    = GT
cmpTypeX env (ForAllTy _ _) (FunTy _ _)    = GT
cmpTypeX env (ForAllTy _ _) (TyConApp _ _) = GT

cmpTypeX env (PredTy _)   t2		= GT

cmpTypeX env _ _ = LT

-------------
cmpTypesX :: RnEnv2 -> [Type] -> [Type] -> Ordering
cmpTypesX env []        []        = EQ
cmpTypesX env (t1:tys1) (t2:tys2) = cmpTypeX env t1 t2 `thenCmp` cmpTypesX env tys1 tys2
cmpTypesX env []        tys       = LT
cmpTypesX env ty        []        = GT

-------------
cmpPredX :: RnEnv2 -> PredType -> PredType -> Ordering
cmpPredX env (IParam n1 ty1) (IParam n2 ty2) = (n1 `compare` n2) `thenCmp` cmpTypeX env ty1 ty2
	-- Compare types as well as names for implicit parameters
	-- This comparison is used exclusively (I think) for the
	-- finite map built in TcSimplify
cmpPredX env (ClassP c1 tys1) (ClassP c2 tys2) = (c1 `compare` c2) `thenCmp` cmpTypesX env tys1 tys2
cmpPredX env (IParam _ _)     (ClassP _ _)     = LT
cmpPredX env (ClassP _ _)     (IParam _ _)     = GT
\end{code}

PredTypes are used as a FM key in TcSimplify, 
so we take the easy path and make them an instance of Ord

\begin{code}
instance Eq  PredType where { (==)    = tcEqPred }
instance Ord PredType where { compare = tcCmpPred }
\end{code}


%************************************************************************
%*									*
		Type substitutions
%*									*
%************************************************************************

\begin{code}
data TvSubst 		
  = TvSubst InScopeSet 	-- The in-scope type variables
	    TvSubstEnv	-- The substitution itself
			-- See Note [Apply Once]

{- ----------------------------------------------------------
	 	Note [Apply Once]

We use TvSubsts to instantiate things, and we might instantiate
	forall a b. ty
\with the types
	[a, b], or [b, a].
So the substition might go [a->b, b->a].  A similar situation arises in Core
when we find a beta redex like
	(/\ a /\ b -> e) b a
Then we also end up with a substition that permutes type variables. Other
variations happen to; for example [a -> (a, b)].  

	***************************************************
	*** So a TvSubst must be applied precisely once ***
	***************************************************

A TvSubst is not idempotent, but, unlike the non-idempotent substitution
we use during unifications, it must not be repeatedly applied.
-------------------------------------------------------------- -}


type TvSubstEnv = TyVarEnv Type
	-- A TvSubstEnv is used both inside a TvSubst (with the apply-once
	-- invariant discussed in Note [Apply Once]), and also independently
	-- in the middle of matching, and unification (see Types.Unify)
	-- So you have to look at the context to know if it's idempotent or
	-- apply-once or whatever
emptyTvSubstEnv :: TvSubstEnv
emptyTvSubstEnv = emptyVarEnv

composeTvSubst :: InScopeSet -> TvSubstEnv -> TvSubstEnv -> TvSubstEnv
-- (compose env1 env2)(x) is env1(env2(x)); i.e. apply env2 then env1
-- It assumes that both are idempotent
-- Typically, env1 is the refinement to a base substitution env2
composeTvSubst in_scope env1 env2
  = env1 `plusVarEnv` mapVarEnv (substTy subst1) env2
	-- First apply env1 to the range of env2
	-- Then combine the two, making sure that env1 loses if
	-- both bind the same variable; that's why env1 is the
	--  *left* argument to plusVarEnv, because the right arg wins
  where
    subst1 = TvSubst in_scope env1

emptyTvSubst = TvSubst emptyInScopeSet emptyVarEnv

isEmptyTvSubst :: TvSubst -> Bool
isEmptyTvSubst (TvSubst _ env) = isEmptyVarEnv env

mkTvSubst :: InScopeSet -> TvSubstEnv -> TvSubst
mkTvSubst = TvSubst

getTvSubstEnv :: TvSubst -> TvSubstEnv
getTvSubstEnv (TvSubst _ env) = env

getTvInScope :: TvSubst -> InScopeSet
getTvInScope (TvSubst in_scope _) = in_scope

isInScope :: Var -> TvSubst -> Bool
isInScope v (TvSubst in_scope _) = v `elemInScopeSet` in_scope

notElemTvSubst :: TyVar -> TvSubst -> Bool
notElemTvSubst tv (TvSubst _ env) = not (tv `elemVarEnv` env)

setTvSubstEnv :: TvSubst -> TvSubstEnv -> TvSubst
setTvSubstEnv (TvSubst in_scope _) env = TvSubst in_scope env

extendTvInScope :: TvSubst -> [Var] -> TvSubst
extendTvInScope (TvSubst in_scope env) vars = TvSubst (extendInScopeSetList in_scope vars) env

extendTvSubst :: TvSubst -> TyVar -> Type -> TvSubst
extendTvSubst (TvSubst in_scope env) tv ty = TvSubst in_scope (extendVarEnv env tv ty)

extendTvSubstList :: TvSubst -> [TyVar] -> [Type] -> TvSubst
extendTvSubstList (TvSubst in_scope env) tvs tys 
  = TvSubst in_scope (extendVarEnvList env (tvs `zip` tys))

-- mkOpenTvSubst and zipOpenTvSubst generate the in-scope set from
-- the types given; but it's just a thunk so with a bit of luck
-- it'll never be evaluated

mkOpenTvSubst :: TvSubstEnv -> TvSubst
mkOpenTvSubst env = TvSubst (mkInScopeSet (tyVarsOfTypes (varEnvElts env))) env

zipOpenTvSubst :: [TyVar] -> [Type] -> TvSubst
zipOpenTvSubst tyvars tys 
#ifdef DEBUG
  | length tyvars /= length tys
  = pprTrace "zipOpenTvSubst" (ppr tyvars $$ ppr tys) emptyTvSubst
  | otherwise
#endif
  = TvSubst (mkInScopeSet (tyVarsOfTypes tys)) (zipTyEnv tyvars tys)

-- mkTopTvSubst is called when doing top-level substitutions.
-- Here we expect that the free vars of the range of the
-- substitution will be empty.
mkTopTvSubst :: [(TyVar, Type)] -> TvSubst
mkTopTvSubst prs = TvSubst emptyInScopeSet (mkVarEnv prs)

zipTopTvSubst :: [TyVar] -> [Type] -> TvSubst
zipTopTvSubst tyvars tys 
#ifdef DEBUG
  | length tyvars /= length tys
  = pprTrace "zipOpenTvSubst" (ppr tyvars $$ ppr tys) emptyTvSubst
  | otherwise
#endif
  = TvSubst emptyInScopeSet (zipTyEnv tyvars tys)

zipTyEnv :: [TyVar] -> [Type] -> TvSubstEnv
zipTyEnv tyvars tys
#ifdef DEBUG
  | length tyvars /= length tys
  = pprTrace "mkTopTvSubst" (ppr tyvars $$ ppr tys) emptyVarEnv
  | otherwise
#endif
  = zip_ty_env tyvars tys emptyVarEnv

-- Later substitutions in the list over-ride earlier ones, 
-- but there should be no loops
zip_ty_env []       []       env = env
zip_ty_env (tv:tvs) (ty:tys) env = zip_ty_env tvs tys (extendVarEnv env tv ty)
	-- There used to be a special case for when 
	--	ty == TyVarTy tv
	-- (a not-uncommon case) in which case the substitution was dropped.
	-- But the type-tidier changes the print-name of a type variable without
	-- changing the unique, and that led to a bug.   Why?  Pre-tidying, we had 
	-- a type {Foo t}, where Foo is a one-method class.  So Foo is really a newtype.
	-- And it happened that t was the type variable of the class.  Post-tiding, 
	-- it got turned into {Foo t2}.  The ext-core printer expanded this using
	-- sourceTypeRep, but that said "Oh, t == t2" because they have the same unique,
	-- and so generated a rep type mentioning t not t2.  
	--
	-- Simplest fix is to nuke the "optimisation"
zip_ty_env tvs      tys      env   = pprTrace "Var/Type length mismatch: " (ppr tvs $$ ppr tys) env
-- zip_ty_env _ _ env = env

instance Outputable TvSubst where
  ppr (TvSubst ins env) 
    = brackets $ sep[ ptext SLIT("TvSubst"),
		      nest 2 (ptext SLIT("In scope:") <+> ppr ins), 
		      nest 2 (ptext SLIT("Env:") <+> ppr env) ]
\end{code}

%************************************************************************
%*									*
		Performing type substitutions
%*									*
%************************************************************************

\begin{code}
substTyWith :: [TyVar] -> [Type] -> Type -> Type
substTyWith tvs tys = ASSERT( length tvs == length tys )
		      substTy (zipOpenTvSubst tvs tys)

substTy :: TvSubst -> Type  -> Type
substTy subst ty | isEmptyTvSubst subst = ty
		 | otherwise	        = subst_ty subst ty

substTys :: TvSubst -> [Type] -> [Type]
substTys subst tys | isEmptyTvSubst subst = tys
	           | otherwise	          = map (subst_ty subst) tys

substTheta :: TvSubst -> ThetaType -> ThetaType
substTheta subst theta
  | isEmptyTvSubst subst = theta
  | otherwise	         = map (substPred subst) theta

substPred :: TvSubst -> PredType -> PredType
substPred subst (IParam n ty)     = IParam n (subst_ty subst ty)
substPred subst (ClassP clas tys) = ClassP clas (map (subst_ty subst) tys)
substPred subst (EqPred ty1 ty2)  = EqPred (subst_ty subst ty1) (subst_ty subst ty2)

deShadowTy :: TyVarSet -> Type -> Type	-- Remove any nested binders mentioning tvs
deShadowTy tvs ty 
  = subst_ty (mkTvSubst in_scope emptyTvSubstEnv) ty
  where
    in_scope = mkInScopeSet tvs

subst_ty :: TvSubst -> Type -> Type
-- subst_ty is the main workhorse for type substitution
--
-- Note that the in_scope set is poked only if we hit a forall
-- so it may often never be fully computed 
subst_ty subst ty
   = go ty
  where
    go (TyVarTy tv)   		   = substTyVar subst tv
    go (TyConApp tc tys)	   = let args = map go tys
				     in  args `seqList` TyConApp tc args

    go (PredTy p)  		   = PredTy $! (substPred subst p)

    go (NoteTy (FTVNote _) ty2)    = go ty2		-- Discard the free tyvar note

    go (FunTy arg res)   	   = (FunTy $! (go arg)) $! (go res)
    go (AppTy fun arg)   	   = mkAppTy (go fun) $! (go arg)
		-- The mkAppTy smart constructor is important
		-- we might be replacing (a Int), represented with App
		-- by [Int], represented with TyConApp
    go (ForAllTy tv ty)		   = case substTyVarBndr subst tv of
					(subst', tv') -> ForAllTy tv' $! (subst_ty subst' ty)

substTyVar :: TvSubst -> TyVar  -> Type
substTyVar subst@(TvSubst in_scope env) tv
  = case lookupTyVar subst tv of {
	Nothing  -> TyVarTy tv;
       	Just ty -> ty	-- See Note [Apply Once]
    } 

lookupTyVar :: TvSubst -> TyVar  -> Maybe Type
lookupTyVar (TvSubst in_scope env) tv = lookupVarEnv env tv

substTyVarBndr :: TvSubst -> TyVar -> (TvSubst, TyVar)	
substTyVarBndr subst@(TvSubst in_scope env) old_var
  = (TvSubst (in_scope `extendInScopeSet` new_var) new_env, new_var)
  where

    new_env | no_change = delVarEnv env old_var
	    | otherwise = extendVarEnv env old_var (TyVarTy new_var)

    no_change = new_var == old_var && not is_co_var
	-- no_change means that the new_var is identical in
	-- all respects to the old_var (same unique, same kind)
	--
	-- In that case we don't need to extend the substitution
	-- to map old to new.  But instead we must zap any 
	-- current substitution for the variable. For example:
	--	(\x.e) with id_subst = [x |-> e']
	-- Here we must simply zap the substitution for x

    new_var = uniqAway in_scope subst_old_var
	-- The uniqAway part makes sure the new variable is not already in scope

    subst_old_var -- subst_old_var is old_var with the substitution applied to its kind
		 -- It's only worth doing the substitution for coercions,
		 -- becuase only they can have free type variables
	| is_co_var = setTyVarKind old_var (substTy subst kind)
	| otherwise = old_var
    kind = tyVarKind old_var
    is_co_var = isCoercionKind kind
\end{code}

----------------------------------------------------
-- Kind Stuff

Kinds
~~~~~
There's a little subtyping at the kind level:  

		 ?
		/ \
	       /   \
	      ??   (#)
	     /  \
            *   #

where	*    [LiftedTypeKind]   means boxed type
	#    [UnliftedTypeKind] means unboxed type
	(#)  [UbxTupleKind]     means unboxed tuple
	??   [ArgTypeKind]      is the lub of *,#
	?    [OpenTypeKind]	means any type at all

In particular:

	error :: forall a:?. String -> a
	(->)  :: ?? -> ? -> *
	(\(x::t) -> ...)	Here t::?? (i.e. not unboxed tuple)

\begin{code}
type KindVar = TyVar  -- invariant: KindVar will always be a 
                      -- TcTyVar with details MetaTv TauTv ...
-- kind var constructors and functions are in TcType

type SimpleKind = Kind
\end{code}

Kind inference
~~~~~~~~~~~~~~
During kind inference, a kind variable unifies only with 
a "simple kind", sk
	sk ::= * | sk1 -> sk2
For example 
	data T a = MkT a (T Int#)
fails.  We give T the kind (k -> *), and the kind variable k won't unify
with # (the kind of Int#).

Type inference
~~~~~~~~~~~~~~
When creating a fresh internal type variable, we give it a kind to express 
constraints on it.  E.g. in (\x->e) we make up a fresh type variable for x, 
with kind ??.  

During unification we only bind an internal type variable to a type
whose kind is lower in the sub-kind hierarchy than the kind of the tyvar.

When unifying two internal type variables, we collect their kind constraints by
finding the GLB of the two.  Since the partial order is a tree, they only
have a glb if one is a sub-kind of the other.  In that case, we bind the
less-informative one to the more informative one.  Neat, eh?


\begin{code}

\end{code}

%************************************************************************
%*									*
	Functions over Kinds		
%*									*
%************************************************************************

\begin{code}
kindFunResult :: Kind -> Kind
kindFunResult k = funResultTy k

splitKindFunTys :: Kind -> ([Kind],Kind)
splitKindFunTys k = splitFunTys k

isUbxTupleKind, isOpenTypeKind, isArgTypeKind, isUnliftedTypeKind :: Kind -> Bool

isOpenTypeKindCon tc    = tyConUnique tc == openTypeKindTyConKey

isOpenTypeKind (TyConApp tc _) = isOpenTypeKindCon tc
isOpenTypeKind other           = False

isUbxTupleKindCon tc = tyConUnique tc == ubxTupleKindTyConKey

isUbxTupleKind (TyConApp tc _) = isUbxTupleKindCon tc
isUbxTupleKind other 	       = False

isArgTypeKindCon tc = tyConUnique tc == argTypeKindTyConKey

isArgTypeKind (TyConApp tc _) = isArgTypeKindCon tc
isArgTypeKind other = False

isUnliftedTypeKindCon tc = tyConUnique tc == unliftedTypeKindTyConKey

isUnliftedTypeKind (TyConApp tc _) = isUnliftedTypeKindCon tc
isUnliftedTypeKind other           = False

isSubOpenTypeKind :: Kind -> Bool
-- True of any sub-kind of OpenTypeKind (i.e. anything except arrow)
isSubOpenTypeKind (FunTy k1 k2)    = ASSERT2 ( isKind k1, text "isSubOpenTypeKind" <+> ppr k1 <+> text "::" <+> ppr (typeKind k1) ) 
                                     ASSERT2 ( isKind k2, text "isSubOpenTypeKind" <+> ppr k2 <+> text "::" <+> ppr (typeKind k2) ) 
                                     False
isSubOpenTypeKind (TyConApp kc []) = ASSERT( isKind (TyConApp kc []) ) True
isSubOpenTypeKind other            = ASSERT( isKind other ) False
         -- This is a conservative answer
         -- It matters in the call to isSubKind in
	 -- checkExpectedKind.

isSubArgTypeKindCon kc
  | isUnliftedTypeKindCon kc = True
  | isLiftedTypeKindCon kc   = True
  | isArgTypeKindCon kc      = True
  | otherwise                = False

isSubArgTypeKind :: Kind -> Bool
-- True of any sub-kind of ArgTypeKind 
isSubArgTypeKind (TyConApp kc []) = isSubArgTypeKindCon kc
isSubArgTypeKind other            = False

isSuperKind :: Type -> Bool
isSuperKind (TyConApp (skc) []) = isSuperKindTyCon skc
isSuperKind other = False

isKind :: Kind -> Bool
isKind k = isSuperKind (typeKind k)



isSubKind :: Kind -> Kind -> Bool
-- (k1 `isSubKind` k2) checks that k1 <: k2
isSubKind (TyConApp kc1 []) (TyConApp kc2 []) = kc1 `isSubKindCon` kc1
isSubKind (FunTy a1 r1) (FunTy a2 r2)	      = (a2 `isSubKind` a1) && (r1 `isSubKind` r2)
isSubKind k1 		k2 		      = False

eqKind :: Kind -> Kind -> Bool
eqKind = tcEqType

isSubKindCon :: TyCon -> TyCon -> Bool
-- (kc1 `isSubKindCon` kc2) checks that kc1 <: kc2
isSubKindCon kc1 kc2
  | isLiftedTypeKindCon kc1   && isLiftedTypeKindCon kc2   = True
  | isUnliftedTypeKindCon kc1 && isUnliftedTypeKindCon kc2 = True
  | isUbxTupleKindCon kc1     && isUbxTupleKindCon kc2     = True
  | isOpenTypeKindCon kc2                                  = True 
                           -- we already know kc1 is not a fun, its a TyCon
  | isArgTypeKindCon kc2      && isSubArgTypeKindCon kc1   = True
  | otherwise                                              = False

defaultKind :: Kind -> Kind
-- Used when generalising: default kind '?' and '??' to '*'
-- 
-- When we generalise, we make generic type variables whose kind is
-- simple (* or *->* etc).  So generic type variables (other than
-- built-in constants like 'error') always have simple kinds.  This is important;
-- consider
--	f x = True
-- We want f to get type
--	f :: forall (a::*). a -> Bool
-- Not 
--	f :: forall (a::??). a -> Bool
-- because that would allow a call like (f 3#) as well as (f True),
--and the calling conventions differ.  This defaulting is done in TcMType.zonkTcTyVarBndr.
defaultKind k 
  | isSubOpenTypeKind k = liftedTypeKind
  | isSubArgTypeKind k  = liftedTypeKind
  | otherwise        = k

isCoercionKind :: Kind -> Bool
-- All coercions are of form (ty1 :=: ty2)
-- This function is here rather than in Coercion, 
-- because it's used by substTy
isCoercionKind k | Just k' <- kindView k = isCoercionKind k'
isCoercionKind (PredTy (EqPred {})) 	 = True
isCoercionKind other			 = False
\end{code}
