%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[Type]{Type - public interface}

\begin{code}
module Type (
        -- re-exports from TypeRep
	TyThing(..), Type, PredType(..), ThetaType, 
	funTyCon,

	-- Re-exports from Kind
	module Kind,

	-- Re-exports from TyCon
	PrimRep(..),

	mkTyVarTy, mkTyVarTys, getTyVar, getTyVar_maybe, isTyVarTy,

	mkAppTy, mkAppTys, splitAppTy, splitAppTys, splitAppTy_maybe,

	mkFunTy, mkFunTys, splitFunTy, splitFunTy_maybe, 
	splitFunTys, splitFunTysN,
	funResultTy, funArgTy, zipFunTys, isFunTy,

	mkGenTyConApp, mkTyConApp, mkTyConTy, 
	tyConAppTyCon, tyConAppArgs, 
	splitTyConApp_maybe, splitTyConApp,

	mkSynTy, 

	repType, typePrimRep, coreView, deepCoreView,

	mkForAllTy, mkForAllTys, splitForAllTy_maybe, splitForAllTys, 
	applyTy, applyTys, isForAllTy, dropForAlls,

	-- Source types
	predTypeRep, mkPredTy, mkPredTys,

	-- Newtypes
	splitRecNewType_maybe,

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
 	extendTvSubst, extendTvSubstList, isInScope, composeTvSubst,

	-- Performing substitution on types
	substTy, substTys, substTyWith, substTheta, substTyVar, substTyVarBndr,
	deShadowTy, 

	-- Pretty-printing
	pprType, pprParendType, pprTyThingCategory,
	pprPred, pprTheta, pprThetaArrow, pprClassPred
    ) where

#include "HsVersions.h"

-- We import the representation and primitive functions from TypeRep.
-- Many things are reexported, but not the representation!

import TypeRep

-- friends:
import Kind
import Var	( Var, TyVar, tyVarKind, tyVarName, setTyVarName )
import VarEnv
import VarSet

import Name	( NamedThing(..), mkInternalName, tidyOccName )
import Class	( Class, classTyCon )
import TyCon	( TyCon, isRecursiveTyCon, isPrimTyCon,
		  isUnboxedTupleTyCon, isUnLiftedTyCon,
		  isFunTyCon, isNewTyCon, newTyConRep, newTyConRhs,
		  isAlgTyCon, isSynTyCon, tyConArity, newTyConRhs_maybe,
	          tyConKind, getSynTyConDefn, PrimRep(..), tyConPrimRep,
		)

-- others
import StaticFlags	( opt_DictsStrict )
import SrcLoc		( noSrcLoc )
import Unique		( Uniquable(..) )
import Util		( mapAccumL, seqList, lengthIs, snocView, thenCmp, isEqual )
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
-- Srips off the *top layer only* of a type to give 
-- its underlying representation type. 
-- Returns Nothing if there is nothing to look through.
--
-- By being non-recursive and inlined, this case analysis gets efficiently
-- joined onto the case analysis that the caller is already doing
coreView (NoteTy _ ty) 	   = Just ty
coreView (PredTy p)    	   = Just (predTypeRep p)
coreView (TyConApp tc tys) = expandNewTcApp tc tys
coreView ty		   = Nothing

deepCoreView :: Type -> Type
-- Apply coreView recursively
deepCoreView ty
  | Just ty' <- coreView ty    = deepCoreView ty'
deepCoreView (TyVarTy tv)      = TyVarTy tv
deepCoreView (TyConApp tc tys) = TyConApp tc (map deepCoreView tys)
deepCoreView (AppTy t1 t2)     = AppTy (deepCoreView t1) (deepCoreView t2)
deepCoreView (FunTy t1 t2)     = FunTy (deepCoreView t1) (deepCoreView t2)
deepCoreView (ForAllTy tv ty)  = ForAllTy tv (deepCoreView ty)
	-- No NoteTy, no PredTy

expandNewTcApp :: TyCon -> [Type] -> Maybe Type
-- A local helper function (not exported)
-- Expands *the outermoset level of* a newtype application to 
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

expandNewTcApp tc tys = case newTyConRhs_maybe tc tys of
			  Nothing          -> Nothing
			  Just (tenv, rhs) -> Just (substTy (mkTopTvSubst tenv) rhs)
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
    mk_app (TyConApp tc tys) = mkGenTyConApp tc (tys ++ [orig_ty2])
    mk_app ty1		     = AppTy orig_ty1 orig_ty2
	-- We call mkGenTyConApp because the TyConApp could be an 
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
    mk_app (TyConApp tc tys) = mkGenTyConApp tc (tys ++ orig_tys2)
				-- mkGenTyConApp: see notes with mkAppTy
    mk_app ty1		     = foldl AppTy orig_ty1 orig_tys2

splitAppTy_maybe :: Type -> Maybe (Type, Type)
splitAppTy_maybe ty | Just ty' <- coreView ty = splitAppTy_maybe ty'
splitAppTy_maybe (FunTy ty1 ty2)   = Just (TyConApp funTyCon [ty1], ty2)
splitAppTy_maybe (AppTy ty1 ty2)   = Just (ty1, ty2)
splitAppTy_maybe (TyConApp tc tys) = case snocView tys of
					Nothing         -> Nothing
					Just (tys',ty') -> Just (TyConApp tc tys', ty')
splitAppTy_maybe other	     	   = Nothing

splitAppTy :: Type -> (Type, Type)
splitAppTy ty = case splitAppTy_maybe ty of
			Just pr -> pr
			Nothing -> panic "splitAppTy"

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
mkGenTyConApp :: TyCon -> [Type] -> Type
mkGenTyConApp tc tys
  | isSynTyCon tc = mkSynTy tc tys
  | otherwise     = mkTyConApp tc tys

mkTyConApp :: TyCon -> [Type] -> Type
-- Assumes TyCon is not a SynTyCon; use mkSynTy instead for those
mkTyConApp tycon tys
  | isFunTyCon tycon, [ty1,ty2] <- tys
  = FunTy ty1 ty2

  | otherwise
  = ASSERT(not (isSynTyCon tycon))
    TyConApp tycon tys

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
\end{code}


---------------------------------------------------------------------
				SynTy
				~~~~~

\begin{code}
mkSynTy tycon tys
  | n_args == arity	-- Exactly saturated
  = mk_syn tys
  | n_args >  arity	-- Over-saturated
  = case splitAt arity tys of { (as,bs) -> mkAppTys (mk_syn as) bs }
	-- Its important to use mkAppTys, rather than (foldl AppTy),
	-- because (mk_syn as) might well return a partially-applied
	-- type constructor; indeed, usually will!
  | otherwise		-- Un-saturated
  = TyConApp tycon tys
	-- For the un-saturated case we build TyConApp directly
	-- (mkTyConApp ASSERTs that the tc isn't a SynTyCon).
	-- Here we are relying on checkValidType to find
	-- the error.  What we can't do is use mkSynTy with
	-- too few arg tys, because that is utterly bogus.

  where
    mk_syn tys = NoteTy (SynNote (TyConApp tycon tys))
			(substTyWith tyvars tys body)

    (tyvars, body) = ASSERT( isSynTyCon tycon ) getSynTyConDefn tycon
    arity 	   = tyConArity tycon
    n_args	   = length tys
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
repType (ForAllTy _ ty)   = repType ty
repType (NoteTy   _ ty)   = repType ty
repType (PredTy  p)       = repType (predTypeRep p)
repType (TyConApp tc tys) 
  | isNewTyCon tc 	  = ASSERT( tys `lengthIs` tyConArity tc )
			    repType (new_type_rep tc tys)
repType ty	 	  = ty

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

-- new_type_rep doesn't ask any questions: 
-- it just expands newtype, whether recursive or not
new_type_rep new_tycon tys = ASSERT( tys `lengthIs` tyConArity new_tycon )
			     case newTyConRep new_tycon of
				 (tvs, rep_ty) -> substTyWith tvs tys rep_ty
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
	(tvs, rep_ty) -> Just (substTyWith tvs tys rep_ty)

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

typeKind (TyVarTy tyvar)	= tyVarKind tyvar
typeKind (TyConApp tycon tys)	= foldr (\_ k -> kindFunResult k) (tyConKind tycon) tys
typeKind (NoteTy _ ty)		= typeKind ty
typeKind (PredTy _)		= liftedTypeKind -- Predicates are always 
						 -- represented by lifted types
typeKind (AppTy fun arg)	= kindFunResult (typeKind fun)
typeKind (FunTy arg res)	= liftedTypeKind
typeKind (ForAllTy tv ty)	= typeKind ty
\end{code}


---------------------------------------------------------------------
		Free variables of a type
		~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
tyVarsOfType :: Type -> TyVarSet
tyVarsOfType (TyVarTy tv)		= unitVarSet tv
tyVarsOfType (TyConApp tycon tys)	= tyVarsOfTypes tys
tyVarsOfType (NoteTy (FTVNote tvs) ty2) = tvs
tyVarsOfType (NoteTy (SynNote ty1) ty2)	= tyVarsOfType ty2	-- See note [Syn] below
tyVarsOfType (PredTy sty)		= tyVarsOfPred sty
tyVarsOfType (FunTy arg res)		= tyVarsOfType arg `unionVarSet` tyVarsOfType res
tyVarsOfType (AppTy fun arg)		= tyVarsOfType fun `unionVarSet` tyVarsOfType arg
tyVarsOfType (ForAllTy tyvar ty)	= tyVarsOfType ty `minusVarSet` unitVarSet tyvar

-- 			Note [Syn]
-- Consider
--	type T a = Int
-- What are the free tyvars of (T x)?  Empty, of course!  
-- Here's the example that Ralf Laemmel showed me:
--	foo :: (forall a. C u a -> C u a) -> u
--	mappend :: Monoid u => u -> u -> u
--
--	bar :: Monoid u => u
--	bar = foo (\t -> t `mappend` t)
-- We have to generalise at the arg to f, and we don't
-- want to capture the constraint (Monad (C u a)) because
-- it appears to mention a.  Pretty silly, but it was useful to him.


tyVarsOfTypes :: [Type] -> TyVarSet
tyVarsOfTypes tys = foldr (unionVarSet.tyVarsOfType) emptyVarSet tys

tyVarsOfPred :: PredType -> TyVarSet
tyVarsOfPred (IParam _ ty)  = tyVarsOfType ty
tyVarsOfPred (ClassP _ tys) = tyVarsOfTypes tys

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
			name'  = mkInternalName (getUnique name) occ' noSrcLoc
				-- Note: make a *user* tyvar, so it printes nicely
				-- Could extract src loc, but no need.
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

    go_note (SynNote ty)        = SynNote $! (go ty)
    go_note note@(FTVNote ftvs) = note	-- No need to tidy the free tyvars

tidyTypes env tys = map (tidyType env) tys

tidyPred :: TidyEnv -> PredType -> PredType
tidyPred env (IParam n ty)     = IParam n (tidyType env ty)
tidyPred env (ClassP clas tys) = ClassP clas (tidyTypes env tys)
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
seqNote (SynNote ty)  = seqType ty
seqNote (FTVNote set) = sizeUniqSet set `seq` ()

seqPred :: PredType -> ()
seqPred (ClassP c tys) = c  `seq` seqTypes tys
seqPred (IParam n ty)  = n  `seq` seqType ty
\end{code}


%************************************************************************
%*									*
		Comparison of types
	(We don't use instances so that we know where it happens)
%*									*
%************************************************************************

Two flavours:

* tcEqType, tcCmpType do *not* look through newtypes, PredTypes
* coreEqType *does* look through them

Note that eqType can respond 'False' for partial applications of newtypes.
Consider
	newtype Parser m a = MkParser (Foogle m a)
Does 	
	Monad (Parser m) `eqType` Monad (Foogle m)
Well, yes, but eqType won't see that they are the same. 
I don't think this is harmful, but it's soemthing to watch out for.

First, the external interface

\begin{code}
coreEqType :: Type -> Type -> Bool
coreEqType t1 t2 = isEqual $ cmpType (deepCoreView t1) (deepCoreView t2)

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

-- NB: we *cannot* short-cut the newtype comparison thus:
-- eqTypeX env (NewTcApp tc1 tys1) (NewTcApp tc2 tys2) 
--	| (tc1 == tc2) = (eqTypeXs env tys1 tys2)
--
-- Consider:
--	newtype T a = MkT [a]
--	newtype Foo m = MkFoo (forall a. m a -> Int)
--	w1 :: Foo []
--	w1 = ...
--	
--	w2 :: Foo T
--	w2 = MkFoo (\(MkT x) -> case w1 of MkFoo f -> f x)
--
-- We end up with w2 = w1; so we need that Foo T = Foo []
-- but we can only expand saturated newtypes, so just comparing
-- T with [] won't do. 

cmpTypeX env (TyVarTy tv1)       (TyVarTy tv2)       = rnOccL env tv1 `compare` rnOccR env tv2
cmpTypeX env (ForAllTy tv1 t1)   (ForAllTy tv2 t2)   = cmpTypeX (rnBndr2 env tv1 tv2) t1 t2
cmpTypeX env (AppTy s1 t1)       (AppTy s2 t2)       = cmpTypeX env s1 s2 `thenCmp` cmpTypeX env t1 t2
cmpTypeX env (FunTy s1 t1)       (FunTy s2 t2)       = cmpTypeX env s1 s2 `thenCmp` cmpTypeX env t1 t2
cmpTypeX env (PredTy p1)         (PredTy p2)         = cmpPredX env p1 p2
cmpTypeX env (TyConApp tc1 tys1) (TyConApp tc2 tys2) = (tc1 `compare` tc2) `thenCmp` cmpTypesX env tys1 tys2
cmpTypeX env (NoteTy _ t1)	t2		     = cmpTypeX env t1 t2
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
  = TvSubst (mkInScopeSet (tyVarsOfTypes tys)) (zipTyEnv tyvars tys)

-- mkTopTvSubst is called when doing top-level substitutions.
-- Here we expect that the free vars of the range of the
-- substitution will be empty.
mkTopTvSubst :: [(TyVar, Type)] -> TvSubst
mkTopTvSubst prs = TvSubst emptyInScopeSet (mkVarEnv prs)

zipTopTvSubst :: [TyVar] -> [Type] -> TvSubst
zipTopTvSubst tyvars tys = TvSubst emptyInScopeSet (zipTyEnv tyvars tys)

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

instance Outputable TvSubst where
  ppr (TvSubst ins env) 
    = sep[ ptext SLIT("<TvSubst"),
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
substTyWith tvs tys = substTy (zipOpenTvSubst tvs tys)

substTy :: TvSubst -> Type  -> Type
substTy subst ty | isEmptyTvSubst subst = ty
		 | otherwise	        = subst_ty subst ty

substTys :: TvSubst -> [Type] -> [Type]
substTys subst tys | isEmptyTvSubst subst = tys
	           | otherwise	          = map (subst_ty subst) tys

deShadowTy :: Type -> Type		-- Remove any shadowing from the type
deShadowTy ty = subst_ty emptyTvSubst ty

substTheta :: TvSubst -> ThetaType -> ThetaType
substTheta subst theta
  | isEmptyTvSubst subst = theta
  | otherwise	         = map (substPred subst) theta

substPred :: TvSubst -> PredType -> PredType
substPred subst (IParam n ty)     = IParam n (subst_ty subst ty)
substPred subst (ClassP clas tys) = ClassP clas (map (subst_ty subst) tys)

-- Note that the in_scope set is poked only if we hit a forall
-- so it may often never be fully computed 
subst_ty subst ty
   = go ty
  where
    go (TyVarTy tv)   		   = substTyVar subst tv
    go (TyConApp tc tys)	   = let args = map go tys
				     in  args `seqList` TyConApp tc args

    go (PredTy p)  		   = PredTy $! (substPred subst p)

    go (NoteTy (SynNote ty1) ty2)  = NoteTy (SynNote $! (go ty1)) $! (go ty2)
    go (NoteTy (FTVNote _) ty2)    = go ty2		-- Discard the free tyvar note

    go (FunTy arg res)   	   = (FunTy $! (go arg)) $! (go res)
    go (AppTy fun arg)   	   = mkAppTy (go fun) $! (go arg)
		-- The mkAppTy smart constructor is important
		-- we might be replacing (a Int), represented with App
		-- by [Int], represented with TyConApp
    go (ForAllTy tv ty)		   = case substTyVarBndr subst tv of
					(subst', tv') -> ForAllTy tv' $! (subst_ty subst' ty)

substTyVar :: TvSubst -> TyVar  -> Type
substTyVar (TvSubst in_scope env) tv
  = case (lookupVarEnv env tv) of
	Nothing  -> TyVarTy tv
       	Just ty' -> ty'	-- See Note [Apply Once]

substTyVarBndr :: TvSubst -> TyVar -> (TvSubst, TyVar)	
substTyVarBndr subst@(TvSubst in_scope env) old_var
  | old_var == new_var	-- No need to clone
			-- But we *must* zap any current substitution for the variable.
			--  For example:
			--	(\x.e) with id_subst = [x |-> e']
			-- Here we must simply zap the substitution for x
			--
			-- The new_id isn't cloned, but it may have a different type
			-- etc, so we must return it, not the old id
  = (TvSubst (in_scope `extendInScopeSet` new_var) 
	     (delVarEnv env old_var),
     new_var)

  | otherwise	-- The new binder is in scope so
		-- we'd better rename it away from the in-scope variables
		-- Extending the substitution to do this renaming also
		-- has the (correct) effect of discarding any existing
		-- substitution for that variable
  = (TvSubst (in_scope `extendInScopeSet` new_var) 
	     (extendVarEnv env old_var (TyVarTy new_var)),
     new_var)
  where
    new_var = uniqAway in_scope old_var
	-- The uniqAway part makes sure the new variable is not already in scope
\end{code}
