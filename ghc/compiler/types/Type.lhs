%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[Type]{Type - public interface}

\begin{code}
module Type (
        -- re-exports from TypeRep:
	TyThing(..),
	Type, PredType(..), ThetaType,
	Kind, TyVarSubst, 

	superKind, superBoxity,				-- KX and BX respectively
	liftedBoxity, unliftedBoxity, 			-- :: BX
	openKindCon, 					-- :: KX
	typeCon,					-- :: BX -> KX
	liftedTypeKind, unliftedTypeKind, openTypeKind,	-- :: KX
	mkArrowKind, mkArrowKinds,			-- :: KX -> KX -> KX
	isTypeKind, isAnyTypeKind,
	funTyCon,

        -- exports from this module:
        hasMoreBoxityInfo, defaultKind,

	mkTyVarTy, mkTyVarTys, getTyVar, getTyVar_maybe, isTyVarTy,

	mkAppTy, mkAppTys, splitAppTy, splitAppTys, splitAppTy_maybe,

	mkFunTy, mkFunTys, splitFunTy, splitFunTy_maybe, splitFunTys, 
	funResultTy, funArgTy, zipFunTys, isFunTy,

	mkGenTyConApp, mkTyConApp, mkTyConTy, 
	tyConAppTyCon, tyConAppArgs, 
	splitTyConApp_maybe, splitTyConApp,

	mkSynTy, 

	repType, typePrimRep,

	mkForAllTy, mkForAllTys, splitForAllTy_maybe, splitForAllTys, 
	applyTy, applyTys, isForAllTy, dropForAlls,

	-- Source types
	isPredTy, predTypeRep, mkPredTy, mkPredTys,

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
	eqType, eqKind, 

	-- Seq
	seqType, seqTypes

    ) where

#include "HsVersions.h"

-- We import the representation and primitive functions from TypeRep.
-- Many things are reexported, but not the representation!

import TypeRep

-- Other imports:

import {-# SOURCE #-}   Subst  ( substTyWith )

-- friends:
import Var	( TyVar, tyVarKind, tyVarName, setTyVarName )
import VarEnv
import VarSet

import Name	( NamedThing(..), mkInternalName, tidyOccName )
import Class	( Class, classTyCon )
import TyCon	( TyCon, isRecursiveTyCon, isPrimTyCon,
		  isUnboxedTupleTyCon, isUnLiftedTyCon,
		  isFunTyCon, isNewTyCon, newTyConRep,
		  isAlgTyCon, isSynTyCon, tyConArity, 
	          tyConKind, getSynTyConDefn,
		  tyConPrimRep, 
		)

-- others
import CmdLineOpts	( opt_DictsStrict )
import SrcLoc		( noSrcLoc )
import PrimRep		( PrimRep(..) )
import Unique		( Uniquable(..) )
import Util		( mapAccumL, seqList, lengthIs, snocView )
import Outputable
import UniqSet		( sizeUniqSet )		-- Should come via VarSet
import Maybe		( isJust )
\end{code}


%************************************************************************
%*									*
\subsection{Stuff to do with kinds.}
%*									*
%************************************************************************

\begin{code}
hasMoreBoxityInfo :: Kind -> Kind -> Bool
-- (k1 `hasMoreBoxityInfo` k2) checks that k1 <: k2
hasMoreBoxityInfo k1 k2
  | k2 `eqKind` openTypeKind = isAnyTypeKind k1
  | otherwise	  	     = k1 `eqKind` k2

isAnyTypeKind :: Kind -> Bool
-- True of kind * and *# and ?
isAnyTypeKind (TyConApp tc _) = tc == typeCon || tc == openKindCon
isAnyTypeKind (NoteTy _ k)    = isAnyTypeKind k
isAnyTypeKind other	      = False

isTypeKind :: Kind -> Bool
-- True of kind * and *#
isTypeKind (TyConApp tc _) = tc == typeCon
isTypeKind (NoteTy _ k)    = isTypeKind k
isTypeKind other	   = False

defaultKind :: Kind -> Kind
-- Used when generalising: default kind '?' to '*'
defaultKind kind | kind `eqKind` openTypeKind = liftedTypeKind
	         | otherwise	 	      = kind
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
getTyVar_maybe (TyVarTy tv) 	 = Just tv
getTyVar_maybe (NoteTy _ t) 	 = getTyVar_maybe t
getTyVar_maybe (PredTy p) 	 = getTyVar_maybe (predTypeRep p)
getTyVar_maybe (NewTcApp tc tys) = getTyVar_maybe (newTypeRep tc tys)
getTyVar_maybe other	         = Nothing
\end{code}


---------------------------------------------------------------------
				AppTy
				~~~~~
We need to be pretty careful with AppTy to make sure we obey the 
invariant that a TyConApp is always visibly so.  mkAppTy maintains the
invariant: use it.

\begin{code}
mkAppTy orig_ty1 orig_ty2
  = ASSERT2( not (isPredTy orig_ty1), crudePprType orig_ty1 )	-- Source types are of kind *
    mk_app orig_ty1
  where
    mk_app (NoteTy _ ty1)    = mk_app ty1
    mk_app (NewTcApp tc tys) = NewTcApp tc (tys ++ [orig_ty2])
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
  = ASSERT( not (isPredTy orig_ty1) )	-- Source types are of kind *
    mk_app orig_ty1
  where
    mk_app (NoteTy _ ty1)    = mk_app ty1
    mk_app (NewTcApp tc tys) = NewTcApp tc (tys ++ orig_tys2)
    mk_app (TyConApp tc tys) = mkTyConApp tc (tys ++ orig_tys2)
				-- Use mkTyConApp in case tc is (->)
    mk_app ty1		     = foldl AppTy orig_ty1 orig_tys2

splitAppTy_maybe :: Type -> Maybe (Type, Type)
splitAppTy_maybe (FunTy ty1 ty2)   = Just (TyConApp funTyCon [ty1], ty2)
splitAppTy_maybe (AppTy ty1 ty2)   = Just (ty1, ty2)
splitAppTy_maybe (NoteTy _ ty)     = splitAppTy_maybe ty
splitAppTy_maybe (PredTy p)        = splitAppTy_maybe (predTypeRep p)
splitAppTy_maybe (NewTcApp tc tys) = splitAppTy_maybe (newTypeRep tc tys)
splitAppTy_maybe (TyConApp tc tys) = case snocView tys of
					Nothing -> Nothing
					Just (tys',ty') -> Just (mkGenTyConApp tc tys', ty')
						-- mkGenTyConApp just in case the tc is a newtype

splitAppTy_maybe other	     	   = Nothing

splitAppTy :: Type -> (Type, Type)
splitAppTy ty = case splitAppTy_maybe ty of
			Just pr -> pr
			Nothing -> panic "splitAppTy"

splitAppTys :: Type -> (Type, [Type])
splitAppTys ty = split ty ty []
  where
    split orig_ty (AppTy ty arg)        args = split ty ty (arg:args)
    split orig_ty (NoteTy _ ty)         args = split orig_ty ty args
    split orig_ty (PredTy p)            args = split orig_ty (predTypeRep p) args
    split orig_ty (NewTcApp tc tc_args) args = split orig_ty (newTypeRep tc tc_args) args
    split orig_ty (TyConApp tc tc_args) args = (mkGenTyConApp tc [], tc_args ++ args)
						-- mkGenTyConApp just in case the tc is a newtype
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
splitFunTy (FunTy arg res)   = (arg, res)
splitFunTy (NoteTy _ ty)     = splitFunTy ty
splitFunTy (PredTy p)        = splitFunTy (predTypeRep p)
splitFunTy (NewTcApp tc tys) = splitFunTy (newTypeRep tc tys)
splitFunTy other	     = pprPanic "splitFunTy" (crudePprType other)

splitFunTy_maybe :: Type -> Maybe (Type, Type)
splitFunTy_maybe (FunTy arg res)   = Just (arg, res)
splitFunTy_maybe (NoteTy _ ty)     = splitFunTy_maybe ty
splitFunTy_maybe (PredTy p)        = splitFunTy_maybe (predTypeRep p)
splitFunTy_maybe (NewTcApp tc tys) = splitFunTy_maybe (newTypeRep tc tys)
splitFunTy_maybe other	           = Nothing

splitFunTys :: Type -> ([Type], Type)
splitFunTys ty = split [] ty ty
  where
    split args orig_ty (FunTy arg res) 	 = split (arg:args) res res
    split args orig_ty (NoteTy _ ty)   	 = split args orig_ty ty
    split args orig_ty (PredTy p)     	 = split args orig_ty (predTypeRep p)
    split args orig_ty (NewTcApp tc tys) = split args orig_ty (newTypeRep tc tys)
    split args orig_ty ty                = (reverse args, orig_ty)

zipFunTys :: Outputable a => [a] -> Type -> ([(a,Type)], Type)
zipFunTys orig_xs orig_ty = split [] orig_xs orig_ty orig_ty
  where
    split acc []     nty ty  	           = (reverse acc, nty)
    split acc (x:xs) nty (FunTy arg res)   = split ((x,arg):acc) xs res res
    split acc xs     nty (NoteTy _ ty)     = split acc           xs nty ty
    split acc xs     nty (PredTy p)        = split acc           xs nty (predTypeRep p)
    split acc xs     nty (NewTcApp tc tys) = split acc           xs nty (newTypeRep tc tys)
    split acc (x:xs) nty ty                = pprPanic "zipFunTys" (ppr orig_xs <+> crudePprType orig_ty)
    
funResultTy :: Type -> Type
funResultTy (FunTy arg res)   = res
funResultTy (NoteTy _ ty)     = funResultTy ty
funResultTy (PredTy p)        = funResultTy (predTypeRep p)
funResultTy (NewTcApp tc tys) = funResultTy (newTypeRep tc tys)
funResultTy ty		      = pprPanic "funResultTy" (crudePprType ty)

funArgTy :: Type -> Type
funArgTy (FunTy arg res)   = arg
funArgTy (NoteTy _ ty)     = funArgTy ty
funArgTy (PredTy p)        = funArgTy (predTypeRep p)
funArgTy (NewTcApp tc tys) = funArgTy (newTypeRep tc tys)
funArgTy ty		   = pprPanic "funArgTy" (crudePprType ty)
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

  | isNewTyCon tycon
  = NewTcApp tycon tys

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
			Nothing	   -> pprPanic "splitTyConApp" (crudePprType ty)

splitTyConApp_maybe :: Type -> Maybe (TyCon, [Type])
splitTyConApp_maybe (TyConApp tc tys) = Just (tc, tys)
splitTyConApp_maybe (FunTy arg res)   = Just (funTyCon, [arg,res])
splitTyConApp_maybe (NoteTy _ ty)     = splitTyConApp_maybe ty
splitTyConApp_maybe (PredTy p)        = splitTyConApp_maybe (predTypeRep p)
splitTyConApp_maybe (NewTcApp tc tys) = splitTyConApp_maybe (newTypeRep tc tys)
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
	(e) [recursive] newtypes
It's useful in the back end.

\begin{code}
repType :: Type -> Type
-- Only applied to types of kind *; hence tycons are saturated
repType (ForAllTy _ ty)   = repType ty
repType (NoteTy   _ ty)   = repType ty
repType (PredTy  p)       = repType (predTypeRep p)
repType (NewTcApp tc tys) = ASSERT( tys `lengthIs` tyConArity tc )
			    repType (new_type_rep tc tys)
repType ty	 	  = ty


typePrimRep :: Type -> PrimRep
typePrimRep ty = case repType ty of
		   TyConApp tc _ -> tyConPrimRep tc
		   FunTy _ _	 -> PtrRep
		   AppTy _ _	 -> PtrRep	-- ??
		   TyVarTy _	 -> PtrRep
		   other	 -> pprPanic "typePrimRep" (crudePprType ty)
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
    splitFAT_m (NoteTy _ ty)		= splitFAT_m ty
    splitFAT_m (PredTy p)		= splitFAT_m (predTypeRep p)
    splitFAT_m (NewTcApp tc tys)	= splitFAT_m (newTypeRep tc tys)
    splitFAT_m (ForAllTy tyvar ty)	= Just(tyvar, ty)
    splitFAT_m _			= Nothing

splitForAllTys :: Type -> ([TyVar], Type)
splitForAllTys ty = split ty ty []
   where
     split orig_ty (ForAllTy tv ty)  tvs = split ty ty (tv:tvs)
     split orig_ty (NoteTy _ ty)     tvs = split orig_ty ty tvs
     split orig_ty (PredTy p)	     tvs = split orig_ty (predTypeRep p) tvs
     split orig_ty (NewTcApp tc tys) tvs = split orig_ty (newTypeRep tc tys) tvs
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
applyTy (PredTy p) 	  arg = applyTy (predTypeRep p) arg
applyTy (NewTcApp tc tys) arg = applyTy (newTypeRep tc tys) arg
applyTy (NoteTy _ fun)    arg = applyTy fun arg
applyTy (ForAllTy tv ty)  arg = substTyWith [tv] [arg] ty
applyTy other		  arg = panic "applyTy"

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
  = ASSERT2( n_tvs > 0, crudePprType orig_fun_ty )	-- Zero case gives infnite loop!
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
predTypeRep (IParam _ ty)     = ty
predTypeRep (ClassP clas tys) = mkTyConApp (classTyCon clas) tys
	-- Result might be a NewTcApp, but the consumer will
	-- look through that too if necessary

isPredTy :: Type -> Bool
isPredTy (NoteTy _ ty) = isPredTy ty
isPredTy (PredTy sty)  = True
isPredTy _	       = False
\end{code}


%************************************************************************
%*									*
		NewTypes
%*									*
%************************************************************************

\begin{code}
splitRecNewType_maybe :: Type -> Maybe Type
-- Newtypes are always represented by a NewTcApp
-- Sometimes we want to look through a recursive newtype, and that's what happens here
-- Only applied to types of kind *, hence the newtype is always saturated
splitRecNewType_maybe (NoteTy _ ty) = splitRecNewType_maybe ty  
splitRecNewType_maybe (NewTcApp tc tys)
  | isRecursiveTyCon tc
  = ASSERT( tys `lengthIs` tyConArity tc && isNewTyCon tc )
	-- The assert should hold because repType should
	-- only be applied to *types* (of kind *)
    Just (new_type_rep tc tys)
splitRecNewType_maybe other = Nothing
			
-----------------------------
newTypeRep :: TyCon -> [Type] -> Type
-- A local helper function (not exported)
-- Expands a newtype application to 
--	*either* a vanilla TyConApp (recursive newtype, or non-saturated)
--	*or*     the newtype representation (otherwise)
-- Either way, the result is not a NewTcApp
--
-- NB: the returned TyConApp is always deconstructed immediately by the 
--     caller... a TyConApp with a newtype type constructor never lives
--     in an ordinary type
newTypeRep tc tys
  | not (isRecursiveTyCon tc),		-- Not recursive and saturated
    tys `lengthIs` tyConArity tc 	-- treat as equivalent to expansion
  = new_type_rep tc tys
  | otherwise
  = TyConApp tc tys
	-- ToDo: Consider caching this substitution in a NType

----------------------------
-- new_type_rep doesn't ask any questions: 
-- it just expands newtype, whether recursive or not
new_type_rep new_tycon tys = ASSERT( tys `lengthIs` tyConArity new_tycon )
			     case newTyConRep new_tycon of
				 (tvs, rep_ty) -> substTyWith tvs tys rep_ty
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
typeKind (TyConApp tycon tys)	= foldr (\_ k -> funResultTy k) (tyConKind tycon) tys
typeKind (NewTcApp tycon tys)	= foldr (\_ k -> funResultTy k) (tyConKind tycon) tys
typeKind (NoteTy _ ty)		= typeKind ty
typeKind (PredTy _)		= liftedTypeKind -- Predicates are always 
						 -- represented by lifted types
typeKind (AppTy fun arg)	= funResultTy (typeKind fun)

typeKind (FunTy arg res)	= fix_up (typeKind res)
				where
				  fix_up (TyConApp tycon _) |  tycon == typeCon
							    || tycon == openKindCon = liftedTypeKind
				  fix_up (NoteTy _ kind) = fix_up kind
				  fix_up kind	         = kind
		-- The basic story is 
		-- 	typeKind (FunTy arg res) = typeKind res
		-- But a function is lifted regardless of its result type
		-- Hence the strange fix-up.
		-- Note that 'res', being the result of a FunTy, can't have 
		-- a strange kind like (*->*).

typeKind (ForAllTy tv ty)	= typeKind ty
\end{code}


---------------------------------------------------------------------
		Free variables of a type
		~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
tyVarsOfType :: Type -> TyVarSet
tyVarsOfType (TyVarTy tv)		= unitVarSet tv
tyVarsOfType (TyConApp tycon tys)	= tyVarsOfTypes tys
tyVarsOfType (NewTcApp tycon tys)	= tyVarsOfTypes tys
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
      (tidy', occ') -> 	-- New occname reqd
			((tidy', subst'), tyvar')
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
    go (NewTcApp tycon tys) = let args = map go tys
			      in args `seqList` NewTcApp tycon args
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

isUnLiftedType (ForAllTy tv ty)  = isUnLiftedType ty
isUnLiftedType (NoteTy _ ty)	 = isUnLiftedType ty
isUnLiftedType (TyConApp tc _)   = isUnLiftedTyCon tc
isUnLiftedType (PredTy _)	 = False		-- All source types are lifted
isUnLiftedType (NewTcApp tc tys) = isUnLiftedType (newTypeRep tc tys)
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
isStrictType (ForAllTy tv ty)  = isStrictType ty
isStrictType (NoteTy _ ty)     = isStrictType ty
isStrictType (TyConApp tc _)   = isUnLiftedTyCon tc
isStrictType (NewTcApp tc tys) = isStrictType (newTypeRep tc tys)
isStrictType (PredTy pred)     = isStrictPred pred
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
seqType (NewTcApp tc tys) = tc `seq` seqTypes tys
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
\subsection{Equality on types}
%*									*
%************************************************************************

Comparison; don't use instances so that we know where it happens.
Look through newtypes but not usage types.

Note that eqType can respond 'False' for partial applications of newtypes.
Consider
	newtype Parser m a = MkParser (Foogle m a)

Does 	
	Monad (Parser m) `eqType` Monad (Foogle m)

Well, yes, but eqType won't see that they are the same. 
I don't think this is harmful, but it's soemthing to watch out for.

\begin{code}
eqType t1 t2 = eq_ty emptyVarEnv t1 t2
eqKind  = eqType	-- No worries about looking 

-- Look through Notes
eq_ty env (NoteTy _ t1)       t2	  	  = eq_ty env t1 t2
eq_ty env t1		      (NoteTy _ t2)       = eq_ty env t1 t2

-- Look through PredTy and NewTcApp.  This is where the looping danger comes from.
-- We don't bother to check for the PredType/PredType case, no good reason
-- Hmm: maybe there is a good reason: see the notes below about newtypes
eq_ty env (PredTy sty1)     t2		  = eq_ty env (predTypeRep sty1) t2
eq_ty env t1		    (PredTy sty2) = eq_ty env t1 (predTypeRep sty2)

-- NB: we *cannot* short-cut the newtype comparison thus:
-- eq_ty env (NewTcApp tc1 tys1) (NewTcApp tc2 tys2) 
--	| (tc1 == tc2) = (eq_tys env tys1 tys2)
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

eq_ty env (NewTcApp tc1 tys1) t2		  = eq_ty env (newTypeRep tc1 tys1) t2
eq_ty env t1		      (NewTcApp tc2 tys2) = eq_ty env t1 (newTypeRep tc2 tys2)

-- The rest is plain sailing
eq_ty env (TyVarTy tv1)       (TyVarTy tv2)       = case lookupVarEnv env tv1 of
							  Just tv1a -> tv1a == tv2
							  Nothing   -> tv1  == tv2
eq_ty env (ForAllTy tv1 t1)   (ForAllTy tv2 t2)   
	| tv1 == tv2				  = eq_ty (delVarEnv env tv1)        t1 t2
	| otherwise				  = eq_ty (extendVarEnv env tv1 tv2) t1 t2
eq_ty env (AppTy s1 t1)       (AppTy s2 t2)       = (eq_ty env s1 s2) && (eq_ty env t1 t2)
eq_ty env (FunTy s1 t1)       (FunTy s2 t2)       = (eq_ty env s1 s2) && (eq_ty env t1 t2)
eq_ty env (TyConApp tc1 tys1) (TyConApp tc2 tys2) = (tc1 == tc2) && (eq_tys env tys1 tys2)
eq_ty env t1		       t2		  = False

eq_tys env []        []        = True
eq_tys env (t1:tys1) (t2:tys2) = (eq_ty env t1 t2) && (eq_tys env tys1 tys2)
eq_tys env tys1      tys2      = False
\end{code}

