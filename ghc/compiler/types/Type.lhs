\begin{code}
module Type (
	GenType(..), TyNote(..), 		-- Representation visible to friends
	Type, GenKind, Kind,
	TyVarSubst, GenTyVarSubst,

	funTyCon, boxedKindCon, unboxedKindCon, openKindCon,

	boxedTypeKind, unboxedTypeKind, openTypeKind, mkArrowKind, mkArrowKinds,
	hasMoreBoxityInfo, superKind,

	mkTyVarTy, mkTyVarTys, getTyVar, getTyVar_maybe, isTyVarTy,

	mkAppTy, mkAppTys, splitAppTy, splitAppTys, splitAppTy_maybe,

	mkFunTy, mkFunTys, splitFunTy_maybe, splitFunTys, funResultTy,

	mkTyConApp, mkTyConTy, splitTyConApp_maybe,
	splitAlgTyConApp_maybe, splitAlgTyConApp,
	mkDictTy, splitDictTy_maybe, isDictTy,

	mkSynTy, isSynTy,

	mkForAllTy, mkForAllTys, splitForAllTy_maybe, splitForAllTys, 
	applyTy, applyTys, isForAllTy,
	mkPiType,

	TauType, RhoType, SigmaType, ThetaType,
	isTauTy,
	mkRhoTy, splitRhoTy,
	mkSigmaTy, splitSigmaTy,

	isUnLiftedType, isUnboxedType, isUnboxedTupleType, isAlgType,
	typePrimRep,

	tyVarsOfType, tyVarsOfTypes, namesOfType, typeKind,
	addFreeTyVars,

	substTy, fullSubstTy, substTyVar,
	substFlexiTy, substFlexiTheta,

	showTypeCategory
    ) where

#include "HsVersions.h"

import {-# SOURCE #-}	DataCon( DataCon )

-- friends:
import Var	( Id, TyVar, GenTyVar, IdOrTyVar,
		  removeTyVarFlexi, 
		  tyVarKind, isId, idType
		)
import VarEnv
import VarSet

import Name	( NamedThing(..), Provenance(..), ExportFlag(..),
		  mkWiredInTyConName, mkGlobalName, varOcc
		)
import NameSet
import Class	( classTyCon, Class )
import TyCon	( TyCon, Boxity(..),
		  mkFunTyCon, mkKindCon, superKindCon,
		  matchesTyCon, isUnboxedTupleTyCon, isUnLiftedTyCon,
		  isFunTyCon, isEnumerationTyCon, 
		  isTupleTyCon, maybeTyConSingleCon,
		  isPrimTyCon, isAlgTyCon, isSynTyCon, tyConArity,
		  tyConKind, tyConDataCons, getSynTyConDefn, 
		  tyConPrimRep, tyConClass_maybe
		)

-- others
import BasicTypes 	( Unused )
import SrcLoc		( mkBuiltinSrcLoc )
import PrelMods		( pREL_GHC )
import Maybes		( maybeToBool )
import PrimRep		( PrimRep(..), isFollowableRep )
import Unique		-- quite a few *Keys
import Util		( thenCmp )
import Outputable

\end{code}

%************************************************************************
%*									*
\subsection{Type Classifications}
%*									*
%************************************************************************

A type is

	*unboxed*	iff its representation is other than a pointer
			Unboxed types cannot instantiate a type variable
			Unboxed types are always unlifted.

	*lifted*	A type is lifted iff it has bottom as an element.
			Closures always have lifted types:  i.e. any
			let-bound identifier in Core must have a lifted
			type.  Operationally, a lifted object is one that
			can be entered.
			(NOTE: previously "pointed").			

	*algebraic*	A type with one or more constructors.  An algebraic
			type is one that can be deconstructed with a case
			expression.  *NOT* the same as lifted types, 
			because we also include unboxed tuples in this
			classification.

	*primitive*	iff it is a built-in type that can't be expressed
			in Haskell.

Currently, all primitive types are unlifted, but that's not necessarily
the case.  (E.g. Int could be primitive.)

Some primitive types are unboxed, such as Int#, whereas some are boxed
but unlifted (such as ByteArray#).  The only primitive types that we
classify as algebraic are the unboxed tuples.

examples of type classifications:

Type		primitive	boxed		lifted		algebraic    
-----------------------------------------------------------------------------
Int#,		Yes		No		No		No
ByteArray#	Yes		Yes		No		No
(# a, b #)	Yes		No		No		Yes
(  a, b  )	No		Yes		Yes		Yes
[a]		No		Yes		Yes		Yes

%************************************************************************
%*									*
\subsection{The data type}
%*									*
%************************************************************************


\begin{code}
type Type  = GenType Unused	-- Used after typechecker

type GenKind flexi = GenType flexi
type Kind  = Type

type TyVarSubst 	 = TyVarEnv Type
type GenTyVarSubst flexi = TyVarEnv (GenType flexi) 

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

  | NoteTy 			-- Saturated application of a type synonym
	(TyNote flexi)
	(GenType flexi)		-- The expanded version

  | ForAllTy
	(GenTyVar flexi)
	(GenType flexi)		-- TypeKind

data TyNote flexi
  = SynNote (GenType flexi)	-- The unexpanded version of the type synonym; always a TyConApp
  | FTVNote (GenTyVarSet flexi)	-- The free type variables of the noted expression
\end{code}


%************************************************************************
%*									*
\subsection{Wired-in type constructors
%*									*
%************************************************************************

We define a few wired-in type constructors here to avoid module knots

\begin{code}
funTyConName = mkWiredInTyConName funTyConKey pREL_GHC SLIT("->") funTyCon
funTyCon = mkFunTyCon funTyConName (mkArrowKinds [boxedTypeKind, boxedTypeKind] boxedTypeKind)
\end{code}

\begin{code}
mk_kind_name key str = mkGlobalName key pREL_GHC (varOcc str)
				  (LocalDef mkBuiltinSrcLoc NotExported)
	-- mk_kind_name is a bit of a hack
	-- The LocalDef means that we print the name without
	-- a qualifier, which is what we want for these kinds.

boxedKindConName = mk_kind_name boxedKindConKey SLIT("*")
boxedKindCon     = mkKindCon boxedKindConName superKind Boxed

unboxedKindConName = mk_kind_name unboxedKindConKey SLIT("*#")
unboxedKindCon     = mkKindCon unboxedKindConName superKind Unboxed

openKindConName = mk_kind_name openKindConKey SLIT("*?")
openKindCon     = mkKindCon openKindConName superKind Open
\end{code}


%************************************************************************
%*									*
\subsection{Kinds}
%*									*
%************************************************************************

\begin{code}
superKind :: GenKind flexi	-- Box, the type of all kinds
superKind = TyConApp superKindCon []

boxedTypeKind, unboxedTypeKind, openTypeKind :: GenKind flexi
boxedTypeKind   = TyConApp boxedKindCon   []
unboxedTypeKind = TyConApp unboxedKindCon []
openTypeKind	= TyConApp openKindCon    []

mkArrowKind :: GenKind flexi -> GenKind flexi -> GenKind flexi
mkArrowKind = FunTy

mkArrowKinds :: [GenKind flexi] -> GenKind flexi -> GenKind flexi
mkArrowKinds arg_kinds result_kind = foldr FunTy result_kind arg_kinds
\end{code}

\begin{code}
hasMoreBoxityInfo :: GenKind flexi -> GenKind flexi -> Bool

(NoteTy _ k1) `hasMoreBoxityInfo` k2 = k1 `hasMoreBoxityInfo` k2
k1 `hasMoreBoxityInfo` (NoteTy _ k2) = k1 `hasMoreBoxityInfo` k2

(TyConApp kc1 ts1) `hasMoreBoxityInfo` (TyConApp kc2 ts2) 
  = ASSERT( null ts1 && null ts2 )
    kc2 `matchesTyCon` kc1	-- NB the reversal of arguments

kind1@(FunTy _ _) `hasMoreBoxityInfo` kind2@(FunTy _ _)
  = ASSERT( kind1 == kind2 )
    True
	-- The two kinds can be arrow kinds; for example when unifying
	-- (m1 Int) and (m2 Int) we end up unifying m1 and m2, which should
	-- have the same kind.

-- Other cases are impossible
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
getTyVar msg (NoteTy _ t) = getTyVar msg t
getTyVar msg other	  = panic ("getTyVar: " ++ msg)

getTyVar_maybe :: GenType flexi -> Maybe (GenTyVar flexi)
getTyVar_maybe (TyVarTy tv) = Just tv
getTyVar_maybe (NoteTy _ t) = getTyVar_maybe t
getTyVar_maybe other	    = Nothing

isTyVarTy :: GenType flexi -> Bool
isTyVarTy (TyVarTy tv)  = True
isTyVarTy (NoteTy _ ty) = isTyVarTy ty
isTyVarTy other         = False
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
    mk_app (NoteTy _ ty1)    = mk_app ty1
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
    mk_app (NoteTy _ ty1)    = mk_app ty1
    mk_app (TyConApp tc tys) = mkTyConApp tc (tys ++ orig_tys2)
    mk_app ty1		     = foldl AppTy orig_ty1 orig_tys2

splitAppTy_maybe :: GenType flexi -> Maybe (GenType flexi, GenType flexi)
splitAppTy_maybe (FunTy ty1 ty2)   = Just (TyConApp funTyCon [ty1], ty2)
splitAppTy_maybe (AppTy ty1 ty2)   = Just (ty1, ty2)
splitAppTy_maybe (NoteTy _ ty)     = splitAppTy_maybe ty
splitAppTy_maybe (TyConApp tc [])  = Nothing
splitAppTy_maybe (TyConApp tc tys) = split tys []
			    where
			       split [ty2]    acc = Just (TyConApp tc (reverse acc), ty2)
			       split (ty:tys) acc = split tys (ty:acc)

splitAppTy_maybe other	     	  = Nothing

splitAppTy :: GenType flexi -> (GenType flexi, GenType flexi)
splitAppTy ty = case splitAppTy_maybe ty of
			Just pr -> pr
			Nothing -> panic "splitAppTy"

splitAppTys :: GenType flexi -> (GenType flexi, [GenType flexi])
splitAppTys ty = split ty ty []
  where
    split orig_ty (AppTy ty arg)        args = split ty ty (arg:args)
    split orig_ty (NoteTy _ ty)         args = split orig_ty ty args
    split orig_ty (FunTy ty1 ty2)       args = ASSERT( null args )
					       (TyConApp funTyCon [], [ty1,ty2])
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
splitFunTy_maybe (NoteTy _ ty)   = splitFunTy_maybe ty
splitFunTy_maybe other	         = Nothing


splitFunTys :: GenType flexi -> ([GenType flexi], GenType flexi)
splitFunTys ty = split [] ty ty
  where
    split args orig_ty (FunTy arg res) = split (arg:args) res res
    split args orig_ty (NoteTy _ ty)   = split args orig_ty ty
    split args orig_ty ty              = (reverse args, orig_ty)

funResultTy :: GenType flexi -> GenType flexi
funResultTy (FunTy arg res) = res
funResultTy (NoteTy _ ty)   = funResultTy ty
funResultTy ty		    = ty
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
splitTyConApp_maybe (FunTy arg res)   = Just (funTyCon, [arg,res])
splitTyConApp_maybe (NoteTy _ ty)     = splitTyConApp_maybe ty
splitTyConApp_maybe other	      = Nothing

-- splitAlgTyConApp_maybe looks for 
--	*saturated* applications of *algebraic* data types
-- "Algebraic" => newtype, data type, or dictionary (not function types)
-- We return the constructors too.

splitAlgTyConApp_maybe :: GenType flexi -> Maybe (TyCon, [GenType flexi], [DataCon])
splitAlgTyConApp_maybe (TyConApp tc tys) 
  | isAlgTyCon tc &&
    tyConArity tc == length tys      = Just (tc, tys, tyConDataCons tc)
splitAlgTyConApp_maybe (NoteTy _ ty) = splitAlgTyConApp_maybe ty
splitAlgTyConApp_maybe other	     = Nothing

splitAlgTyConApp :: GenType flexi -> (TyCon, [GenType flexi], [DataCon])
	-- Here the "algebraic" property is an *assertion*
splitAlgTyConApp (TyConApp tc tys) = ASSERT( isAlgTyCon tc && tyConArity tc == length tys )
	      			     (tc, tys, tyConDataCons tc)
splitAlgTyConApp (NoteTy _ ty)     = splitAlgTyConApp ty
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

splitDictTy_maybe (NoteTy _ ty)	= splitDictTy_maybe ty
splitDictTy_maybe other		= Nothing

isDictTy :: GenType flexi -> Bool
	-- This version is slightly more efficient than (maybeToBool . splitDictTy)
isDictTy (TyConApp tc tys) 
  |  maybeToBool (tyConClass_maybe tc)
  && tyConArity tc == length tys
  = True
isDictTy (NoteTy _ ty)	= isDictTy ty
isDictTy other		= False
\end{code}


---------------------------------------------------------------------
				SynTy
				~~~~~

\begin{code}
mkSynTy syn_tycon tys
  = ASSERT(isSynTyCon syn_tycon)
    NoteTy (SynNote (TyConApp syn_tycon tys))
	   (substFlexiTy (zipVarEnv tyvars tys) body)
		-- The "flexi" is needed so we can get a TcType from a synonym
  where
    (tyvars, body) = getSynTyConDefn syn_tycon

isSynTy (NoteTy (SynNote _) _) = True
isSynTy other                  = False
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
splitForAllTy_maybe (NoteTy _ ty)       = splitForAllTy_maybe ty
splitForAllTy_maybe (ForAllTy tyvar ty) = Just(tyvar, ty)
splitForAllTy_maybe _		        = Nothing

isForAllTy :: GenType flexi -> Bool
isForAllTy (NoteTy _ ty)       = isForAllTy ty
isForAllTy (ForAllTy tyvar ty) = True
isForAllTy _		     = False

splitForAllTys :: GenType flexi -> ([GenTyVar flexi], GenType flexi)
splitForAllTys ty = split ty ty []
   where
     split orig_ty (ForAllTy tv ty) tvs = split ty ty (tv:tvs)
     split orig_ty (NoteTy _ ty)    tvs = split orig_ty ty tvs
     split orig_ty t	            tvs = (reverse tvs, orig_ty)
\end{code}

@mkPiType@ makes a (->) type or a forall type, depending on whether
it is given a type variable or a term variable.

\begin{code}
mkPiType :: IdOrTyVar -> Type -> Type	-- The more polymorphic version doesn't work...
mkPiType v ty | isId v    = mkFunTy (idType v) ty
	      | otherwise = ForAllTy v ty
\end{code}

\begin{code}
applyTy :: GenType flexi -> GenType flexi -> GenType flexi
applyTy (NoteTy _ fun)   arg = applyTy fun arg
applyTy (ForAllTy tv ty) arg = substTy (mkVarEnv [(tv,arg)]) ty
applyTy other		 arg = panic "applyTy"

applyTys :: GenType flexi -> [GenType flexi] -> GenType flexi
applyTys fun_ty arg_tys
 = go [] fun_ty arg_tys
 where
   go env ty               []         = substTy (mkVarEnv env) ty
   go env (NoteTy _ fun)   args       = go env fun args
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
isTauTy (NoteTy _ ty)  	 = isTauTy ty
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
  split orig_ty (NoteTy _ ty) ts   = split orig_ty ty ts
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
-- typeKind is only ever used on Types, never Kinds
-- If it were used on Kinds, the typeKind of FunTy would not be boxedTypeKind;
-- yet at the type level functions are boxed even if neither argument nor
-- result are boxed.   This seems pretty fishy to me.

typeKind :: GenType flexi -> Kind

typeKind (TyVarTy tyvar) 	= tyVarKind tyvar
typeKind (TyConApp tycon tys)	= foldr (\_ k -> funResultTy k) (tyConKind tycon) tys
typeKind (NoteTy _ ty)		= typeKind ty
typeKind (FunTy fun arg)	= boxedTypeKind
typeKind (AppTy fun arg)	= funResultTy (typeKind fun)
typeKind (ForAllTy _ _)		= boxedTypeKind
\end{code}


---------------------------------------------------------------------
		Free variables of a type
		~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
tyVarsOfType :: GenType flexi -> GenTyVarSet flexi

tyVarsOfType (TyVarTy tv)		= unitVarSet tv
tyVarsOfType (TyConApp tycon tys)	= tyVarsOfTypes tys
tyVarsOfType (NoteTy (FTVNote tvs) ty2) = tvs
tyVarsOfType (NoteTy (SynNote ty1) ty2)	= tyVarsOfType ty1
tyVarsOfType (FunTy arg res)		= tyVarsOfType arg `unionVarSet` tyVarsOfType res
tyVarsOfType (AppTy fun arg)		= tyVarsOfType fun `unionVarSet` tyVarsOfType arg
tyVarsOfType (ForAllTy tyvar ty)	= tyVarsOfType ty `minusVarSet` unitVarSet tyvar

tyVarsOfTypes :: [GenType flexi] -> GenTyVarSet flexi
tyVarsOfTypes tys = foldr (unionVarSet.tyVarsOfType) emptyVarSet tys

-- Add a Note with the free tyvars to the top of the type
addFreeTyVars :: GenType flexi -> GenType flexi
addFreeTyVars ty@(NoteTy (FTVNote _) _) = ty
addFreeTyVars ty			= NoteTy (FTVNote (tyVarsOfType ty)) ty

-- Find the free names of a type, including the type constructors and classes it mentions
namesOfType :: GenType flexi -> NameSet
namesOfType (TyVarTy tv)		= unitNameSet (getName tv)
namesOfType (TyConApp tycon tys)	= unitNameSet (getName tycon) `unionNameSets`
					  namesOfTypes tys
namesOfType (NoteTy (SynNote ty1) ty2)	= namesOfType ty1
namesOfType (NoteTy other_note    ty2)	= namesOfType ty2
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

@substTy@ applies a substitution to a type.  It deals correctly with name capture.

\begin{code}
substTy :: GenTyVarSubst flexi -> GenType flexi -> GenType flexi
substTy tenv ty = subst_ty tenv tset ty
	         where
		    tset = foldVarEnv (unionVarSet . tyVarsOfType) emptyVarSet tenv
				-- If ty doesn't have any for-alls, then this thunk
				-- will never be evaluated
\end{code}

@fullSubstTy@ is like @substTy@ except that it needs to be given a set
of in-scope type variables.  In exchange it's a bit more efficient, at least
if you happen to have that set lying around.

\begin{code}
fullSubstTy :: GenTyVarSubst flexi	  	-- Substitution to apply
            -> GenTyVarSet flexi		-- Superset of the free tyvars of
						-- the range of the tyvar env
            -> GenType flexi  -> GenType flexi
-- ASSUMPTION: The substitution is idempotent.
-- Equivalently: No tyvar is both in scope, and in the domain of the substitution.
fullSubstTy tenv tset ty | isEmptyVarEnv tenv = ty
		         | otherwise	      = subst_ty tenv tset ty

-- subst_ty does the business
subst_ty tenv tset ty
   = go ty
  where
    go (TyConApp tc tys)	   = TyConApp tc (map go tys)
    go (NoteTy (SynNote ty1) ty2)  = NoteTy (SynNote (go ty1)) (go ty2)
    go (NoteTy (FTVNote _) ty2)    = go ty2		-- Discard the free tyvar note
    go (FunTy arg res)   	   = FunTy (go arg) (go res)
    go (AppTy fun arg)   	   = mkAppTy (go fun) (go arg)
    go ty@(TyVarTy tv)   	   = case (lookupVarEnv tenv tv) of
	       			      Nothing  -> ty
       				      Just ty' -> ty'
    go (ForAllTy tv ty)		   = case substTyVar tenv tset tv of
					(tenv', tset', tv') -> ForAllTy tv' (subst_ty tenv' tset' ty)

substTyVar ::  GenTyVarSubst flexi -> GenTyVarSet flexi -> GenTyVar flexi
	   -> (GenTyVarSubst flexi,   GenTyVarSet flexi,   GenTyVar flexi)

substTyVar tenv tset tv
  | not (tv `elemVarSet` tset)	-- No need to clone
				-- But must delete from substitution
  = (tenv `delVarEnv` tv, tset `extendVarSet` tv, tv)

  | otherwise	-- The forall's variable is in scope so
		-- we'd better rename it away from the in-scope variables
		-- Extending the substitution to do this renaming also
		-- has the (correct) effect of discarding any existing
		-- substitution for that variable
  = (extendVarEnv tenv tv (TyVarTy tv'), tset `extendVarSet` tv', tv')
  where
     tv' = uniqAway tset tv
\end{code}


@substFlexiTy@ applies a substitution to a (GenType flexi1) returning
a (GenType flexi2).  Note that we convert from one flexi status to another.

Two assumptions, for (substFlexiTy env ty)
	(a) the substitution, env, must cover all free tyvars of the type, ty
	(b) the free vars of the range of the substitution must be
		different than any of the forall'd variables in the type, ty

The latter assumption is reasonable because, after all, ty has a different
type to the range of the substitution.

\begin{code}
substFlexiTy :: GenTyVarSubst flexi2 -> GenType flexi1 -> GenType flexi2
substFlexiTy env ty = go ty
  where
    go (TyVarTy tv)      	  = case lookupVarEnv env tv of
					Just ty -> ty
                                        Nothing -> pprPanic "substFlexiTy" (ppr tv)
    go (TyConApp tc tys) 	  = TyConApp tc (map go tys)
    go (NoteTy (SynNote ty1) ty2) = NoteTy (SynNote (go ty1)) (go ty2)
    go (NoteTy (FTVNote _)   ty2) = go ty2	-- Discard free tyvar note
    go (FunTy arg res)	          = FunTy (go arg) (go res)
    go (AppTy fun arg)	 	  = mkAppTy (go fun) (go arg)
    go (ForAllTy tv ty)  	  = ForAllTy tv' (substFlexiTy env' ty)
				  where
				    tv' = removeTyVarFlexi tv
				    env' = extendVarEnv env tv (TyVarTy tv')

substFlexiTheta :: GenTyVarSubst flexi2 -> [(Class, [GenType flexi1])]
					-> [(Class, [GenType flexi2])]
substFlexiTheta env theta = [(clas, map (substFlexiTy env) tys) | (clas,tys) <- theta]
\end{code}


%************************************************************************
%*									*
\subsection{Boxedness and liftedness}
%*									*
%************************************************************************

\begin{code}
isUnboxedType :: GenType flexi -> Bool
isUnboxedType ty = not (isFollowableRep (typePrimRep ty))

isUnLiftedType :: GenType flexi -> Bool
isUnLiftedType ty = case splitTyConApp_maybe ty of
			   Just (tc, ty_args) -> isUnLiftedTyCon tc
			   other	      -> False

isUnboxedTupleType :: GenType flexi -> Bool
isUnboxedTupleType ty = case splitTyConApp_maybe ty of
			   Just (tc, ty_args) -> isUnboxedTupleTyCon tc
			   other	      -> False

isAlgType :: GenType flexi -> Bool
isAlgType ty = case splitTyConApp_maybe ty of
			Just (tc, ty_args) -> isAlgTyCon tc
			other		   -> False

typePrimRep :: GenType flexi -> PrimRep
typePrimRep ty = case splitTyConApp_maybe ty of
		   Just (tc, ty_args) -> tyConPrimRep tc
		   other	      -> PtrRep
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
  = cmp emptyVarEnv ty1 ty2
  where
  -- The "env" maps type variables in ty1 to type variables in ty2
  -- So when comparing for-alls.. (forall tv1 . t1) (forall tv2 . t2)
  -- we in effect substitute tv2 for tv1 in t1 before continuing
    lookup env tv1 = case lookupVarEnv env tv1 of
			  Just tv2 -> tv2
			  Nothing  -> tv1

    -- Get rid of NoteTy
    cmp env (NoteTy _ ty1) ty2 = cmp env ty1 ty2
    cmp env ty1 (NoteTy _ ty2) = cmp env ty1 ty2
    
    -- Deal with equal constructors
    cmp env (TyVarTy tv1) (TyVarTy tv2) = lookup env tv1 `compare` tv2
    cmp env (AppTy f1 a1) (AppTy f2 a2) = cmp env f1 f2 `thenCmp` cmp env a1 a2
    cmp env (FunTy f1 a1) (FunTy f2 a2) = cmp env f1 f2 `thenCmp` cmp env a1 a2
    cmp env (TyConApp tc1 tys1) (TyConApp tc2 tys2) = (tc1 `compare` tc2) `thenCmp` (cmps env tys1 tys2)
    cmp env (ForAllTy tv1 t1)   (ForAllTy tv2 t2)   = cmp (extendVarEnv env tv1 tv2) t1 t2
    
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
          let utc = getUnique tycon in
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
