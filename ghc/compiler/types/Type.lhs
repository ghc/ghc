%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[Type]{Type}

\begin{code}
module Type (
	Type(..), TyNote(..), UsageAnn(..),		-- Representation visible to friends
	Kind, TyVarSubst,

	superKind, superBoxity,				-- :: SuperKind

	boxedKind,					-- :: Kind :: BX
	anyBoxKind,					-- :: Kind :: BX
	typeCon,					-- :: KindCon :: BX -> KX
	anyBoxCon,					-- :: KindCon :: BX

	boxedTypeKind, unboxedTypeKind, openTypeKind, 	-- Kind :: superKind

	mkArrowKind, mkArrowKinds, hasMoreBoxityInfo,

	funTyCon,

	mkTyVarTy, mkTyVarTys, getTyVar, getTyVar_maybe, isTyVarTy,

	mkAppTy, mkAppTys, splitAppTy, splitAppTys, splitAppTy_maybe,

	mkFunTy, mkFunTys, splitFunTy_maybe, splitFunTys, funResultTy,
	zipFunTys,

	mkTyConApp, mkTyConTy, splitTyConApp_maybe,
	splitAlgTyConApp_maybe, splitAlgTyConApp,
	mkDictTy, splitDictTy_maybe, isDictTy,

	mkSynTy, isSynTy, deNoteType,

        mkUsgTy, isUsgTy{- dont use -}, isNotUsgTy, splitUsgTy, unUsgTy, tyUsg,

	mkForAllTy, mkForAllTys, splitForAllTy_maybe, splitForAllTys, 
	applyTy, applyTys, isForAllTy,
	mkPiType,

	TauType, RhoType, SigmaType, ThetaType,
	isTauTy,
	mkRhoTy, splitRhoTy,
	mkSigmaTy, splitSigmaTy,

	-- Lifting and boxity
	isUnLiftedType, isUnboxedType, isUnboxedTupleType, isAlgType, isDataType,
	typePrimRep,

	-- Free variables
	tyVarsOfType, tyVarsOfTypes, namesOfType, typeKind,
	addFreeTyVars,

	-- Substitution
	substTy, substTheta, fullSubstTy, substTyVar,
	substTopTy, substTopTheta,

	-- Tidying up for printing
	tidyType,     tidyTypes,
	tidyOpenType, tidyOpenTypes,
	tidyTyVar,    tidyTyVars,
	tidyTopType
    ) where

#include "HsVersions.h"

import {-# SOURCE #-}	DataCon( DataCon )
import {-# SOURCE #-}	PprType( pprType )	-- Only called in debug messages

-- friends:
import Var	( Id, TyVar, IdOrTyVar, UVar,
		  tyVarKind, tyVarName, isId, idType, setTyVarName, setVarOcc
		)
import VarEnv
import VarSet

import Name	( NamedThing(..), Provenance(..), ExportFlag(..),
		  mkWiredInTyConName, mkGlobalName, mkLocalName, mkKindOccFS, tcName,
		  tidyOccName, TidyOccEnv
		)
import NameSet
import Class	( classTyCon, Class )
import TyCon	( TyCon, KindCon, 
		  mkFunTyCon, mkKindCon, mkSuperKindCon,
		  matchesTyCon, isUnboxedTupleTyCon, isUnLiftedTyCon,
		  isFunTyCon, isDataTyCon,
		  isAlgTyCon, isSynTyCon, tyConArity,
		  tyConKind, tyConDataCons, getSynTyConDefn, 
		  tyConPrimRep, tyConClass_maybe
		)

-- others
import BasicTypes 	( Unused )
import SrcLoc		( mkBuiltinSrcLoc, noSrcLoc )
import PrelMods		( pREL_GHC )
import Maybes		( maybeToBool )
import PrimRep		( PrimRep(..), isFollowableRep )
import Unique		-- quite a few *Keys
import Util		( thenCmp, mapAccumL, seqList, ($!) )
import Outputable

\end{code}

%************************************************************************
%*									*
\subsection{Type Classifications}
%*									*
%************************************************************************

A type is

	*unboxed*	iff its representation is other than a pointer
			Unboxed types cannot instantiate a type variable.
			Unboxed types are always unlifted.

	*lifted*	A type is lifted iff it has bottom as an element.
			Closures always have lifted types:  i.e. any
			let-bound identifier in Core must have a lifted
			type.  Operationally, a lifted object is one that
			can be entered.
			(NOTE: previously "pointed").			

	*algebraic*	A type with one or more constructors, whether declared
			with "data" or "newtype".   
			An algebraic type is one that can be deconstructed
			with a case expression.  
			*NOT* the same as lifted types,  because we also 
			include unboxed tuples in this classification.

	*data*		A type declared with "data".  Also boxed tuples.

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
type SuperKind = Type
type Kind      = Type

type TyVarSubst 	 = TyVarEnv Type

data Type
  = TyVarTy TyVar

  | AppTy
	Type		-- Function is *not* a TyConApp
	Type

  | TyConApp			-- Application of a TyCon
	TyCon			-- *Invariant* saturated appliations of FunTyCon and
				-- 	synonyms have their own constructors, below.
	[Type]		-- Might not be saturated.

  | FunTy			-- Special case of TyConApp: TyConApp FunTyCon [t1,t2]
	Type
	Type

  | NoteTy 			-- Saturated application of a type synonym
	TyNote
	Type		-- The expanded version

  | ForAllTy
	TyVar
	Type		-- TypeKind

data TyNote
  = SynNote Type	-- The unexpanded version of the type synonym; always a TyConApp
  | FTVNote TyVarSet	-- The free type variables of the noted expression
  | UsgNote UsageAnn    -- The usage annotation at this node

data UsageAnn
  = UsOnce		-- Used at most once
  | UsMany		-- Used possibly many times (no info; this annotation can be omitted)
  | UsVar UVar		-- Annotation is variable (should only happen inside analysis)
\end{code}


%************************************************************************
%*									*
\subsection{Kinds}
%*									*
%************************************************************************

Kinds
~~~~~
k::K = Type bx
     | k -> k
     | kv

kv :: KX is a kind variable

Type :: BX -> KX

bx::BX = Boxed 
      |  Unboxed
      |  AnyBox		-- Used *only* for special built-in things
			-- like error :: forall (a::*?). String -> a
			-- Here, the 'a' can be instantiated to a boxed or
			-- unboxed type.
      |  bv

bxv :: BX is a boxity variable

sk = KX		-- A kind
   | BX		-- A boxity
   | sk -> sk	-- In ptic (BX -> KX)

\begin{code}
mk_kind_name key str = mkGlobalName key pREL_GHC (mkKindOccFS tcName str)
				    (LocalDef mkBuiltinSrcLoc NotExported)
	-- mk_kind_name is a bit of a hack
	-- The LocalDef means that we print the name without
	-- a qualifier, which is what we want for these kinds.
	-- It's used for both Kinds and Boxities
\end{code}

Define KX, BX.

\begin{code}
superKind :: SuperKind 		-- KX, the type of all kinds
superKindName = mk_kind_name kindConKey SLIT("KX")
superKind = TyConApp (mkSuperKindCon superKindName) []

superBoxity :: SuperKind		-- BX, the type of all boxities
superBoxityName = mk_kind_name boxityConKey SLIT("BX")
superBoxity = TyConApp (mkSuperKindCon superBoxityName) []
\end{code}

Define Boxed, Unboxed, AnyBox

\begin{code}
boxedKind, unboxedKind, anyBoxKind :: Kind	-- Of superkind superBoxity

boxedConName = mk_kind_name boxedConKey SLIT("*")
boxedKind    = TyConApp (mkKindCon boxedConName superBoxity) []

unboxedConName = mk_kind_name unboxedConKey SLIT("#")
unboxedKind    = TyConApp (mkKindCon unboxedConName superBoxity) []

anyBoxConName = mk_kind_name anyBoxConKey SLIT("?")
anyBoxCon     = mkKindCon anyBoxConName superBoxity	-- A kind of wild card
anyBoxKind    = TyConApp anyBoxCon []
\end{code}

Define Type

\begin{code}
typeCon :: KindCon
typeConName = mk_kind_name typeConKey SLIT("Type")
typeCon     = mkKindCon typeConName (superBoxity `FunTy` superKind)
\end{code}

Define (Type Boxed), (Type Unboxed), (Type AnyBox)

\begin{code}
boxedTypeKind, unboxedTypeKind, openTypeKind :: Kind
boxedTypeKind   = TyConApp typeCon [boxedKind]
unboxedTypeKind = TyConApp typeCon [unboxedKind]
openTypeKind	= TyConApp typeCon [anyBoxKind]

mkArrowKind :: Kind -> Kind -> Kind
mkArrowKind k1 k2 = k1 `FunTy` k2

mkArrowKinds :: [Kind] -> Kind -> Kind
mkArrowKinds arg_kinds result_kind = foldr mkArrowKind result_kind arg_kinds
\end{code}

\begin{code}
hasMoreBoxityInfo :: Kind -> Kind -> Bool
hasMoreBoxityInfo k1 k2
  | k2 == openTypeKind = ASSERT( is_type_kind k1) True
  | otherwise	       = k1 == k2
  where
	-- Returns true for things of form (Type x)
    is_type_kind k = case splitTyConApp_maybe k of
			Just (tc,[_]) -> tc == typeCon
			Nothing	      -> False
\end{code}


%************************************************************************
%*									*
\subsection{Wired-in type constructors
%*									*
%************************************************************************

We define a few wired-in type constructors here to avoid module knots

\begin{code}
funTyConName = mkWiredInTyConName funTyConKey pREL_GHC SLIT("(->)") funTyCon
funTyCon = mkFunTyCon funTyConName (mkArrowKinds [boxedTypeKind, boxedTypeKind] boxedTypeKind)
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
getTyVar msg (TyVarTy tv) = tv
getTyVar msg (NoteTy _ t) = getTyVar msg t
getTyVar msg other	  = panic ("getTyVar: " ++ msg)

getTyVar_maybe :: Type -> Maybe TyVar
getTyVar_maybe (TyVarTy tv) = Just tv
getTyVar_maybe (NoteTy _ t) = getTyVar_maybe t
getTyVar_maybe other	    = Nothing

isTyVarTy :: Type -> Bool
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
mkAppTy orig_ty1 orig_ty2 = ASSERT2( isNotUsgTy orig_ty1 && isNotUsgTy orig_ty2, pprType orig_ty1 <+> text "to" <+> pprType orig_ty2 )
                            mk_app orig_ty1
  where
    mk_app (NoteTy _ ty1)    = mk_app ty1
    mk_app (TyConApp tc tys) = mkTyConApp tc (tys ++ [orig_ty2])
    mk_app ty1		     = AppTy orig_ty1 orig_ty2

mkAppTys :: Type -> [Type] -> Type
mkAppTys orig_ty1 []	    = orig_ty1
	-- This check for an empty list of type arguments
	-- avoids the needless of a type synonym constructor.
	-- For example: mkAppTys Rational []
	--   returns to (Ratio Integer), which has needlessly lost
	--   the Rational part.
mkAppTys orig_ty1 orig_tys2 = ASSERT2( isNotUsgTy orig_ty1, pprType orig_ty1 )
                              mk_app orig_ty1
  where
    mk_app (NoteTy _ ty1)    = mk_app ty1
    mk_app (TyConApp tc tys) = mkTyConApp tc (tys ++ orig_tys2)
    mk_app ty1		     = ASSERT2( all isNotUsgTy orig_tys2, pprType orig_ty1 <+> text "to" <+> hsep (map pprType orig_tys2) )
                               foldl AppTy orig_ty1 orig_tys2

splitAppTy_maybe :: Type -> Maybe (Type, Type)
splitAppTy_maybe (FunTy ty1 ty2)   = Just (TyConApp funTyCon [ty1], ty2)
splitAppTy_maybe (AppTy ty1 ty2)   = Just (ty1, ty2)
splitAppTy_maybe (NoteTy _ ty)     = splitAppTy_maybe ty
splitAppTy_maybe (TyConApp tc [])  = Nothing
splitAppTy_maybe (TyConApp tc tys) = split tys []
			    where
			       split [ty2]    acc = Just (TyConApp tc (reverse acc), ty2)
			       split (ty:tys) acc = split tys (ty:acc)

splitAppTy_maybe other	     	  = Nothing

splitAppTy :: Type -> (Type, Type)
splitAppTy ty = case splitAppTy_maybe ty of
			Just pr -> pr
			Nothing -> panic "splitAppTy"

splitAppTys :: Type -> (Type, [Type])
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
mkFunTy :: Type -> Type -> Type
mkFunTy arg res = FunTy arg res

mkFunTys :: [Type] -> Type -> Type
mkFunTys tys ty = foldr FunTy ty tys

splitFunTy_maybe :: Type -> Maybe (Type, Type)
splitFunTy_maybe (FunTy arg res) = Just (arg, res)
splitFunTy_maybe (NoteTy _ ty)   = splitFunTy_maybe ty
splitFunTy_maybe other	         = Nothing


splitFunTys :: Type -> ([Type], Type)
splitFunTys ty = split [] ty ty
  where
    split args orig_ty (FunTy arg res) = split (arg:args) res res
    split args orig_ty (NoteTy _ ty)   = split args orig_ty ty
    split args orig_ty ty              = (reverse args, orig_ty)

zipFunTys :: Outputable a => [a] -> Type -> ([(a,Type)], Type)
zipFunTys orig_xs orig_ty = split [] orig_xs orig_ty orig_ty
  where
    split acc []     nty ty  	         = (reverse acc, nty)
    split acc (x:xs) nty (FunTy arg res) = split ((x,arg):acc) xs res res
    split acc xs     nty (NoteTy _ ty)   = split acc           xs nty ty
    split acc (x:xs) nty ty              = pprPanic "zipFunTys" (ppr orig_xs <+> pprType orig_ty)
    
funResultTy :: Type -> Type
funResultTy (FunTy arg res) = res
funResultTy (NoteTy _ ty)   = funResultTy ty
funResultTy ty		    = pprPanic "funResultTy" (pprType ty)
\end{code}


---------------------------------------------------------------------
				TyConApp
				~~~~~~~~

\begin{code}
mkTyConApp :: TyCon -> [Type] -> Type
mkTyConApp tycon tys
  | isFunTyCon tycon && length tys == 2
  = case tys of 
	(ty1:ty2:_) -> FunTy ty1 ty2

  | otherwise
  = ASSERT(not (isSynTyCon tycon))
    TyConApp tycon tys

mkTyConTy :: TyCon -> Type
mkTyConTy tycon = ASSERT( not (isSynTyCon tycon) ) 
		  TyConApp tycon []

-- splitTyConApp "looks through" synonyms, because they don't
-- mean a distinct type, but all other type-constructor applications
-- including functions are returned as Just ..

splitTyConApp_maybe :: Type -> Maybe (TyCon, [Type])
splitTyConApp_maybe (TyConApp tc tys) = Just (tc, tys)
splitTyConApp_maybe (FunTy arg res)   = Just (funTyCon, [arg,res])
splitTyConApp_maybe (NoteTy _ ty)     = splitTyConApp_maybe ty
splitTyConApp_maybe other	      = Nothing

-- splitAlgTyConApp_maybe looks for 
--	*saturated* applications of *algebraic* data types
-- "Algebraic" => newtype, data type, or dictionary (not function types)
-- We return the constructors too.

splitAlgTyConApp_maybe :: Type -> Maybe (TyCon, [Type], [DataCon])
splitAlgTyConApp_maybe (TyConApp tc tys) 
  | isAlgTyCon tc &&
    tyConArity tc == length tys      = Just (tc, tys, tyConDataCons tc)
splitAlgTyConApp_maybe (NoteTy _ ty) = splitAlgTyConApp_maybe ty
splitAlgTyConApp_maybe other	     = Nothing

splitAlgTyConApp :: Type -> (TyCon, [Type], [DataCon])
	-- Here the "algebraic" property is an *assertion*
splitAlgTyConApp (TyConApp tc tys) = ASSERT( isAlgTyCon tc && tyConArity tc == length tys )
	      			     (tc, tys, tyConDataCons tc)
splitAlgTyConApp (NoteTy _ ty)     = splitAlgTyConApp ty
\end{code}

"Dictionary" types are just ordinary data types, but you can
tell from the type constructor whether it's a dictionary or not.

\begin{code}
mkDictTy :: Class -> [Type] -> Type
mkDictTy clas tys = TyConApp (classTyCon clas) tys

splitDictTy_maybe :: Type -> Maybe (Class, [Type])
splitDictTy_maybe (TyConApp tc tys) 
  |  maybeToBool maybe_class
  && tyConArity tc == length tys = Just (clas, tys)
  where
     maybe_class = tyConClass_maybe tc
     Just clas   = maybe_class

splitDictTy_maybe (NoteTy _ ty)	= splitDictTy_maybe ty
splitDictTy_maybe other		= Nothing

isDictTy :: Type -> Bool
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
  = ASSERT( isSynTyCon syn_tycon )
    ASSERT( isNotUsgTy body )
    NoteTy (SynNote (TyConApp syn_tycon tys))
	   (substTopTy (zipVarEnv tyvars tys) body)
  where
    (tyvars, body) = getSynTyConDefn syn_tycon

isSynTy (NoteTy (SynNote _) _) = True
isSynTy other                  = False

deNoteType :: Type -> Type
	-- Sorry for the cute name
deNoteType ty@(TyVarTy tyvar)	= ty
deNoteType (TyConApp tycon tys) = TyConApp tycon (map deNoteType tys)
deNoteType (NoteTy _ ty)	= deNoteType ty
deNoteType (AppTy fun arg)	= AppTy (deNoteType fun) (deNoteType arg)
deNoteType (FunTy fun arg)	= FunTy (deNoteType fun) (deNoteType arg)
deNoteType (ForAllTy tv ty)	= ForAllTy tv (deNoteType ty)
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
				UsgNote
				~~~~~~~

NB: Invariant: if present, usage note is at the very top of the type.
This should be carefully preserved.

In some parts of the compiler, comments use the _Once Upon a
Polymorphic Type_ (POPL'99) usage of "sigma = usage-annotated type;
tau = un-usage-annotated type"; unfortunately this conflicts with the
rho/tau/theta/sigma usage in the rest of the compiler.
(KSW 1999-04)

\begin{code}
mkUsgTy :: UsageAnn -> Type -> Type
#ifndef USMANY
mkUsgTy UsMany ty = ASSERT2( isNotUsgTy ty, pprType ty )
                    ty
#endif
mkUsgTy usg    ty = ASSERT2( isNotUsgTy ty, pprType ty )
                    NoteTy (UsgNote usg) ty

-- The isUsgTy function is utterly useless if UsManys are omitted.
-- Be warned!  KSW 1999-04.
isUsgTy :: Type -> Bool
#ifndef USMANY
isUsgTy _ = True
#else
isUsgTy (NoteTy (UsgNote _) _) = True
isUsgTy other                  = False
#endif

-- The isNotUsgTy function may return a false True if UsManys are omitted;
-- in other words, A SSERT( isNotUsgTy ty ) may be useful but
-- A SSERT( not (isNotUsg ty) ) is asking for trouble.  KSW 1999-04.
isNotUsgTy :: Type -> Bool
isNotUsgTy (NoteTy (UsgNote _) _) = False
isNotUsgTy other                  = True

-- splitUsgTy_maybe is not exported, since it is meaningless if
-- UsManys are omitted.  It is used in several places in this module,
-- however.  KSW 1999-04.
splitUsgTy_maybe :: Type -> Maybe (UsageAnn,Type)
splitUsgTy_maybe (NoteTy (UsgNote usg) ty2) = ASSERT( isNotUsgTy ty2 )
                                              Just (usg,ty2)
splitUsgTy_maybe ty                         = Nothing

splitUsgTy :: Type -> (UsageAnn,Type)
splitUsgTy ty = case splitUsgTy_maybe ty of
                  Just ans -> ans
                  Nothing  -> 
#ifndef USMANY
                              (UsMany,ty)
#else
                              pprPanic "splitUsgTy: no usage annot:" $ pprType ty
#endif

tyUsg :: Type -> UsageAnn
tyUsg = fst . splitUsgTy

unUsgTy :: Type -> Type
-- strip outer usage annotation if present
unUsgTy ty = case splitUsgTy_maybe ty of
               Just (_,ty1) -> ASSERT2( isNotUsgTy ty1, pprType ty )
                               ty1
               Nothing      -> ty
\end{code}



---------------------------------------------------------------------
				ForAllTy
				~~~~~~~~

We need to be clever here with usage annotations; they need to be
lifted or lowered through the forall as appropriate.

\begin{code}
mkForAllTy :: TyVar -> Type -> Type
mkForAllTy tyvar ty = case splitUsgTy_maybe ty of
                        Just (usg,ty') -> NoteTy (UsgNote usg)
						 (ForAllTy tyvar ty')
                        Nothing        -> ForAllTy tyvar ty

mkForAllTys :: [TyVar] -> Type -> Type
mkForAllTys tyvars ty = case splitUsgTy_maybe ty of
                          Just (usg,ty') -> NoteTy (UsgNote usg)
						   (foldr ForAllTy ty' tyvars)
                          Nothing        -> foldr ForAllTy ty tyvars

splitForAllTy_maybe :: Type -> Maybe (TyVar, Type)
splitForAllTy_maybe ty = case splitUsgTy_maybe ty of
                           Just (usg,ty') -> do (tyvar,ty'') <- splitFAT_m ty'
					        return (tyvar, NoteTy (UsgNote usg) ty'')
			   Nothing        -> splitFAT_m ty
  where
    splitFAT_m (NoteTy _ ty)       = splitFAT_m ty
    splitFAT_m (ForAllTy tyvar ty) = Just(tyvar, ty)
    splitFAT_m _		   = Nothing

isForAllTy :: Type -> Bool
isForAllTy (NoteTy _ ty)       = isForAllTy ty
isForAllTy (ForAllTy tyvar ty) = True
isForAllTy _		     = False

splitForAllTys :: Type -> ([TyVar], Type)
splitForAllTys ty = case splitUsgTy_maybe ty of
                      Just (usg,ty') -> let (tvs,ty'') = split ty' ty' []
					in  (tvs, NoteTy (UsgNote usg) ty'')
		      Nothing        -> split ty ty []
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
	      | otherwise = mkForAllTy v ty
\end{code}

\begin{code}
applyTy :: Type -> Type -> Type
applyTy (NoteTy note@(UsgNote _) fun) arg = NoteTy note (applyTy fun arg)
applyTy (NoteTy _ fun)                arg = applyTy fun arg
applyTy (ForAllTy tv ty)              arg = ASSERT( isNotUsgTy arg )
                                            substTy (mkVarEnv [(tv,arg)]) ty
applyTy other		              arg = panic "applyTy"

applyTys :: Type -> [Type] -> Type
applyTys fun_ty arg_tys
 = go [] fun_ty arg_tys
 where
   go env ty               []         = substTy (mkVarEnv env) ty
   go env (NoteTy note@(UsgNote _) fun)
                           args       = NoteTy note (go env fun args)
   go env (NoteTy _ fun)   args       = go env fun args
   go env (ForAllTy tv ty) (arg:args) = ASSERT2( isNotUsgTy arg, vcat ((map pprType arg_tys) ++ [text "in application of" <+> pprType fun_ty]) )
                                        go ((tv,arg):env) ty args
   go env other            args       = panic "applyTys"
\end{code}

Note that we allow applications to be of usage-annotated- types, as an
extension: we handle them by lifting the annotation outside.  The
argument, however, must still be unannotated.

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
isTauTy :: Type -> Bool
isTauTy (TyVarTy v)      = True
isTauTy (TyConApp _ tys) = all isTauTy tys
isTauTy (AppTy a b)    	 = isTauTy a && isTauTy b
isTauTy (FunTy a b)  	 = isTauTy a && isTauTy b
isTauTy (NoteTy _ ty)  	 = isTauTy ty
isTauTy other	       	 = False
\end{code}

\begin{code}
mkRhoTy :: [(Class, [Type])] -> Type -> Type
mkRhoTy theta ty = foldr (\(c,t) r -> FunTy (mkDictTy c t) r) ty theta

splitRhoTy :: Type -> ([(Class, [Type])], Type)
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

splitSigmaTy :: Type -> ([TyVar], [(Class, [Type])], Type)
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
typeKind :: Type -> Kind

typeKind (TyVarTy tyvar)	= tyVarKind tyvar
typeKind (TyConApp tycon tys)	= foldr (\_ k -> funResultTy k) (tyConKind tycon) tys
typeKind (NoteTy _ ty)		= typeKind ty
typeKind (AppTy fun arg)	= funResultTy (typeKind fun)
typeKind (FunTy fun arg)	= typeKindF arg
typeKind (ForAllTy _ ty)	= typeKindF ty	-- We could make this a new kind polyTypeKind
						-- to prevent a forall type unifying with a 
						-- boxed type variable, but I didn't think it
						-- was worth it yet.

-- The complication is that a *function* is boxed even if
-- its *result* type is unboxed.  Seems wierd.

typeKindF :: Type -> Kind
typeKindF (NoteTy _ ty)   = typeKindF ty
typeKindF (FunTy _ ty)    = typeKindF ty
typeKindF (ForAllTy _ ty) = typeKindF ty
typeKindF other		  = fix_up (typeKind other)
  where
    fix_up (TyConApp kc _) | kc == typeCon = boxedTypeKind
		-- Functions at the type level are always boxed
    fix_up (NoteTy _ kind) = fix_up kind
    fix_up kind            = kind
\end{code}


---------------------------------------------------------------------
		Free variables of a type
		~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
tyVarsOfType :: Type -> TyVarSet

tyVarsOfType (TyVarTy tv)		= unitVarSet tv
tyVarsOfType (TyConApp tycon tys)	= tyVarsOfTypes tys
tyVarsOfType (NoteTy (FTVNote tvs) ty2) = tvs
tyVarsOfType (NoteTy (SynNote ty1) ty2)	= tyVarsOfType ty1
tyVarsOfType (NoteTy (UsgNote _) ty)	= tyVarsOfType ty
tyVarsOfType (FunTy arg res)		= tyVarsOfType arg `unionVarSet` tyVarsOfType res
tyVarsOfType (AppTy fun arg)		= tyVarsOfType fun `unionVarSet` tyVarsOfType arg
tyVarsOfType (ForAllTy tyvar ty)	= tyVarsOfType ty `minusVarSet` unitVarSet tyvar

tyVarsOfTypes :: [Type] -> TyVarSet
tyVarsOfTypes tys = foldr (unionVarSet.tyVarsOfType) emptyVarSet tys

-- Add a Note with the free tyvars to the top of the type
-- (but under a usage if there is one)
addFreeTyVars :: Type -> Type
addFreeTyVars (NoteTy note@(UsgNote _) ty) = NoteTy note (addFreeTyVars ty)
addFreeTyVars ty@(NoteTy (FTVNote _) _)    = ty
addFreeTyVars ty			   = NoteTy (FTVNote (tyVarsOfType ty)) ty

-- Find the free names of a type, including the type constructors and classes it mentions
namesOfType :: Type -> NameSet
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
substTy :: TyVarSubst -> Type -> Type
substTy tenv ty 
  | isEmptyVarEnv tenv = ty
  | otherwise	       = subst_ty tenv tset ty
  where
    tset = foldVarEnv (unionVarSet . tyVarsOfType) emptyVarSet tenv
		-- If ty doesn't have any for-alls, then this thunk
		-- will never be evaluated

substTheta :: TyVarSubst -> ThetaType -> ThetaType
substTheta tenv theta
  | isEmptyVarEnv tenv = theta
  | otherwise	       = [(clas, map (subst_ty tenv tset) tys) | (clas, tys) <- theta]
  where
    tset = foldVarEnv (unionVarSet . tyVarsOfType) emptyVarSet tenv
		-- If ty doesn't have any for-alls, then this thunk
		-- will never be evaluated

substTopTy :: TyVarSubst -> Type -> Type
substTopTy = substTy	-- Called when doing top-level substitutions.
			-- Here we expect that the free vars of the range of the
			-- substitution will be empty; but during typechecking I'm
			-- a bit dubious about that (mutable tyvars bouund to Int, say)
			-- So I've left it as substTy for the moment.  SLPJ Nov 98
substTopTheta = substTheta
\end{code}

@fullSubstTy@ is like @substTy@ except that it needs to be given a set
of in-scope type variables.  In exchange it's a bit more efficient, at least
if you happen to have that set lying around.

\begin{code}
fullSubstTy :: TyVarSubst 	  	-- Substitution to apply
            -> TyVarSet 		-- Superset of the free tyvars of
					-- the range of the tyvar env
            -> Type  -> Type
-- ASSUMPTION: The substitution is idempotent.
-- Equivalently: No tyvar is both in scope, and in the domain of the substitution.
fullSubstTy tenv tset ty | isEmptyVarEnv tenv = ty
		         | otherwise	      = subst_ty tenv tset ty

-- subst_ty does the business
subst_ty tenv tset ty
   = go ty
  where
    go (TyConApp tc tys)	   = let args = map go tys
				     in  args `seqList` TyConApp tc args
    go (NoteTy (SynNote ty1) ty2)  = NoteTy (SynNote $! (go ty1)) $! (go ty2)
    go (NoteTy (FTVNote _) ty2)    = go ty2		-- Discard the free tyvar note
    go (NoteTy (UsgNote usg) ty2)  = (NoteTy $! (UsgNote usg)) $! (go ty2)  -- Keep usage annot
    go (FunTy arg res)   	   = FunTy (go arg) (go res)
    go (AppTy fun arg)   	   = mkAppTy (go fun) (go arg)
    go ty@(TyVarTy tv)   	   = case (lookupVarEnv tenv tv) of
	       			      Nothing  -> ty
       				      Just ty' -> ty'
    go (ForAllTy tv ty)		   = case substTyVar tenv tset tv of
					(tenv', tset', tv') -> ForAllTy tv' $! (subst_ty tenv' tset' ty)

substTyVar ::  TyVarSubst -> TyVarSet -> TyVar
	   -> (TyVarSubst,   TyVarSet,   TyVar)

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


%************************************************************************
%*									*
\subsection{TidyType}
%*									*
%************************************************************************

tidyTy tidies up a type for printing in an error message, or in
an interface file.

It doesn't change the uniques at all, just the print names.

\begin{code}
tidyTyVar :: TidyEnv -> TyVar -> (TidyEnv, TyVar)
tidyTyVar env@(tidy_env, subst) tyvar
  = case lookupVarEnv subst tyvar of

	Just tyvar' -> 	-- Already substituted
		(env, tyvar')

	Nothing -> 	-- Make a new nice name for it

		case tidyOccName tidy_env (getOccName name) of
		    (tidy', occ') -> 	-- New occname reqd
				((tidy', subst'), tyvar')
			      where
				subst' = extendVarEnv subst tyvar tyvar'
				tyvar' = setTyVarName tyvar name'
				name'  = mkLocalName (getUnique name) occ' noSrcLoc
					-- Note: make a *user* tyvar, so it printes nicely
					-- Could extract src loc, but no need.
  where
    name = tyVarName tyvar

tidyTyVars env tyvars = mapAccumL tidyTyVar env tyvars

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
    go (AppTy fun arg)	    = (AppTy $! (go fun)) $! (go arg)
    go (FunTy fun arg)	    = (FunTy $! (go fun)) $! (go arg)
    go (ForAllTy tv ty)	    = ForAllTy tv' $! (tidyType env' ty)
			    where
			      (env', tv') = tidyTyVar env tv

    go_note (SynNote ty)        = SynNote $! (go ty)
    go_note note@(FTVNote ftvs) = note	-- No need to tidy the free tyvars
    go_note note@(UsgNote _)    = note  -- Usage annotation is already tidy

tidyTypes  env tys    = map (tidyType env) tys
\end{code}


@tidyOpenType@ grabs the free type varibles, tidies them
and then uses @tidyType@ to work over the type itself

\begin{code}
tidyOpenType :: TidyEnv -> Type -> (TidyEnv, Type)
tidyOpenType env ty
  = (env', tidyType env' ty)
  where
    env'         = foldl go env (varSetElems (tyVarsOfType ty))
    go env tyvar = fst (tidyTyVar env tyvar)

tidyOpenTypes :: TidyEnv -> [Type] -> (TidyEnv, [Type])
tidyOpenTypes env tys = mapAccumL tidyOpenType env tys

tidyTopType :: Type -> Type
tidyTopType ty = tidyType emptyTidyEnv ty
\end{code}


%************************************************************************
%*									*
\subsection{Boxedness and liftedness}
%*									*
%************************************************************************

\begin{code}
isUnboxedType :: Type -> Bool
isUnboxedType ty = not (isFollowableRep (typePrimRep ty))

isUnLiftedType :: Type -> Bool
isUnLiftedType ty = case splitTyConApp_maybe ty of
			   Just (tc, ty_args) -> isUnLiftedTyCon tc
			   other	      -> False

isUnboxedTupleType :: Type -> Bool
isUnboxedTupleType ty = case splitTyConApp_maybe ty of
			   Just (tc, ty_args) -> isUnboxedTupleTyCon tc
			   other	      -> False

-- Should only be applied to *types*; hence the assert
isAlgType :: Type -> Bool
isAlgType ty = case splitTyConApp_maybe ty of
			Just (tc, ty_args) -> ASSERT( length ty_args == tyConArity tc )
					      isAlgTyCon tc
			other		   -> False

-- Should only be applied to *types*; hence the assert
isDataType :: Type -> Bool
isDataType ty = case splitTyConApp_maybe ty of
			Just (tc, ty_args) -> ASSERT( length ty_args == tyConArity tc )
					      isDataTyCon tc
			other		   -> False

typePrimRep :: Type -> PrimRep
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
instance Eq Type where
  ty1 == ty2 = case ty1 `cmpTy` ty2 of { EQ -> True; other -> False }

instance Ord Type where
  compare ty1 ty2 = cmpTy ty1 ty2

cmpTy :: Type -> Type -> Ordering
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


