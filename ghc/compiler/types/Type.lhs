%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[Type]{Type - public interface}

\begin{code}
module Type (
        -- re-exports from TypeRep:
	Type,
	Kind, TyVarSubst,

	superKind, superBoxity,				-- :: SuperKind

	boxedKind,					-- :: Kind :: BX
	anyBoxKind,					-- :: Kind :: BX
	typeCon,					-- :: KindCon :: BX -> KX
	anyBoxCon,					-- :: KindCon :: BX

	boxedTypeKind, unboxedTypeKind, openTypeKind, 	-- Kind :: superKind

	mkArrowKind, mkArrowKinds, -- mentioned below: hasMoreBoxityInfo,

	funTyCon,

        -- exports from this module:
        hasMoreBoxityInfo,

	mkTyVarTy, mkTyVarTys, getTyVar, getTyVar_maybe, isTyVarTy,

	mkAppTy, mkAppTys, splitAppTy, splitAppTys, splitAppTy_maybe,

	mkFunTy, mkFunTys, splitFunTy, splitFunTy_maybe, splitFunTys, splitFunTysN,
	funResultTy, funArgTy, zipFunTys,

	mkTyConApp, mkTyConTy, splitTyConApp_maybe,
	splitAlgTyConApp_maybe, splitAlgTyConApp, 
	mkDictTy, mkDictTys, mkPredTy, splitPredTy_maybe, splitDictTy_maybe, isDictTy,

	mkSynTy, isSynTy, deNoteType, 

	repType, splitRepFunTys, splitNewType_maybe, typePrimRep,

        UsageAnn(..), mkUsgTy, isUsgTy{- dont use -}, isNotUsgTy, splitUsgTy, unUsgTy, tyUsg,
        mkUsForAllTy, mkUsForAllTys, splitUsForAllTys, substUsTy, 

	mkForAllTy, mkForAllTys, splitForAllTy_maybe, splitForAllTys, 
	applyTy, applyTys, mkPiType, hoistForAllTys,

	TauType, RhoType, SigmaType, PredType(..), ThetaType,
	ClassPred, ClassContext, mkClassPred,
	getClassTys_maybe, ipName_maybe, classesToPreds, classesOfPreds,
	isTauTy, mkRhoTy, splitRhoTy,
	mkSigmaTy, isSigmaTy, splitSigmaTy,

	-- Lifting and boxity
	isUnLiftedType, isUnboxedType, isUnboxedTupleType, isAlgType, isDataType, isNewType,

	-- Free variables
	tyVarsOfType, tyVarsOfTypes, tyVarsOfPred, tyVarsOfTheta,
	namesOfType, typeKind, addFreeTyVars,

	-- Tidying up for printing
	tidyType,     tidyTypes,
	tidyOpenType, tidyOpenTypes,
	tidyTyVar,    tidyTyVars,
	tidyTopType,

	-- Seq
	seqType, seqTypes

    ) where

#include "HsVersions.h"

-- We import the representation and primitive functions from TypeRep.
-- Many things are reexported, but not the representation!

import TypeRep

-- Other imports:

import {-# SOURCE #-}	DataCon( DataCon, dataConRepType )
import {-# SOURCE #-}	PprType( pprType, pprPred )	-- Only called in debug messages
import {-# SOURCE #-}   Subst  ( mkTyVarSubst, substTy )

-- friends:
import Var	( TyVar, Var, UVar,
		  tyVarKind, tyVarName, setTyVarName, isId, idType,
		)
import VarEnv
import VarSet

import Name	( Name, NamedThing(..), mkLocalName, tidyOccName
		)
import NameSet
import Class	( classTyCon, Class )
import TyCon	( TyCon,
		  isUnboxedTupleTyCon, isUnLiftedTyCon,
		  isFunTyCon, isDataTyCon, isNewTyCon, newTyConRep,
		  isAlgTyCon, isSynTyCon, tyConArity,
	          tyConKind, tyConDataCons, getSynTyConDefn,
		  tyConPrimRep, tyConClass_maybe
		)

-- others
import SrcLoc		( noSrcLoc )
import Maybes		( maybeToBool )
import PrimRep		( PrimRep(..), isFollowableRep )
import Unique		( Uniquable(..) )
import Util		( mapAccumL, seqList )
import Outputable
import UniqSet		( sizeUniqSet )		-- Should come via VarSet
\end{code}


%************************************************************************
%*									*
\subsection{Stuff to do with kinds.}
%*									*
%************************************************************************

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

splitFunTy :: Type -> (Type, Type)
splitFunTy (FunTy arg res) = (arg, res)
splitFunTy (NoteTy _ ty)   = splitFunTy ty

splitFunTy_maybe :: Type -> Maybe (Type, Type)
splitFunTy_maybe (FunTy arg res)	= Just (arg, res)
splitFunTy_maybe (NoteTy (IPNote _) ty)	= Nothing
splitFunTy_maybe (NoteTy _ ty)   	= splitFunTy_maybe ty
splitFunTy_maybe other	         	= Nothing

splitFunTys :: Type -> ([Type], Type)
splitFunTys ty = split [] ty ty
  where
    split args orig_ty (FunTy arg res) = split (arg:args) res res
    split args orig_ty (NoteTy (IPNote _) ty)
				       = (reverse args, orig_ty)
    split args orig_ty (NoteTy _ ty)   = split args orig_ty ty
    split args orig_ty ty              = (reverse args, orig_ty)

splitFunTysN :: String -> Int -> Type -> ([Type], Type)
splitFunTysN msg orig_n orig_ty = split orig_n [] orig_ty orig_ty
  where
    split 0 args syn_ty ty		= (reverse args, syn_ty) 
    split n args syn_ty (FunTy arg res) = split (n-1) (arg:args) res    res
    split n args syn_ty (NoteTy _ ty)   = split n     args       syn_ty ty
    split n args syn_ty ty              = pprPanic ("splitFunTysN: " ++ msg) (int orig_n <+> pprType orig_ty)

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

funArgTy :: Type -> Type
funArgTy (FunTy arg res) = arg
funArgTy (NoteTy _ ty)   = funArgTy ty
funArgTy ty		 = pprPanic "funArgTy" (pprType ty)
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
splitTyConApp_maybe (TyConApp tc tys)	   = Just (tc, tys)
splitTyConApp_maybe (FunTy arg res)	   = Just (funTyCon, [arg,res])
splitTyConApp_maybe (NoteTy (IPNote _) ty) = Nothing
splitTyConApp_maybe (NoteTy _ ty)	   = splitTyConApp_maybe ty
splitTyConApp_maybe other		   = Nothing

-- splitAlgTyConApp_maybe looks for 
--	*saturated* applications of *algebraic* data types
-- "Algebraic" => newtype, data type, or dictionary (not function types)
-- We return the constructors too.

splitAlgTyConApp_maybe :: Type -> Maybe (TyCon, [Type], [DataCon])
splitAlgTyConApp_maybe (TyConApp tc tys) 
  | isAlgTyCon tc && 
    tyConArity tc == length tys      = Just (tc, tys, tyConDataCons tc)
splitAlgTyConApp_maybe (NoteTy (IPNote _) ty)
				     = Nothing
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

mkDictTys :: ClassContext -> [Type]
mkDictTys cxt = [mkDictTy cls tys | (cls,tys) <- cxt]

mkPredTy :: PredType -> Type
mkPredTy (Class clas tys) = TyConApp (classTyCon clas) tys
mkPredTy (IParam n ty)    = NoteTy (IPNote n) ty

splitPredTy_maybe :: Type -> Maybe PredType
splitPredTy_maybe (TyConApp tc tys) 
  |  maybeToBool maybe_class
  && tyConArity tc == length tys = Just (Class clas tys)
  where
     maybe_class = tyConClass_maybe tc
     Just clas   = maybe_class

splitPredTy_maybe (NoteTy (IPNote n) ty)
				= Just (IParam n ty)
splitPredTy_maybe (NoteTy _ ty)	= splitPredTy_maybe ty
splitPredTy_maybe other		= Nothing

splitDictTy_maybe :: Type -> Maybe (Class, [Type])
splitDictTy_maybe ty
  = case splitPredTy_maybe ty of
    Just p  -> getClassTys_maybe p
    Nothing -> Nothing

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
    ASSERT( length tyvars == length tys )
    NoteTy (SynNote (TyConApp syn_tycon tys))
	   (substTy (mkTyVarSubst tyvars tys) body)
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


		Representation types
		~~~~~~~~~~~~~~~~~~~~

repType looks through 
	(a) for-alls, and
	(b) newtypes
	(c) synonyms
It's useful in the back end where we're not
interested in newtypes anymore.

\begin{code}
repType :: Type -> Type
repType (ForAllTy _ ty) = repType ty
repType (NoteTy   _ ty) = repType ty
repType ty	 	= case splitNewType_maybe ty of
			    Just ty' -> repType ty'	-- Still re-apply repType in case of for-all
			    Nothing  -> ty

splitRepFunTys :: Type -> ([Type], Type)
-- Like splitFunTys, but looks through newtypes and for-alls
splitRepFunTys ty = split [] (repType ty)
  where
    split args (FunTy arg res)  = split (arg:args) (repType res)
    split args ty               = (reverse args, ty)

typePrimRep :: Type -> PrimRep
typePrimRep ty = case repType ty of
		   TyConApp tc _ -> tyConPrimRep tc
		   FunTy _ _	 -> PtrRep
		   AppTy _ _	 -> PtrRep	-- ??
		   TyVarTy _	 -> PtrRep

splitNewType_maybe :: Type -> Maybe Type
-- Find the representation of a newtype, if it is one
-- Looks through multiple levels of newtype, but does not look through for-alls
splitNewType_maybe (NoteTy (IPNote _) ty)
				     = Nothing
splitNewType_maybe (NoteTy _ ty)     = splitNewType_maybe ty
splitNewType_maybe (TyConApp tc tys) = case newTyConRep tc of
					 Just rep_ty -> ASSERT( length tys == tyConArity tc )
						-- The assert should hold because repType should
						-- only be applied to *types* (of kind *)
							Just (applyTys rep_ty tys)
					 Nothing     -> Nothing
splitNewType_maybe other 	     = Nothing						
\end{code}



---------------------------------------------------------------------
				UsgNote
				~~~~~~~

NB: Invariant: if present, usage note is at the very top of the type.
This should be carefully preserved.

In some parts of the compiler, comments use the _Once Upon a
Polymorphic Type_ (POPL'99) usage of "rho = generalised
usage-annotated type; sigma = usage-annotated type; tau =
usage-annotated type except on top"; unfortunately this conflicts with
the rho/tau/theta/sigma usage in the rest of the compiler.  (KSW
1999-07)

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
isUsgTy (NoteTy (UsgForAll _) ty) = isUsgTy ty
isUsgTy (NoteTy (UsgNote   _) _ ) = True
isUsgTy other                     = False
#endif

-- The isNotUsgTy function may return a false True if UsManys are omitted;
-- in other words, A SSERT( isNotUsgTy ty ) may be useful but
-- A SSERT( not (isNotUsg ty) ) is asking for trouble.  KSW 1999-04.
isNotUsgTy :: Type -> Bool
isNotUsgTy (NoteTy (UsgForAll _) _) = False
isNotUsgTy (NoteTy (UsgNote   _) _) = False
isNotUsgTy other                    = True

-- splitUsgTy_maybe is not exported, since it is meaningless if
-- UsManys are omitted.  It is used in several places in this module,
-- however.  KSW 1999-04.
splitUsgTy_maybe :: Type -> Maybe (UsageAnn,Type)
splitUsgTy_maybe (NoteTy (UsgNote usg) ty2) = ASSERT( isNotUsgTy ty2 )
                                              Just (usg,ty2)
splitUsgTy_maybe ty@(NoteTy (UsgForAll _) _) = pprPanic "splitUsgTy_maybe:" $ pprType ty
splitUsgTy_maybe ty                          = Nothing

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

mkUsForAllTy :: UVar -> Type -> Type
mkUsForAllTy uv ty = NoteTy (UsgForAll uv) ty

mkUsForAllTys :: [UVar] -> Type -> Type
mkUsForAllTys uvs ty = foldr (NoteTy . UsgForAll) ty uvs

splitUsForAllTys :: Type -> ([UVar],Type)
splitUsForAllTys ty = split ty []
  where split (NoteTy (UsgForAll u) ty) uvs = split ty (u:uvs)
        split other_ty                  uvs = (reverse uvs, other_ty)

substUsTy :: VarEnv UsageAnn -> Type -> Type
-- assumes range is fresh uvars, so no conflicts
substUsTy ve    (NoteTy  note@(UsgNote (UsVar u))
                                            ty ) = NoteTy (case lookupVarEnv ve u of
                                                             Just ua -> UsgNote ua
                                                             Nothing -> note)
                                                          (substUsTy ve ty)
substUsTy ve    (NoteTy  note@(UsgNote   _) ty ) = NoteTy note (substUsTy ve ty)
substUsTy ve    (NoteTy  note@(UsgForAll _) ty ) = NoteTy note (substUsTy ve ty)
substUsTy ve    (NoteTy  (SynNote ty1)      ty2) = NoteTy (SynNote (substUsTy ve ty1))
                                                          (substUsTy ve ty2)
substUsTy ve    (NoteTy  note@(FTVNote _)   ty ) = NoteTy note (substUsTy ve ty)
substUsTy ve ty@(TyVarTy _                     ) = ty
substUsTy ve    (AppTy   ty1                ty2) = AppTy (substUsTy ve ty1)
                                                         (substUsTy ve ty2)
substUsTy ve    (FunTy   ty1                ty2) = FunTy (substUsTy ve ty1)
                                                         (substUsTy ve ty2)
substUsTy ve    (TyConApp tyc               tys) = TyConApp tyc (map (substUsTy ve) tys)
substUsTy ve    (ForAllTy yv                ty ) = ForAllTy yv (substUsTy ve ty)
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
    splitFAT_m (NoteTy (IPNote _) ty)	= Nothing
    splitFAT_m (NoteTy _ ty)		= splitFAT_m ty
    splitFAT_m (ForAllTy tyvar ty)	= Just(tyvar, ty)
    splitFAT_m _			= Nothing

splitForAllTys :: Type -> ([TyVar], Type)
splitForAllTys ty = case splitUsgTy_maybe ty of
                      Just (usg,ty') -> let (tvs,ty'') = split ty' ty' []
					in  (tvs, NoteTy (UsgNote usg) ty'')
		      Nothing        -> split ty ty []
   where
     split orig_ty (ForAllTy tv ty)	  tvs = split ty ty (tv:tvs)
     split orig_ty (NoteTy (IPNote _) ty) tvs = (reverse tvs, orig_ty)
     split orig_ty (NoteTy _ ty)	  tvs = split orig_ty ty tvs
     split orig_ty t			  tvs = (reverse tvs, orig_ty)
\end{code}

@mkPiType@ makes a (->) type or a forall type, depending on whether
it is given a type variable or a term variable.

\begin{code}
mkPiType :: Var -> Type -> Type		-- The more polymorphic version doesn't work...
mkPiType v ty | isId v    = mkFunTy (idType v) ty
	      | otherwise = mkForAllTy v ty
\end{code}

Applying a for-all to its arguments

\begin{code}
applyTy :: Type -> Type -> Type
applyTy (NoteTy note@(UsgNote   _) fun) arg = NoteTy note (applyTy fun arg)
applyTy (NoteTy note@(UsgForAll _) fun) arg = NoteTy note (applyTy fun arg)
applyTy (NoteTy _ fun)                  arg = applyTy fun arg
applyTy (ForAllTy tv ty)                arg = ASSERT( isNotUsgTy arg )
                                              substTy (mkTyVarSubst [tv] [arg]) ty
applyTy other		                arg = panic "applyTy"

applyTys :: Type -> [Type] -> Type
applyTys fun_ty arg_tys
 = substTy (mkTyVarSubst tvs arg_tys) ty
 where
   (tvs, ty) = split fun_ty arg_tys
   
   split fun_ty               []         = ([], fun_ty)
   split (NoteTy note@(UsgNote   _) fun_ty)
                              args       = case split fun_ty args of
                                             (tvs, ty) -> (tvs, NoteTy note ty)
   split (NoteTy note@(UsgForAll _) fun_ty)
                              args       = case split fun_ty args of
                                             (tvs, ty) -> (tvs, NoteTy note ty)
   split (NoteTy _ fun_ty)    args       = split fun_ty args
   split (ForAllTy tv fun_ty) (arg:args) = ASSERT2( isNotUsgTy arg, vcat (map pprType arg_tys) $$
								    text "in application of" <+> pprType fun_ty)
					   case split fun_ty args of
						  (tvs, ty) -> (tv:tvs, ty)
   split other_ty             args       = panic "applyTys"
\end{code}

Note that we allow applications to be of usage-annotated- types, as an
extension: we handle them by lifting the annotation outside.  The
argument, however, must still be unannotated.

\begin{code}
hoistForAllTys :: Type -> Type
	-- Move all the foralls to the top
	-- e.g.  T -> forall a. a  ==>   forall a. T -> a
hoistForAllTys ty
  = case hoist ty of { (tvs, body) -> mkForAllTys tvs body }
  where
    hoist :: Type -> ([TyVar], Type)
    hoist ty = case splitFunTys    ty  of { (args, res) -> 
	       case splitForAllTys res of {
		  ([], body)  -> ([], ty) ;
		  (tvs1, body1) -> case hoist body1 of { (tvs2,body2) ->
				   (tvs1 ++ tvs2, mkFunTys args body2)
	       }}}
\end{code}


%************************************************************************
%*									*
\subsection{Stuff to do with the source-language types}

PredType and ThetaType are used in types for expressions and bindings.
ClassPred and ClassContext are used in class and instance declarations.
%*									*
%************************************************************************

\begin{code}
type RhoType   = Type
type TauType   = Type
data PredType  = Class  Class [Type]
	       | IParam Name  Type
type ThetaType = [PredType]
type ClassPred = (Class, [Type])
type ClassContext = [ClassPred]
type SigmaType = Type
\end{code}

\begin{code}
instance Outputable PredType where
    ppr = pprPred
\end{code}

\begin{code}
mkClassPred clas tys = Class clas tys

getClassTys_maybe :: PredType -> Maybe ClassPred
getClassTys_maybe (Class clas tys) = Just (clas, tys)
getClassTys_maybe _		   = Nothing

ipName_maybe :: PredType -> Maybe Name
ipName_maybe (IParam n _) = Just n
ipName_maybe _		  = Nothing

classesToPreds cts = map (uncurry Class) cts

classesOfPreds theta = concatMap cvt theta
    where cvt (Class clas tys) = [(clas, tys)]
	  cvt (IParam _   _  ) = []
\end{code}

@isTauTy@ tests for nested for-alls.

\begin{code}
isTauTy :: Type -> Bool
isTauTy (TyVarTy v)		= True
isTauTy (TyConApp _ tys)	= all isTauTy tys
isTauTy (AppTy a b)		= isTauTy a && isTauTy b
isTauTy (FunTy a b)		= isTauTy a && isTauTy b
isTauTy (NoteTy (IPNote _) ty)	= False
isTauTy (NoteTy _ ty)		= isTauTy ty
isTauTy other			= False
\end{code}

\begin{code}
mkRhoTy :: [PredType] -> Type -> Type
mkRhoTy theta ty = foldr (\p r -> FunTy (mkPredTy p) r) ty theta

splitRhoTy :: Type -> ([PredType], Type)
splitRhoTy ty = split ty ty []
 where
  split orig_ty (FunTy arg res) ts = case splitPredTy_maybe arg of
					Just p -> split res res (p:ts)
					Nothing   -> (reverse ts, orig_ty)
  split orig_ty (NoteTy (IPNote _) ty)	ts = (reverse ts, orig_ty)
  split orig_ty (NoteTy _ ty)		ts = split orig_ty ty ts
  split orig_ty ty			ts = (reverse ts, orig_ty)
\end{code}



\begin{code}
mkSigmaTy tyvars theta tau = mkForAllTys tyvars (mkRhoTy theta tau)

isSigmaTy :: Type -> Bool
isSigmaTy (FunTy a b)		= isPredTy a
    where isPredTy (NoteTy (IPNote _) _) = True
	  -- JRL could be a dict ty, but that would be polymorphic,
	  -- and thus there would have been an outer ForAllTy
	  isPredTy _			 = False
isSigmaTy (NoteTy (IPNote _) _) = False
isSigmaTy (NoteTy _ ty)		= isSigmaTy ty
isSigmaTy (ForAllTy tyvar ty)	= True
isSigmaTy _			= False

splitSigmaTy :: Type -> ([TyVar], [PredType], Type)
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

typeKind (FunTy arg res)	= boxedTypeKind	-- A function is boxed regardless of its result type
						-- No functions at the type level, hence we don't need
						-- to say (typeKind res).

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
tyVarsOfType (NoteTy (SynNote ty1) ty2)	= tyVarsOfType ty1
tyVarsOfType (NoteTy (UsgNote _) ty)	= tyVarsOfType ty
tyVarsOfType (NoteTy (UsgForAll _) ty)	= tyVarsOfType ty
tyVarsOfType (NoteTy (IPNote _) ty)	= tyVarsOfType ty
tyVarsOfType (FunTy arg res)		= tyVarsOfType arg `unionVarSet` tyVarsOfType res
tyVarsOfType (AppTy fun arg)		= tyVarsOfType fun `unionVarSet` tyVarsOfType arg
tyVarsOfType (ForAllTy tyvar ty)	= tyVarsOfType ty `minusVarSet` unitVarSet tyvar

tyVarsOfTypes :: [Type] -> TyVarSet
tyVarsOfTypes tys = foldr (unionVarSet.tyVarsOfType) emptyVarSet tys

tyVarsOfPred :: PredType -> TyVarSet
tyVarsOfPred (Class clas tys) = tyVarsOfTypes tys
tyVarsOfPred (IParam n ty)    = tyVarsOfType ty

tyVarsOfTheta :: ThetaType -> TyVarSet
tyVarsOfTheta = foldr (unionVarSet . tyVarsOfPred) emptyVarSet

-- Add a Note with the free tyvars to the top of the type
-- (but under a usage if there is one)
addFreeTyVars :: Type -> Type
addFreeTyVars (NoteTy note@(UsgNote   _) ty) = NoteTy note (addFreeTyVars ty)
addFreeTyVars (NoteTy note@(UsgForAll _) ty) = NoteTy note (addFreeTyVars ty)
addFreeTyVars ty@(NoteTy (FTVNote _) _)      = ty
addFreeTyVars ty			     = NoteTy (FTVNote (tyVarsOfType ty)) ty

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
    go (NoteTy note ty)     = (NoteTy SAPPLY (go_note note)) SAPPLY (go ty)
    go (AppTy fun arg)	    = (AppTy SAPPLY (go fun)) SAPPLY (go arg)
    go (FunTy fun arg)	    = (FunTy SAPPLY (go fun)) SAPPLY (go arg)
    go (ForAllTy tv ty)	    = ForAllTy tvp SAPPLY (tidyType envp ty)
			      where
			        (envp, tvp) = tidyTyVar env tv

    go_note (SynNote ty)        = SynNote SAPPLY (go ty)
    go_note note@(FTVNote ftvs) = note	-- No need to tidy the free tyvars
    go_note note@(UsgNote _)    = note  -- Usage annotation is already tidy
    go_note note@(UsgForAll _)  = note  -- Uvar binder is already tidy
    go_note (IPNote n)		= IPNote (tidyIPName n)

tidyTypes  env tys    = map (tidyType env) tys
\end{code}


@tidyOpenType@ grabs the free type variables, tidies them
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

\begin{code}
tidyIPName :: Name -> Name
tidyIPName name
  = mkLocalName (getUnique name) (getOccName name) noSrcLoc
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
	-- isUnLiftedType returns True for forall'd unlifted types:
	--	x :: forall a. Int#
	-- I found bindings like these were getting floated to the top level.
	-- They are pretty bogus types, mind you.  It would be better never to
	-- construct them

isUnLiftedType (ForAllTy tv ty) = isUnLiftedType ty
isUnLiftedType (NoteTy _ ty)	= isUnLiftedType ty
isUnLiftedType (TyConApp tc _)  = isUnLiftedTyCon tc
isUnLiftedType other		= False

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

isNewType :: Type -> Bool
isNewType ty = case splitTyConApp_maybe ty of
			Just (tc, ty_args) -> ASSERT( length ty_args == tyConArity tc )
					      isNewTyCon tc
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
seqType (TyConApp tc tys) = tc `seq` seqTypes tys
seqType (ForAllTy tv ty)  = tv `seq` seqType ty

seqTypes :: [Type] -> ()
seqTypes []       = ()
seqTypes (ty:tys) = seqType ty `seq` seqTypes tys

seqNote :: TyNote -> ()
seqNote (SynNote ty)  = seqType ty
seqNote (FTVNote set) = sizeUniqSet set `seq` ()
seqNote (UsgNote usg) = usg `seq` ()
seqNote (IPNote nm)   = nm `seq` ()
\end{code}
