%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[Type]{Type - public interface}

\begin{code}
module Type (
        -- re-exports from TypeRep:
	Type,
	Kind, TyVarSubst,

	superKind, superBoxity,				-- KX and BX respectively
	boxedBoxity, unboxedBoxity, 			-- :: BX
	openKindCon, 					-- :: KX
	typeCon,					-- :: BX -> KX
	boxedTypeKind, unboxedTypeKind, openTypeKind, 	-- :: KX
	mkArrowKind, mkArrowKinds,			-- :: KX -> KX -> KX

	funTyCon,

        -- exports from this module:
        hasMoreBoxityInfo, defaultKind,

	mkTyVarTy, mkTyVarTys, getTyVar, getTyVar_maybe, isTyVarTy,

	mkAppTy, mkAppTys, splitAppTy, splitAppTys, splitAppTy_maybe,

	mkFunTy, mkFunTys, splitFunTy, splitFunTy_maybe, splitFunTys, splitFunTysN,
	funResultTy, funArgTy, zipFunTys,

	mkTyConApp, mkTyConTy, splitTyConApp_maybe,
	splitAlgTyConApp_maybe, splitAlgTyConApp, 

	-- Predicates and the like
	mkDictTy, mkDictTys, mkPredTy, splitPredTy_maybe, 
	splitDictTy, splitDictTy_maybe, isDictTy, predRepTy,

	mkSynTy, isSynTy, deNoteType, 

	repType, splitRepFunTys, splitNewType_maybe, typePrimRep,

        UsageAnn(..), mkUsgTy, isUsgTy{- dont use -}, isNotUsgTy, splitUsgTy, unUsgTy, tyUsg,
        mkUsForAllTy, mkUsForAllTys, splitUsForAllTys, substUsTy, 

	mkForAllTy, mkForAllTys, splitForAllTy_maybe, splitForAllTys, 
	applyTy, applyTys, hoistForAllTys,

	TauType, RhoType, SigmaType, PredType(..), ThetaType,
	ClassPred, ClassContext, mkClassPred,
	getClassTys_maybe, ipName_maybe, classesToPreds, classesOfPreds,
	isTauTy, mkRhoTy, splitRhoTy,
	mkSigmaTy, isSigmaTy, splitSigmaTy,
	getDFunTyKey,

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
import {-# SOURCE #-}	PprType( pprType )	-- Only called in debug messages
import {-# SOURCE #-}   Subst  ( mkTyVarSubst, substTy )

-- friends:
import Var	( TyVar, Var, UVar,
		  tyVarKind, tyVarName, setTyVarName, isId, idType,
		)
import VarEnv
import VarSet

import Name	( Name, NamedThing(..), OccName, mkLocalName, tidyOccName )
import NameSet
import Class	( classTyCon, Class, ClassPred, ClassContext )
import TyCon	( TyCon,
		  isUnboxedTupleTyCon, isUnLiftedTyCon,
		  isFunTyCon, isDataTyCon, isNewTyCon, newTyConRep,
		  isAlgTyCon, isSynTyCon, tyConArity,
	          tyConKind, tyConDataCons, getSynTyConDefn,
		  tyConPrimRep
		)

-- others
import SrcLoc		( noSrcLoc )
import PrimRep		( PrimRep(..), isFollowableRep )
import Unique		( Uniquable(..) )
import Util		( mapAccumL, seqList, thenCmp )
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
  | k2 == openTypeKind = True
  | otherwise	       = k1 == k2

defaultKind :: Kind -> Kind
-- Used when generalising: default kind '?' to '*'
defaultKind kind | kind == openTypeKind = boxedTypeKind
	         | otherwise	 	= kind
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
getTyVar msg (PredTy p)   = getTyVar msg (predRepTy p)
getTyVar msg (NoteTy _ t) = getTyVar msg t
getTyVar msg other	  = panic ("getTyVar: " ++ msg)

getTyVar_maybe :: Type -> Maybe TyVar
getTyVar_maybe (TyVarTy tv) = Just tv
getTyVar_maybe (NoteTy _ t) = getTyVar_maybe t
getTyVar_maybe (PredTy p)   = getTyVar_maybe (predRepTy p)
getTyVar_maybe other	    = Nothing

isTyVarTy :: Type -> Bool
isTyVarTy (TyVarTy tv)  = True
isTyVarTy (NoteTy _ ty) = isTyVarTy ty
isTyVarTy (PredTy p)    = isTyVarTy (predRepTy p)
isTyVarTy other         = False
\end{code}


---------------------------------------------------------------------
				AppTy
				~~~~~
We need to be pretty careful with AppTy to make sure we obey the 
invariant that a TyConApp is always visibly so.  mkAppTy maintains the
invariant: use it.

\begin{code}
mkAppTy orig_ty1 orig_ty2
  = ASSERT2( isNotUsgTy orig_ty1 && isNotUsgTy orig_ty2, pprType orig_ty1 <+> text "to" <+> pprType orig_ty2 )
    ASSERT( not (isPredTy orig_ty1) )	-- Predicates are of kind *
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
mkAppTys orig_ty1 orig_tys2
  = ASSERT2( isNotUsgTy orig_ty1, pprType orig_ty1 )
    ASSERT( not (isPredTy orig_ty1) )	-- Predicates are of kind *
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
splitAppTy_maybe (PredTy p)        = splitAppTy_maybe (predRepTy p)
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
    split orig_ty (PredTy p)            args = split orig_ty (predRepTy p) args
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
splitFunTy (PredTy p)      = splitFunTy (predRepTy p)

splitFunTy_maybe :: Type -> Maybe (Type, Type)
splitFunTy_maybe (FunTy arg res) = Just (arg, res)
splitFunTy_maybe (NoteTy _ ty)   = splitFunTy_maybe ty
splitFunTy_maybe (PredTy p)    	 = splitFunTy_maybe (predRepTy p)
splitFunTy_maybe other	         = Nothing

splitFunTys :: Type -> ([Type], Type)
splitFunTys ty = split [] ty ty
  where
    split args orig_ty (FunTy arg res) = split (arg:args) res res
    split args orig_ty (NoteTy _ ty)   = split args orig_ty ty
    split args orig_ty (PredTy p)      = split args orig_ty (predRepTy p)
    split args orig_ty ty              = (reverse args, orig_ty)

splitFunTysN :: String -> Int -> Type -> ([Type], Type)
splitFunTysN msg orig_n orig_ty = split orig_n [] orig_ty orig_ty
  where
    split 0 args syn_ty ty		= (reverse args, syn_ty) 
    split n args syn_ty (FunTy arg res) = split (n-1) (arg:args) res    res
    split n args syn_ty (NoteTy _ ty)   = split n     args       syn_ty ty
    split n args syn_ty (PredTy p)      = split n     args       syn_ty (predRepTy p)
    split n args syn_ty ty              = pprPanic ("splitFunTysN: " ++ msg) (int orig_n <+> pprType orig_ty)

zipFunTys :: Outputable a => [a] -> Type -> ([(a,Type)], Type)
zipFunTys orig_xs orig_ty = split [] orig_xs orig_ty orig_ty
  where
    split acc []     nty ty  	         = (reverse acc, nty)
    split acc (x:xs) nty (FunTy arg res) = split ((x,arg):acc) xs res res
    split acc xs     nty (NoteTy _ ty)   = split acc           xs nty ty
    split acc xs     nty (PredTy p)      = split acc           xs nty (predRepTy p)
    split acc (x:xs) nty ty              = pprPanic "zipFunTys" (ppr orig_xs <+> pprType orig_ty)
    
funResultTy :: Type -> Type
funResultTy (FunTy arg res) = res
funResultTy (NoteTy _ ty)   = funResultTy ty
funResultTy (PredTy p)      = funResultTy (predRepTy p)
funResultTy ty		    = pprPanic "funResultTy" (pprType ty)

funArgTy :: Type -> Type
funArgTy (FunTy arg res) = arg
funArgTy (NoteTy _ ty)   = funArgTy ty
funArgTy (PredTy p)      = funArgTy (predRepTy p)
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
splitTyConApp_maybe (TyConApp tc tys) = Just (tc, tys)
splitTyConApp_maybe (FunTy arg res)   = Just (funTyCon, [arg,res])
splitTyConApp_maybe (NoteTy _ ty)     = splitTyConApp_maybe ty
splitTyConApp_maybe (PredTy p)	      = splitTyConApp_maybe (predRepTy p)
splitTyConApp_maybe other	      = Nothing

-- splitAlgTyConApp_maybe looks for 
--	*saturated* applications of *algebraic* data types
-- "Algebraic" => newtype, data type, or dictionary (not function types)
-- We return the constructors too, so there had better be some.

splitAlgTyConApp_maybe :: Type -> Maybe (TyCon, [Type], [DataCon])
splitAlgTyConApp_maybe (TyConApp tc tys) 
  | isAlgTyCon tc && 
    tyConArity tc == length tys      = Just (tc, tys, tyConDataCons tc)
splitAlgTyConApp_maybe (NoteTy _ ty) = splitAlgTyConApp_maybe ty
splitAlgTyConApp_maybe (PredTy p)    = splitAlgTyConApp_maybe (predRepTy p)
splitAlgTyConApp_maybe other	     = Nothing

splitAlgTyConApp :: Type -> (TyCon, [Type], [DataCon])
	-- Here the "algebraic" property is an *assertion*
splitAlgTyConApp (TyConApp tc tys) = ASSERT( isAlgTyCon tc && tyConArity tc == length tys )
	      			     (tc, tys, tyConDataCons tc)
splitAlgTyConApp (NoteTy _ ty)     = splitAlgTyConApp ty
splitAlgTyConApp (PredTy p)        = splitAlgTyConApp (predRepTy p)
#ifdef DEBUG
splitAlgTyConApp ty = pprPanic "splitAlgTyConApp" (pprType ty)
#endif
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
	-- Remove synonyms, but not Preds
deNoteType ty@(TyVarTy tyvar)	= ty
deNoteType (TyConApp tycon tys) = TyConApp tycon (map deNoteType tys)
deNoteType (PredTy p)		= PredTy p
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
	(d) predicates
It's useful in the back end where we're not
interested in newtypes anymore.

\begin{code}
repType :: Type -> Type
repType (ForAllTy _ ty) = repType ty
repType (NoteTy   _ ty) = repType ty
repType (PredTy  p)     = repType (predRepTy p)
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
splitNewType_maybe (NoteTy _ ty)     = splitNewType_maybe ty
splitNewType_maybe (PredTy p)        = splitNewType_maybe (predRepTy p)
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
substUsTy ve (NoteTy note@(UsgNote (UsVar u))
                                         ty ) = NoteTy (case lookupVarEnv ve u of
                                                          Just ua -> UsgNote ua
                                                          Nothing -> note)
                                                       (substUsTy ve ty)
substUsTy ve (NoteTy (SynNote ty1)      ty2) = NoteTy (SynNote (substUsTy ve ty1)) (substUsTy ve ty2)
substUsTy ve (NoteTy note ty) 		     = NoteTy note (substUsTy ve ty)
	     
substUsTy ve (PredTy (Class c tys)) = PredTy (Class c (map (substUsTy ve) tys))
substUsTy ve (PredTy (IParam n ty)) = PredTy (IParam n (substUsTy ve ty))
substUsTy ve (TyVarTy tv) 	    =  TyVarTy tv
substUsTy ve (AppTy  ty1 ty2)       = AppTy (substUsTy ve ty1) (substUsTy ve ty2)
substUsTy ve (FunTy  ty1 ty2)       = FunTy (substUsTy ve ty1) (substUsTy ve ty2)
substUsTy ve (TyConApp tyc tys)     = TyConApp tyc (map (substUsTy ve) tys)
substUsTy ve (ForAllTy yv ty )      = ForAllTy yv (substUsTy ve ty)
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
    splitFAT_m (NoteTy _ ty)		= splitFAT_m ty
    splitFAT_m (PredTy p)		= splitFAT_m (predRepTy p)
    splitFAT_m (ForAllTy tyvar ty)	= Just(tyvar, ty)
    splitFAT_m _			= Nothing

splitForAllTys :: Type -> ([TyVar], Type)
splitForAllTys ty = case splitUsgTy_maybe ty of
                      Just (usg,ty') -> let (tvs,ty'') = split ty' ty' []
					in  (tvs, NoteTy (UsgNote usg) ty'')
		      Nothing        -> split ty ty []
   where
     split orig_ty (ForAllTy tv ty)	  tvs = split ty ty (tv:tvs)
     split orig_ty (NoteTy _ ty)	  tvs = split orig_ty ty tvs
     split orig_ty (PredTy p)		  tvs = split orig_ty (predRepTy p) tvs
     split orig_ty t			  tvs = (reverse tvs, orig_ty)
\end{code}

-- (mkPiType now in CoreUtils)

Applying a for-all to its arguments

\begin{code}
applyTy :: Type -> Type -> Type
applyTy (NoteTy note@(UsgNote   _) fun) arg = NoteTy note (applyTy fun arg)
applyTy (NoteTy note@(UsgForAll _) fun) arg = NoteTy note (applyTy fun arg)
applyTy (PredTy p) 	                arg = applyTy (predRepTy p) arg
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
   split (PredTy p)	      args       = split (predRepTy p) args
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

"Dictionary" types are just ordinary data types, but you can
tell from the type constructor whether it's a dictionary or not.

\begin{code}
mkClassPred clas tys = Class clas tys

mkDictTy :: Class -> [Type] -> Type
mkDictTy clas tys = mkPredTy (Class clas tys)

mkDictTys :: ClassContext -> [Type]
mkDictTys cxt = [mkDictTy cls tys | (cls,tys) <- cxt]

mkPredTy :: PredType -> Type
mkPredTy pred = PredTy pred

predRepTy :: PredType -> Type
-- Convert a predicate to its "representation type";
-- the type of evidence for that predicate, which is actually passed at runtime
predRepTy (Class clas tys) = TyConApp (classTyCon clas) tys
predRepTy (IParam n ty)    = ty

isPredTy :: Type -> Bool
isPredTy (NoteTy _ ty) = isPredTy ty
isPredTy (PredTy _)    = True
isPredTy _	       = False

isDictTy :: Type -> Bool
isDictTy (NoteTy _ ty)	      = isDictTy ty
isDictTy (PredTy (Class _ _)) = True
isDictTy other		      = False

splitPredTy_maybe :: Type -> Maybe PredType
splitPredTy_maybe (NoteTy _ ty) = splitPredTy_maybe ty
splitPredTy_maybe (PredTy p)    = Just p
splitPredTy_maybe other	        = Nothing

splitDictTy :: Type -> (Class, [Type])
splitDictTy (NoteTy _ ty) = splitDictTy ty
splitDictTy (PredTy (Class clas tys)) = (clas, tys)

splitDictTy_maybe :: Type -> Maybe (Class, [Type])
splitDictTy_maybe (NoteTy _ ty) = splitDictTy ty
splitDictTy_maybe (PredTy (Class clas tys)) = Just (clas, tys)
splitDictTy_maybe other			    = Nothing

getClassTys_maybe :: PredType -> Maybe ClassPred
getClassTys_maybe (Class clas tys) = Just (clas, tys)
getClassTys_maybe _		   = Nothing

ipName_maybe :: PredType -> Maybe Name
ipName_maybe (IParam n _) = Just n
ipName_maybe _		  = Nothing

classesToPreds :: ClassContext -> ThetaType
classesToPreds cts = map (uncurry Class) cts

classesOfPreds :: ThetaType -> ClassContext
classesOfPreds theta = [(clas,tys) | Class clas tys <- theta]
\end{code}

@isTauTy@ tests for nested for-alls.

\begin{code}
isTauTy :: Type -> Bool
isTauTy (TyVarTy v)	 = True
isTauTy (TyConApp _ tys) = all isTauTy tys
isTauTy (AppTy a b)	 = isTauTy a && isTauTy b
isTauTy (FunTy a b)	 = isTauTy a && isTauTy b
isTauTy (PredTy p)	 = isTauTy (predRepTy p)
isTauTy (NoteTy _ ty)	 = isTauTy ty
isTauTy other		 = False
\end{code}

\begin{code}
mkRhoTy :: [PredType] -> Type -> Type
mkRhoTy theta ty = foldr (\p r -> FunTy (mkPredTy p) r) ty theta

splitRhoTy :: Type -> ([PredType], Type)
splitRhoTy ty = split ty ty []
 where
  split orig_ty (FunTy arg res) ts = case splitPredTy_maybe arg of
					Just p  -> split res res (p:ts)
					Nothing -> (reverse ts, orig_ty)
  split orig_ty (NoteTy _ ty)	ts = split orig_ty ty ts
  split orig_ty ty		ts = (reverse ts, orig_ty)
\end{code}


isSigmaType returns true of any qualified type.  It doesn't *necessarily* have 
any foralls.  E.g.
	f :: (?x::Int) => Int -> Int

\begin{code}
mkSigmaTy tyvars theta tau = mkForAllTys tyvars (mkRhoTy theta tau)

isSigmaTy :: Type -> Bool
isSigmaTy (ForAllTy tyvar ty)	= True
isSigmaTy (FunTy a b)		= isPredTy a
isSigmaTy (NoteTy _ ty)		= isSigmaTy ty
isSigmaTy _			= False

splitSigmaTy :: Type -> ([TyVar], [PredType], Type)
splitSigmaTy ty =
  (tyvars, theta, tau)
 where
  (tyvars,rho) = splitForAllTys ty
  (theta,tau)  = splitRhoTy rho
\end{code}

\begin{code}
getDFunTyKey :: Type -> OccName	-- Get some string from a type, to be used to 
				-- construct a dictionary function name
getDFunTyKey (TyVarTy tv)    = getOccName tv
getDFunTyKey (TyConApp tc _) = getOccName tc
getDFunTyKey (AppTy fun _)   = getDFunTyKey fun
getDFunTyKey (NoteTy _ t)    = getDFunTyKey t
getDFunTyKey (FunTy arg _)   = getOccName funTyCon
getDFunTyKey (ForAllTy _ t)  = getDFunTyKey t
-- PredTy shouldn't happen
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
typeKind (PredTy _)		= boxedTypeKind		-- Predicates are always 
							-- represented by boxed types
typeKind (AppTy fun arg)	= funResultTy (typeKind fun)

typeKind (FunTy arg res)	= fix_up (typeKind res)
				where
				  fix_up (TyConApp tycon _) |  tycon == typeCon
							    || tycon == openKindCon = boxedTypeKind
				  fix_up (NoteTy _ kind) = fix_up kind
				  fix_up kind	         = kind
		-- The basic story is 
		-- 	typeKind (FunTy arg res) = typeKind res
		-- But a function is boxed regardless of its result type
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
tyVarsOfType (NoteTy (FTVNote tvs) ty2) = tvs
tyVarsOfType (NoteTy (SynNote ty1) ty2)	= tyVarsOfType ty1
tyVarsOfType (NoteTy (UsgNote _) ty)	= tyVarsOfType ty
tyVarsOfType (NoteTy (UsgForAll _) ty)	= tyVarsOfType ty
tyVarsOfType (PredTy p)			= tyVarsOfPred p
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
namesOfType (PredTy p)			= namesOfType (predRepTy p)
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
    go (PredTy p)	    = PredTy (go_pred p)
    go (AppTy fun arg)	    = (AppTy SAPPLY (go fun)) SAPPLY (go arg)
    go (FunTy fun arg)	    = (FunTy SAPPLY (go fun)) SAPPLY (go arg)
    go (ForAllTy tv ty)	    = ForAllTy tvp SAPPLY (tidyType envp ty)
			      where
			        (envp, tvp) = tidyTyVar env tv

    go_note (SynNote ty)        = SynNote SAPPLY (go ty)
    go_note note@(FTVNote ftvs) = note	-- No need to tidy the free tyvars
    go_note note@(UsgNote _)    = note  -- Usage annotation is already tidy
    go_note note@(UsgForAll _)  = note  -- Uvar binder is already tidy

    go_pred (Class c tys) = Class c (tidyTypes env tys)
    go_pred (IParam n ty) = IParam n (go ty)

tidyTypes env tys = map (tidyType env) tys
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
seqType (PredTy p) 	  = seqPred p
seqType (TyConApp tc tys) = tc `seq` seqTypes tys
seqType (ForAllTy tv ty)  = tv `seq` seqType ty

seqTypes :: [Type] -> ()
seqTypes []       = ()
seqTypes (ty:tys) = seqType ty `seq` seqTypes tys

seqNote :: TyNote -> ()
seqNote (SynNote ty)  = seqType ty
seqNote (FTVNote set) = sizeUniqSet set `seq` ()
seqNote (UsgNote usg) = usg `seq` ()

seqPred :: PredType -> ()
seqPred (Class c tys) = c `seq` seqTypes tys
seqPred (IParam n ty) = n `seq` seqType ty
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
  ty1 == ty2 = case ty1 `compare` ty2 of { EQ -> True; other -> False }

instance Ord Type where
  compare ty1 ty2 = cmpTy emptyVarEnv ty1 ty2

cmpTy :: TyVarEnv TyVar -> Type -> Type -> Ordering
  -- The "env" maps type variables in ty1 to type variables in ty2
  -- So when comparing for-alls.. (forall tv1 . t1) (forall tv2 . t2)
  -- we in effect substitute tv2 for tv1 in t1 before continuing

    -- Get rid of NoteTy
cmpTy env (NoteTy _ ty1) ty2 = cmpTy env ty1 ty2
cmpTy env ty1 (NoteTy _ ty2) = cmpTy env ty1 ty2

    -- Get rid of PredTy
cmpTy env (PredTy p1) (PredTy p2) = cmpPred env p1 p2
cmpTy env (PredTy p1) ty2	  = cmpTy env (predRepTy p1) ty2
cmpTy env ty1         (PredTy p2) = cmpTy env ty1 (predRepTy p2)

    -- Deal with equal constructors
cmpTy env (TyVarTy tv1) (TyVarTy tv2) = case lookupVarEnv env tv1 of
					  Just tv1a -> tv1a `compare` tv2
					  Nothing   -> tv1  `compare` tv2

cmpTy env (AppTy f1 a1) (AppTy f2 a2) = cmpTy env f1 f2 `thenCmp` cmpTy env a1 a2
cmpTy env (FunTy f1 a1) (FunTy f2 a2) = cmpTy env f1 f2 `thenCmp` cmpTy env a1 a2
cmpTy env (TyConApp tc1 tys1) (TyConApp tc2 tys2) = (tc1 `compare` tc2) `thenCmp` (cmpTys env tys1 tys2)
cmpTy env (ForAllTy tv1 t1)   (ForAllTy tv2 t2)   = cmpTy (extendVarEnv env tv1 tv2) t1 t2
    
    -- Deal with the rest: TyVarTy < AppTy < FunTy < TyConApp < ForAllTy
cmpTy env (AppTy _ _) (TyVarTy _) = GT
    
cmpTy env (FunTy _ _) (TyVarTy _) = GT
cmpTy env (FunTy _ _) (AppTy _ _) = GT
    
cmpTy env (TyConApp _ _) (TyVarTy _) = GT
cmpTy env (TyConApp _ _) (AppTy _ _) = GT
cmpTy env (TyConApp _ _) (FunTy _ _) = GT
    
cmpTy env (ForAllTy _ _) other       = GT
    
cmpTy env _ _		             = LT


cmpTys env []       []	     = EQ
cmpTys env (t:ts)   []       = GT
cmpTys env []	    (t:ts)   = LT
cmpTys env (t1:t1s) (t2:t2s) = cmpTy env t1 t2 `thenCmp` cmpTys env t1s t2s
\end{code}

\begin{code}
instance Eq PredType where
  p1 == p2 = case p1 `compare` p2 of { EQ -> True; other -> False }

instance Ord PredType where
  compare p1 p2 = cmpPred emptyVarEnv p1 p2

cmpPred :: TyVarEnv TyVar -> PredType -> PredType -> Ordering
cmpPred env (IParam n1 t)   (IParam n2 t2)  = n1 `compare` n2
	-- Just compare the names!
cmpPred env (Class c1 tys1) (Class c2 tys2) = (c1 `compare` c2) `thenCmp` (cmpTys env tys1 tys2)
cmpPred env (IParam _ _)    (Class _ _)     = LT
cmpPred env (Class _ _)     (IParam _ _)    = GT
\end{code}
