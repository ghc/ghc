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

        usageKindCon,					-- :: KX
        usageTypeKind,					-- :: KX
        usOnceTyCon, usManyTyCon,			-- :: $
        usOnce, usMany,					-- :: $

        -- exports from this module:
        hasMoreBoxityInfo, defaultKind,

	mkTyVarTy, mkTyVarTys, getTyVar, getTyVar_maybe, isTyVarTy,

	mkAppTy, mkAppTys, splitAppTy, splitAppTys, splitAppTy_maybe,

	mkFunTy, mkFunTys, splitFunTy, splitFunTy_maybe, splitFunTys, splitFunTysN,
	funResultTy, funArgTy, zipFunTys,

	mkTyConApp, mkTyConTy, splitTyConApp_maybe,
	splitAlgTyConApp_maybe, splitAlgTyConApp, 

	mkUTy, splitUTy, splitUTy_maybe,
        isUTy, uaUTy, unUTy, liftUTy, mkUTyM,
        isUsageKind, isUsage, isUTyVar,

	-- Predicates and the like
	mkDictTy, mkDictTys, mkPredTy, splitPredTy_maybe, 
	splitDictTy, splitDictTy_maybe, isDictTy, predRepTy, splitDFunTy,

	mkSynTy, deNoteType, 

	repType, splitRepFunTys, splitNewType_maybe, typePrimRep,

	mkForAllTy, mkForAllTys, splitForAllTy_maybe, splitForAllTys, 
	applyTy, applyTys, hoistForAllTys, isForAllTy,

	TauType, RhoType, SigmaType, PredType(..), ThetaType,
	ClassPred, ClassContext, mkClassPred,
	getClassTys_maybe, ipName_maybe, classesOfPreds,
	isTauTy, mkRhoTy, splitRhoTy,
	mkSigmaTy, isSigmaTy, splitSigmaTy,
	getDFunTyKey,

	-- Lifting and boxity
	isUnLiftedType, isUnboxedType, isUnboxedTupleType, isAlgType, isDataType, isNewType,

	-- Free variables
	tyVarsOfType, tyVarsOfTypes, tyVarsOfPred, tyVarsOfTheta,
	namesOfType, usageAnnOfType, typeKind, addFreeTyVars,

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

import {-# SOURCE #-}	DataCon( DataCon )
import {-# SOURCE #-}	PprType( pprType )	-- Only called in debug messages
import {-# SOURCE #-}   Subst  ( mkTyVarSubst, substTy )

-- friends:
import Var	( Var, TyVar, tyVarKind, tyVarName, setTyVarName )
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
import Maybes		( maybeToBool )
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
getTyVar msg ty@(UsageTy _ _) = pprPanic "getTyVar: UTy:" (text msg $$ pprType ty)
getTyVar msg other	  = panic ("getTyVar: " ++ msg)

getTyVar_maybe :: Type -> Maybe TyVar
getTyVar_maybe (TyVarTy tv) = Just tv
getTyVar_maybe (NoteTy _ t) = getTyVar_maybe t
getTyVar_maybe (PredTy p)   = getTyVar_maybe (predRepTy p)
getTyVar_maybe ty@(UsageTy _ _) = pprPanic "getTyVar_maybe: UTy:" (pprType ty)
getTyVar_maybe other	    = Nothing

isTyVarTy :: Type -> Bool
isTyVarTy (TyVarTy tv)  = True
isTyVarTy (NoteTy _ ty) = isTyVarTy ty
isTyVarTy (PredTy p)    = isTyVarTy (predRepTy p)
isTyVarTy ty@(UsageTy _ _) = pprPanic "isTyVarTy: UTy:" (pprType ty)
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
  = ASSERT( not (isPredTy orig_ty1) )	-- Predicates are of kind *
    UASSERT2( not (isUTy orig_ty2), pprType orig_ty1 <+> pprType orig_ty2 )
                                        -- argument must be unannotated
    mk_app orig_ty1
  where
    mk_app (NoteTy _ ty1)    = mk_app ty1
    mk_app (TyConApp tc tys) = mkTyConApp tc (tys ++ [orig_ty2])
    mk_app ty@(UsageTy _ _)  = pprPanic "mkAppTy: UTy:" (pprType ty)
    mk_app ty1		     = AppTy orig_ty1 orig_ty2

mkAppTys :: Type -> [Type] -> Type
mkAppTys orig_ty1 []	    = orig_ty1
	-- This check for an empty list of type arguments
	-- avoids the needless loss of a type synonym constructor.
	-- For example: mkAppTys Rational []
	--   returns to (Ratio Integer), which has needlessly lost
	--   the Rational part.
mkAppTys orig_ty1 orig_tys2
  = ASSERT( not (isPredTy orig_ty1) )	-- Predicates are of kind *
    UASSERT2( not (any isUTy orig_tys2), pprType orig_ty1 <+> fsep (map pprType orig_tys2) )
                                        -- arguments must be unannotated
    mk_app orig_ty1
  where
    mk_app (NoteTy _ ty1)    = mk_app ty1
    mk_app (TyConApp tc tys) = mkTyConApp tc (tys ++ orig_tys2)
    mk_app ty@(UsageTy _ _)  = pprPanic "mkAppTys: UTy:" (pprType ty)
    mk_app ty1		     = foldl AppTy orig_ty1 orig_tys2

splitAppTy_maybe :: Type -> Maybe (Type, Type)
splitAppTy_maybe (FunTy ty1 ty2)   = Just (TyConApp funTyCon [unUTy ty1], unUTy ty2)
splitAppTy_maybe (AppTy ty1 ty2)   = Just (ty1, ty2)
splitAppTy_maybe (NoteTy _ ty)     = splitAppTy_maybe ty
splitAppTy_maybe (PredTy p)        = splitAppTy_maybe (predRepTy p)
splitAppTy_maybe (TyConApp tc [])  = Nothing
splitAppTy_maybe (TyConApp tc tys) = split tys []
			    where
			       split [ty2]    acc = Just (TyConApp tc (reverse acc), ty2)
			       split (ty:tys) acc = split tys (ty:acc)

splitAppTy_maybe ty@(UsageTy _ _)  = pprPanic "splitAppTy_maybe: UTy:" (pprType ty)
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
					       (TyConApp funTyCon [], [unUTy ty1,unUTy ty2])
    split orig_ty (TyConApp tc tc_args) args = (TyConApp tc [], tc_args ++ args)
    split orig_ty (UsageTy _ _)         args = pprPanic "splitAppTys: UTy:" (pprType orig_ty)
    split orig_ty ty		        args = (orig_ty, args)
\end{code}


---------------------------------------------------------------------
				FunTy
				~~~~~

\begin{code}
mkFunTy :: Type -> Type -> Type
mkFunTy arg res = UASSERT2( isUTy arg && isUTy res, pprType arg <+> pprType res )
                  FunTy arg res

mkFunTys :: [Type] -> Type -> Type
mkFunTys tys ty = UASSERT2( all isUTy (ty:tys), fsep (map pprType (tys++[ty])) )
                  foldr FunTy ty tys

splitFunTy :: Type -> (Type, Type)
splitFunTy (FunTy arg res) = (arg, res)
splitFunTy (NoteTy _ ty)   = splitFunTy ty
splitFunTy (PredTy p)      = splitFunTy (predRepTy p)
splitFunTy ty@(UsageTy _ _) = pprPanic "splitFunTy: UTy:" (pprType ty)

splitFunTy_maybe :: Type -> Maybe (Type, Type)
splitFunTy_maybe (FunTy arg res) = Just (arg, res)
splitFunTy_maybe (NoteTy _ ty)   = splitFunTy_maybe ty
splitFunTy_maybe (PredTy p)    	 = splitFunTy_maybe (predRepTy p)
splitFunTy_maybe ty@(UsageTy _ _) = pprPanic "splitFunTy_maybe: UTy:" (pprType ty)
splitFunTy_maybe other	         = Nothing

splitFunTys :: Type -> ([Type], Type)
splitFunTys ty = split [] ty ty
  where
    split args orig_ty (FunTy arg res) = split (arg:args) res res
    split args orig_ty (NoteTy _ ty)   = split args orig_ty ty
    split args orig_ty (PredTy p)      = split args orig_ty (predRepTy p)
    split args orig_ty (UsageTy _ _)   = pprPanic "splitFunTys: UTy:" (pprType orig_ty)
    split args orig_ty ty              = (reverse args, orig_ty)

splitFunTysN :: String -> Int -> Type -> ([Type], Type)
splitFunTysN msg orig_n orig_ty = split orig_n [] orig_ty orig_ty
  where
    split 0 args syn_ty ty		= (reverse args, syn_ty) 
    split n args syn_ty (FunTy arg res) = split (n-1) (arg:args) res    res
    split n args syn_ty (NoteTy _ ty)   = split n     args       syn_ty ty
    split n args syn_ty (PredTy p)      = split n     args       syn_ty (predRepTy p)
    split n args syn_ty (UsageTy _ _)   = pprPanic "splitFunTysN: UTy:" (pprType orig_ty)
    split n args syn_ty ty              = pprPanic ("splitFunTysN: " ++ msg) (int orig_n <+> pprType orig_ty)

zipFunTys :: Outputable a => [a] -> Type -> ([(a,Type)], Type)
zipFunTys orig_xs orig_ty = split [] orig_xs orig_ty orig_ty
  where
    split acc []     nty ty  	         = (reverse acc, nty)
    split acc (x:xs) nty (FunTy arg res) = split ((x,arg):acc) xs res res
    split acc xs     nty (NoteTy _ ty)   = split acc           xs nty ty
    split acc xs     nty (PredTy p)      = split acc           xs nty (predRepTy p)
    split acc xs     nty (UsageTy _ _)   = pprPanic "zipFunTys: UTy:" (ppr orig_xs <+> pprType orig_ty)
    split acc (x:xs) nty ty              = pprPanic "zipFunTys" (ppr orig_xs <+> pprType orig_ty)
    
funResultTy :: Type -> Type
funResultTy (FunTy arg res) = res
funResultTy (NoteTy _ ty)   = funResultTy ty
funResultTy (PredTy p)      = funResultTy (predRepTy p)
funResultTy (UsageTy _ ty)  = funResultTy ty
funResultTy ty		    = pprPanic "funResultTy" (pprType ty)

funArgTy :: Type -> Type
funArgTy (FunTy arg res) = arg
funArgTy (NoteTy _ ty)   = funArgTy ty
funArgTy (PredTy p)      = funArgTy (predRepTy p)
funArgTy (UsageTy _ ty)  = funArgTy ty
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
	(ty1:ty2:_) -> FunTy (mkUTyM ty1) (mkUTyM ty2)

  | otherwise
  = ASSERT(not (isSynTyCon tycon))
    UASSERT2( not (any isUTy tys), ppr tycon <+> fsep (map pprType tys) )
    TyConApp tycon tys

mkTyConTy :: TyCon -> Type
mkTyConTy tycon = ASSERT( not (isSynTyCon tycon) ) 
		  TyConApp tycon []

-- splitTyConApp "looks through" synonyms, because they don't
-- mean a distinct type, but all other type-constructor applications
-- including functions are returned as Just ..

splitTyConApp_maybe :: Type -> Maybe (TyCon, [Type])
splitTyConApp_maybe (TyConApp tc tys) = Just (tc, tys)
splitTyConApp_maybe (FunTy arg res)   = Just (funTyCon, [unUTy arg,unUTy res])
splitTyConApp_maybe (NoteTy _ ty)     = splitTyConApp_maybe ty
splitTyConApp_maybe (PredTy p)	      = splitTyConApp_maybe (predRepTy p)
splitTyConApp_maybe (UsageTy _ ty)    = splitTyConApp_maybe ty
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
splitAlgTyConApp_maybe (UsageTy _ ty)= splitAlgTyConApp_maybe ty
splitAlgTyConApp_maybe other	     = Nothing

splitAlgTyConApp :: Type -> (TyCon, [Type], [DataCon])
	-- Here the "algebraic" property is an *assertion*
splitAlgTyConApp (TyConApp tc tys) = ASSERT( isAlgTyCon tc && tyConArity tc == length tys )
	      			     (tc, tys, tyConDataCons tc)
splitAlgTyConApp (NoteTy _ ty)     = splitAlgTyConApp ty
splitAlgTyConApp (PredTy p)        = splitAlgTyConApp (predRepTy p)
splitAlgTyConApp (UsageTy _ ty)    = splitAlgTyConApp ty
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
    ASSERT( length tyvars == length tys )
    NoteTy (SynNote (TyConApp syn_tycon tys))
	   (substTy (mkTyVarSubst tyvars tys) body)
  where
    (tyvars, body) = getSynTyConDefn syn_tycon

deNoteType :: Type -> Type
	-- Remove synonyms, but not Preds
deNoteType ty@(TyVarTy tyvar)	= ty
deNoteType (TyConApp tycon tys) = TyConApp tycon (map deNoteType tys)
deNoteType (PredTy p)		= PredTy (deNotePred p)
deNoteType (NoteTy _ ty)	= deNoteType ty
deNoteType (AppTy fun arg)	= AppTy (deNoteType fun) (deNoteType arg)
deNoteType (FunTy fun arg)	= FunTy (deNoteType fun) (deNoteType arg)
deNoteType (ForAllTy tv ty)	= ForAllTy tv (deNoteType ty)
deNoteType (UsageTy u ty)	= UsageTy u (deNoteType ty)

deNotePred :: PredType -> PredType
deNotePred (Class c tys) = Class c (map deNoteType tys)
deNotePred (IParam n ty) = IParam n (deNoteType ty)
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
	(e) usage annotations
It's useful in the back end where we're not
interested in newtypes anymore.

\begin{code}
repType :: Type -> Type
repType (ForAllTy _ ty) = repType ty
repType (NoteTy   _ ty) = repType ty
repType (PredTy  p)     = repType (predRepTy p)
repType (UsageTy  _ ty) = repType ty
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
splitNewType_maybe (UsageTy _ ty)    = splitNewType_maybe ty
splitNewType_maybe (TyConApp tc tys) = case newTyConRep tc of
					 Just rep_ty -> ASSERT( length tys == tyConArity tc )
						-- The assert should hold because repType should
						-- only be applied to *types* (of kind *)
							Just (applyTys rep_ty tys)
					 Nothing     -> Nothing
splitNewType_maybe other 	     = Nothing						
\end{code}



---------------------------------------------------------------------
				ForAllTy
				~~~~~~~~

\begin{code}
mkForAllTy :: TyVar -> Type -> Type
mkForAllTy tyvar ty
  = mkForAllTys [tyvar] ty

mkForAllTys :: [TyVar] -> Type -> Type
mkForAllTys tyvars ty
  = case splitUTy_maybe ty of
      Just (u,ty1) -> UASSERT2( not (mkVarSet tyvars `intersectsVarSet` tyVarsOfType u),
                                ptext SLIT("mkForAllTys: usage scope")
                                <+> ppr tyvars <+> pprType ty )
                      mkUTy u (foldr ForAllTy ty1 tyvars)  -- we lift usage annotations over foralls
      Nothing      -> foldr ForAllTy ty tyvars

isForAllTy :: Type -> Bool
isForAllTy (NoteTy _ ty)  = isForAllTy ty
isForAllTy (ForAllTy _ _) = True
isForAllTy (UsageTy _ ty) = isForAllTy ty
isForAllTy other_ty	  = False

splitForAllTy_maybe :: Type -> Maybe (TyVar, Type)
splitForAllTy_maybe ty = splitFAT_m ty
  where
    splitFAT_m (NoteTy _ ty)		= splitFAT_m ty
    splitFAT_m (PredTy p)		= splitFAT_m (predRepTy p)
    splitFAT_m (ForAllTy tyvar ty)	= Just(tyvar, ty)
    splitFAT_m (UsageTy _ ty)           = splitFAT_m ty
    splitFAT_m _			= Nothing

splitForAllTys :: Type -> ([TyVar], Type)
splitForAllTys ty = split ty ty []
   where
     split orig_ty (ForAllTy tv ty)	  tvs = split ty ty (tv:tvs)
     split orig_ty (NoteTy _ ty)	  tvs = split orig_ty ty tvs
     split orig_ty (PredTy p)		  tvs = split orig_ty (predRepTy p) tvs
     split orig_ty (UsageTy _ ty)         tvs = split orig_ty ty tvs
     split orig_ty t			  tvs = (reverse tvs, orig_ty)
\end{code}

-- (mkPiType now in CoreUtils)

Applying a for-all to its arguments.  Lift usage annotation as required.

\begin{code}
applyTy :: Type -> Type -> Type
applyTy (PredTy p) 	                arg = applyTy (predRepTy p) arg
applyTy (NoteTy _ fun)                  arg = applyTy fun arg
applyTy (ForAllTy tv ty)                arg = UASSERT2( not (isUTy arg),
                                                        ptext SLIT("applyTy")
                                                        <+> pprType ty <+> pprType arg )
                                              substTy (mkTyVarSubst [tv] [arg]) ty
applyTy (UsageTy u ty)                  arg = UsageTy u (applyTy ty arg)
applyTy other		                arg = panic "applyTy"

applyTys :: Type -> [Type] -> Type
applyTys fun_ty arg_tys
 = UASSERT2( not (any isUTy arg_tys), ptext SLIT("applyTys") <+> pprType fun_ty )
   (case mu of
      Just u  -> UsageTy u
      Nothing -> id) $
   substTy (mkTyVarSubst tvs arg_tys) ty
 where
   (mu, tvs, ty) = split fun_ty arg_tys
   
   split fun_ty               []         = (Nothing, [], fun_ty)
   split (NoteTy _ fun_ty)    args       = split fun_ty args
   split (PredTy p)	      args       = split (predRepTy p) args
   split (ForAllTy tv fun_ty) (arg:args) = case split fun_ty args of
						  (mu, tvs, ty) -> (mu, tv:tvs, ty)
   split (UsageTy u ty)       args       = case split ty args of
                                                  (Nothing, tvs, ty) -> (Just u, tvs, ty)
                                                  (Just _ , _  , _ ) -> pprPanic "applyTys:"
                                                                          (pprType fun_ty)
   split other_ty             args       = panic "applyTys"
\end{code}

\begin{code}
hoistForAllTys :: Type -> Type
	-- Move all the foralls to the top
	-- e.g.  T -> forall a. a  ==>   forall a. T -> a
        -- Careful: LOSES USAGE ANNOTATIONS!
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


---------------------------------------------------------------------
				UsageTy
				~~~~~~~

Constructing and taking apart usage types.

\begin{code}
mkUTy :: Type -> Type -> Type
mkUTy u ty
  = ASSERT2( typeKind u == usageTypeKind, ptext SLIT("mkUTy:") <+> pprType u <+> pprType ty )
    UASSERT2( not (isUTy ty), ptext SLIT("mkUTy:") <+> pprType u <+> pprType ty )
    -- if u == usMany then ty else  : ToDo? KSW 2000-10
#ifdef DO_USAGES
    UsageTy u ty
#else
    ty
#endif

splitUTy :: Type -> (Type {- :: $ -}, Type)
splitUTy orig_ty
  = case splitUTy_maybe orig_ty of
      Just (u,ty) -> (u,ty)
#ifdef DO_USAGES
      Nothing     -> pprPanic "splitUTy:" (pprType orig_ty)
#else
      Nothing     -> (usMany,orig_ty)  -- default annotation ToDo KSW 2000-10
#endif

splitUTy_maybe :: Type -> Maybe (Type {- :: $ -}, Type)
splitUTy_maybe (UsageTy u ty) = Just (u,ty)
splitUTy_maybe (NoteTy _ ty)  = splitUTy_maybe ty
splitUTy_maybe other_ty       = Nothing

isUTy :: Type -> Bool
  -- has usage annotation
isUTy = maybeToBool . splitUTy_maybe

uaUTy :: Type -> Type
  -- extract annotation
uaUTy = fst . splitUTy

unUTy :: Type -> Type
  -- extract unannotated type
unUTy = snd . splitUTy
\end{code}

\begin{code}
liftUTy :: (Type -> Type) -> Type -> Type
  -- lift outer usage annot over operation on unannotated types
liftUTy f ty
  = let
      (u,ty') = splitUTy ty
    in
    mkUTy u (f ty')
\end{code}

\begin{code}
mkUTyM :: Type -> Type
  -- put TOP (no info) annotation on unannotated type
mkUTyM ty = mkUTy usMany ty
\end{code}

\begin{code}
isUsageKind :: Kind -> Bool
isUsageKind k
  = ASSERT( typeKind k == superKind )
    k == usageTypeKind

isUsage :: Type -> Bool
isUsage ty
  = isUsageKind (typeKind ty)

isUTyVar :: Var -> Bool
isUTyVar v
  = isUsageKind (tyVarKind v)
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
mkClassPred clas tys = UASSERT2( not (any isUTy tys), ppr clas <+> fsep (map pprType tys) )
                       Class clas tys

mkDictTy :: Class -> [Type] -> Type
mkDictTy clas tys = UASSERT2( not (any isUTy tys), ppr clas <+> fsep (map pprType tys) )
                    mkPredTy (Class clas tys)

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
isPredTy (UsageTy _ ty)= isPredTy ty
isPredTy _	       = False

isDictTy :: Type -> Bool
isDictTy (NoteTy _ ty)	      = isDictTy ty
isDictTy (PredTy (Class _ _)) = True
isDictTy (UsageTy _ ty)       = isDictTy ty
isDictTy other		      = False

splitPredTy_maybe :: Type -> Maybe PredType
splitPredTy_maybe (NoteTy _ ty) = splitPredTy_maybe ty
splitPredTy_maybe (PredTy p)    = Just p
splitPredTy_maybe (UsageTy _ ty)= splitPredTy_maybe ty
splitPredTy_maybe other	        = Nothing

splitDictTy :: Type -> (Class, [Type])
splitDictTy (NoteTy _ ty) = splitDictTy ty
splitDictTy (PredTy (Class clas tys)) = (clas, tys)

splitDictTy_maybe :: Type -> Maybe (Class, [Type])
splitDictTy_maybe (NoteTy _ ty) = Just (splitDictTy ty)
splitDictTy_maybe (PredTy (Class clas tys)) = Just (clas, tys)
splitDictTy_maybe other			    = Nothing

splitDFunTy :: Type -> ([TyVar], [PredType], Class, [Type])
-- Split the type of a dictionary function
splitDFunTy ty 
  = case splitSigmaTy ty of { (tvs, theta, tau) -> 
    case splitDictTy tau of { (clas, tys) ->
    (tvs, theta, clas, tys) }}

getClassTys_maybe :: PredType -> Maybe ClassPred
getClassTys_maybe (Class clas tys) = Just (clas, tys)
getClassTys_maybe _		   = Nothing

ipName_maybe :: PredType -> Maybe Name
ipName_maybe (IParam n _) = Just n
ipName_maybe _		  = Nothing

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
isTauTy (UsageTy _ ty)   = isTauTy ty
isTauTy other		 = False
\end{code}

\begin{code}
mkRhoTy :: [PredType] -> Type -> Type
mkRhoTy theta ty = UASSERT2( not (isUTy ty), pprType ty )
                   foldr (\p r -> FunTy (mkUTyM (mkPredTy p)) (mkUTyM r)) ty theta

splitRhoTy :: Type -> ([PredType], Type)
splitRhoTy ty = split ty ty []
 where
  split orig_ty (FunTy arg res) ts = case splitPredTy_maybe arg of
					Just p  -> split res res (p:ts)
					Nothing -> (reverse ts, orig_ty)
  split orig_ty (NoteTy _ ty)	ts = split orig_ty ty ts
  split orig_ty (UsageTy _ ty)  ts = split orig_ty ty ts
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
isSigmaTy (UsageTy _ ty)	= isSigmaTy ty
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
getDFunTyKey (UsageTy _ t)   = getDFunTyKey t
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
typeKind (UsageTy _ ty)         = typeKind ty  -- we don't have separate kinds for ann/unann
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
tyVarsOfType (PredTy p)			= tyVarsOfPred p
tyVarsOfType (FunTy arg res)		= tyVarsOfType arg `unionVarSet` tyVarsOfType res
tyVarsOfType (AppTy fun arg)		= tyVarsOfType fun `unionVarSet` tyVarsOfType arg
tyVarsOfType (ForAllTy tyvar ty)	= tyVarsOfType ty `minusVarSet` unitVarSet tyvar
tyVarsOfType (UsageTy u ty)		= tyVarsOfType u `unionVarSet` tyVarsOfType ty

tyVarsOfTypes :: [Type] -> TyVarSet
tyVarsOfTypes tys = foldr (unionVarSet.tyVarsOfType) emptyVarSet tys

tyVarsOfPred :: PredType -> TyVarSet
tyVarsOfPred (Class clas tys) = tyVarsOfTypes tys
tyVarsOfPred (IParam n ty)    = tyVarsOfType ty

tyVarsOfTheta :: ThetaType -> TyVarSet
tyVarsOfTheta = foldr (unionVarSet . tyVarsOfPred) emptyVarSet

-- Add a Note with the free tyvars to the top of the type
addFreeTyVars :: Type -> Type
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
namesOfType (ForAllTy tyvar ty)		= namesOfType ty `delFromNameSet` getName tyvar
namesOfType (UsageTy u ty)		= namesOfType u `unionNameSets` namesOfType ty

namesOfTypes tys = foldr (unionNameSets . namesOfType) emptyNameSet tys
\end{code}

Usage annotations of a type
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Get a list of usage annotations of a type, *in left-to-right pre-order*.

\begin{code}
usageAnnOfType :: Type -> [Type]
usageAnnOfType ty
  = goS ty
  where
    goT (TyVarTy _)       = []
    goT (AppTy ty1 ty2)   = goT ty1 ++ goT ty2
    goT (TyConApp tc tys) = concatMap goT tys
    goT (FunTy sty1 sty2) = goS sty1 ++ goS sty2
    goT (ForAllTy mv ty)  = goT ty
    goT (PredTy p)        = goT (predRepTy p)
    goT ty@(UsageTy _ _)  = pprPanic "usageAnnOfType: unexpected usage:" (pprType ty)
    goT (NoteTy note ty)  = goT ty

    goS sty = case splitUTy sty of
                (u,tty) -> u : goT tty
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
    go (UsageTy u ty)	    = (UsageTy SAPPLY (go u)) SAPPLY (go ty)

    go_note (SynNote ty)        = SynNote SAPPLY (go ty)
    go_note note@(FTVNote ftvs) = note	-- No need to tidy the free tyvars

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
isUnLiftedType (UsageTy _ ty)	= isUnLiftedType ty
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
seqType (UsageTy u ty)	  = seqType u `seq` seqType ty

seqTypes :: [Type] -> ()
seqTypes []       = ()
seqTypes (ty:tys) = seqType ty `seq` seqTypes tys

seqNote :: TyNote -> ()
seqNote (SynNote ty)  = seqType ty
seqNote (FTVNote set) = sizeUniqSet set `seq` ()

seqPred :: PredType -> ()
seqPred (Class c tys) = c `seq` seqTypes tys
seqPred (IParam n ty) = n `seq` seqType ty
\end{code}


%************************************************************************
%*									*
\subsection{Equality on types}
%*									*
%************************************************************************


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
cmpTy env (UsageTy   u1 t1)   (UsageTy   u2 t2)   = cmpTy env u1 u2 `thenCmp` cmpTy env t1 t2
    
    -- Deal with the rest: TyVarTy < AppTy < FunTy < TyConApp < ForAllTy < UsageTy
cmpTy env (AppTy _ _) (TyVarTy _) = GT
    
cmpTy env (FunTy _ _) (TyVarTy _) = GT
cmpTy env (FunTy _ _) (AppTy _ _) = GT
    
cmpTy env (TyConApp _ _) (TyVarTy _) = GT
cmpTy env (TyConApp _ _) (AppTy _ _) = GT
cmpTy env (TyConApp _ _) (FunTy _ _) = GT
    
cmpTy env (ForAllTy _ _) (TyVarTy _)    = GT
cmpTy env (ForAllTy _ _) (AppTy _ _)    = GT
cmpTy env (ForAllTy _ _) (FunTy _ _)    = GT
cmpTy env (ForAllTy _ _) (TyConApp _ _) = GT

cmpTy env (UsageTy  _ _) other       = GT
    
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
