%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[Type]{Type - public interface}

\begin{code}
module Type (
        -- re-exports from TypeRep:
	Type, PredType, TauType, ThetaType,
	Kind, TyVarSubst,

	superKind, superBoxity,				-- KX and BX respectively
	liftedBoxity, unliftedBoxity, 			-- :: BX
	openKindCon, 					-- :: KX
	typeCon,					-- :: BX -> KX
	liftedTypeKind, unliftedTypeKind, openTypeKind,	-- :: KX
	mkArrowKind, mkArrowKinds,			-- :: KX -> KX -> KX
	isTypeKind,
	funTyCon,

        usageKindCon,					-- :: KX
        usageTypeKind,					-- :: KX
        usOnceTyCon, usManyTyCon,			-- :: $
        usOnce, usMany,					-- :: $

        -- exports from this module:
        hasMoreBoxityInfo, defaultKind,

	mkTyVarTy, mkTyVarTys, getTyVar, getTyVar_maybe, isTyVarTy,

	mkAppTy, mkAppTys, splitAppTy, splitAppTys, splitAppTy_maybe,

	mkFunTy, mkFunTys, splitFunTy, splitFunTy_maybe, splitFunTys, 
	funResultTy, funArgTy, zipFunTys,

	mkTyConApp, mkTyConTy, 
	tyConAppTyCon, tyConAppArgs, 
	splitTyConApp_maybe, splitTyConApp,

	mkUTy, splitUTy, splitUTy_maybe,
        isUTy, uaUTy, unUTy, liftUTy, mkUTyM,
        isUsageKind, isUsage, isUTyVar,

	mkSynTy, 

	repType, splitRepFunTys, typePrimRep,

	mkForAllTy, mkForAllTys, splitForAllTy_maybe, splitForAllTys, 
	applyTy, applyTys, isForAllTy,

	-- Source types
	SourceType(..), sourceTypeRep, mkPredTy, mkPredTys,

	-- Newtypes
	splitNewType_maybe,

	-- Lifting and boxity
	isUnLiftedType, isUnboxedTupleType, isAlgType, isStrictType, isPrimitiveType,

	-- Free variables
	tyVarsOfType, tyVarsOfTypes, tyVarsOfPred, tyVarsOfTheta,
	usageAnnOfType, typeKind, addFreeTyVars,

	-- Tidying up for printing
	tidyType,      tidyTypes,
	tidyOpenType,  tidyOpenTypes,
	tidyTyVarBndr, tidyFreeTyVars,
	tidyOpenTyVar, tidyOpenTyVars,
	tidyTopType,   tidyPred,

	-- Comparison
	eqType, eqKind, eqUsage, 

	-- Seq
	seqType, seqTypes

    ) where

#include "HsVersions.h"

-- We import the representation and primitive functions from TypeRep.
-- Many things are reexported, but not the representation!

import TypeRep

-- Other imports:

import {-# SOURCE #-}	PprType( pprType )	-- Only called in debug messages
import {-# SOURCE #-}   Subst  ( substTyWith )

-- friends:
import Var	( Var, TyVar, tyVarKind, tyVarName, setTyVarName )
import VarEnv
import VarSet

import Name	( NamedThing(..), mkLocalName, tidyOccName )
import Class	( classTyCon )
import TyCon	( TyCon, isRecursiveTyCon, isPrimTyCon,
		  isUnboxedTupleTyCon, isUnLiftedTyCon,
		  isFunTyCon, isNewTyCon, newTyConRep,
		  isAlgTyCon, isSynTyCon, tyConArity, 
	          tyConKind, getSynTyConDefn,
		  tyConPrimRep, 
		)

-- others
import CmdLineOpts	( opt_DictsStrict )
import Maybes		( maybeToBool )
import SrcLoc		( noSrcLoc )
import PrimRep		( PrimRep(..) )
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
  | k2 `eqKind` openTypeKind = True
  | otherwise	  	     = k1 `eqType` k2

defaultKind :: Kind -> Kind
-- Used when generalising: default kind '?' to '*'
defaultKind kind | kind `eqKind` openTypeKind = liftedTypeKind
	         | otherwise	 	      = kind

isTypeKind :: Kind -> Bool
-- True of kind * and *#
isTypeKind k = case splitTyConApp_maybe k of
		 Just (tc,[k]) -> tc == typeCon
		 other	       -> False
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
getTyVar msg (TyVarTy tv)     = tv
getTyVar msg (SourceTy p)     = getTyVar msg (sourceTypeRep p)
getTyVar msg (NoteTy _ t)     = getTyVar msg t
getTyVar msg ty@(UsageTy _ _) = pprPanic "getTyVar: UTy:" (text msg $$ pprType ty)
getTyVar msg other	      = panic ("getTyVar: " ++ msg)

getTyVar_maybe :: Type -> Maybe TyVar
getTyVar_maybe (TyVarTy tv) 	= Just tv
getTyVar_maybe (NoteTy _ t) 	= getTyVar_maybe t
getTyVar_maybe (SourceTy p) 	= getTyVar_maybe (sourceTypeRep p)
getTyVar_maybe ty@(UsageTy _ _) = pprPanic "getTyVar_maybe: UTy:" (pprType ty)
getTyVar_maybe other	        = Nothing

isTyVarTy :: Type -> Bool
isTyVarTy (TyVarTy tv)     = True
isTyVarTy (NoteTy _ ty)    = isTyVarTy ty
isTyVarTy (SourceTy p)     = isTyVarTy (sourceTypeRep p)
isTyVarTy ty@(UsageTy _ _) = pprPanic "isTyVarTy: UTy:" (pprType ty)
isTyVarTy other            = False
\end{code}


---------------------------------------------------------------------
				AppTy
				~~~~~
We need to be pretty careful with AppTy to make sure we obey the 
invariant that a TyConApp is always visibly so.  mkAppTy maintains the
invariant: use it.

\begin{code}
mkAppTy orig_ty1 orig_ty2
  = ASSERT( not (isSourceTy orig_ty1) )	-- Source types are of kind *
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
  = ASSERT( not (isSourceTy orig_ty1) )	-- Source types are of kind *
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
splitAppTy_maybe (SourceTy p)        = splitAppTy_maybe (sourceTypeRep p)
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
    split orig_ty (SourceTy p)            args = split orig_ty (sourceTypeRep p) args
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
splitFunTy (SourceTy p)      = splitFunTy (sourceTypeRep p)
splitFunTy ty@(UsageTy _ _) = pprPanic "splitFunTy: UTy:" (pprType ty)

splitFunTy_maybe :: Type -> Maybe (Type, Type)
splitFunTy_maybe (FunTy arg res) = Just (arg, res)
splitFunTy_maybe (NoteTy _ ty)   = splitFunTy_maybe ty
splitFunTy_maybe (SourceTy p)    	 = splitFunTy_maybe (sourceTypeRep p)
splitFunTy_maybe ty@(UsageTy _ _) = pprPanic "splitFunTy_maybe: UTy:" (pprType ty)
splitFunTy_maybe other	         = Nothing

splitFunTys :: Type -> ([Type], Type)
splitFunTys ty = split [] ty ty
  where
    split args orig_ty (FunTy arg res) = split (arg:args) res res
    split args orig_ty (NoteTy _ ty)   = split args orig_ty ty
    split args orig_ty (SourceTy p)      = split args orig_ty (sourceTypeRep p)
    split args orig_ty (UsageTy _ _)   = pprPanic "splitFunTys: UTy:" (pprType orig_ty)
    split args orig_ty ty              = (reverse args, orig_ty)

zipFunTys :: Outputable a => [a] -> Type -> ([(a,Type)], Type)
zipFunTys orig_xs orig_ty = split [] orig_xs orig_ty orig_ty
  where
    split acc []     nty ty  	         = (reverse acc, nty)
    split acc (x:xs) nty (FunTy arg res) = split ((x,arg):acc) xs res res
    split acc xs     nty (NoteTy _ ty)   = split acc           xs nty ty
    split acc xs     nty (SourceTy p)      = split acc           xs nty (sourceTypeRep p)
    split acc xs     nty (UsageTy _ _)   = pprPanic "zipFunTys: UTy:" (ppr orig_xs <+> pprType orig_ty)
    split acc (x:xs) nty ty              = pprPanic "zipFunTys" (ppr orig_xs <+> pprType orig_ty)
    
funResultTy :: Type -> Type
funResultTy (FunTy arg res) = res
funResultTy (NoteTy _ ty)   = funResultTy ty
funResultTy (SourceTy p)      = funResultTy (sourceTypeRep p)
funResultTy (UsageTy _ ty)  = funResultTy ty
funResultTy ty		    = pprPanic "funResultTy" (pprType ty)

funArgTy :: Type -> Type
funArgTy (FunTy arg res) = arg
funArgTy (NoteTy _ ty)   = funArgTy ty
funArgTy (SourceTy p)      = funArgTy (sourceTypeRep p)
funArgTy (UsageTy _ ty)  = funArgTy ty
funArgTy ty		 = pprPanic "funArgTy" (pprType ty)
\end{code}


---------------------------------------------------------------------
				TyConApp
				~~~~~~~~
@mkTyConApp@ is a key function, because it builds a TyConApp, FunTy or SourceTy,
as apppropriate.

\begin{code}
mkTyConApp :: TyCon -> [Type] -> Type
-- Assumes TyCon is not a SynTyCon; use mkSynTy instead for those
mkTyConApp tycon tys
  | isFunTyCon tycon, [ty1,ty2] <- tys
  = FunTy (mkUTyM ty1) (mkUTyM ty2)

  | isNewTyCon tycon,			-- A saturated newtype application;
    not (isRecursiveTyCon tycon),	-- Not recursive (we don't use SourceTypes for them)
    length tys == tyConArity tycon	-- use the SourceType form
  = SourceTy (NType tycon tys)

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

tyConAppTyCon :: Type -> TyCon
tyConAppTyCon ty = fst (splitTyConApp ty)

tyConAppArgs :: Type -> [Type]
tyConAppArgs ty = snd (splitTyConApp ty)

splitTyConApp :: Type -> (TyCon, [Type])
splitTyConApp ty = case splitTyConApp_maybe ty of
			Just stuff -> stuff
			Nothing	   -> pprPanic "splitTyConApp" (pprType ty)

splitTyConApp_maybe :: Type -> Maybe (TyCon, [Type])
splitTyConApp_maybe (TyConApp tc tys) = Just (tc, tys)
splitTyConApp_maybe (FunTy arg res)   = Just (funTyCon, [unUTy arg,unUTy res])
splitTyConApp_maybe (NoteTy _ ty)     = splitTyConApp_maybe ty
splitTyConApp_maybe (SourceTy p)      = splitTyConApp_maybe (sourceTypeRep p)
splitTyConApp_maybe (UsageTy _ ty)    = splitTyConApp_maybe ty
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
  = foldl AppTy (mk_syn (take arity tys)) (drop arity tys)
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

Remember, non-recursive newtypes get expanded as part of the SourceTy case,
but recursive ones are represented by TyConApps and have to be expanded
by steam.

\begin{code}
repType :: Type -> Type
repType (ForAllTy _ ty)   = repType ty
repType (NoteTy   _ ty)   = repType ty
repType (SourceTy  p)     = repType (sourceTypeRep p)
repType (UsageTy  _ ty)   = repType ty
repType (TyConApp tc tys) | isNewTyCon tc && length tys == tyConArity tc
			  = repType (newTypeRep tc tys)
repType ty	 	  = ty

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
    splitFAT_m (SourceTy p)		= splitFAT_m (sourceTypeRep p)
    splitFAT_m (ForAllTy tyvar ty)	= Just(tyvar, ty)
    splitFAT_m (UsageTy _ ty)           = splitFAT_m ty
    splitFAT_m _			= Nothing

splitForAllTys :: Type -> ([TyVar], Type)
splitForAllTys ty = split ty ty []
   where
     split orig_ty (ForAllTy tv ty)	  tvs = split ty ty (tv:tvs)
     split orig_ty (NoteTy _ ty)	  tvs = split orig_ty ty tvs
     split orig_ty (SourceTy p)		  tvs = split orig_ty (sourceTypeRep p) tvs
     split orig_ty (UsageTy _ ty)         tvs = split orig_ty ty tvs
     split orig_ty t			  tvs = (reverse tvs, orig_ty)
\end{code}

-- (mkPiType now in CoreUtils)

Applying a for-all to its arguments.  Lift usage annotation as required.

\begin{code}
applyTy :: Type -> Type -> Type
applyTy (SourceTy p) 	                arg = applyTy (sourceTypeRep p) arg
applyTy (NoteTy _ fun)                  arg = applyTy fun arg
applyTy (ForAllTy tv ty)                arg = UASSERT2( not (isUTy arg),
                                                        ptext SLIT("applyTy")
                                                        <+> pprType ty <+> pprType arg )
                                              substTyWith [tv] [arg] ty
applyTy (UsageTy u ty)                  arg = UsageTy u (applyTy ty arg)
applyTy other		                arg = panic "applyTy"

applyTys :: Type -> [Type] -> Type
applyTys fun_ty arg_tys
 = UASSERT2( not (any isUTy arg_tys), ptext SLIT("applyTys") <+> pprType fun_ty )
   (case mu of
      Just u  -> UsageTy u
      Nothing -> id) $
   substTyWith tvs arg_tys ty
 where
   (mu, tvs, ty) = split fun_ty arg_tys
   
   split fun_ty               []         = (Nothing, [], fun_ty)
   split (NoteTy _ fun_ty)    args       = split fun_ty args
   split (SourceTy p)	      args       = split (sourceTypeRep p) args
   split (ForAllTy tv fun_ty) (arg:args) = case split fun_ty args of
						  (mu, tvs, ty) -> (mu, tv:tvs, ty)
   split (UsageTy u ty)       args       = case split ty args of
                                                  (Nothing, tvs, ty) -> (Just u, tvs, ty)
                                                  (Just _ , _  , _ ) -> pprPanic "applyTys:"
                                                                          (pprType fun_ty)
   split other_ty             args       = panic "applyTys"
\end{code}


---------------------------------------------------------------------
				UsageTy
				~~~~~~~

Constructing and taking apart usage types.

\begin{code}
mkUTy :: Type -> Type -> Type
mkUTy u ty
  = ASSERT2( typeKind u `eqKind` usageTypeKind, 
	     ptext SLIT("mkUTy:") <+> pprType u <+> pprType ty )
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
  = ASSERT( typeKind k `eqKind` superKind )
    k `eqKind` usageTypeKind

isUsage :: Type -> Bool
isUsage ty
  = isUsageKind (typeKind ty)

isUTyVar :: Var -> Bool
isUTyVar v
  = isUsageKind (tyVarKind v)
\end{code}


%************************************************************************
%*									*
\subsection{Source types}
%*									*
%************************************************************************

A "source type" is a type that is a separate type as far as the type checker is
concerned, but which has low-level representation as far as the back end is concerned.

Source types are always lifted.

The key function is sourceTypeRep which gives the representation of a source type:

\begin{code}
mkPredTy :: PredType -> Type
mkPredTy pred = SourceTy pred

mkPredTys :: ThetaType -> [Type]
mkPredTys preds = map SourceTy preds

sourceTypeRep :: SourceType -> Type
-- Convert a predicate to its "representation type";
-- the type of evidence for that predicate, which is actually passed at runtime
sourceTypeRep (IParam n ty)     = ty
sourceTypeRep (ClassP clas tys) = mkTyConApp (classTyCon clas) tys
	-- Note the mkTyConApp; the classTyCon might be a newtype!
sourceTypeRep (NType  tc tys)   = newTypeRep tc tys
	-- ToDo: Consider caching this substitution in a NType

isSourceTy :: Type -> Bool
isSourceTy (NoteTy _ ty)  = isSourceTy ty
isSourceTy (UsageTy _ ty) = isSourceTy ty
isSourceTy (SourceTy sty) = True
isSourceTy _	          = False


splitNewType_maybe :: Type -> Maybe Type
-- Newtypes that are recursive are reprsented by TyConApp, just
-- as they always were.  Occasionally we want to find their representation type.
-- NB: remember that in this module, non-recursive newtypes are transparent

splitNewType_maybe ty
  = case splitTyConApp_maybe ty of
	Just (tc,tys) | isNewTyCon tc -> ASSERT( length tys == tyConArity tc )
						-- The assert should hold because repType should
						-- only be applied to *types* (of kind *)
					 Just (newTypeRep tc tys)
	other -> Nothing
			
-- A local helper function (not exported)
newTypeRep new_tycon tys = case newTyConRep new_tycon of
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
typeKind (NoteTy _ ty)		= typeKind ty
typeKind (SourceTy _)		= liftedTypeKind -- Predicates are always 
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
tyVarsOfType (SourceTy sty)		= tyVarsOfSourceType sty
tyVarsOfType (FunTy arg res)		= tyVarsOfType arg `unionVarSet` tyVarsOfType res
tyVarsOfType (AppTy fun arg)		= tyVarsOfType fun `unionVarSet` tyVarsOfType arg
tyVarsOfType (ForAllTy tyvar ty)	= tyVarsOfType ty `minusVarSet` unitVarSet tyvar
tyVarsOfType (UsageTy u ty)		= tyVarsOfType u `unionVarSet` tyVarsOfType ty

tyVarsOfTypes :: [Type] -> TyVarSet
tyVarsOfTypes tys = foldr (unionVarSet.tyVarsOfType) emptyVarSet tys

tyVarsOfPred :: PredType -> TyVarSet
tyVarsOfPred = tyVarsOfSourceType	-- Just a subtype

tyVarsOfSourceType :: SourceType -> TyVarSet
tyVarsOfSourceType (IParam n ty)     = tyVarsOfType ty
tyVarsOfSourceType (ClassP clas tys) = tyVarsOfTypes tys
tyVarsOfSourceType (NType tc tys)    = tyVarsOfTypes tys

tyVarsOfTheta :: ThetaType -> TyVarSet
tyVarsOfTheta = foldr (unionVarSet . tyVarsOfSourceType) emptyVarSet

-- Add a Note with the free tyvars to the top of the type
addFreeTyVars :: Type -> Type
addFreeTyVars ty@(NoteTy (FTVNote _) _)      = ty
addFreeTyVars ty			     = NoteTy (FTVNote (tyVarsOfType ty)) ty
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
    goT (SourceTy p)      = goT (sourceTypeRep p)
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
tidyTyVarBndr :: TidyEnv -> TyVar -> (TidyEnv, TyVar)
tidyTyVarBndr (tidy_env, subst) tyvar
  = case tidyOccName tidy_env (getOccName name) of
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
    go (NoteTy note ty)     = (NoteTy SAPPLY (go_note note)) SAPPLY (go ty)
    go (SourceTy sty)	    = SourceTy (tidySourceType env sty)
    go (AppTy fun arg)	    = (AppTy SAPPLY (go fun)) SAPPLY (go arg)
    go (FunTy fun arg)	    = (FunTy SAPPLY (go fun)) SAPPLY (go arg)
    go (ForAllTy tv ty)	    = ForAllTy tvp SAPPLY (tidyType envp ty)
			      where
			        (envp, tvp) = tidyTyVarBndr env tv
    go (UsageTy u ty)	    = (UsageTy SAPPLY (go u)) SAPPLY (go ty)

    go_note (SynNote ty)        = SynNote SAPPLY (go ty)
    go_note note@(FTVNote ftvs) = note	-- No need to tidy the free tyvars

tidyTypes env tys = map (tidyType env) tys

tidyPred :: TidyEnv -> SourceType -> SourceType
tidyPred = tidySourceType

tidySourceType :: TidyEnv -> SourceType -> SourceType
tidySourceType env (IParam n ty)     = IParam n (tidyType env ty)
tidySourceType env (ClassP clas tys) = ClassP clas (tidyTypes env tys)
tidySourceType env (NType tc tys)    = NType  tc   (tidyTypes env tys)
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

isUnLiftedType (ForAllTy tv ty) = isUnLiftedType ty
isUnLiftedType (NoteTy _ ty)	= isUnLiftedType ty
isUnLiftedType (TyConApp tc _)  = isUnLiftedTyCon tc
isUnLiftedType (UsageTy _ ty)	= isUnLiftedType ty
isUnLiftedType (SourceTy _)	= False		-- All source types are lifted
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
\end{code}

@isStrictType@ computes whether an argument (or let RHS) should
be computed strictly or lazily, based only on its type.
Works just like isUnLiftedType, except that it has a special case 
for dictionaries.  Since it takes account of ClassP, you might think
this function should be in TcType, but isStrictType is used by DataCon,
which is below TcType in the hierarchy, so it's convenient to put it here.

\begin{code}
isStrictType (ForAllTy tv ty)		= isStrictType ty
isStrictType (NoteTy _ ty)   		= isStrictType ty
isStrictType (TyConApp tc _)		= isUnLiftedTyCon tc
isStrictType (UsageTy _ ty)		= isStrictType ty
isStrictType (SourceTy (ClassP clas _)) = opt_DictsStrict && not (isNewTyCon (classTyCon clas))
	-- We may be strict in dictionary types, but only if it 
	-- has more than one component.
	-- [Being strict in a single-component dictionary risks
	--  poking the dictionary component, which is wrong.]
isStrictType other			= False	
\end{code}

\begin{code}
isPrimitiveType :: Type -> Bool
-- Returns types that are opaque to Haskell.
-- Most of these are unlifted, but now that we interact with .NET, we
-- may have primtive (foreign-imported) types that are lifted
isPrimitiveType ty = case splitTyConApp_maybe ty of
			Just (tc, ty_args) -> ASSERT( length ty_args == tyConArity tc )
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
seqType (SourceTy p) 	  = seqPred p
seqType (TyConApp tc tys) = tc `seq` seqTypes tys
seqType (ForAllTy tv ty)  = tv `seq` seqType ty
seqType (UsageTy u ty)	  = seqType u `seq` seqType ty

seqTypes :: [Type] -> ()
seqTypes []       = ()
seqTypes (ty:tys) = seqType ty `seq` seqTypes tys

seqNote :: TyNote -> ()
seqNote (SynNote ty)  = seqType ty
seqNote (FTVNote set) = sizeUniqSet set `seq` ()

seqPred :: SourceType -> ()
seqPred (ClassP c tys) = c  `seq` seqTypes tys
seqPred (NType tc tys) = tc `seq` seqTypes tys
seqPred (IParam n ty)  = n  `seq` seqType ty
\end{code}


%************************************************************************
%*									*
\subsection{Equality on types}
%*									*
%************************************************************************

Comparison; don't use instances so that we know where it happens.
Look through newtypes but not usage types.

\begin{code}
eqType t1 t2 = eq_ty emptyVarEnv t1 t2
eqKind  = eqType	-- No worries about looking 
eqUsage = eqType	-- through source types for these two

-- Look through Notes
eq_ty env (NoteTy _ t1)       t2	  	  = eq_ty env t1 t2
eq_ty env t1		      (NoteTy _ t2)       = eq_ty env t1 t2

-- Look through SourceTy.  This is where the looping danger comes from
eq_ty env (SourceTy sty1)     t2		  = eq_ty env (sourceTypeRep sty1) t2
eq_ty env t1		      (SourceTy sty2)     = eq_ty env t1 (sourceTypeRep sty2)

-- The rest is plain sailing
eq_ty env (TyVarTy tv1)       (TyVarTy tv2)       = case lookupVarEnv env tv1 of
							  Just tv1a -> tv1a == tv2
							  Nothing   -> tv1  == tv2
eq_ty env (ForAllTy tv1 t1)   (ForAllTy tv2 t2)   
	| tv1 == tv2				  = eq_ty (delVarEnv env tv1)        t1 t2
	| otherwise				  = eq_ty (extendVarEnv env tv1 tv2) t1 t2
eq_ty env (AppTy s1 t1)       (AppTy s2 t2)       = (eq_ty env s1 s2) && (eq_ty env t1 t2)
eq_ty env (FunTy s1 t1)       (FunTy s2 t2)       = (eq_ty env s1 s2) && (eq_ty env t1 t2)
eq_ty env (UsageTy _ t1)      (UsageTy _ t2)	  = eq_ty env t1 t2
eq_ty env (TyConApp tc1 tys1) (TyConApp tc2 tys2) = (tc1 == tc2) && (eq_tys env tys1 tys2)
eq_ty env t1		       t2		  = False

eq_tys env []        []        = True
eq_tys env (t1:tys1) (t2:tys2) = (eq_ty env t1 t2) && (eq_tys env tys1 tys2)
eq_tys env tys1      tys2      = False
\end{code}

