
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcType]{Types used in the typechecker}

This module provides the Type interface for front-end parts of the 
compiler.  These parts 

	* treat "source types" as opaque: 
		newtypes, and predicates are meaningful. 
	* look through usage types

The "tc" prefix is for "typechechecker", because the type checker
is the principal client.

\begin{code}
module TcType (
  --------------------------------
  -- Types 
  TcType, TcSigmaType, TcRhoType, TcTauType, TcPredType, TcThetaType, 
  TcTyVar, TcTyVarSet, TcKind, 

  --------------------------------
  -- MetaDetails
  TcTyVarDetails(..),
  MetaDetails(Flexi, Indirect), SkolemInfo(..), pprSkolemTyVar,
  isImmutableTyVar, isSkolemTyVar, isMetaTyVar, skolemTvInfo, metaTvRef,
  isFlexi, isIndirect,

  --------------------------------
  -- Builders
  mkPhiTy, mkSigmaTy, 

  --------------------------------
  -- Splitters  
  -- These are important because they do not look through newtypes
  tcSplitForAllTys, tcSplitPhiTy, 
  tcSplitFunTy_maybe, tcSplitFunTys, tcFunArgTy, tcFunResultTy,
  tcSplitTyConApp, tcSplitTyConApp_maybe, tcTyConAppTyCon, tcTyConAppArgs,
  tcSplitAppTy_maybe, tcSplitAppTy, tcSplitAppTys, tcSplitSigmaTy,
  tcSplitMethodTy, tcGetTyVar_maybe, tcGetTyVar,

  ---------------------------------
  -- Predicates. 
  -- Again, newtypes are opaque
  tcEqType, tcEqTypes, tcEqPred, tcCmpType, tcCmpTypes, tcCmpPred,
  isSigmaTy, isOverloadedTy, 
  isDoubleTy, isFloatTy, isIntTy,
  isIntegerTy, isAddrTy, isBoolTy, isUnitTy,
  isTauTy, tcIsTyVarTy, tcIsForAllTy,

  ---------------------------------
  -- Misc type manipulators
  deNoteType, classesOfTheta,
  tyClsNamesOfType, tyClsNamesOfDFunHead, 
  getDFunTyKey,

  ---------------------------------
  -- Predicate types  
  getClassPredTys_maybe, getClassPredTys, 
  isClassPred, isTyVarClassPred, 
  mkDictTy, tcSplitPredTy_maybe, 
  isPredTy, isDictTy, tcSplitDFunTy, predTyUnique, 
  mkClassPred, isInheritablePred, isLinearPred, isIPPred, mkPredName, 

  ---------------------------------
  -- Foreign import and export
  isFFIArgumentTy,     -- :: DynFlags -> Safety -> Type -> Bool
  isFFIImportResultTy, -- :: DynFlags -> Type -> Bool
  isFFIExportResultTy, -- :: Type -> Bool
  isFFIExternalTy,     -- :: Type -> Bool
  isFFIDynArgumentTy,  -- :: Type -> Bool
  isFFIDynResultTy,    -- :: Type -> Bool
  isFFILabelTy,        -- :: Type -> Bool
  isFFIDotnetTy,       -- :: DynFlags -> Type -> Bool
  isFFIDotnetObjTy,    -- :: Type -> Bool
  isFFITy,	       -- :: Type -> Bool
  
  toDNType,            -- :: Type -> DNType

  --------------------------------
  -- Rexported from Type
  Kind, 	-- Stuff to do with kinds is insensitive to pre/post Tc
  unliftedTypeKind, liftedTypeKind, openTypeKind, mkArrowKind, mkArrowKinds, 
  isLiftedTypeKind, isUnliftedTypeKind, isOpenTypeKind, 
  isArgTypeKind, isSubKind, defaultKind, 

  Type, PredType(..), ThetaType, 
  mkForAllTy, mkForAllTys, 
  mkFunTy, mkFunTys, zipFunTys, 
  mkTyConApp, mkGenTyConApp, mkAppTy, mkAppTys, mkSynTy, applyTy, applyTys,
  mkTyVarTy, mkTyVarTys, mkTyConTy, mkPredTy, mkPredTys, 

  -- Type substitutions
  TvSubst(..), 	-- Representation visible to a few friends
  TvSubstEnv, emptyTvSubst,
  mkTvSubst, zipTvSubst, zipTopTvSubst, mkTopTvSubst,
  getTvSubstEnv, setTvSubstEnv, getTvInScope, extendTvInScope,
  extendTvSubst, extendTvSubstList, isInScope,
  substTy, substTys, substTyWith, substTheta, substTyVar, 

  isUnLiftedType,	-- Source types are always lifted
  isUnboxedTupleType,	-- Ditto
  isPrimitiveType, 

  tidyTopType, tidyType, tidyPred, tidyTypes, tidyFreeTyVars, tidyOpenType, tidyOpenTypes,
  tidyTyVarBndr, tidyOpenTyVar, tidyOpenTyVars,
  typeKind, 

  tyVarsOfType, tyVarsOfTypes, tyVarsOfPred, tyVarsOfTheta,

  pprKind, pprParendKind,
  pprType, pprParendType, pprTyThingCategory,
  pprPred, pprTheta, pprThetaArrow, pprClassPred

  ) where

#include "HsVersions.h"

-- friends:
import TypeRep		( Type(..), TyNote(..), funTyCon )  -- friend

import Type		(	-- Re-exports
			  tyVarsOfType, tyVarsOfTypes, tyVarsOfPred,
			  tyVarsOfTheta, Kind, Type, PredType(..),
			  ThetaType, unliftedTypeKind, 
			  liftedTypeKind, openTypeKind, mkArrowKind,
		  	  isLiftedTypeKind, isUnliftedTypeKind, 
			  isOpenTypeKind, 
			  mkArrowKinds, mkForAllTy, mkForAllTys,
			  defaultKind, isArgTypeKind, isOpenTypeKind,
			  mkFunTy, mkFunTys, zipFunTys, 
			  mkTyConApp, mkGenTyConApp, mkAppTy,
			  mkAppTys, mkSynTy, applyTy, applyTys,
			  mkTyVarTy, mkTyVarTys, mkTyConTy, mkPredTy,
			  mkPredTys, isUnLiftedType, 
			  isUnboxedTupleType, isPrimitiveType,
			  splitTyConApp_maybe,
			  tidyTopType, tidyType, tidyPred, tidyTypes,
			  tidyFreeTyVars, tidyOpenType, tidyOpenTypes,
			  tidyTyVarBndr, tidyOpenTyVar,
			  tidyOpenTyVars, 
			  isSubKind, 
			  TvSubst(..),
			  TvSubstEnv, emptyTvSubst,
			  mkTvSubst, zipTvSubst, zipTopTvSubst, mkTopTvSubst,
			  getTvSubstEnv, setTvSubstEnv, getTvInScope, extendTvInScope,
			  extendTvSubst, extendTvSubstList, isInScope,
		  	  substTy, substTys, substTyWith, substTheta, substTyVar, 

			  typeKind, repType,
			  pprKind, pprParendKind,
			  pprType, pprParendType, pprTyThingCategory,
			  pprPred, pprTheta, pprThetaArrow, pprClassPred
			)
import TyCon		( TyCon, isUnLiftedTyCon, tyConUnique )
import DataCon		( DataCon )
import Class		( Class )
import Var		( TyVar, Id, isTcTyVar, tcTyVarDetails )
import ForeignCall	( Safety, playSafe, DNType(..) )
import VarEnv
import VarSet

-- others:
import CmdLineOpts	( DynFlags, DynFlag( Opt_GlasgowExts ), dopt )
import Name		( Name, NamedThing(..), mkInternalName, getSrcLoc )
import NameSet
import OccName		( OccName, mkDictOcc )
import PrelNames	-- Lots (e.g. in isFFIArgumentTy)
import TysWiredIn	( unitTyCon, charTyCon, listTyCon )
import BasicTypes	( IPName(..), ipNameName )
import Unique		( Unique, Uniquable(..) )
import SrcLoc		( SrcLoc, SrcSpan )
import Util		( cmpList, thenCmp, snocView )
import Maybes		( maybeToBool, expectJust )
import Outputable
import DATA_IOREF
\end{code}


%************************************************************************
%*									*
\subsection{Types}
%*									*
%************************************************************************

The type checker divides the generic Type world into the 
following more structured beasts:

sigma ::= forall tyvars. phi
	-- A sigma type is a qualified type
	--
	-- Note that even if 'tyvars' is empty, theta
	-- may not be: e.g.   (?x::Int) => Int

	-- Note that 'sigma' is in prenex form:
	-- all the foralls are at the front.
	-- A 'phi' type has no foralls to the right of
	-- an arrow

phi :: theta => rho

rho ::= sigma -> rho
     |  tau

-- A 'tau' type has no quantification anywhere
-- Note that the args of a type constructor must be taus
tau ::= tyvar
     |  tycon tau_1 .. tau_n
     |  tau_1 tau_2
     |  tau_1 -> tau_2

-- In all cases, a (saturated) type synonym application is legal,
-- provided it expands to the required form.

\begin{code}
type TcType = Type 		-- A TcType can have mutable type variables
	-- Invariant on ForAllTy in TcTypes:
	-- 	forall a. T
	-- a cannot occur inside a MutTyVar in T; that is,
	-- T is "flattened" before quantifying over a

type TcPredType     = PredType
type TcThetaType    = ThetaType
type TcSigmaType    = TcType
type TcRhoType      = TcType
type TcTauType      = TcType
type TcKind         = Kind
type TcTyVarSet     = TyVarSet
\end{code}


%************************************************************************
%*									*
\subsection{TyVarDetails}
%*									*
%************************************************************************

TyVarDetails gives extra info about type variables, used during type
checking.  It's attached to mutable type variables only.
It's knot-tied back to Var.lhs.  There is no reason in principle
why Var.lhs shouldn't actually have the definition, but it "belongs" here.

\begin{code}
type TcTyVar = TyVar  	-- Used only during type inference

-- A TyVarDetails is inside a TyVar
data TcTyVarDetails
  = SkolemTv SkolemInfo		-- A skolem constant
  | MetaTv (IORef MetaDetails)	-- A meta type variable stands for a tau-type

data SkolemInfo
  = SigSkol Name	-- Bound at a type signature
  | ClsSkol Class	-- Bound at a class decl
  | InstSkol Id		-- Bound at an instance decl
  | PatSkol DataCon	-- An existential type variable bound by a pattern for
	    SrcSpan	-- a data constructor with an existential type. E.g.
			--	data T = forall a. Eq a => MkT a
			-- 	f (MkT x) = ...
			-- The pattern MkT x will allocate an existential type
			-- variable for 'a'.  
  | ArrowSkol SrcSpan	-- An arrow form (see TcArrows)

  | GenSkol TcType	-- Bound when doing a subsumption check for this type
	    SrcSpan

data MetaDetails
  = Flexi          -- Flexi type variables unify to become 
                   -- Indirects.  

  | Indirect TcType  -- Type indirections, treated as wobbly 
                     -- for the purpose of GADT unification.

pprSkolemTyVar :: TcTyVar -> SDoc
pprSkolemTyVar tv
  = ASSERT( isSkolemTyVar tv )
    quotes (ppr tv) <+> ptext SLIT("is bound by") <+> ppr (skolemTvInfo tv)

instance Outputable SkolemInfo where
  ppr (SigSkol id)  = ptext SLIT("the type signature for") <+> quotes (ppr id)
  ppr (ClsSkol cls) = ptext SLIT("the class declaration for") <+> quotes (ppr cls)
  ppr (InstSkol df) = ptext SLIT("the instance declaration at") <+> ppr (getSrcLoc df)
  ppr (ArrowSkol loc)  = ptext SLIT("the arrow form at") <+> ppr loc
  ppr (PatSkol dc loc) = sep [ptext SLIT("the pattern for") <+> quotes (ppr dc),
	       		    nest 2 (ptext SLIT("at") <+> ppr loc)]
  ppr (GenSkol ty loc) = sep [ptext SLIT("the polymorphic type") <+> quotes (ppr ty),
			    nest 2 (ptext SLIT("at") <+> ppr loc)]

instance Outputable MetaDetails where
  ppr Flexi 	    = ptext SLIT("Flexi")
  ppr (Indirect ty) = ptext SLIT("Indirect") <+> ppr ty

isImmutableTyVar, isSkolemTyVar, isMetaTyVar :: TyVar -> Bool
isImmutableTyVar tv
  | isTcTyVar tv = isSkolemTyVar tv
  | otherwise    = True

isSkolemTyVar tv 
  = ASSERT( isTcTyVar tv )
    case tcTyVarDetails tv of
	SkolemTv _ -> True
	MetaTv _   -> False

isMetaTyVar tv 
  = ASSERT( isTcTyVar tv )
    case tcTyVarDetails tv of
	SkolemTv _ -> False
	MetaTv _   -> True

skolemTvInfo :: TyVar -> SkolemInfo
skolemTvInfo tv 
  = ASSERT( isTcTyVar tv )
    case tcTyVarDetails tv of
	SkolemTv info -> info

metaTvRef :: TyVar -> IORef MetaDetails
metaTvRef tv 
  = ASSERT( isTcTyVar tv )
    case tcTyVarDetails tv of
	 MetaTv ref -> ref

isFlexi, isIndirect :: MetaDetails -> Bool
isFlexi Flexi = True
isFlexi other = False

isIndirect (Indirect _) = True
isIndirect other        = False
\end{code}


%************************************************************************
%*									*
\subsection{Tau, sigma and rho}
%*									*
%************************************************************************

\begin{code}
mkSigmaTy tyvars theta tau = mkForAllTys tyvars (mkPhiTy theta tau)

mkPhiTy :: [PredType] -> Type -> Type
mkPhiTy theta ty = foldr (\p r -> FunTy (mkPredTy p) r) ty theta
\end{code}

@isTauTy@ tests for nested for-alls.

\begin{code}
isTauTy :: Type -> Bool
isTauTy (TyVarTy v)	 = True
isTauTy (TyConApp _ tys) = all isTauTy tys
isTauTy (NewTcApp _ tys) = all isTauTy tys
isTauTy (AppTy a b)	 = isTauTy a && isTauTy b
isTauTy (FunTy a b)	 = isTauTy a && isTauTy b
isTauTy (PredTy p)	 = True		-- Don't look through source types
isTauTy (NoteTy _ ty)	 = isTauTy ty
isTauTy other		 = False
\end{code}

\begin{code}
getDFunTyKey :: Type -> OccName	-- Get some string from a type, to be used to 
				-- construct a dictionary function name
getDFunTyKey (TyVarTy tv)    = getOccName tv
getDFunTyKey (TyConApp tc _) = getOccName tc
getDFunTyKey (NewTcApp tc _) = getOccName tc
getDFunTyKey (AppTy fun _)   = getDFunTyKey fun
getDFunTyKey (NoteTy _ t)    = getDFunTyKey t
getDFunTyKey (FunTy arg _)   = getOccName funTyCon
getDFunTyKey (ForAllTy _ t)  = getDFunTyKey t
getDFunTyKey ty		     = pprPanic "getDFunTyKey" (pprType ty)
-- PredTy shouldn't happen
\end{code}


%************************************************************************
%*									*
\subsection{Expanding and splitting}
%*									*
%************************************************************************

These tcSplit functions are like their non-Tc analogues, but
	a) they do not look through newtypes
	b) they do not look through PredTys
	c) [future] they ignore usage-type annotations

However, they are non-monadic and do not follow through mutable type
variables.  It's up to you to make sure this doesn't matter.

\begin{code}
tcSplitForAllTys :: Type -> ([TyVar], Type)
tcSplitForAllTys ty = split ty ty []
   where
     split orig_ty (ForAllTy tv ty) tvs = split ty ty (tv:tvs)
     split orig_ty (NoteTy n  ty)   tvs = split orig_ty ty tvs
     split orig_ty t		    tvs = (reverse tvs, orig_ty)

tcIsForAllTy (ForAllTy tv ty) = True
tcIsForAllTy (NoteTy n ty)    = tcIsForAllTy ty
tcIsForAllTy t		      = False

tcSplitPhiTy :: Type -> ([PredType], Type)
tcSplitPhiTy ty = split ty ty []
 where
  split orig_ty (FunTy arg res) ts = case tcSplitPredTy_maybe arg of
					Just p  -> split res res (p:ts)
					Nothing -> (reverse ts, orig_ty)
  split orig_ty (NoteTy n ty)	ts = split orig_ty ty ts
  split orig_ty ty		ts = (reverse ts, orig_ty)

tcSplitSigmaTy ty = case tcSplitForAllTys ty of
			(tvs, rho) -> case tcSplitPhiTy rho of
					(theta, tau) -> (tvs, theta, tau)

tcTyConAppTyCon :: Type -> TyCon
tcTyConAppTyCon ty = fst (tcSplitTyConApp ty)

tcTyConAppArgs :: Type -> [Type]
tcTyConAppArgs ty = snd (tcSplitTyConApp ty)

tcSplitTyConApp :: Type -> (TyCon, [Type])
tcSplitTyConApp ty = case tcSplitTyConApp_maybe ty of
			Just stuff -> stuff
			Nothing	   -> pprPanic "tcSplitTyConApp" (pprType ty)

tcSplitTyConApp_maybe :: Type -> Maybe (TyCon, [Type])
tcSplitTyConApp_maybe (TyConApp tc tys) = Just (tc, tys)
tcSplitTyConApp_maybe (NewTcApp tc tys) = Just (tc, tys)
tcSplitTyConApp_maybe (FunTy arg res)   = Just (funTyCon, [arg,res])
tcSplitTyConApp_maybe (NoteTy n ty)     = tcSplitTyConApp_maybe ty
	-- Newtypes are opaque, so they may be split
	-- However, predicates are not treated
	-- as tycon applications by the type checker
tcSplitTyConApp_maybe other	        	= Nothing

tcSplitFunTys :: Type -> ([Type], Type)
tcSplitFunTys ty = case tcSplitFunTy_maybe ty of
			Nothing	       -> ([], ty)
			Just (arg,res) -> (arg:args, res')
				       where
					  (args,res') = tcSplitFunTys res

tcSplitFunTy_maybe :: Type -> Maybe (Type, Type)
tcSplitFunTy_maybe (FunTy arg res)  = Just (arg, res)
tcSplitFunTy_maybe (NoteTy n ty)    = tcSplitFunTy_maybe ty
tcSplitFunTy_maybe other	    = Nothing

tcFunArgTy    ty = case tcSplitFunTy_maybe ty of { Just (arg,res) -> arg }
tcFunResultTy ty = case tcSplitFunTy_maybe ty of { Just (arg,res) -> res }


tcSplitAppTy_maybe :: Type -> Maybe (Type, Type)
tcSplitAppTy_maybe (FunTy ty1 ty2)   = Just (TyConApp funTyCon [ty1], ty2)
tcSplitAppTy_maybe (AppTy ty1 ty2)   = Just (ty1, ty2)
tcSplitAppTy_maybe (NoteTy n ty)     = tcSplitAppTy_maybe ty
tcSplitAppTy_maybe (TyConApp tc tys) = case snocView tys of
					Just (tys', ty') -> Just (TyConApp tc tys', ty')
					Nothing		 -> Nothing
tcSplitAppTy_maybe (NewTcApp tc tys) = case snocView tys of
					Just (tys', ty') -> Just (NewTcApp tc tys', ty')
					Nothing		 -> Nothing
tcSplitAppTy_maybe other	     = Nothing

tcSplitAppTy ty = case tcSplitAppTy_maybe ty of
		    Just stuff -> stuff
		    Nothing    -> pprPanic "tcSplitAppTy" (pprType ty)

tcSplitAppTys :: Type -> (Type, [Type])
tcSplitAppTys ty
  = go ty []
  where
    go ty args = case tcSplitAppTy_maybe ty of
		   Just (ty', arg) -> go ty' (arg:args)
		   Nothing	   -> (ty,args)

tcGetTyVar_maybe :: Type -> Maybe TyVar
tcGetTyVar_maybe (TyVarTy tv) 	= Just tv
tcGetTyVar_maybe (NoteTy _ t) 	= tcGetTyVar_maybe t
tcGetTyVar_maybe other	        = Nothing

tcGetTyVar :: String -> Type -> TyVar
tcGetTyVar msg ty = expectJust msg (tcGetTyVar_maybe ty)

tcIsTyVarTy :: Type -> Bool
tcIsTyVarTy ty = maybeToBool (tcGetTyVar_maybe ty)
\end{code}

The type of a method for class C is always of the form:
	Forall a1..an. C a1..an => sig_ty
where sig_ty is the type given by the method's signature, and thus in general
is a ForallTy.  At the point that splitMethodTy is called, it is expected
that the outer Forall has already been stripped off.  splitMethodTy then
returns (C a1..an, sig_ty') where sig_ty' is sig_ty with any Notes stripped off.

\begin{code}
tcSplitMethodTy :: Type -> (PredType, Type)
tcSplitMethodTy ty = split ty
 where
  split (FunTy arg res) = case tcSplitPredTy_maybe arg of
			    Just p  -> (p, res)
			    Nothing -> panic "splitMethodTy"
  split (NoteTy n ty)	= split ty
  split _               = panic "splitMethodTy"

tcSplitDFunTy :: Type -> ([TyVar], [PredType], Class, [Type])
-- Split the type of a dictionary function
tcSplitDFunTy ty 
  = case tcSplitSigmaTy ty       of { (tvs, theta, tau) ->
    case tcSplitPredTy_maybe tau of { Just (ClassP clas tys) -> 
    (tvs, theta, clas, tys) }}
\end{code}



%************************************************************************
%*									*
\subsection{Predicate types}
%*									*
%************************************************************************

\begin{code}
tcSplitPredTy_maybe :: Type -> Maybe PredType
   -- Returns Just for predicates only
tcSplitPredTy_maybe (NoteTy _ ty) = tcSplitPredTy_maybe ty
tcSplitPredTy_maybe (PredTy p)    = Just p
tcSplitPredTy_maybe other	  = Nothing
	
predTyUnique :: PredType -> Unique
predTyUnique (IParam n _)      = getUnique (ipNameName n)
predTyUnique (ClassP clas tys) = getUnique clas

mkPredName :: Unique -> SrcLoc -> PredType -> Name
mkPredName uniq loc (ClassP cls tys) = mkInternalName uniq (mkDictOcc (getOccName cls)) loc
mkPredName uniq loc (IParam ip ty)   = mkInternalName uniq (getOccName (ipNameName ip)) loc
\end{code}


--------------------- Dictionary types ---------------------------------

\begin{code}
mkClassPred clas tys = ClassP clas tys

isClassPred :: PredType -> Bool
isClassPred (ClassP clas tys) = True
isClassPred other	      = False

isTyVarClassPred (ClassP clas tys) = all tcIsTyVarTy tys
isTyVarClassPred other		   = False

getClassPredTys_maybe :: PredType -> Maybe (Class, [Type])
getClassPredTys_maybe (ClassP clas tys) = Just (clas, tys)
getClassPredTys_maybe _		        = Nothing

getClassPredTys :: PredType -> (Class, [Type])
getClassPredTys (ClassP clas tys) = (clas, tys)

mkDictTy :: Class -> [Type] -> Type
mkDictTy clas tys = mkPredTy (ClassP clas tys)

isDictTy :: Type -> Bool
isDictTy (PredTy p)   = isClassPred p
isDictTy (NoteTy _ ty)	= isDictTy ty
isDictTy other		= False
\end{code}

--------------------- Implicit parameters ---------------------------------

\begin{code}
isIPPred :: PredType -> Bool
isIPPred (IParam _ _) = True
isIPPred other	      = False

isInheritablePred :: PredType -> Bool
-- Can be inherited by a context.  For example, consider
--	f x = let g y = (?v, y+x)
--	      in (g 3 with ?v = 8, 
--		  g 4 with ?v = 9)
-- The point is that g's type must be quantifed over ?v:
--	g :: (?v :: a) => a -> a
-- but it doesn't need to be quantified over the Num a dictionary
-- which can be free in g's rhs, and shared by both calls to g
isInheritablePred (ClassP _ _) = True
isInheritablePred other	     = False

isLinearPred :: TcPredType -> Bool
isLinearPred (IParam (Linear n) _) = True
isLinearPred other		   = False
\end{code}


%************************************************************************
%*									*
\subsection{Comparison}
%*									*
%************************************************************************

Comparison, taking note of newtypes, predicates, etc,

\begin{code}
tcEqType :: Type -> Type -> Bool
tcEqType ty1 ty2 = case ty1 `tcCmpType` ty2 of { EQ -> True; other -> False }

tcEqTypes :: [Type] -> [Type] -> Bool
tcEqTypes ty1 ty2 = case ty1 `tcCmpTypes` ty2 of { EQ -> True; other -> False }

tcEqPred :: PredType -> PredType -> Bool
tcEqPred p1 p2 = case p1 `tcCmpPred` p2 of { EQ -> True; other -> False }

-------------
tcCmpType :: Type -> Type -> Ordering
tcCmpType ty1 ty2 = cmpTy emptyVarEnv ty1 ty2

tcCmpTypes tys1 tys2 = cmpTys emptyVarEnv tys1 tys2

tcCmpPred p1 p2 = cmpPredTy emptyVarEnv p1 p2
-------------
cmpTys env tys1 tys2 = cmpList (cmpTy env) tys1 tys2

-------------
cmpTy :: TyVarEnv TyVar -> Type -> Type -> Ordering
  -- The "env" maps type variables in ty1 to type variables in ty2
  -- So when comparing for-alls.. (forall tv1 . t1) (forall tv2 . t2)
  -- we in effect substitute tv2 for tv1 in t1 before continuing

    -- Look through NoteTy
cmpTy env (NoteTy _ ty1) ty2 = cmpTy env ty1 ty2
cmpTy env ty1 (NoteTy _ ty2) = cmpTy env ty1 ty2

    -- Deal with equal constructors
cmpTy env (TyVarTy tv1) (TyVarTy tv2) = case lookupVarEnv env tv1 of
					  Just tv1a -> tv1a `compare` tv2
					  Nothing   -> tv1  `compare` tv2

cmpTy env (PredTy p1) (PredTy p2) = cmpPredTy env p1 p2
cmpTy env (AppTy f1 a1) (AppTy f2 a2) = cmpTy env f1 f2 `thenCmp` cmpTy env a1 a2
cmpTy env (FunTy f1 a1) (FunTy f2 a2) = cmpTy env f1 f2 `thenCmp` cmpTy env a1 a2
cmpTy env (TyConApp tc1 tys1) (TyConApp tc2 tys2) = (tc1 `compare` tc2) `thenCmp` (cmpTys env tys1 tys2)
cmpTy env (NewTcApp tc1 tys1) (NewTcApp tc2 tys2) = (tc1 `compare` tc2) `thenCmp` (cmpTys env tys1 tys2)
cmpTy env (ForAllTy tv1 t1)   (ForAllTy tv2 t2)   = cmpTy (extendVarEnv env tv1 tv2) t1 t2
    
    -- Deal with the rest: TyVarTy < AppTy < FunTy < TyConApp < NewTcApp < ForAllTy < PredTy
cmpTy env (AppTy _ _) (TyVarTy _) = GT
    
cmpTy env (FunTy _ _) (TyVarTy _) = GT
cmpTy env (FunTy _ _) (AppTy _ _) = GT
    
cmpTy env (TyConApp _ _) (TyVarTy _) = GT
cmpTy env (TyConApp _ _) (AppTy _ _) = GT
cmpTy env (TyConApp _ _) (FunTy _ _) = GT
    
cmpTy env (NewTcApp _ _) (TyVarTy _) 	= GT
cmpTy env (NewTcApp _ _) (AppTy _ _) 	= GT
cmpTy env (NewTcApp _ _) (FunTy _ _) 	= GT
cmpTy env (NewTcApp _ _) (TyConApp _ _) = GT
    
cmpTy env (ForAllTy _ _) (TyVarTy _)    = GT
cmpTy env (ForAllTy _ _) (AppTy _ _)    = GT
cmpTy env (ForAllTy _ _) (FunTy _ _)    = GT
cmpTy env (ForAllTy _ _) (TyConApp _ _) = GT
cmpTy env (ForAllTy _ _) (NewTcApp _ _) = GT

cmpTy env (PredTy _)   t2		= GT

cmpTy env _ _ = LT
\end{code}

\begin{code}
cmpPredTy :: TyVarEnv TyVar -> PredType -> PredType -> Ordering
cmpPredTy env (IParam n1 ty1) (IParam n2 ty2) = (n1 `compare` n2) `thenCmp` (cmpTy env ty1 ty2)
	-- Compare types as well as names for implicit parameters
	-- This comparison is used exclusively (I think) for the
	-- finite map built in TcSimplify
cmpPredTy env (IParam _ _)     (ClassP _ _)	  = LT
cmpPredTy env (ClassP _ _)     (IParam _ _)     = GT
cmpPredTy env (ClassP c1 tys1) (ClassP c2 tys2) = (c1 `compare` c2) `thenCmp` (cmpTys env tys1 tys2)
\end{code}

PredTypes are used as a FM key in TcSimplify, 
so we take the easy path and make them an instance of Ord

\begin{code}
instance Eq  PredType where { (==)    = tcEqPred }
instance Ord PredType where { compare = tcCmpPred }
\end{code}


%************************************************************************
%*									*
\subsection{Predicates}
%*									*
%************************************************************************

isSigmaTy returns true of any qualified type.  It doesn't *necessarily* have 
any foralls.  E.g.
	f :: (?x::Int) => Int -> Int

\begin{code}
isSigmaTy :: Type -> Bool
isSigmaTy (ForAllTy tyvar ty) = True
isSigmaTy (FunTy a b)	      = isPredTy a
isSigmaTy (NoteTy n ty)	      = isSigmaTy ty
isSigmaTy _		      = False

isOverloadedTy :: Type -> Bool
isOverloadedTy (ForAllTy tyvar ty) = isOverloadedTy ty
isOverloadedTy (FunTy a b)	   = isPredTy a
isOverloadedTy (NoteTy n ty)	   = isOverloadedTy ty
isOverloadedTy _		   = False

isPredTy :: Type -> Bool	-- Belongs in TcType because it does 
				-- not look through newtypes, or predtypes (of course)
isPredTy (NoteTy _ ty) = isPredTy ty
isPredTy (PredTy sty)  = True
isPredTy _	       = False
\end{code}

\begin{code}
isFloatTy      = is_tc floatTyConKey
isDoubleTy     = is_tc doubleTyConKey
isIntegerTy    = is_tc integerTyConKey
isIntTy        = is_tc intTyConKey
isAddrTy       = is_tc addrTyConKey
isBoolTy       = is_tc boolTyConKey
isUnitTy       = is_tc unitTyConKey

is_tc :: Unique -> Type -> Bool
-- Newtypes are opaque to this
is_tc uniq ty = case tcSplitTyConApp_maybe ty of
			Just (tc, _) -> uniq == getUnique tc
			Nothing	     -> False
\end{code}


%************************************************************************
%*									*
\subsection{Misc}
%*									*
%************************************************************************

\begin{code}
deNoteType :: Type -> Type
	-- Remove synonyms, but not predicate types
deNoteType ty@(TyVarTy tyvar)	= ty
deNoteType (TyConApp tycon tys) = TyConApp tycon (map deNoteType tys)
deNoteType (NewTcApp tycon tys) = NewTcApp tycon (map deNoteType tys)
deNoteType (PredTy p)		= PredTy (deNotePredType p)
deNoteType (NoteTy _ ty)	= deNoteType ty
deNoteType (AppTy fun arg)	= AppTy (deNoteType fun) (deNoteType arg)
deNoteType (FunTy fun arg)	= FunTy (deNoteType fun) (deNoteType arg)
deNoteType (ForAllTy tv ty)	= ForAllTy tv (deNoteType ty)

deNotePredType :: PredType -> PredType
deNotePredType (ClassP c tys)   = ClassP c (map deNoteType tys)
deNotePredType (IParam n ty)    = IParam n (deNoteType ty)
\end{code}

Find the free tycons and classes of a type.  This is used in the front
end of the compiler.

\begin{code}
tyClsNamesOfType :: Type -> NameSet
tyClsNamesOfType (TyVarTy tv)		    = emptyNameSet
tyClsNamesOfType (TyConApp tycon tys)	    = unitNameSet (getName tycon) `unionNameSets` tyClsNamesOfTypes tys
tyClsNamesOfType (NewTcApp tycon tys)	    = unitNameSet (getName tycon) `unionNameSets` tyClsNamesOfTypes tys
tyClsNamesOfType (NoteTy (SynNote ty1) ty2) = tyClsNamesOfType ty1
tyClsNamesOfType (NoteTy other_note    ty2) = tyClsNamesOfType ty2
tyClsNamesOfType (PredTy (IParam n ty))   = tyClsNamesOfType ty
tyClsNamesOfType (PredTy (ClassP cl tys)) = unitNameSet (getName cl) `unionNameSets` tyClsNamesOfTypes tys
tyClsNamesOfType (FunTy arg res)	    = tyClsNamesOfType arg `unionNameSets` tyClsNamesOfType res
tyClsNamesOfType (AppTy fun arg)	    = tyClsNamesOfType fun `unionNameSets` tyClsNamesOfType arg
tyClsNamesOfType (ForAllTy tyvar ty)	    = tyClsNamesOfType ty

tyClsNamesOfTypes tys = foldr (unionNameSets . tyClsNamesOfType) emptyNameSet tys

tyClsNamesOfDFunHead :: Type -> NameSet
-- Find the free type constructors and classes 
-- of the head of the dfun instance type
-- The 'dfun_head_type' is because of
--	instance Foo a => Baz T where ...
-- The decl is an orphan if Baz and T are both not locally defined,
--	even if Foo *is* locally defined
tyClsNamesOfDFunHead dfun_ty 
  = case tcSplitSigmaTy dfun_ty of
	(tvs,_,head_ty) -> tyClsNamesOfType head_ty

classesOfTheta :: ThetaType -> [Class]
-- Looks just for ClassP things; maybe it should check
classesOfTheta preds = [ c | ClassP c _ <- preds ]
\end{code}


%************************************************************************
%*									*
\subsection[TysWiredIn-ext-type]{External types}
%*									*
%************************************************************************

The compiler's foreign function interface supports the passing of a
restricted set of types as arguments and results (the restricting factor
being the )

\begin{code}
isFFITy :: Type -> Bool
-- True for any TyCon that can possibly be an arg or result of an FFI call
isFFITy ty = checkRepTyCon legalFFITyCon ty

isFFIArgumentTy :: DynFlags -> Safety -> Type -> Bool
-- Checks for valid argument type for a 'foreign import'
isFFIArgumentTy dflags safety ty 
   = checkRepTyCon (legalOutgoingTyCon dflags safety) ty

isFFIExternalTy :: Type -> Bool
-- Types that are allowed as arguments of a 'foreign export'
isFFIExternalTy ty = checkRepTyCon legalFEArgTyCon ty

isFFIImportResultTy :: DynFlags -> Type -> Bool
isFFIImportResultTy dflags ty 
  = checkRepTyCon (legalFIResultTyCon dflags) ty

isFFIExportResultTy :: Type -> Bool
isFFIExportResultTy ty = checkRepTyCon legalFEResultTyCon ty

isFFIDynArgumentTy :: Type -> Bool
-- The argument type of a foreign import dynamic must be Ptr, FunPtr, Addr,
-- or a newtype of either.
isFFIDynArgumentTy = checkRepTyConKey [ptrTyConKey, funPtrTyConKey, addrTyConKey]

isFFIDynResultTy :: Type -> Bool
-- The result type of a foreign export dynamic must be Ptr, FunPtr, Addr,
-- or a newtype of either.
isFFIDynResultTy = checkRepTyConKey [ptrTyConKey, funPtrTyConKey, addrTyConKey]

isFFILabelTy :: Type -> Bool
-- The type of a foreign label must be Ptr, FunPtr, Addr,
-- or a newtype of either.
isFFILabelTy = checkRepTyConKey [ptrTyConKey, funPtrTyConKey, addrTyConKey]

isFFIDotnetTy :: DynFlags -> Type -> Bool
isFFIDotnetTy dflags ty
  = checkRepTyCon (\ tc -> not (isByteArrayLikeTyCon tc) &&
  			   (legalFIResultTyCon dflags tc || 
			   isFFIDotnetObjTy ty || isStringTy ty)) ty

-- Support String as an argument or result from a .NET FFI call.
isStringTy ty = 
  case tcSplitTyConApp_maybe (repType ty) of
    Just (tc, [arg_ty])
      | tc == listTyCon ->
        case tcSplitTyConApp_maybe (repType arg_ty) of
	  Just (cc,[]) -> cc == charTyCon
	  _ -> False
    _ -> False

-- Support String as an argument or result from a .NET FFI call.
isFFIDotnetObjTy ty = 
  let
   (_, t_ty) = tcSplitForAllTys ty
  in
  case tcSplitTyConApp_maybe (repType t_ty) of
    Just (tc, [arg_ty]) | getName tc == objectTyConName -> True
    _ -> False

toDNType :: Type -> DNType
toDNType ty
  | isStringTy ty = DNString
  | isFFIDotnetObjTy ty = DNObject
  | Just (tc,argTys) <- tcSplitTyConApp_maybe ty = 
     case lookup (getUnique tc) dn_assoc of
       Just x  -> x
       Nothing 
         | tc `hasKey` ioTyConKey -> toDNType (head argTys)
	 | otherwise -> pprPanic ("toDNType: unsupported .NET type") (pprType ty <+> parens (hcat (map pprType argTys)) <+> ppr tc)
    where
      dn_assoc :: [ (Unique, DNType) ]
      dn_assoc = [ (unitTyConKey,   DNUnit)
      		 , (intTyConKey,    DNInt)
      	         , (int8TyConKey,   DNInt8)
		 , (int16TyConKey,  DNInt16)
		 , (int32TyConKey,  DNInt32)
		 , (int64TyConKey,  DNInt64)
		 , (wordTyConKey,   DNInt)
		 , (word8TyConKey,  DNWord8)
		 , (word16TyConKey, DNWord16)
		 , (word32TyConKey, DNWord32)
		 , (word64TyConKey, DNWord64)
		 , (floatTyConKey,  DNFloat)
		 , (doubleTyConKey, DNDouble)
		 , (addrTyConKey,   DNPtr)
		 , (ptrTyConKey,    DNPtr)
		 , (funPtrTyConKey, DNPtr)
		 , (charTyConKey,   DNChar)
		 , (boolTyConKey,   DNBool)
		 ]

checkRepTyCon :: (TyCon -> Bool) -> Type -> Bool
	-- Look through newtypes
	-- Non-recursive ones are transparent to splitTyConApp,
	-- but recursive ones aren't.  Manuel had:
	--	newtype T = MkT (Ptr T)
	-- and wanted it to work...
checkRepTyCon check_tc ty 
  | Just (tc,_) <- splitTyConApp_maybe (repType ty) = check_tc tc
  | otherwise				  	    = False

checkRepTyConKey :: [Unique] -> Type -> Bool
-- Like checkRepTyCon, but just looks at the TyCon key
checkRepTyConKey keys
  = checkRepTyCon (\tc -> tyConUnique tc `elem` keys)
\end{code}

----------------------------------------------
These chaps do the work; they are not exported
----------------------------------------------

\begin{code}
legalFEArgTyCon :: TyCon -> Bool
-- It's illegal to return foreign objects and (mutable)
-- bytearrays from a _ccall_ / foreign declaration
-- (or be passed them as arguments in foreign exported functions).
legalFEArgTyCon tc
  | isByteArrayLikeTyCon tc
  = False
  -- It's also illegal to make foreign exports that take unboxed
  -- arguments.  The RTS API currently can't invoke such things.  --SDM 7/2000
  | otherwise
  = boxedMarshalableTyCon tc

legalFIResultTyCon :: DynFlags -> TyCon -> Bool
legalFIResultTyCon dflags tc
  | isByteArrayLikeTyCon tc = False
  | tc == unitTyCon         = True
  | otherwise	            = marshalableTyCon dflags tc

legalFEResultTyCon :: TyCon -> Bool
legalFEResultTyCon tc
  | isByteArrayLikeTyCon tc = False
  | tc == unitTyCon         = True
  | otherwise               = boxedMarshalableTyCon tc

legalOutgoingTyCon :: DynFlags -> Safety -> TyCon -> Bool
-- Checks validity of types going from Haskell -> external world
legalOutgoingTyCon dflags safety tc
  | playSafe safety && isByteArrayLikeTyCon tc
  = False
  | otherwise
  = marshalableTyCon dflags tc

legalFFITyCon :: TyCon -> Bool
-- True for any TyCon that can possibly be an arg or result of an FFI call
legalFFITyCon tc
  = isUnLiftedTyCon tc || boxedMarshalableTyCon tc || tc == unitTyCon

marshalableTyCon dflags tc
  =  (dopt Opt_GlasgowExts dflags && isUnLiftedTyCon tc)
  || boxedMarshalableTyCon tc

boxedMarshalableTyCon tc
   = getUnique tc `elem` [ intTyConKey, int8TyConKey, int16TyConKey
			 , int32TyConKey, int64TyConKey
			 , wordTyConKey, word8TyConKey, word16TyConKey
			 , word32TyConKey, word64TyConKey
			 , floatTyConKey, doubleTyConKey
			 , addrTyConKey, ptrTyConKey, funPtrTyConKey
			 , charTyConKey
			 , stablePtrTyConKey
			 , byteArrayTyConKey, mutableByteArrayTyConKey
			 , boolTyConKey
			 ]

isByteArrayLikeTyCon :: TyCon -> Bool
isByteArrayLikeTyCon tc = 
  getUnique tc `elem` [byteArrayTyConKey, mutableByteArrayTyConKey]
\end{code}


