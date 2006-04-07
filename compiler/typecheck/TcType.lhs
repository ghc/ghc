
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

  BoxyTyVar, BoxySigmaType, BoxyRhoType, BoxyThetaType, BoxyType,

  --------------------------------
  -- MetaDetails
  UserTypeCtxt(..), pprUserTypeCtxt,
  TcTyVarDetails(..), BoxInfo(..), pprTcTyVarDetails,
  MetaDetails(Flexi, Indirect), SkolemInfo(..), pprSkolTvBinding, pprSkolInfo,
  isImmutableTyVar, isSkolemTyVar, isMetaTyVar, isBoxyTyVar, isSigTyVar, isExistentialTyVar, 
  metaTvRef, 
  isFlexi, isIndirect, 

  --------------------------------
  -- Builders
  mkPhiTy, mkSigmaTy, 

  --------------------------------
  -- Splitters  
  -- These are important because they do not look through newtypes
  tcView,
  tcSplitForAllTys, tcSplitPhiTy, 
  tcSplitFunTy_maybe, tcSplitFunTys, tcFunArgTy, tcFunResultTy, tcSplitFunTysN,
  tcSplitTyConApp, tcSplitTyConApp_maybe, tcTyConAppTyCon, tcTyConAppArgs,
  tcSplitAppTy_maybe, tcSplitAppTy, tcSplitAppTys, 
  tcValidInstHeadTy, tcGetTyVar_maybe, tcGetTyVar,
  tcSplitSigmaTy, tcMultiSplitSigmaTy, 

  ---------------------------------
  -- Predicates. 
  -- Again, newtypes are opaque
  tcEqType, tcEqTypes, tcEqPred, tcCmpType, tcCmpTypes, tcCmpPred, tcEqTypeX,
  isSigmaTy, isOverloadedTy, isRigidTy, isBoxyTy,
  isDoubleTy, isFloatTy, isIntTy, isStringTy,
  isIntegerTy, isAddrTy, isBoolTy, isUnitTy,
  isTauTy, isTauTyCon, tcIsTyVarTy, tcIsForAllTy, 

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
  isPredTy, isDictTy, tcSplitDFunTy, tcSplitDFunHead, predTyUnique, 
  mkClassPred, isInheritablePred, isLinearPred, isIPPred, mkPredName, 
  dataConsStupidTheta, isRefineableTy,

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
  mkTyConApp, mkAppTy, mkAppTys, applyTy, applyTys,
  mkTyVarTy, mkTyVarTys, mkTyConTy, mkPredTy, mkPredTys, 

  -- Type substitutions
  TvSubst(..), 	-- Representation visible to a few friends
  TvSubstEnv, emptyTvSubst,
  mkOpenTvSubst, zipOpenTvSubst, zipTopTvSubst, mkTopTvSubst, notElemTvSubst,
  getTvSubstEnv, setTvSubstEnv, getTvInScope, extendTvInScope, lookupTyVar,
  extendTvSubst, extendTvSubstList, isInScope, mkTvSubst, zipTyEnv,
  substTy, substTys, substTyWith, substTheta, substTyVar, substTyVarBndr,

  isUnLiftedType,	-- Source types are always lifted
  isUnboxedTupleType,	-- Ditto
  isPrimitiveType, 

  tidyTopType, tidyType, tidyPred, tidyTypes, tidyFreeTyVars, tidyOpenType, tidyOpenTypes,
  tidyTyVarBndr, tidyOpenTyVar, tidyOpenTyVars, tidySkolemTyVar,
  typeKind, tidyKind,

  tyVarsOfType, tyVarsOfTypes, tyVarsOfPred, tyVarsOfTheta,
  tcTyVarsOfType, tcTyVarsOfTypes, exactTyVarsOfType, exactTyVarsOfTypes,

  pprKind, pprParendKind,
  pprType, pprParendType, pprTyThingCategory,
  pprPred, pprTheta, pprThetaArrow, pprClassPred

  ) where

#include "HsVersions.h"

-- friends:
import TypeRep		( Type(..), funTyCon )  -- friend

import Type		(	-- Re-exports
			  tyVarsOfType, tyVarsOfTypes, tyVarsOfPred,
			  tyVarsOfTheta, Kind, PredType(..),
			  ThetaType, unliftedTypeKind, 
			  liftedTypeKind, openTypeKind, mkArrowKind,
		  	  isLiftedTypeKind, isUnliftedTypeKind, 
			  mkArrowKinds, mkForAllTy, mkForAllTys,
			  defaultKind, isArgTypeKind, isOpenTypeKind,
			  mkFunTy, mkFunTys, zipFunTys, 
			  mkTyConApp, mkAppTy,
			  mkAppTys, applyTy, applyTys,
			  mkTyVarTy, mkTyVarTys, mkTyConTy, mkPredTy,
			  mkPredTys, isUnLiftedType, 
			  isUnboxedTupleType, isPrimitiveType,
			  splitTyConApp_maybe,
			  tidyTopType, tidyType, tidyPred, tidyTypes,
			  tidyFreeTyVars, tidyOpenType, tidyOpenTypes,
			  tidyTyVarBndr, tidyOpenTyVar,
			  tidyOpenTyVars, tidyKind,
			  isSubKind, tcView,

			  tcEqType, tcEqTypes, tcCmpType, tcCmpTypes, 
			  tcEqPred, tcCmpPred, tcEqTypeX, 

			  TvSubst(..),
			  TvSubstEnv, emptyTvSubst, mkTvSubst, zipTyEnv,
			  mkOpenTvSubst, zipOpenTvSubst, zipTopTvSubst, mkTopTvSubst,
			  getTvSubstEnv, setTvSubstEnv, getTvInScope, extendTvInScope,
			  extendTvSubst, extendTvSubstList, isInScope, notElemTvSubst,
		  	  substTy, substTys, substTyWith, substTheta, 
			  substTyVar, substTyVarBndr, substPred, lookupTyVar,

			  typeKind, repType,
			  pprKind, pprParendKind,
			  pprType, pprParendType, pprTyThingCategory,
			  pprPred, pprTheta, pprThetaArrow, pprClassPred
			)
import TyCon		( TyCon, isUnLiftedTyCon, isSynTyCon, synTyConDefn, tyConUnique )
import DataCon		( DataCon, dataConStupidTheta, dataConResTys )
import Class		( Class )
import Var		( TyVar, Id, isTcTyVar, mkTcTyVar, tyVarName, tyVarKind, tcTyVarDetails )
import ForeignCall	( Safety, playSafe, DNType(..) )
import Unify		( tcMatchTys )
import VarSet

-- others:
import DynFlags		( DynFlags, DynFlag( Opt_GlasgowExts ), dopt )
import Name		( Name, NamedThing(..), mkInternalName, getSrcLoc )
import NameSet
import VarEnv		( TidyEnv )
import OccName		( OccName, mkDictOcc )
import PrelNames	-- Lots (e.g. in isFFIArgumentTy)
import TysWiredIn	( unitTyCon, charTyCon, listTyCon )
import BasicTypes	( IPName(..), Arity, ipNameName )
import SrcLoc		( SrcLoc, SrcSpan )
import Util		( snocView, equalLength )
import Maybes		( maybeToBool, expectJust, mapCatMaybes )
import ListSetOps	( hasNoDups )
import List		( nubBy )
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
type TcTyVar = TyVar  	-- Used only during type inference
type TcType = Type 	-- A TcType can have mutable type variables
	-- Invariant on ForAllTy in TcTypes:
	-- 	forall a. T
	-- a cannot occur inside a MutTyVar in T; that is,
	-- T is "flattened" before quantifying over a

-- These types do not have boxy type variables in them
type TcPredType     = PredType
type TcThetaType    = ThetaType
type TcSigmaType    = TcType
type TcRhoType      = TcType
type TcTauType      = TcType
type TcKind         = Kind
type TcTyVarSet     = TyVarSet

-- These types may have boxy type variables in them
type BoxyTyVar	    = TcTyVar
type BoxyRhoType    = TcType	
type BoxyThetaType  = TcThetaType	
type BoxySigmaType  = TcType		
type BoxyType       = TcType		
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


Note [Signature skolems]
~~~~~~~~~~~~~~~~~~~~~~~~
Consider this

  x :: [a]
  y :: b
  (x,y,z) = ([y,z], z, head x)

Here, x and y have type sigs, which go into the environment.  We used to
instantiate their types with skolem constants, and push those types into
the RHS, so we'd typecheck the RHS with type
	( [a*], b*, c )
where a*, b* are skolem constants, and c is an ordinary meta type varible.

The trouble is that the occurrences of z in the RHS force a* and b* to 
be the *same*, so we can't make them into skolem constants that don't unify
with each other.  Alas.

On the other hand, we *must* use skolems for signature type variables, 
becuase GADT type refinement refines skolems only.  

One solution would be insist that in the above defn the programmer uses
the same type variable in both type signatures.  But that takes explanation.

The alternative (currently implemented) is to have a special kind of skolem
constant, SigSkokTv, which can unify with other SigSkolTvs.  


\begin{code}
-- A TyVarDetails is inside a TyVar
data TcTyVarDetails
  = SkolemTv SkolemInfo			-- A skolem constant

  | MetaTv BoxInfo (IORef MetaDetails)

data BoxInfo 
   = BoxTv	-- The contents is a (non-boxy) sigma-type
		-- That is, this MetaTv is a "box"

   | TauTv	-- The contents is a (non-boxy) tau-type
		-- That is, this MetaTv is an ordinary unification variable

   | SigTv SkolemInfo	-- A variant of TauTv, except that it should not be
			-- unified with a type, only with a type variable
			-- SigTvs are only distinguished to improve error messages
			--      see Note [Signature skolems]        
			--      The MetaDetails, if filled in, will 
			--      always be another SigTv or a SkolemTv

-- INVARIANTS:
--  	A TauTv is always filled in with a tau-type, which
--	never contains any BoxTvs, nor any ForAlls 
--
--	However, a BoxTv can contain a type that contains further BoxTvs
--	Notably, when typechecking an explicit list, say [e1,e2], with
--	expected type being a box b1, we fill in b1 with (List b2), where
--	b2 is another (currently empty) box.

data MetaDetails
  = Flexi          -- Flexi type variables unify to become 
                   -- Indirects.  

  | Indirect TcType  -- INVARIANT:
		     --   For a BoxTv, this type must be non-boxy
                     --   For a TauTv, this type must be a tau-type

data SkolemInfo
  = SigSkol UserTypeCtxt	-- A skolem that is created by instantiating
				-- a programmer-supplied type signature
				-- Location of the binding site is on the TyVar

	-- The rest are for non-scoped skolems
  | ClsSkol Class	-- Bound at a class decl
  | InstSkol Id		-- Bound at an instance decl
  | PatSkol DataCon	-- An existential type variable bound by a pattern for
	    SrcSpan	-- a data constructor with an existential type. E.g.
			--	data T = forall a. Eq a => MkT a
			-- 	f (MkT x) = ...
			-- The pattern MkT x will allocate an existential type
			-- variable for 'a'.  
  | ArrowSkol SrcSpan	-- An arrow form (see TcArrows)

  | GenSkol [TcTyVar]	-- Bound when doing a subsumption check for 
	    TcType	-- 	(forall tvs. ty)
	    SrcSpan

  | UnkSkol		-- Unhelpful info (until I improve it)

-------------------------------------
-- UserTypeCtxt describes the places where a 
-- programmer-written type signature can occur
data UserTypeCtxt 
  = FunSigCtxt Name	-- Function type signature
			-- Also used for types in SPECIALISE pragmas
  | ExprSigCtxt		-- Expression type signature
  | ConArgCtxt Name	-- Data constructor argument
  | TySynCtxt Name	-- RHS of a type synonym decl
  | GenPatCtxt		-- Pattern in generic decl
			-- 	f{| a+b |} (Inl x) = ...
  | LamPatSigCtxt		-- Type sig in lambda pattern
			-- 	f (x::t) = ...
  | BindPatSigCtxt	-- Type sig in pattern binding pattern
			--	(x::t, y) = e
  | ResSigCtxt		-- Result type sig
			-- 	f x :: t = ....
  | ForSigCtxt Name	-- Foreign inport or export signature
  | RuleSigCtxt Name 	-- Signature on a forall'd variable in a RULE
  | DefaultDeclCtxt	-- Types in a default declaration
  | SpecInstCtxt	-- SPECIALISE instance pragma

-- Notes re TySynCtxt
-- We allow type synonyms that aren't types; e.g.  type List = []
--
-- If the RHS mentions tyvars that aren't in scope, we'll 
-- quantify over them:
--	e.g. 	type T = a->a
-- will become	type T = forall a. a->a
--
-- With gla-exts that's right, but for H98 we should complain. 
\end{code}

%************************************************************************
%*									*
		Pretty-printing
%*									*
%************************************************************************

\begin{code}
pprTcTyVarDetails :: TcTyVarDetails -> SDoc
-- For debugging
pprTcTyVarDetails (SkolemTv _)         = ptext SLIT("sk")
pprTcTyVarDetails (MetaTv BoxTv _)     = ptext SLIT("box")
pprTcTyVarDetails (MetaTv TauTv _)     = ptext SLIT("tau")
pprTcTyVarDetails (MetaTv (SigTv _) _) = ptext SLIT("sig")

pprUserTypeCtxt :: UserTypeCtxt -> SDoc
pprUserTypeCtxt (FunSigCtxt n)  = ptext SLIT("the type signature for") <+> quotes (ppr n)
pprUserTypeCtxt ExprSigCtxt     = ptext SLIT("an expression type signature")
pprUserTypeCtxt (ConArgCtxt c)  = ptext SLIT("the type of the constructor") <+> quotes (ppr c)
pprUserTypeCtxt (TySynCtxt c)   = ptext SLIT("the RHS of the type synonym") <+> quotes (ppr c)
pprUserTypeCtxt GenPatCtxt      = ptext SLIT("the type pattern of a generic definition")
pprUserTypeCtxt LamPatSigCtxt   = ptext SLIT("a pattern type signature")
pprUserTypeCtxt BindPatSigCtxt  = ptext SLIT("a pattern type signature")
pprUserTypeCtxt ResSigCtxt      = ptext SLIT("a result type signature")
pprUserTypeCtxt (ForSigCtxt n)  = ptext SLIT("the foreign declaration for") <+> quotes (ppr n)
pprUserTypeCtxt (RuleSigCtxt n) = ptext SLIT("the type signature for") <+> quotes (ppr n)
pprUserTypeCtxt DefaultDeclCtxt = ptext SLIT("a type in a `default' declaration")
pprUserTypeCtxt SpecInstCtxt    = ptext SLIT("a SPECIALISE instance pragma")


--------------------------------
tidySkolemTyVar :: TidyEnv -> TcTyVar -> (TidyEnv, TcTyVar)
-- Tidy the type inside a GenSkol, preparatory to printing it
tidySkolemTyVar env tv
  = ASSERT( isSkolemTyVar tv )
    (env1, mkTcTyVar (tyVarName tv) (tyVarKind tv) info1)
  where
    (env1, info1) = case tcTyVarDetails tv of
		      SkolemTv (GenSkol tvs ty loc) -> (env2, SkolemTv (GenSkol tvs1 ty1 loc))
			    where
			      (env1, tvs1) = tidyOpenTyVars env tvs
			      (env2, ty1)  = tidyOpenType env1 ty
		      info -> (env, info)
		     
pprSkolTvBinding :: TcTyVar -> SDoc
-- Print info about the binding of a skolem tyvar, 
-- or nothing if we don't have anything useful to say
pprSkolTvBinding tv
  = ppr_details (tcTyVarDetails tv)
  where
    ppr_details (MetaTv TauTv _)   = quotes (ppr tv) <+> ptext SLIT("is a meta type variable")
    ppr_details (MetaTv BoxTv _)   = quotes (ppr tv) <+> ptext SLIT("is a boxy type variable")
    ppr_details (MetaTv (SigTv info) _) = ppr_skol info
    ppr_details (SkolemTv info)		= ppr_skol info

    ppr_skol UnkSkol 	     = empty	-- Unhelpful; omit
    ppr_skol (SigSkol ctxt)  = sep [quotes (ppr tv) <+> ptext SLIT("is bound by") <+> pprUserTypeCtxt ctxt,
				    nest 2 (ptext SLIT("at") <+> ppr (getSrcLoc tv))]
    ppr_skol info            = quotes (ppr tv) <+> pprSkolInfo info
 
pprSkolInfo :: SkolemInfo -> SDoc
pprSkolInfo (SigSkol ctxt)   = ptext SLIT("is bound by") <+> pprUserTypeCtxt ctxt
pprSkolInfo (ClsSkol cls)    = ptext SLIT("is bound by the class declaration for") <+> quotes (ppr cls)
pprSkolInfo (InstSkol df)    = ptext SLIT("is bound by the instance declaration at") <+> ppr (getSrcLoc df)
pprSkolInfo (ArrowSkol loc)  = ptext SLIT("is bound by the arrow form at") <+> ppr loc
pprSkolInfo (PatSkol dc loc) = sep [ptext SLIT("is bound by the pattern for") <+> quotes (ppr dc),
           		            nest 2 (ptext SLIT("at") <+> ppr loc)]
pprSkolInfo (GenSkol tvs ty loc) = sep [sep [ptext SLIT("is bound by the polymorphic type"), 
				   	     nest 2 (quotes (ppr (mkForAllTys tvs ty)))],
				        nest 2 (ptext SLIT("at") <+> ppr loc)]
-- UnkSkol, SigSkol
-- For type variables the others are dealt with by pprSkolTvBinding.  
-- For Insts, these cases should not happen
pprSkolInfo UnkSkol = panic "UnkSkol"

instance Outputable MetaDetails where
  ppr Flexi 	    = ptext SLIT("Flexi")
  ppr (Indirect ty) = ptext SLIT("Indirect") <+> ppr ty
\end{code}


%************************************************************************
%*									*
		Predicates
%*									*
%************************************************************************

\begin{code}
isImmutableTyVar, isSkolemTyVar, isExistentialTyVar, isBoxyTyVar, isMetaTyVar :: TyVar -> Bool
isImmutableTyVar tv
  | isTcTyVar tv = isSkolemTyVar tv
  | otherwise    = True

isSkolemTyVar tv 
  = ASSERT( isTcTyVar tv )
    case tcTyVarDetails tv of
	SkolemTv _         -> True
 	MetaTv _ _         -> False

isExistentialTyVar tv 	-- Existential type variable, bound by a pattern
  = ASSERT( isTcTyVar tv )
    case tcTyVarDetails tv of
	SkolemTv (PatSkol _ _) -> True
	other 		       -> False

isMetaTyVar tv 
  = ASSERT2( isTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
	MetaTv _ _ -> True
	other      -> False

isBoxyTyVar tv 
  = ASSERT( isTcTyVar tv )
    case tcTyVarDetails tv of
	MetaTv BoxTv _ -> True
	other          -> False

isSigTyVar tv 
  = ASSERT( isTcTyVar tv )
    case tcTyVarDetails tv of
	MetaTv (SigTv _) _ -> True
	other              -> False

metaTvRef :: TyVar -> IORef MetaDetails
metaTvRef tv 
  = ASSERT( isTcTyVar tv )
    case tcTyVarDetails tv of
	MetaTv _ ref -> ref
	other	   -> pprPanic "metaTvRef" (ppr tv)

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

@isTauTy@ tests for nested for-alls.  It should not be called on a boxy type.

\begin{code}
isTauTy :: Type -> Bool
isTauTy ty | Just ty' <- tcView ty = isTauTy ty'
isTauTy (TyVarTy tv)	 = ASSERT( not (isTcTyVar tv && isBoxyTyVar tv) )
			   True
isTauTy (TyConApp tc tys) = all isTauTy tys && isTauTyCon tc
isTauTy (AppTy a b)	  = isTauTy a && isTauTy b
isTauTy (FunTy a b)	  = isTauTy a && isTauTy b
isTauTy (PredTy p)	  = True		-- Don't look through source types
isTauTy other		  = False


isTauTyCon :: TyCon -> Bool
-- Returns False for type synonyms whose expansion is a polytype
isTauTyCon tc | isSynTyCon tc = isTauTy (snd (synTyConDefn tc))
	      | otherwise     = True

---------------
isBoxyTy :: TcType -> Bool
isBoxyTy ty = any isBoxyTyVar (varSetElems (tcTyVarsOfType ty))

isRigidTy :: TcType -> Bool
-- A type is rigid if it has no meta type variables in it
isRigidTy ty = all isSkolemTyVar (varSetElems (tcTyVarsOfType ty))

isRefineableTy :: TcType -> Bool
-- A type should have type refinements applied to it if it has
-- free type variables, and they are all rigid
isRefineableTy ty = not (null tc_tvs) && all isSkolemTyVar tc_tvs
		    where
		      tc_tvs = varSetElems (tcTyVarsOfType ty)

---------------
getDFunTyKey :: Type -> OccName	-- Get some string from a type, to be used to 
				-- construct a dictionary function name
getDFunTyKey ty | Just ty' <- tcView ty = getDFunTyKey ty'
getDFunTyKey (TyVarTy tv)    = getOccName tv
getDFunTyKey (TyConApp tc _) = getOccName tc
getDFunTyKey (AppTy fun _)   = getDFunTyKey fun
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
     split orig_ty ty tvs | Just ty' <- tcView ty = split orig_ty ty' tvs
     split orig_ty (ForAllTy tv ty) tvs = split ty ty (tv:tvs)
     split orig_ty t		    tvs = (reverse tvs, orig_ty)

tcIsForAllTy ty | Just ty' <- tcView ty = tcIsForAllTy ty'
tcIsForAllTy (ForAllTy tv ty) = True
tcIsForAllTy t		      = False

tcSplitPhiTy :: Type -> ([PredType], Type)
tcSplitPhiTy ty = split ty ty []
 where
  split orig_ty ty tvs | Just ty' <- tcView ty = split orig_ty ty' tvs
  split orig_ty (FunTy arg res) ts = case tcSplitPredTy_maybe arg of
					Just p  -> split res res (p:ts)
					Nothing -> (reverse ts, orig_ty)
  split orig_ty ty		ts = (reverse ts, orig_ty)

tcSplitSigmaTy ty = case tcSplitForAllTys ty of
			(tvs, rho) -> case tcSplitPhiTy rho of
					(theta, tau) -> (tvs, theta, tau)

-----------------------
tcMultiSplitSigmaTy
	:: TcSigmaType
	-> ( [([TyVar], ThetaType)],	-- forall as.C => forall bs.D
	     TcSigmaType)		-- The rest of the type

-- We need a loop here because we are now prepared to entertain
-- types like
-- 	f:: forall a. Eq a => forall b. Baz b => tau
-- We want to instantiate this to
-- 	f2::tau		{f2 = f1 b (Baz b), f1 = f a (Eq a)}

tcMultiSplitSigmaTy sigma
  = case (tcSplitSigmaTy sigma) of
	([],[],ty) -> ([], sigma)
	(tvs, theta, ty) -> case tcMultiSplitSigmaTy ty of
				(pairs, rest) -> ((tvs,theta):pairs, rest)

-----------------------
tcTyConAppTyCon :: Type -> TyCon
tcTyConAppTyCon ty = fst (tcSplitTyConApp ty)

tcTyConAppArgs :: Type -> [Type]
tcTyConAppArgs ty = snd (tcSplitTyConApp ty)

tcSplitTyConApp :: Type -> (TyCon, [Type])
tcSplitTyConApp ty = case tcSplitTyConApp_maybe ty of
			Just stuff -> stuff
			Nothing	   -> pprPanic "tcSplitTyConApp" (pprType ty)

tcSplitTyConApp_maybe :: Type -> Maybe (TyCon, [Type])
tcSplitTyConApp_maybe ty | Just ty' <- tcView ty = tcSplitTyConApp_maybe ty'
tcSplitTyConApp_maybe (TyConApp tc tys) = Just (tc, tys)
tcSplitTyConApp_maybe (FunTy arg res)   = Just (funTyCon, [arg,res])
	-- Newtypes are opaque, so they may be split
	-- However, predicates are not treated
	-- as tycon applications by the type checker
tcSplitTyConApp_maybe other	      	= Nothing

-----------------------
tcSplitFunTys :: Type -> ([Type], Type)
tcSplitFunTys ty = case tcSplitFunTy_maybe ty of
			Nothing	       -> ([], ty)
			Just (arg,res) -> (arg:args, res')
				       where
					  (args,res') = tcSplitFunTys res

tcSplitFunTy_maybe :: Type -> Maybe (Type, Type)
tcSplitFunTy_maybe ty | Just ty' <- tcView ty = tcSplitFunTy_maybe ty'
tcSplitFunTy_maybe (FunTy arg res)  = Just (arg, res)
tcSplitFunTy_maybe other	    = Nothing

tcSplitFunTysN
	:: TcRhoType 
	-> Arity		-- N: Number of desired args
	-> ([TcSigmaType], 	-- Arg types (N or fewer)
	    TcSigmaType)	-- The rest of the type

tcSplitFunTysN ty n_args
  | n_args == 0
  = ([], ty)
  | Just (arg,res) <- tcSplitFunTy_maybe ty
  = case tcSplitFunTysN res (n_args - 1) of
	(args, res) -> (arg:args, res)
  | otherwise
  = ([], ty)

tcFunArgTy    ty = case tcSplitFunTy_maybe ty of { Just (arg,res) -> arg }
tcFunResultTy ty = case tcSplitFunTy_maybe ty of { Just (arg,res) -> res }


-----------------------
tcSplitAppTy_maybe :: Type -> Maybe (Type, Type)
tcSplitAppTy_maybe ty | Just ty' <- tcView ty = tcSplitAppTy_maybe ty'
tcSplitAppTy_maybe (FunTy ty1 ty2)   = Just (TyConApp funTyCon [ty1], ty2)
tcSplitAppTy_maybe (AppTy ty1 ty2)   = Just (ty1, ty2)
tcSplitAppTy_maybe (TyConApp tc tys) = case snocView tys of
					Just (tys', ty') -> Just (TyConApp tc tys', ty')
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

-----------------------
tcGetTyVar_maybe :: Type -> Maybe TyVar
tcGetTyVar_maybe ty | Just ty' <- tcView ty = tcGetTyVar_maybe ty'
tcGetTyVar_maybe (TyVarTy tv) 	= Just tv
tcGetTyVar_maybe other	        = Nothing

tcGetTyVar :: String -> Type -> TyVar
tcGetTyVar msg ty = expectJust msg (tcGetTyVar_maybe ty)

tcIsTyVarTy :: Type -> Bool
tcIsTyVarTy ty = maybeToBool (tcGetTyVar_maybe ty)

-----------------------
tcSplitDFunTy :: Type -> ([TyVar], [PredType], Class, [Type])
-- Split the type of a dictionary function
tcSplitDFunTy ty 
  = case tcSplitSigmaTy ty   of { (tvs, theta, tau) ->
    case tcSplitDFunHead tau of { (clas, tys) -> 
    (tvs, theta, clas, tys) }}

tcSplitDFunHead :: Type -> (Class, [Type])
tcSplitDFunHead tau  
  = case tcSplitPredTy_maybe tau of 
	Just (ClassP clas tys) -> (clas, tys)

tcValidInstHeadTy :: Type -> Bool
-- Used in Haskell-98 mode, for the argument types of an instance head
-- These must not be type synonyms, but everywhere else type synonyms
-- are transparent, so we need a special function here
tcValidInstHeadTy ty
  = case ty of
	NoteTy _ ty     -> tcValidInstHeadTy ty
	TyConApp tc tys -> not (isSynTyCon tc) && ok tys
	FunTy arg res   -> ok [arg, res]
	other		-> False
  where
	-- Check that all the types are type variables,
	-- and that each is distinct
    ok tys = equalLength tvs tys && hasNoDups tvs
	   where
	     tvs = mapCatMaybes get_tv tys

    get_tv (NoteTy _ ty) = get_tv ty 	-- Again, do not look
    get_tv (TyVarTy tv)  = Just tv	-- through synonyms
    get_tv other  	 = Nothing
\end{code}



%************************************************************************
%*									*
\subsection{Predicate types}
%*									*
%************************************************************************

\begin{code}
tcSplitPredTy_maybe :: Type -> Maybe PredType
   -- Returns Just for predicates only
tcSplitPredTy_maybe ty | Just ty' <- tcView ty = tcSplitPredTy_maybe ty'
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
isDictTy ty | Just ty' <- tcView ty = isDictTy ty'
isDictTy (PredTy p)   = isClassPred p
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

--------------------- The stupid theta (sigh) ---------------------------------

\begin{code}
dataConsStupidTheta :: [DataCon] -> ThetaType
-- Union the stupid thetas from all the specified constructors (non-empty)
-- All the constructors should have the same result type, modulo alpha conversion
-- The resulting ThetaType uses type variables from the *first* constructor in the list
--
-- It's here because it's used in MkId.mkRecordSelId, and in TcExpr
dataConsStupidTheta (con1:cons)
  = nubBy tcEqPred all_preds
  where
    all_preds 	  = dataConStupidTheta con1 ++ other_stupids
    res_tys1  	  = dataConResTys con1
    tvs1      	  = tyVarsOfTypes res_tys1
    other_stupids = [ substPred subst pred
		    | con <- cons
		    , let Just subst = tcMatchTys tvs1 res_tys1 (dataConResTys con)
		    , pred <- dataConStupidTheta con ]
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
isSigmaTy ty | Just ty' <- tcView ty = isSigmaTy ty'
isSigmaTy (ForAllTy tyvar ty) = True
isSigmaTy (FunTy a b)	      = isPredTy a
isSigmaTy _		      = False

isOverloadedTy :: Type -> Bool
isOverloadedTy ty | Just ty' <- tcView ty = isOverloadedTy ty'
isOverloadedTy (ForAllTy tyvar ty) = isOverloadedTy ty
isOverloadedTy (FunTy a b)	   = isPredTy a
isOverloadedTy _		   = False

isPredTy :: Type -> Bool	-- Belongs in TcType because it does 
				-- not look through newtypes, or predtypes (of course)
isPredTy ty | Just ty' <- tcView ty = isPredTy ty'
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
-- Remove all *outermost* type synonyms and other notes
deNoteType ty | Just ty' <- tcView ty = deNoteType ty'
deNoteType ty = ty
\end{code}

\begin{code}
tcTyVarsOfType :: Type -> TcTyVarSet
-- Just the tc type variables free in the type
tcTyVarsOfType (TyVarTy tv)	    = if isTcTyVar tv then unitVarSet tv
						      else emptyVarSet
tcTyVarsOfType (TyConApp tycon tys) = tcTyVarsOfTypes tys
tcTyVarsOfType (NoteTy _ ty)	    = tcTyVarsOfType ty
tcTyVarsOfType (PredTy sty)	    = tcTyVarsOfPred sty
tcTyVarsOfType (FunTy arg res)	    = tcTyVarsOfType arg `unionVarSet` tcTyVarsOfType res
tcTyVarsOfType (AppTy fun arg)	    = tcTyVarsOfType fun `unionVarSet` tcTyVarsOfType arg
tcTyVarsOfType (ForAllTy tyvar ty)  = tcTyVarsOfType ty `delVarSet` tyvar
	-- We do sometimes quantify over skolem TcTyVars

tcTyVarsOfTypes :: [Type] -> TyVarSet
tcTyVarsOfTypes tys = foldr (unionVarSet.tcTyVarsOfType) emptyVarSet tys

tcTyVarsOfPred :: PredType -> TyVarSet
tcTyVarsOfPred (IParam _ ty)  = tcTyVarsOfType ty
tcTyVarsOfPred (ClassP _ tys) = tcTyVarsOfTypes tys
\end{code}

Note [Silly type synonym]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
	type T a = Int
What are the free tyvars of (T x)?  Empty, of course!  
Here's the example that Ralf Laemmel showed me:
	foo :: (forall a. C u a -> C u a) -> u
	mappend :: Monoid u => u -> u -> u

	bar :: Monoid u => u
	bar = foo (\t -> t `mappend` t)
We have to generalise at the arg to f, and we don't
want to capture the constraint (Monad (C u a)) because
it appears to mention a.  Pretty silly, but it was useful to him.

exactTyVarsOfType is used by the type checker to figure out exactly
which type variables are mentioned in a type.  It's also used in the
smart-app checking code --- see TcExpr.tcIdApp

\begin{code}
exactTyVarsOfType :: TcType -> TyVarSet
-- Find the free type variables (of any kind)
-- but *expand* type synonyms.  See Note [Silly type synonym] belos.
exactTyVarsOfType ty
  = go ty
  where
    go ty | Just ty' <- tcView ty = go ty'	-- This is the key line
    go (TyVarTy tv)         	  = unitVarSet tv
    go (TyConApp tycon tys) 	  = exactTyVarsOfTypes tys
    go (PredTy ty)	    	  = go_pred ty
    go (FunTy arg res)	    	  = go arg `unionVarSet` go res
    go (AppTy fun arg)	    	  = go fun `unionVarSet` go arg
    go (ForAllTy tyvar ty)  	  = delVarSet (go ty) tyvar

    go_pred (IParam _ ty)  = go ty
    go_pred (ClassP _ tys) = exactTyVarsOfTypes tys

exactTyVarsOfTypes :: [TcType] -> TyVarSet
exactTyVarsOfTypes tys = foldr (unionVarSet . exactTyVarsOfType) emptyVarSet tys
\end{code}

Find the free tycons and classes of a type.  This is used in the front
end of the compiler.

\begin{code}
tyClsNamesOfType :: Type -> NameSet
tyClsNamesOfType (TyVarTy tv)		    = emptyNameSet
tyClsNamesOfType (TyConApp tycon tys)	    = unitNameSet (getName tycon) `unionNameSets` tyClsNamesOfTypes tys
tyClsNamesOfType (NoteTy _ ty2) 	    = tyClsNamesOfType ty2
tyClsNamesOfType (PredTy (IParam n ty))     = tyClsNamesOfType ty
tyClsNamesOfType (PredTy (ClassP cl tys))   = unitNameSet (getName cl) `unionNameSets` tyClsNamesOfTypes tys
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
