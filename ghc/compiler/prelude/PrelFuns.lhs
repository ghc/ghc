%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[PrelFuns]{Help functions for prelude-related stuff}

\begin{code}
#include "HsVersions.h"

module PrelFuns (
	pRELUDE, pRELUDE_BUILTIN, pRELUDE_CORE, pRELUDE_RATIO,
	pRELUDE_LIST, pRELUDE_TEXT,  --OLD: pRELUDE_ARRAY, pRELUDE_COMPLEX,
	pRELUDE_PRIMIO, pRELUDE_IO, pRELUDE_PS,
	gLASGOW_ST, {-gLASGOW_IO,-} gLASGOW_MISC,

	alpha_tv, alpha, beta_tv, beta,
	gamma_tv, gamma, delta_tv, delta, epsilon_tv, epsilon,
	alpha_tyvar, alpha_ty, beta_tyvar, beta_ty,
	gamma_tyvar, gamma_ty, delta_tyvar, delta_ty,
	epsilon_tyvar, epsilon_ty,

	pcDataTyCon, pcPrimTyCon,
	pcDataCon, pcMiscPrelId,
	pcGenerateSpecs, pcGenerateDataSpecs, pcGenerateTupleSpecs,

	-- mkBuild, mkListFilter,

	-- re-export a few helpful things
	mkPreludeCoreName, nullSpecEnv,

	IdInfo, ArityInfo, DemandInfo, SpecEnv, StrictnessInfo,
	UpdateInfo, ArgUsageInfo, ArgUsage, DeforestInfo, FBTypeInfo,
	FBType, FBConsum, FBProd,
	OptIdInfo(..),	-- class
	noIdInfo,
	mkArityInfo, arityMaybe,
	noInfo_UF, mkUnfolding, UnfoldingGuidance(..), UnfoldingDetails,

	-- and to make the interface self-sufficient...
	Outputable(..), NamedThing(..),
	ExportFlag, SrcLoc, Unique,
	Pretty(..), PprStyle, PrettyRep,
	-- urgh: because their instances go out w/ Outputable(..)
	BasicLit, CoreBinding, CoreCaseAlternatives, CoreArg,
	CoreCaseDefault, CoreExpr, CoreAtom, TyVarEnv(..),
	IdEnv(..), UniqFM,
#ifdef DPH
	CoreParQuals,
	CoreParCommunicate,
#endif {- Data Parallel Haskell -}

	PrimOp(..),			-- NB: non-abstract
	PrimKind(..),			-- NB: non-abstract
	Name(..),				-- NB: non-abstract
	UniType(..),				-- Mega-NB: non-abstract

	Class, ClassOp, Id, FullName, ShortName, TyCon, TyVarTemplate,
	TyVar, Arity(..), TauType(..), ThetaType(..), SigmaType(..),
	CostCentre, GlobalSwitch, Maybe, BinderInfo, PlainCoreExpr(..),
	PlainCoreAtom(..), InstTemplate, Demand, Bag
	IF_ATTACK_PRAGMAS(COMMA cmpTyCon)
#ifndef __GLASGOW_HASKELL__
	,TAG_
#endif
    ) where

import AbsUniType	( mkDataTyCon, mkPrimTyCon,
			  specialiseTy, splitType, applyTyCon,
			  alpha_tv, alpha, beta_tv, beta, gamma_tv,
			  gamma, alpha_tyvar, alpha_ty, beta_tyvar,
			  beta_ty, gamma_tyvar, gamma_ty, delta_tv,
			  delta, epsilon_tv, epsilon, delta_tyvar,
			  delta_ty, epsilon_tyvar, epsilon_ty, TyVar,
			  TyVarTemplate, Class, ClassOp, TyCon,
			  Arity(..), ThetaType(..), TauType(..),
			  SigmaType(..), UniType, InstTemplate
			  IF_ATTACK_PRAGMAS(COMMA pprUniType)
			  IF_ATTACK_PRAGMAS(COMMA cmpTyCon COMMA cmpTyVar)
			)
import Id		( mkPreludeId, mkSpecId, mkDataCon, getIdUniType,
			  mkTemplateLocals, DataCon(..)
			)
import IdInfo		-- lots
import Maybes		( Maybe(..) )
import Name		( Name(..) )
import NameTypes	( mkShortName, mkPreludeCoreName, ShortName, FullName )
import Outputable
import PlainCore
import Pretty
import PrimKind		( PrimKind(..) )
import PrimOps		( PrimOp(..)
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import SrcLoc		( mkBuiltinSrcLoc, SrcLoc )
import TysPrim		( charPrimTy, intPrimTy, doublePrimTy )
import UniType		( UniType(..)	-- **** CAN SEE THE CONSTRUCTORS ****
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)
import Util
\end{code}

\begin{code}
pRELUDE		= SLIT("Prelude")
pRELUDE_BUILTIN = SLIT("PreludeBuiltin")
pRELUDE_CORE	= SLIT("PreludeCore")
pRELUDE_RATIO	= SLIT("PreludeRatio")
pRELUDE_LIST	= SLIT("PreludeList")
--OLD:pRELUDE_ARRAY	= SLIT("PreludeArray")
pRELUDE_TEXT	= SLIT("PreludeText")
--OLD:pRELUDE_COMPLEX	= SLIT("PreludeComplex")
pRELUDE_PRIMIO	= SLIT("PreludePrimIO")
pRELUDE_IO	= SLIT("PreludeIO")
pRELUDE_PS	= SLIT("PreludePS")
gLASGOW_ST	= SLIT("PreludeGlaST")
--gLASGOW_IO	= SLIT("PreludeGlaIO")
gLASGOW_MISC	= SLIT("PreludeGlaMisc")
\end{code}

\begin{code}
-- things for TyCons -----------------------------------------------------

pcDataTyCon :: Unique{-TyConKey-} -> FAST_STRING -> FAST_STRING -> [TyVarTemplate] -> [Id] -> TyCon
pcDataTyCon key mod name tyvars cons
  = mkDataTyCon key full_name arity tyvars cons [{-no derivings-}] True
  where
    arity     = length tyvars
    full_name = mkPreludeCoreName mod name

pcPrimTyCon :: Unique{-TyConKey-} -> FAST_STRING -> Int -> ([PrimKind] -> PrimKind) -> TyCon
pcPrimTyCon key name arity kind_fn
  = mkPrimTyCon key full_name arity kind_fn
  where
    full_name = mkPreludeCoreName pRELUDE_BUILTIN name
\end{code}

\begin{code}
-- things for Ids -----------------------------------------------------

pcDataCon :: Unique{-DataConKey-} -> FAST_STRING -> FAST_STRING -> [TyVarTemplate] -> ThetaType -> [TauType] -> TyCon -> SpecEnv -> Id
pcDataCon key mod name tyvars context arg_tys tycon specenv
  = mkDataCon key (mkPreludeCoreName mod name) tyvars context arg_tys tycon specenv

pcMiscPrelId :: Unique{-IdKey-} -> FAST_STRING -> FAST_STRING -> UniType -> IdInfo -> Id

pcMiscPrelId key mod name ty info
 = mkPreludeId	key (mkPreludeCoreName mod name) ty info
\end{code}

@mkBuild@ is suger for building a build !
@mkbuild ty tv c n e@ $Rightarrow$ @build ty (/\ tv -> \ c n -> e)@
@ty@ is the type of the list.
@tv@ is always a new type variable.
@c,n@ are Id's for the abstract cons and nil
\begin{verbatim}
	c :: a -> b -> b
	n :: b
	v :: (\/ b . (a -> b -> b) -> b -> b) -> [a]
--  \/ a .  (\/ b . (a -> b -> b) -> b -> b) -> [a]
\end{verbatim}
@e@ is the object right inside the @build@

\begin{code}
--LATER: mkBuild :: UniType
--LATER: 	-> TyVar
--LATER: 	-> Id
--LATER: 	-> Id
--LATER: 	-> PlainCoreExpr
--LATER: 	-> PlainCoreExpr
--LATER: mkBuild ty tv c n expr
--LATER:  = CoApp (CoTyApp (CoVar buildId) ty)
--LATER:               (CoTyLam tv (mkCoLam [c,n] expr))
--LATER: -- CoCon buildDataCon [ty] [CoTyLam tv (mkCoLam [c,n] expr)]
\end{code}

\begin{code}
--LATER: mkListFilter tys args ty ity c n exp
--LATER:   = foldr CoTyLam
--LATER: 	 (CoLam args (mkBuild ty ity c n exp))
--LATER: 	  tys
\end{code}


%************************************************************************
%*									*
\subsection[PrelFuns-specialisations]{Specialisations for builtin values}
%*									*
%************************************************************************

The specialisations which exist for the builtin values must be recorded in
their IdInfos.

NOTE: THE USES OF THE pcGenerate... FUNCTIONS MUST CORRESPOND
      TO THE SPECIALISATIONS DECLARED IN THE PRELUDE !!!

HACK: We currently use the same unique for the specialised Ids.

The list @specing_types@ determines the types for which specialised
versions are created. Note: This should correspond with the
types passed to the pre-processor with the -genSPECS arg (see ghc.lprl).

ToDo: Create single mkworld definition which is grabbed here and in ghc.lprl

\begin{code}
pcGenerateSpecs :: Unique -> Id -> IdInfo -> UniType -> SpecEnv
pcGenerateSpecs key id info ty
  = pc_gen_specs True key id info ty

pcGenerateDataSpecs :: UniType -> SpecEnv
pcGenerateDataSpecs ty
  = pc_gen_specs False err err err ty
  where
    err = panic "PrelFuns:GenerateDataSpecs"

pcGenerateTupleSpecs :: Int -> UniType -> SpecEnv
pcGenerateTupleSpecs arity ty
  = if arity < 5 then
	pcGenerateDataSpecs ty
    else if arity == 5 then
	let
	    tup5_spec jty = SpecInfo (take 5 (repeat jty))
	 			     0 (panic "SpecData:SpecInfo:SpecId")
	in
	mkSpecEnv (map tup5_spec (tail specing_types))
    else if arity == 19 then
	mkSpecEnv [SpecInfo (Nothing : Just doublePrimTy : take 17 (repeat Nothing))
			    0 (panic "SpecData:SpecInfo:SpecId")]
    else
        nullSpecEnv

pc_gen_specs is_id key id info ty
 = mkSpecEnv spec_infos
 where
   spec_infos = [ let spec_ty = specialiseTy ty spec_tys 0
		      spec_id = if is_id 
			        then mkSpecId key {- HACK WARNING: same unique! -}
				              id spec_tys spec_ty info
				else panic "SpecData:SpecInfo:SpecId"
		  in
		  SpecInfo spec_tys (length ctxts) spec_id
		| spec_tys <- specialisations ]

   (tyvars, ctxts, _) = splitType ty
   no_tyvars	      = length tyvars

   specialisations    = if no_tyvars == 0
			then []
		        else tail (cross_product no_tyvars specing_types)

			-- N.B. tail removes fully polymorphic specialisation

cross_product 0 tys = []
cross_product 1 tys = map (:[]) tys
cross_product n tys = concat [map (:cp) tys | cp <- cross_product (n-1) tys]


specing_types = [Nothing,	
		 Just charPrimTy,
		 Just doublePrimTy,
		 Just intPrimTy ]
\end{code}
