%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[AbsUniType]{@AbsUniType@: the abstract interface to @UniType@}

The module @AbsUniType@ is the ``outside world's'' interface to the
@UniType@ datatype.  It imports and re-exports the appropriate
@UniType@ stuff.

The prototype compiler's lack of original namery means it is good to
include @Class@, @TyVar@ and @TyCon@ stuff here, too, and to let this
module also present the ``outside-world'' interface for them.

\begin{code}
#include "HsVersions.h"

module AbsUniType (
	-- Class and ClassOp stuff -------------------------------------
	Class,
	mkClass,
	getClassKey, getClassOps,
	getSuperDictSelId, getClassOpId, getDefaultMethodId,
	getConstMethodId,
	getClassSig, getClassBigSig, getClassInstEnv,
--UNUSED: getClassDefaultMethodsInfo,
	isSuperClassOf,
	cmpClass,
	derivableClassKeys,
	isNumericClass, isStandardClass, -- UNUSED: isDerivableClass,

	ClassOp,
	mkClassOp,
	getClassOpTag, getClassOpString,
--UNUSED: getClassOpSig,
	getClassOpLocalType,

	-- TyVar stuff -------------------------------------------------
	TyVar,
	TyVarTemplate,

	mkUserTyVar, mkPolySysTyVar, mkOpenSysTyVar,
--UNUSED: mkPrimSysTyVar, isPrimTyVar,

--	getTyVarUnique,

	cmpTyVar, eqTyVar, ltTyVar,  -- used a lot!

	mkUserTyVarTemplate, mkSysTyVarTemplate, mkTemplateTyVars, 

	cloneTyVarFromTemplate,
	cloneTyVar,
	instantiateTyVarTemplates,

	-- a supply of template tyvars 
	alphaTyVars,
	alpha_tv, beta_tv, gamma_tv, delta_tv, epsilon_tv,		 -- templates
	alpha_tyvar, beta_tyvar, gamma_tyvar, delta_tyvar, epsilon_tyvar,-- real tyvars

	-- TyCon stuff -------------------------------------------------
	TyCon,
	Arity(..),	-- synonym for Int
	mkSynonymTyCon, mkDataTyCon, mkTupleTyCon,
	mkPrimTyCon, mkSpecTyCon,
#ifdef DPH
	mkProcessorTyCon, mkPodizedPodTyCon,
#endif {- Data Parallel Haskell -}

	isSynTyCon, isVisibleSynTyCon, isDataTyCon,
	isPrimTyCon, isBoxedTyCon,
	maybeCharLikeTyCon, maybeIntLikeTyCon,
	maybeFloatLikeTyCon, maybeDoubleLikeTyCon,
	isEnumerationTyCon, --UNUSED: isEnumerationTyConMostly,
	isTupleTyCon,
	isLocalSpecTyCon, isLocalGenTyCon, isBigTupleTyCon,
	maybeSingleConstructorTyCon,
	derivedFor, --UNUSED: preludeClassDerivedFor,
	cmpTyCon, eqTyCon,

	getTyConArity, getTyConDataCons,
	getTyConTyVarTemplates,
	getTyConKind,
	getTyConDerivings,
	getTyConFamilySize,

	-- UniType stuff -----------------------------------------------
	UniType,

	-- USEFUL SYNONYMS
	SigmaType(..), RhoType(..), TauType(..),
	ThetaType(..),			-- synonym for [(Class,UniType)]

	-- CONSTRUCTION
	mkTyVarTy, mkTyVarTemplateTy, mkDictTy,
	-- use applyTyCon to make UniDatas, UniSyns
	mkRhoTy, mkForallTy, mkSigmaTy,	-- ToDo: perhaps nuke one?

	-- QUANTIFICATION & INSTANTIATION
	quantifyTy,
	instantiateTy,  instantiateTauTy,  instantiateThetaTy,

	-- COMPARISON (use sparingly!)
	cmpUniType,
	cmpUniTypeMaybeList,

	-- PRE-BUILT TYPES (for Prelude)
	alpha, beta, gamma, delta, epsilon,		 	-- these have templates in them
	alpha_ty, beta_ty, gamma_ty, delta_ty, epsilon_ty,	-- these have tyvars in them

	-- UniTyFuns stuff ---------------------------------------------
	-- CONSTRUCTION
	applyTy, applyTyCon, applySynTyCon, applyNonSynTyCon,
	glueTyArgs, mkSuperDictSelType, --UNUSED: mkDictFunType,
	specialiseTy,

	-- DESTRUCTION
--not exported:	expandTySyns,
	expandVisibleTySyn,
	getTyVar, getTyVarMaybe, getTyVarTemplateMaybe,
	splitType, splitForalls, getTauType, splitTyArgs,
	splitTypeWithDictsAsArgs,
--not exported/unused:	sourceTypes, targetType,
	funResultTy,
	splitDictType,
	kindFromType,
	getUniDataTyCon, getUniDataTyCon_maybe,
	getUniDataSpecTyCon, getUniDataSpecTyCon_maybe,
	unDictifyTy,
	getMentionedTyCons,
#ifdef USE_SEMANTIQUE_STRANAL
	getReferredToTyCons,
#endif {- Semantique strictness analyser -}
	getMentionedTyConsAndClassesFromUniType,
	getMentionedTyConsAndClassesFromTyCon,
    	getMentionedTyConsAndClassesFromClass,
	getUniTyDescription,

	-- FREE-VARIABLE EXTRACTION
	extractTyVarsFromTy, extractTyVarsFromTys,
	extractTyVarTemplatesFromTy,

	-- PREDICATES
	isTyVarTy, isTyVarTemplateTy,
	maybeUnpackFunTy, isFunType,
	isPrimType, isUnboxedDataType, --UNUSED: isDataConType,
	isLeakFreeType,
	maybeBoxedPrimType,
--UNUSED: hasHigherOrderArg,
	isDictTy, isGroundTy, isGroundOrTyVarTy,
	instanceIsExported,
--UNUSED:	isSynTarget,
	isTauTy, isForAllTy,
	maybePurelyLocalTyCon, maybePurelyLocalClass,
	maybePurelyLocalType,
	returnsRealWorld, -- HACK courtesy of SLPJ
#ifdef DPH
        isProcessorTy,
	isProcessorTyCon,
	isPodizedPodTyCon,
	getPodizedPodDimension,
	runtimeUnpodizableType,
#endif {- Data Parallel Haskell -}

	-- SUBSTITUTION
	applyTypeEnvToTy, applyTypeEnvToThetaTy,
--not exported:	applyTypeEnvToTauTy,
	mapOverTyVars,
--	genInstantiateTyUS, -- ToDo: ???

	-- PRETTY PRINTING AND FORCING
	pprUniType, pprParendUniType, pprMaybeTy,
	pprTyCon, pprIfaceClass, pprClassOp,
	getTypeString,
	typeMaybeString,
	specMaybeTysSuffix,
	showTyCon,
	showTypeCategory,

	-- MATCHING
	matchTy, -- UNUSED: matchTys,

	-- and, finally, stuff to make the interface self-contained...
--	Outputable(..), NamedThing(..),
	ExportFlag, Pretty(..), PprStyle, PrettyRep,

	GlobalSwitch, UnfoldingDetails, Id, DataCon(..), IdEnv(..),
	InstTemplate, Maybe, Name, FullName, ShortName,
	PrimKind, TyVarEnv(..), TypeEnv(..), Unique, ClassInstEnv(..),
	MatchEnv(..), InstTyEnv(..), UniqFM, Bag

	IF_ATTACK_PRAGMAS(COMMA assocMaybe)

#ifndef __GLASGOW_HASKELL__
	,TAG_
#endif
    ) where

import Class
import TyVar
import TyCon
import UniType
import UniTyFuns

import AbsSyn		( RenamedBinds(..), RenamedExpr(..), RenamedGRHS(..),
			  RenamedGRHSsAndBinds(..), RenamedPat(..), Binds,
			  Expr, GRHS, GRHSsAndBinds, InPat
			)
import InstEnv		( ClassInstEnv(..), MatchEnv(..) )
import Maybes		( assocMaybe, Maybe(..) ) -- (..) for pragmas only
import NameTypes	( ShortName, FullName ) -- pragmas only
import Outputable
import Pretty		( Pretty(..)
			  IF_ATTACK_PRAGMAS(COMMA ppStr COMMA ppDouble COMMA ppInteger)
			)
import TyVarEnv		-- ( TyVarEnv )
import Unique		( Unique, UniqueSupply )
#if USE_ATTACK_PRAGMAS
import Util
#else
#ifndef __GLASGOW_HASKELL__
import Util		( TAG_ )
#endif
#endif
\end{code}
