%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[PlainCore]{``Plain'' core syntax: the usual parameterisation}

This module defines a particular parameterisation of the @CoreSyntax@
data type.  Both binders and bindees are just @Ids@.  This is the
normal thing.

\begin{code}
#include "HsVersions.h"

module PlainCore (
	PlainCoreProgram(..), PlainCoreBinding(..), PlainCoreExpr(..),
	PlainCoreAtom(..), PlainCoreCaseAlternatives(..),
	PlainCoreCaseDefault(..), PlainCoreArg(..),
#ifdef DPH
	PlainCoreParQuals(..),
	PlainCoreParCommunicate(..),
	CoreParCommunicate(..),
	CoreParQuals(..),
	isParCoreCaseAlternative,
	mkNonRecBinds, 
#endif
	pprPlainCoreBinding,
	pprBigCoreBinder, pprTypedCoreBinder, -- not exported: pprBabyCoreBinder,

	CoreBinding(..), CoreExpr(..), CoreAtom(..), -- re-exported
	CoreCaseAlternatives(..), CoreCaseDefault(..),
	pprCoreExpr,

	CoreArg(..), applyToArgs, decomposeArgs, collectArgs,

	-- and the related utility functions from CoreFuns...

	typeOfCoreExpr,  typeOfCoreAlts,
	instCoreExpr,   substCoreExpr,   -- UNUSED: cloneCoreExpr,
	substCoreExprUS, -- UNUSED: instCoreExprUS, cloneCoreExprUS,
	instCoreBindings,
	mkCoLam, mkCoreIfThenElse,
--	mkCoApp, mkCoCon, mkCoPrim, -- no need for export
	mkCoApps,
	mkCoLetAny, mkCoLetNoUnboxed, mkCoLetUnboxedToCase,
	mkCoLetsAny, mkCoLetsNoUnboxed, mkCoLetsUnboxedToCase,
	mkCoLetrecAny, mkCoLetrecNoUnboxed,
	mkCoTyLam, mkCoTyApp, mkCoTyApps,
	mkErrorCoApp, escErrorMsg,
	pairsFromCoreBinds,
	mkFunction, atomToExpr,
	digForLambdas,
	exprSmallEnoughToDup,
	manifestlyWHNF, manifestlyBottom, --UNUSED: manifestWHNFArgs,
	coreExprArity,
	isWrapperFor,
	maybeErrorApp,
--UNUSED: boilsDownToConApp,
	nonErrorRHSs, bindersOf,
	squashableDictishCcExpr,

	calcUnfoldingGuidance,
	pprCoreUnfolding,
	mentionedInUnfolding,

	-- and one variant of free-var-finding stuff:
	addTopBindsFVs, FVCoreExpr(..), FVCoreBinding(..),

	-- and to make the interface self-sufficient ...
	Outputable(..), NamedThing(..),
	ExportFlag, SrcLoc, Unique,
	Pretty(..), PprStyle, PrettyRep,

	BasicLit, BinderInfo, Class, Id, Demand, IdInfo, FullName,
	UnfoldingGuidance, UniType, TauType(..), ThetaType(..),
	SigmaType(..), TyVar, TyCon, CostCentre, PrimOp, UniqueSupply,
	UniqSM(..), IdEnv(..), UniqFM,
	TyVarEnv(..), TypeEnv(..), IdSet(..), UniqSet(..),
	Maybe, Bag
	IF_ATTACK_PRAGMAS(COMMA cmpClass)
	IF_ATTACK_PRAGMAS(COMMA cmpUniType)
	IF_ATTACK_PRAGMAS(COMMA initUs) -- profiling

-- NOTE(hilly) Added UniqSM for cloneFunctions

    ) where

--IMPORT_Trace	-- ToDo: rm (debugging)

import CoreSyn		-- mostly re-exporting this stuff
import CoreFuns
import CoreUnfold

import AbsUniType	( TauType(..), ThetaType(..), SigmaType(..),
			  Class, UniType, FullName
			  IF_ATTACK_PRAGMAS(COMMA cmpClass)
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)
import FreeVars
import Id		( getIdUniType, getIdStrictness, getIdInfo,
			  Id, TypeEnv(..)
			)
import IdEnv		-- ( nullIdEnv, IdEnv )
import IdInfo
import Maybes		( Maybe(..) )
import Outputable
import Pretty
import Unique		( UniqSM(..), Unique
			  IF_ATTACK_PRAGMAS(COMMA initUs)
			)
import Util

infixr 9 `thenUf`, `thenUf_`
\end{code}

The ``Core things'' just described are parameterised with respect to
the information kept about binding occurrences and bound occurrences
of variables.

The ``Plain Core things'' are instances of the ``Core things'' in
which nothing but a name is kept, for both binders and variables.
\begin{code}
type PlainCoreProgram = [CoreBinding Id Id]
type PlainCoreBinding = CoreBinding  Id Id
type PlainCoreExpr    = CoreExpr     Id Id
type PlainCoreAtom    = CoreAtom        Id
#ifdef DPH
type PlainCoreParQuals		= CoreParQuals Id Id
type PlainCoreParCommunicate	= CoreParCommunicate Id Id
#endif {- Data Parallel Haskell -}
type PlainCoreCaseAlternatives	= CoreCaseAlternatives Id Id
type PlainCoreCaseDefault	= CoreCaseDefault Id Id

type PlainCoreArg = CoreArg Id
\end{code}

%************************************************************************
%*									*
\subsection[printing-PlainCore]{Printing @PlainCore@ things}
%*									*
%************************************************************************

The most common core-printing interface:
\begin{code}
pprPlainCoreBinding :: PprStyle -> PlainCoreBinding -> Pretty

pprPlainCoreBinding sty (CoNonRec binder expr)
  = ppHang (ppCat [pprBigCoreBinder sty binder, ppEquals])
    	 4 (pprCoreExpr sty pprBigCoreBinder pprBabyCoreBinder ppr expr)

pprPlainCoreBinding sty (CoRec binds)
  = ppAboves [ifPprDebug sty (ppStr "{- plain CoRec -}"),
	      ppAboves (map ppr_bind binds),
	      ifPprDebug sty (ppStr "{- end plain CoRec -}")]
  where
    ppr_bind (binder, expr)
      = ppHang (ppCat [pprBigCoreBinder sty binder, ppEquals])
	     4 (pprCoreExpr sty pprBigCoreBinder pprBabyCoreBinder ppr expr)
\end{code}

Other printing bits-and-bobs used with the general @pprCoreBinding@
and @pprCoreExpr@ functions.
\begin{code}
pprBigCoreBinder sty binder
  = ppAboves [sig, pragmas, ppr sty binder]
  where
    sig = ifnotPprShowAll sty (
	    ppHang (ppCat [ppr sty binder, ppStr "::"])
		 4 (ppr sty (getIdUniType binder)))

    pragmas = ifnotPprForUser sty (
	    ppIdInfo sty binder True{-specs, please-} id nullIdEnv (getIdInfo binder))

pprBabyCoreBinder sty binder
  = ppCat [ppr sty binder, pp_strictness]
  where
    pp_strictness
      = case (getIdStrictness binder) of
	  NoStrictnessInfo    -> ppNil
	  BottomGuaranteed    -> ppStr "{- _!_ -}"
	  StrictnessInfo xx _ -> ppStr ("{- " ++ (showList xx "") ++ " -}")

pprTypedCoreBinder sty binder
  = ppBesides [ppLparen, ppCat [ppr sty binder,
	ppStr "::", ppr sty (getIdUniType binder)],
	ppRparen]
\end{code}
