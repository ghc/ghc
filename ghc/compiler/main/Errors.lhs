%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Errors]{Error reporting}

This module now merely re-exports the work of @ErrsRn@ and @ErrsTc@;
this is the public interface. (WDP 94/06)

\begin{code}
#include "HsVersions.h"

module Errors (
	Error(..),
	pprBagOfErrors,

	-- renamer errors:
	badClassOpErr,
	badExportNameErr,
	badImportNameErr,
	derivingInIfaceErr,
	derivingNonStdClassErr,
	dupNamesErr,
	dupPreludeNameErr,
	dupSigDeclErr,
	duplicateImportsInInterfaceErr,
	inlineInRecursiveBindsErr,
	missingSigErr,
--	mismatchedPragmasErr, UNUSED
	shadowedNameErr,
	unknownNameErr,
	unknownSigDeclErr,
	weirdImportExportConstraintErr,

	-- typechecker errors:
	ambigErr,
	badMatchErr,
	badSpecialisationErr,
	confusedNameErr,
	classCycleErr,
	typeCycleErr,
	dataConArityErr,
	defaultErr,
	derivingEnumErr,
	derivingIxErr,
	derivingWhenInstanceExistsErr,
--	derivingNoSuperClassInstanceErr, UNUSED
	dupInstErr,
--	extraMethodsErr, UNUSED
	genCantGenErr,
--	genPrimTyVarErr, UNUSED
	noInstanceErr,
--	instOpErr, UNUSED
	instTypeErr,
--	methodInstErr, UNUSED
	methodBindErr,
	lurkingRank2Err,
	methodTypeLacksTyVarErr,
--	missingClassOpErr, UNUSED
	naughtyCCallContextErr,
	nonBoxedPrimCCallErr,
	notAsPolyAsSigErr,
--	patMatchWithPrimErr, UNUSED
	preludeInstanceErr,
--	purelyLocalErr, UNUSED
	reduceErr,
	sigContextsErr,
	specGroundnessErr,
	specCtxtGroundnessErr,
	specDataNoSpecErr,
	specDataUnboxedErr,
	specInstUnspecInstNotFoundErr,
	topLevelUnboxedDeclErr,
	tyConArityErr,
	underAppliedTyErr,
	unifyErr,
	varyingArgsErr,
#ifdef DPH
	podCompLhsError,
	pprPodizedWarning,
	PodWarning,
#endif {- Data Parallel Haskell -}

	UnifyErrContext(..),
	UnifyErrInfo(..),

	-- and to make the interface self-sufficient
	Bag, Class, ClassOp, MonoBinds, ProtoNameMonoBinds(..), Sig,
	RenamedSig(..), Expr, RenamedExpr(..), GRHS, RenamedGRHS(..),
	GRHSsAndBinds, RenamedGRHSsAndBinds(..), Match, IE,
	RenamedMatch(..), InPat, ProtoNamePat(..), RenamedPat(..),
	GenPragmas, Id, Inst, Name, PprStyle, Pretty(..), PrettyRep,
	ProtoName, SrcLoc, TyCon, TyVar, TyVarTemplate, UniType,
	TauType(..), Maybe, SignatureInfo, TypecheckedPat,
	TypecheckedExpr(..)
    ) where

-- I don't know how much of this is needed... (WDP 94/06)

import ErrsRn
import ErrsTc
import ErrUtils

import AbsSyn		-- we print a bunch of stuff in here
import UniType		( UniType(..) )		-- Concrete, to make some errors
						-- more informative.
import AbsUniType	( TyVar, TyVarTemplate, TyCon,
			  TauType(..), Class, ClassOp
			  IF_ATTACK_PRAGMAS(COMMA pprUniType)
			)
import Bag		( Bag, bagToList )
import GenSpecEtc	( SignatureInfo(..) )
import HsMatches	( pprMatches, pprMatch, pprGRHS )
import Id		( getIdUniType, Id, isSysLocalId )
import Inst		( getInstOrigin, getDictClassAndType, Inst )
import Maybes		( Maybe(..) )
import Name		( cmpName )
import Outputable
import Pretty		-- to pretty-print error messages
#ifdef DPH
import PodizeMonad	( PodWarning(..) )
#endif {- Data Parallel Haskell -}
import SrcLoc		( mkUnknownSrcLoc, SrcLoc )
import Util
\end{code}
