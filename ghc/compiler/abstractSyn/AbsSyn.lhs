%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[AbsSyntax]{Abstract syntax definition}

This module glues together the pieces of the Haskell abstract syntax,
which is declared in the various \tr{Hs*} modules.  This module,
therefore, is almost nothing but re-exporting.

The abstract syntax, used in the front end of the compiler, follows
that of a paper on the static semantics of Haskell by Simon Peyton
Jones and Phil Wadler.

The abstract syntax is parameterised with respect to variables
(abbrev: \tr{name}) and patterns (abbrev: \tr{pat}); here is a typical
example:
\begin{pseudocode}
type ProtoNameExpr   = Expr ProtoName	ProtoNamePat
type TypecheckedExpr = Expr Id		TypecheckedPat
\end{pseudocode}
Some parts of the syntax are unparameterised, because there is no
need for them to be.

\begin{code}
#include "HsVersions.h"

module AbsSyn (
	-- the mostly-parameterised data types
	ArithSeqInfo(..),
	Bind(..),
	Binds(..),
	ClassDecl(..),
	ClassPragmas,	-- abstract
	ConDecl(..),
	DefaultDecl(..),
	Expr(..),
	FixityDecl(..),
	GRHSsAndBinds(..),
	GRHS(..),
	IE(..),
	ImportedInterface(..),
	IfaceImportDecl(..),
	InPat(..),
	InstDecl(..),
	InstancePragmas,    -- abstract
	Interface(..),
	Literal(..),
	Match(..),
	Module(..),
	MonoBinds(..),
	MonoType(..),
	PolyType(..),
	Qual(..),
	Renaming(..),
	Sig(..),
	GenPragmas,	    -- abstract
	ClassOpPragmas,	    -- abstract
	TyDecl(..),
	DataPragmas,	    -- abstract
	TypePragmas,        -- abstract
	TypecheckedPat(..),
	SpecialisedInstanceSig(..), -- a user pragma
	DataTypeSig(..),

	Context(..),	-- synonyms
	ClassAssertion(..),

	-- synonyms for the (unparameterised) typechecker input
	ProtoNameArithSeqInfo(..),
	ProtoNameBind(..),
	ProtoNameBinds(..),
	ProtoNameClassDecl(..),
	ProtoNameClassPragmas(..),
	ProtoNameConDecl(..),
	ProtoNameContext(..),
	ProtoNameDefaultDecl(..),
	ProtoNameExpr(..),
	ProtoNameFixityDecl(..),
	ProtoNameGRHSsAndBinds(..),
	ProtoNameGRHS(..),
	ProtoNameImportedInterface(..),
	ProtoNameInstDecl(..),
	ProtoNameInstancePragmas(..),
	ProtoNameInterface(..),
	ProtoNameMatch(..),
	ProtoNameModule(..),
	ProtoNameMonoBinds(..),
	ProtoNameMonoType(..),
	ProtoNamePat(..),
	ProtoNamePolyType(..),
	ProtoNameQual(..),
	ProtoNameSig(..),
	ProtoNameClassOpSig(..),
	ProtoNameGenPragmas(..),
	ProtoNameClassOpPragmas(..),
	ProtoNameTyDecl(..),
	ProtoNameDataPragmas(..),
	ProtoNameSpecialisedInstanceSig(..),
	ProtoNameDataTypeSig(..),

	RenamedArithSeqInfo(..),
	RenamedBind(..),
	RenamedBinds(..),
	RenamedClassDecl(..),
	RenamedClassPragmas(..),
	RenamedConDecl(..),
	RenamedContext(..),
	RenamedDefaultDecl(..),
	RenamedExpr(..),
	RenamedFixityDecl(..),
	RenamedGRHSsAndBinds(..),
	RenamedGRHS(..),
	RenamedImportedInterface(..),
	RenamedInstDecl(..),
	RenamedInstancePragmas(..),
	RenamedInterface(..),
	RenamedMatch(..),
	RenamedModule(..),
	RenamedMonoBinds(..),
	RenamedMonoType(..),
	RenamedPat(..),
	RenamedPolyType(..),
	RenamedQual(..),
	RenamedSig(..),
	RenamedClassOpSig(..),
	RenamedGenPragmas(..),
	RenamedClassOpPragmas(..),
	RenamedTyDecl(..),
	RenamedDataPragmas(..),
	RenamedSpecialisedInstanceSig(..),
	RenamedDataTypeSig(..),

	-- synonyms for the (unparameterised) typechecker output
	TypecheckedArithSeqInfo(..),
	TypecheckedBind(..),
	TypecheckedBinds(..),
	TypecheckedExpr(..),
	TypecheckedGRHSsAndBinds(..),
	TypecheckedGRHS(..),
	TypecheckedMatch(..),
	TypecheckedMonoBinds(..),
	TypecheckedModule(..),
	TypecheckedQual(..),

	-- little help functions (AbsSynFuns)
	collectTopLevelBinders,
	collectBinders, collectTypedBinders,
	collectMonoBinders,
	collectMonoBindersAndLocs,
	collectQualBinders,
	collectPatBinders,
	collectTypedPatBinders,
	extractMonoTyNames,
	cmpInstanceTypes, getNonPrelOuterTyCon,
        getIEStrings, getRawIEStrings, ImExportListInfo(..),
--OLD:	getMentionedVars,
	mkDictApp,
	mkDictLam,
	mkTyApp,
	mkTyLam,
	nullBinds,
	nullMonoBinds,
	isLitPat, patsAreAllLits, isConPat, patsAreAllCons,
	irrefutablePat,
#ifdef DPH
	patsAreAllProcessor,
#endif
	unfailablePat, unfailablePats,
	pprContext,
	typeOfPat,
	negLiteral,

	eqConDecls, eqMonoType, cmpPolyType,

	-- imported things so we get a closed interface
	Outputable(..), NamedThing(..),
	ExportFlag, SrcLoc,
	Pretty(..), PprStyle, PrettyRep,

	OptIdInfo(..), -- I hate the instance virus!
	IdInfo, SpecEnv, StrictnessInfo, UpdateInfo, ArityInfo,
	DemandInfo, Demand, ArgUsageInfo, ArgUsage, DeforestInfo,
	FBTypeInfo, FBType, FBConsum, FBProd,

	Name(..), 	-- NB: goes out *WITH* constructors
	Id, DictVar(..), Inst, ProtoName, TyVar, UniType, TauType(..),
	Maybe, PreludeNameFun(..), Unique,
	FullName, ShortName, Arity(..), TyCon, Class, ClassOp,
	UnfoldingGuidance, BinderInfo, BasicLit, PrimOp, PrimKind,
	IdEnv(..), UniqFM, FiniteMap,
	CoreExpr, CoreAtom, UnfoldingCoreAtom, UnfoldingCoreExpr,
	UnfoldingPrimOp, UfCostCentre, Bag
	IF_ATTACK_PRAGMAS(COMMA cmpClass COMMA cmpTyCon COMMA cmpTyVar)
	IF_ATTACK_PRAGMAS(COMMA cmpUniType COMMA pprPrimOp)
#ifndef __GLASGOW_HASKELL__
	,TAG_
#endif
#ifdef DPH
        ,ParQuals(..), ProtoNameParQuals(..),
        RenamedParQuals(..), TypecheckedParQuals(..),
	collectParQualBinders
#endif {- Data Parallel Haskell -}
     ) where


import AbsSynFuns	-- help functions

import HsBinds		-- the main stuff to export
import HsCore
import HsDecls
import HsExpr
import HsImpExp
import HsLit
import HsMatches
import HsPat
import HsPragmas
import HsTypes

import AbsPrel		( PrimKind, PrimOp
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import AbsUniType	( TyVar, TyCon, Arity(..), Class, ClassOp, TauType(..)
			  IF_ATTACK_PRAGMAS(COMMA cmpTyVar)
			  IF_ATTACK_PRAGMAS(COMMA cmpClass)
			  IF_ATTACK_PRAGMAS(COMMA cmpTyCon)
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)
import BasicLit		( BasicLit )
import FiniteMap	( FiniteMap )
import Id		( Id, DictVar(..), DataCon(..) )
import IdInfo
import Inst		( Inst )
import Maybes		( Maybe )
import Name
import NameTypes	( ShortName, FullName ) -- .. for pragmas only
import Outputable
import Pretty
import ProtoName	( ProtoName(..) ) -- .. for pragmas only
import SrcLoc		( SrcLoc )
import Unique		( Unique )
import Util
\end{code}

All we actually declare here is the top-level structure for a module.
\begin{code}
data Module name pat
  = Module
	FAST_STRING		-- module name
	[IE]			-- export list
	[ImportedInterface name pat]
				-- We snaffle interesting stuff out of the
				-- imported interfaces early on, adding that
				-- info to TyDecls/etc; so this list is
				-- often empty, downstream.
	[FixityDecl name]
	[TyDecl name]
	[DataTypeSig name]	-- user pragmas that modify TyDecls;
				-- (much like "Sigs" modify value "Binds")
	[ClassDecl name pat]
	[InstDecl  name pat]
	[SpecialisedInstanceSig name] -- user pragmas that modify InstDecls
	[DefaultDecl name]
	(Binds name pat)	-- the main stuff!
	[Sig name]		-- "Sigs" are folded into the "Binds"
				-- pretty early on, so this list is
				-- often either empty or just the
				-- interface signatures.
	SrcLoc
\end{code}

\begin{code}
type ProtoNameModule    = Module ProtoName ProtoNamePat
type RenamedModule	= Module Name	   RenamedPat
type TypecheckedModule 	= Module Id	   TypecheckedPat
\end{code}

\begin{code}
instance (NamedThing name, Outputable name, NamedThing pat, Outputable pat) =>
	Outputable (Module name pat) where

    ppr sty (Module name exports imports fixities
		      typedecls typesigs classdecls instdecls instsigs
		      defdecls binds sigs src_loc)
      = ppAboves [
	    ifPprShowAll sty (ppr sty src_loc),
	    if (null exports)
	    then (ppCat [ppPStr SLIT("module"), ppPStr name, ppPStr SLIT("where")])
	    else (ppAboves [
		    ppCat [ppPStr SLIT("module"), ppPStr name, ppLparen],
		    ppNest 8 (interpp'SP sty exports),
		    ppNest 4 (ppPStr SLIT(") where"))
		  ]),
	    ppr sty imports,	ppr sty fixities,
	    ppr sty typedecls,	ppr sty typesigs,
	    ppr sty classdecls,
	    ppr sty instdecls,	ppr sty instsigs,
	    ppr sty defdecls,
	    ppr sty binds,	ppr sty sigs
	]
\end{code}
