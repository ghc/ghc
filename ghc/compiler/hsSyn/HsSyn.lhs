%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section{Haskell abstract syntax definition}

This module glues together the pieces of the Haskell abstract syntax,
which is declared in the various \tr{Hs*} modules.  This module,
therefore, is almost nothing but re-exporting.

\begin{code}
#include "HsVersions.h"

module HsSyn (

	-- NB: don't reexport HsCore or HsPragmas;
	-- this module tells about "real Haskell"

	EXP_MODULE(HsSyn) ,
#if (! defined(REALLY_HASKELL_1_3)) || PATRICK_FIXES_MODULE_DOTDOT_THING
	EXP_MODULE(HsBinds) ,
	EXP_MODULE(HsDecls) ,
	EXP_MODULE(HsExpr) ,
	EXP_MODULE(HsImpExp) ,
	EXP_MODULE(HsLit) ,
	EXP_MODULE(HsMatches) ,
	EXP_MODULE(HsPat) ,
	EXP_MODULE(HsTypes)
#else
	ArithSeqInfo(..),
	BangType(..),
	Bind(..),
	ClassDecl(..),
	ConDecl(..),
	DefaultDecl(..),
	FixityDecl(..),
	GRHS(..),
	GRHSsAndBinds(..),
	HsBinds(..),
	HsExpr(..),
	HsLit(..),
	IE(..),
	ImportDecl(..),
	InPat(..),
	InstDecl(..),
	Match(..),
	MonoBinds(..),
	MonoType(..),
	OutPat(..),
	PolyType(..),
	Qualifier(..),
	Sig(..),
	SpecDataSig(..),
	SpecInstSig(..),
	Stmt(..),
	TyDecl(..),
	bindIsRecursive,
	cmpContext,
	cmpMonoType,
	cmpPolyType,
	collectBinders,
	collectMonoBinders,
	collectMonoBindersAndLocs,
	collectPatBinders,
	collectTopLevelBinders,
	extractCtxtTyNames,
	extractMonoTyNames,
	failureFreePat,
	irrefutablePat,
	irrefutablePats,
	isConPat,
	isLitPat,
	negLiteral,
	nullBind,
	nullBinds,
	nullMonoBinds,
	patsAreAllCons,
	patsAreAllLits,
	pp_condecls,
	pp_decl_head,
	pp_dotdot,
	pp_rbinds,
	pp_tydecl,
	pprContext,
	pprExpr,
	pprGRHS,
	pprGRHSsAndBinds,
	pprMatch,
	pprMatches,
	pprParendExpr,
	pprParendMonoType,
	pprParendPolyType,
	ppr_bang,
	print_it,
	SYN_IE(ClassAssertion),
	SYN_IE(Context),
	SYN_IE(HsRecordBinds)
#endif
     ) where

IMP_Ubiq()

-- friends:
import HsBinds
import HsDecls
import HsExpr
import HsImpExp
import HsLit
import HsMatches
import HsPat
import HsTypes
import HsPragmas	( ClassPragmas, ClassOpPragmas,
			  DataPragmas, GenPragmas, InstancePragmas )
-- others:
import FiniteMap	( FiniteMap )
import Outputable	( ifPprShowAll, ifnotPprForUser, interpp'SP, Outputable(..) )
import Pretty
import SrcLoc		( SrcLoc )
\end{code}

@Fake@ is a placeholder type; for when tyvars and uvars aren't used.
\begin{code}
data Fake = Fake
instance Eq Fake
instance Outputable Fake
\end{code}

All we actually declare here is the top-level structure for a module.
\begin{code}
type Version = Int

data HsModule tyvar uvar name pat
  = HsModule
	Module			-- module name
	(Maybe Version)		-- source interface version number
	(Maybe [IE name])	-- export list; Nothing => export everything
				-- Just [] => export *nothing* (???)
				-- Just [...] => as you would expect...
	[ImportDecl name]	-- We snaffle interesting stuff out of the
				-- imported interfaces early on, adding that
				-- info to TyDecls/etc; so this list is
				-- often empty, downstream.
	[FixityDecl name]
	[TyDecl name]
	[SpecDataSig name]		-- user pragmas that modify TyDecls
	[ClassDecl tyvar uvar name pat]
	[InstDecl  tyvar uvar name pat]
	[SpecInstSig name] 		-- user pragmas that modify InstDecls
	[DefaultDecl name]
	(HsBinds tyvar uvar name pat)	-- the main stuff, includes source sigs
	[Sig name]			-- interface sigs
	SrcLoc
\end{code}

\begin{code}
instance (NamedThing name, Outputable name, Outputable pat,
	  Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar)
	=> Outputable (HsModule tyvar uvar name pat) where

    ppr sty (HsModule name iface_version exports imports fixities
		      typedecls typesigs classdecls instdecls instsigs
		      defdecls binds sigs src_loc)
      = ppAboves [
	    ifPprShowAll sty (ppr sty src_loc),
	    ifnotPprForUser sty (pp_iface_version iface_version),
	    case exports of
	      Nothing -> ppCat [ppPStr SLIT("module"), ppPStr name, ppPStr SLIT("where")]
	      Just es -> ppAboves [
			    ppCat [ppPStr SLIT("module"), ppPStr name, ppLparen],
			    ppNest 8 (interpp'SP sty es),
			    ppNest 4 (ppPStr SLIT(") where"))
			  ],
	    pp_nonnull imports,
	    pp_nonnull fixities,
	    pp_nonnull typedecls,
	    pp_nonnull typesigs,
	    pp_nonnull classdecls,
	    pp_nonnull instdecls,
	    pp_nonnull instsigs,
	    pp_nonnull defdecls,
	    ppr sty binds,
	    pp_nonnull sigs
	]
      where
	pp_nonnull [] = ppNil
	pp_nonnull xs = ppAboves (map (ppr sty) xs)

	pp_iface_version Nothing  = ppNil
	pp_iface_version (Just n) = ppCat [ppStr "{-# INTERFACE", ppInt n, ppStr "#-}"]
\end{code}
