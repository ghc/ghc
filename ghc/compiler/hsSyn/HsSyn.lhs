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

	HsSyn.. ,
	HsBinds.. ,
	HsDecls.. ,
	HsExpr.. ,
	HsImpExp.. ,
	HsLit.. ,
	HsMatches.. ,
	HsPat.. ,
	HsTypes..

     ) where

import Ubiq{-uitous-}

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
			  DataPragmas, GenPragmas, InstancePragmas
			)
-- others:
import FiniteMap	( FiniteMap )
import Outputable	( ifPprShowAll, interpp'SP, Outputable(..){-instances-} )
import Pretty
import SrcLoc		( SrcLoc{-instances-} )
\end{code}

@Fake@ is a placeholder type; for when tyvars and uvars aren't used.
\begin{code}
data Fake = Fake
instance Eq Fake
instance Outputable Fake
\end{code}

All we actually declare here is the top-level structure for a module.
\begin{code}
data HsModule tyvar uvar name pat
  = HsModule
	FAST_STRING		-- module name
	(Maybe [IE name])	-- export list; Nothing => export everything
				-- Just [] => export *nothing* (???)
				-- Just [...] => as you would expect...
	[ImportedInterface tyvar uvar name pat]
				-- We snaffle interesting stuff out of the
				-- imported interfaces early on, adding that
				-- info to TyDecls/etc; so this list is
				-- often empty, downstream.
	[FixityDecl name]
	[TyDecl name]
	[SpecDataSig name]	-- user pragmas that modify TyDecls
	[ClassDecl tyvar uvar name pat]
	[InstDecl  tyvar uvar name pat]
	[SpecInstSig name] 	-- user pragmas that modify InstDecls
	[DefaultDecl name]
	(HsBinds tyvar uvar name pat)	-- the main stuff!
	[Sig name]		-- "Sigs" are folded into the "HsBinds"
				-- pretty early on, so this list is
				-- often either empty or just the
				-- interface signatures.
	SrcLoc
\end{code}

\begin{code}
instance (NamedThing name, Outputable name, Outputable pat,
	  Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar)
	=> Outputable (HsModule tyvar uvar name pat) where

    ppr sty (HsModule name exports imports fixities
		      typedecls typesigs classdecls instdecls instsigs
		      defdecls binds sigs src_loc)
      = ppAboves [
	    ifPprShowAll sty (ppr sty src_loc),
	    case exports of
	      Nothing -> ppCat [ppPStr SLIT("module"), ppPStr name, ppPStr SLIT("where")]
	      Just es -> ppAboves [
			    ppCat [ppPStr SLIT("module"), ppPStr name, ppLparen],
			    ppNest 8 (interpp'SP sty es),
			    ppNest 4 (ppPStr SLIT(") where"))
			  ],
	    pp_nonnull imports,	    pp_nonnull fixities,
	    pp_nonnull typedecls,   pp_nonnull typesigs,
	    pp_nonnull classdecls,
	    pp_nonnull instdecls,   pp_nonnull instsigs,
	    pp_nonnull defdecls,
	    ppr sty binds,	    pp_nonnull sigs
	]
      where
	pp_nonnull [] = ppNil
	pp_nonnull xs = ppAboves (map (ppr sty) xs)
\end{code}
