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
	EXP_MODULE(HsBinds) ,
	EXP_MODULE(HsDecls) ,
	EXP_MODULE(HsExpr) ,
	EXP_MODULE(HsImpExp) ,
	EXP_MODULE(HsLit) ,
	EXP_MODULE(HsMatches) ,
	EXP_MODULE(HsPat) ,
	EXP_MODULE(HsTypes)
     ) where

IMP_Ubiq()

-- friends:
import HsBinds
import HsDecls		( HsDecl(..), TyDecl(..), InstDecl(..), ClassDecl(..), 
			  DefaultDecl(..), 
			  FixityDecl(..), Fixity(..), FixityDirection(..), 
			  ConDecl(..), BangType(..),
			  IfaceSig(..), HsIdInfo,  SpecDataSig(..), SpecInstSig(..),
			  hsDeclName
			)
import HsExpr
import HsImpExp
import HsLit
import HsMatches
import HsPat
import HsTypes
import HsPragmas	( ClassPragmas, ClassOpPragmas,
			  DataPragmas, GenPragmas, InstancePragmas )
import HsCore

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
	[HsDecl tyvar uvar name pat]	-- Type, class, value, and interface signature decls
	SrcLoc
\end{code}

\begin{code}
instance (NamedThing name, Outputable name, Outputable pat,
	  Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar)
	=> Outputable (HsModule tyvar uvar name pat) where

    ppr sty (HsModule name iface_version exports imports fixities
		      decls src_loc)
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
	    pp_nonnull decls
	]
      where
	pp_nonnull [] = ppNil
	pp_nonnull xs = ppAboves (map (ppr sty) xs)

	pp_iface_version Nothing  = ppNil
	pp_iface_version (Just n) = ppCat [ppStr "{-# INTERFACE", ppInt n, ppStr "#-}"]
\end{code}
