%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnIfaces]{Cacheing and Renaming of Interfaces}

\begin{code}
#include "HsVersions.h"

module RnIfaces (
	cacheInterface,
	readInterface,
	rnInterfaces,
	finalIfaceInfo,
	IfaceCache(..),
	VersionInfo(..),
	ParsedIface(..)
    ) where

import PreludeGlaST	( returnPrimIO, thenPrimIO,
			  readVar, writeVar, MutableVar(..) )

import Ubiq

import HsSyn
import RdrHsSyn
import RnHsSyn

import RnMonad
import RnUtils		( RnEnv(..) )

import Bag		( emptyBag )
import ErrUtils		( Error(..), Warning(..) )
import FiniteMap	( emptyFM, lookupFM, addToFM )
import Pretty
import Maybes		( MaybeErr(..) )
import Util		( panic )

\end{code}


\begin{code}
type IfaceCache = MutableVar _RealWorld (FiniteMap Module ParsedIface,
				         FiniteMap Module FAST_STRING)

data ParsedIface = ParsedIface


cacheInterface :: IfaceCache -> Module
	       -> PrimIO (MaybeErr ParsedIface Error)

cacheInterface iface_var mod
  = readVar iface_var `thenPrimIO` \ (iface_fm, file_fm) ->
    case lookupFM iface_fm mod of
      Just iface -> returnPrimIO (Succeeded iface)
      Nothing    ->
      	case lookupFM file_fm mod of
	  Nothing   -> returnPrimIO (Failed (noIfaceErr mod))
	  Just file ->
	    readInterface file mod `thenPrimIO` \ read_iface ->
	    case read_iface of
	      Failed err      -> returnPrimIO (Failed err)
	      Succeeded iface ->
		let
		    iface_fm' = addToFM iface_fm mod iface
		in
		writeVar iface_var (iface_fm', file_fm) `thenPrimIO` \ _ ->
		returnPrimIO (Succeeded iface)


readInterface :: FAST_STRING -> Module
	      -> PrimIO (MaybeErr ParsedIface Error)

readInterface file mod = panic "readInterface"
\end{code}


\begin{code}
rnInterfaces ::
	   IfaceCache				-- iface cache
	-> RnEnv				-- original name env
	-> UniqSupply
	-> RenamedHsModule			-- module to extend with iface decls
	-> [RnName]				-- imported names required
	-> PrimIO (RenamedHsModule,		-- extended module
	           ImplicitEnv,			-- implicit names required
		   Bag Error,
		   Bag Warning)

rnInterfaces iface_var occ_env us rn_module todo
  = returnPrimIO (rn_module, (emptyFM, emptyFM), emptyBag, emptyBag)
\end{code}


\begin{code}
finalIfaceInfo ::
	   IfaceCache				-- iface cache
	-> [RnName]				-- all imported names required
	-> [Module]				-- directly imported modules
	-> PrimIO (VersionInfo,			-- info about version numbers
		   [Module])			-- special instance modules

type VersionInfo = [(Module, Version, [(FAST_STRING, Version)])]

finalIfaceInfo iface_var imps_reqd imp_mods
  = returnPrimIO ([], [])
\end{code}


\begin{code}
noIfaceErr mod sty
  = ppCat [ppStr "Could not find interface for", ppPStr mod]
\end{code}
