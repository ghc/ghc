%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnIfaces]{Cacheing and Renaming of Interfaces}

\begin{code}
#include "HsVersions.h"

module RnIfaces (
	findHiFiles,
	cachedIface,
	readIface,
	rnIfaces,
	finalIfaceInfo,
	IfaceCache(..),
	VersionInfo(..)
    ) where

import Ubiq

import LibDirectory
import PreludeGlaST	( returnPrimIO, thenPrimIO, seqPrimIO,
			  readVar, writeVar, MutableVar(..)
			)

import HsSyn
import RdrHsSyn
import RnHsSyn

import RnMonad
import RnUtils		( RnEnv(..) )
import ParseIface	( parseIface, ParsedIface )

import Bag		( emptyBag )
import CmdLineOpts	( opt_HiSuffix, opt_SysHiSuffix )
import ErrUtils		( Error(..), Warning(..) )
import FiniteMap	( emptyFM, lookupFM, addToFM )
import Pretty
import Maybes		( MaybeErr(..) )
import Util		( startsWith, panic )
\end{code}

\begin{code}
type ModuleToIfaceContents = FiniteMap Module ParsedIface
type ModuleToIfaceFilePath = FiniteMap Module FilePath

type IfaceCache
  = MutableVar _RealWorld (ModuleToIfaceContents,
			   ModuleToIfaceFilePath)
\end{code}

*********************************************************
*							*
\subsection{Looking for interface files}
*							*
*********************************************************

Return a mapping from module-name to
absolute-filename-for-that-interface.
\begin{code}
findHiFiles :: [FilePath] -> [FilePath] -> IO (FiniteMap Module FilePath)

findHiFiles dirs sysdirs
  = do_dirs emptyFM (dirs ++ sysdirs)
  where
    do_dirs env [] = return env
    do_dirs env (dir:dirs)
      = do_dir  env     dir	>>= \ new_env ->
	do_dirs new_env dirs
    -------
    do_dir env dir
      = --trace ("Having a go on..."++dir) $
	getDirectoryContents dir    >>= \ entries ->
	do_entries env entries
    -------
    do_entries env [] = return env
    do_entries env (e:es)
      = do_entry   env     e	>>= \ new_env ->
        do_entries new_env es
    -------
    do_entry env e
      = case (acceptable_hi (reverse e)) of
	  Nothing  -> --trace ("Deemed uncool:"++e) $
		      return env
	  Just mod -> let
			    pmod = _PK_ mod
		      in
		      case (lookupFM env pmod) of
			Nothing -> --trace ("Adding "++mod++" -> "++e) $
				   return (addToFM env pmod e)
			Just xx -> trace ("Already mapped: "++mod++" -> "++xx) $
				   return env
    -------
    acceptable_hi rev_e -- looking at pathname *backwards*
      = case (startsWith (reverse opt_HiSuffix) rev_e) of
	  Nothing -> Nothing
	  Just xs -> plausible_modname xs{-reversed-}

    -------
    plausible_modname rev_e
      = let
	    cand = reverse (takeWhile is_modname_char rev_e)
	in
	if null cand || not (isUpper (head cand))
	then Nothing
	else Just cand
      where
	is_modname_char c = isAlphanum c || c == '_'
\end{code}

*********************************************************
*							*
\subsection{Reading interface files}
*							*
*********************************************************

Return cached info about a Module's interface; otherwise,
read the interface (using our @ModuleToIfaceFilePath@ map
to decide where to look).

\begin{code}
cachedIface :: IfaceCache
	    -> Module
	    -> IO (MaybeErr ParsedIface Error)

cachedIface iface_var mod
  = readVar iface_var `thenPrimIO` \ (iface_fm, file_fm) ->

    case (lookupFM iface_fm mod) of
      Just iface -> return (Succeeded iface)
      Nothing    ->
      	case (lookupFM file_fm mod) of
	  Nothing   -> return (Failed (noIfaceErr mod))
	  Just file ->
	    readIface file mod >>= \ read_iface ->
	    case read_iface of
	      Failed err      -> return (Failed err)
	      Succeeded iface ->
		let
		    iface_fm' = addToFM iface_fm mod iface
		in
		writeVar iface_var (iface_fm', file_fm) `seqPrimIO`
		return (Succeeded iface)
\end{code}

\begin{code}
readIface :: FilePath -> Module
	      -> IO (MaybeErr ParsedIface Error)

readIface file mod
  = readFile file   `thenPrimIO` \ read_result ->
    case read_result of
      Left  err      -> return (Failed    (cannaeReadErr file))
      Right contents -> return (Succeeded (parseIface contents))
\end{code}


\begin{code}
rnIfaces :: IfaceCache				-- iface cache
	 -> RnEnv				-- original name env
	 -> UniqSupply
	 -> RenamedHsModule			-- module to extend with iface decls
	 -> [RnName]				-- imported names required
	 -> PrimIO (RenamedHsModule,		-- extended module
		    ImplicitEnv,		-- implicit names required
		    Bag Error,
		    Bag Warning)

rnIfaces iface_var occ_env us rn_module todo
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
  = ppCat [ppPStr SLIT("Could not find interface for:"), ppPStr mod]

cannaeReadErr file sty
  = ppCat [ppPStr SLIT("Failed in reading file:"), ppStr file]
\end{code}
