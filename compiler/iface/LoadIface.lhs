%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Loading interface files

\begin{code}
module LoadIface (
	loadInterface, loadInterfaceForName, loadWiredInHomeIface, 
	loadSrcInterface, loadSysInterface, loadOrphanModules, 
	findAndReadIface, readIface,	-- Used when reading the module's old interface
	loadDecls,	-- Should move to TcIface and be renamed
	initExternalPackageState,

	ifaceStats, pprModIface, showIface
   ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TcIface( tcIfaceDecl, tcIfaceRules, tcIfaceInst, 
				 tcIfaceFamInst )

import DynFlags
import IfaceSyn
import IfaceEnv
import HscTypes

import BasicTypes hiding (SuccessFlag(..))
import TcRnMonad
import Type

import PrelNames
import PrelInfo
import PrelRules
import Rules
import InstEnv
import FamInstEnv
import Name
import NameEnv
import MkId
import Module
import OccName
import SrcLoc
import Maybes
import ErrUtils
import Finder
import UniqFM
import StaticFlags
import Outputable
import BinIface
import Panic

import Data.List
import Data.Maybe
import Data.IORef
\end{code}


%************************************************************************
%*									*
	loadSrcInterface, loadOrphanModules, loadHomeInterface

		These three are called from TcM-land	
%*									*
%************************************************************************

\begin{code}
-- | Load the interface corresponding to an @import@ directive in 
-- source code.  On a failure, fail in the monad with an error message.
loadSrcInterface :: SDoc -> ModuleName -> IsBootInterface -> RnM ModIface
loadSrcInterface doc mod want_boot  = do 	
  -- We must first find which Module this import refers to.  This involves
  -- calling the Finder, which as a side effect will search the filesystem
  -- and create a ModLocation.  If successful, loadIface will read the
  -- interface; it will call the Finder again, but the ModLocation will be
  -- cached from the first search.
  hsc_env <- getTopEnv
  res <- ioToIOEnv $ findImportedModule hsc_env mod Nothing
  case res of
    Found _ mod -> do
      mb_iface <- initIfaceTcRn $ loadInterface doc mod (ImportByUser want_boot)
      case mb_iface of
	Failed err      -> failWithTc err
	Succeeded iface -> return iface
    err ->
        let dflags = hsc_dflags hsc_env in
	failWithTc (cannotFindInterface dflags mod err)

-- | Load interfaces for a collection of orphan modules.
loadOrphanModules :: [Module]	      -- the modules
		  -> Bool	      -- these are family instance-modules
		  -> TcM ()
loadOrphanModules mods isFamInstMod
  | null mods = returnM ()
  | otherwise = initIfaceTcRn $
		do { traceIf (text "Loading orphan modules:" <+> 
		     		 fsep (map ppr mods))
		   ; mappM_ load mods
		   ; returnM () }
  where
    load mod   = loadSysInterface (mk_doc mod) mod
    mk_doc mod 
      | isFamInstMod = ppr mod <+> ptext SLIT("is a family-instance module")
      | otherwise    = ppr mod <+> ptext SLIT("is a orphan-instance module")

-- | Loads the interface for a given Name.
loadInterfaceForName :: SDoc -> Name -> TcRn ModIface
loadInterfaceForName doc name
  = do	{ 
#ifdef DEBUG
		-- Should not be called with a name from the module being compiled
	  this_mod <- getModule
	; ASSERT2( not (nameIsLocalOrFrom this_mod name), ppr name <+> parens doc )
#endif
	  initIfaceTcRn $ loadSysInterface doc (nameModule name)
    }

-- | An 'IfM' function to load the home interface for a wired-in thing,
-- so that we're sure that we see its instance declarations and rules
loadWiredInHomeIface :: Name -> IfM lcl ()
loadWiredInHomeIface name
  = ASSERT( isWiredInName name )
    do loadSysInterface doc (nameModule name); return ()
  where
    doc = ptext SLIT("Need home interface for wired-in thing") <+> ppr name

-- | A wrapper for 'loadInterface' that throws an exception if it fails
loadSysInterface :: SDoc -> Module -> IfM lcl ModIface
loadSysInterface doc mod_name
  = do	{ mb_iface <- loadInterface doc mod_name ImportBySystem
	; case mb_iface of 
	    Failed err      -> ghcError (ProgramError (showSDoc err))
	    Succeeded iface -> return iface }
\end{code}


%*********************************************************
%*							*
		loadInterface

	The main function to load an interface
	for an imported module, and put it in
	the External Package State
%*							*
%*********************************************************

\begin{code}
loadInterface :: SDoc -> Module -> WhereFrom
	      -> IfM lcl (MaybeErr Message ModIface)

-- loadInterface looks in both the HPT and PIT for the required interface
-- If not found, it loads it, and puts it in the PIT (always). 

-- If it can't find a suitable interface file, we
--	a) modify the PackageIfaceTable to have an empty entry
--		(to avoid repeated complaints)
--	b) return (Left message)
--
-- It's not necessarily an error for there not to be an interface
-- file -- perhaps the module has changed, and that interface 
-- is no longer used

loadInterface doc_str mod from
  = do	{ 	-- Read the state
	  (eps,hpt) <- getEpsAndHpt

	; traceIf (text "Considering whether to load" <+> ppr mod <+> ppr from)

		-- Check whether we have the interface already
 	; dflags <- getDOpts
	; case lookupIfaceByModule dflags hpt (eps_PIT eps) mod of {
	    Just iface 
		-> returnM (Succeeded iface) ;	-- Already loaded
			-- The (src_imp == mi_boot iface) test checks that the already-loaded
			-- interface isn't a boot iface.  This can conceivably happen,
			-- if an earlier import had a before we got to real imports.   I think.
	    other -> do

	{ let { hi_boot_file = case from of
				ImportByUser usr_boot -> usr_boot
				ImportBySystem        -> sys_boot

	      ; mb_dep   = lookupUFM (eps_is_boot eps) (moduleName mod)
	      ; sys_boot = case mb_dep of
				Just (_, is_boot) -> is_boot
				Nothing		  -> False
			-- The boot-ness of the requested interface, 
	      }		-- based on the dependencies in directly-imported modules

	-- READ THE MODULE IN
	; read_result <- findAndReadIface doc_str mod hi_boot_file
	; case read_result of {
	    Failed err -> do
	  	{ let fake_iface = emptyModIface mod

		; updateEps_ $ \eps ->
			eps { eps_PIT = extendModuleEnv (eps_PIT eps) (mi_module fake_iface) fake_iface }
			-- Not found, so add an empty iface to 
			-- the EPS map so that we don't look again
				
		; returnM (Failed err) } ;

	-- Found and parsed!
	    Succeeded (iface, file_path) 	-- Sanity check:
		| ImportBySystem <- from,	--   system-importing...
		  modulePackageId (mi_module iface) == thisPackage dflags,
		  				--   a home-package module...
		  Nothing <- mb_dep		--   that we know nothing about
		-> returnM (Failed (badDepMsg mod))

		| otherwise ->

	let 
	    loc_doc = text file_path
	in 
	initIfaceLcl mod loc_doc $ do

	-- 	Load the new ModIface into the External Package State
	-- Even home-package interfaces loaded by loadInterface 
	-- 	(which only happens in OneShot mode; in Batch/Interactive 
	--  	mode, home-package modules are loaded one by one into the HPT)
	-- are put in the EPS.
	--
	-- The main thing is to add the ModIface to the PIT, but
	-- we also take the
	--	IfaceDecls, IfaceInst, IfaceRules
	-- out of the ModIface and put them into the big EPS pools

	-- NB: *first* we do loadDecl, so that the provenance of all the locally-defined
	---    names is done correctly (notably, whether this is an .hi file or .hi-boot file).
	--     If we do loadExport first the wrong info gets into the cache (unless we
	-- 	explicitly tag each export which seems a bit of a bore)

	; ignore_prags      <- doptM Opt_IgnoreInterfacePragmas
	; new_eps_decls     <- loadDecls ignore_prags (mi_decls iface)
	; new_eps_insts     <- mapM tcIfaceInst (mi_insts iface)
	; new_eps_fam_insts <- mapM tcIfaceFamInst (mi_fam_insts iface)
	; new_eps_rules     <- tcIfaceRules ignore_prags (mi_rules iface)

	; let {	final_iface = iface {	mi_decls = panic "No mi_decls in PIT",
					mi_insts = panic "No mi_insts in PIT",
					mi_rules = panic "No mi_rules in PIT" } }

	; updateEps_  $ \ eps -> 
	    eps { 
	      eps_PIT          = extendModuleEnv (eps_PIT eps) mod final_iface,
	      eps_PTE          = addDeclsToPTE   (eps_PTE eps) new_eps_decls,
	      eps_rule_base    = extendRuleBaseList (eps_rule_base eps) 
						    new_eps_rules,
	      eps_inst_env     = extendInstEnvList (eps_inst_env eps)  
						   new_eps_insts,
	      eps_fam_inst_env = extendFamInstEnvList (eps_fam_inst_env eps)
						      new_eps_fam_insts,
	      eps_stats        = addEpsInStats (eps_stats eps) 
					       (length new_eps_decls)
	      (length new_eps_insts) (length new_eps_rules) }

	; return (Succeeded final_iface)
    }}}}

badDepMsg mod 
  = hang (ptext SLIT("Interface file inconsistency:"))
       2 (sep [ptext SLIT("home-package module") <+> quotes (ppr mod) <+> ptext SLIT("is mentioned is needed,"), 
	       ptext SLIT("but is not among the dependencies of interfaces directly imported by the module being compiled")])

-----------------------------------------------------
--	Loading type/class/value decls
-- We pass the full Module name here, replete with
-- its package info, so that we can build a Name for
-- each binder with the right package info in it
-- All subsequent lookups, including crucially lookups during typechecking
-- the declaration itself, will find the fully-glorious Name
--
-- We handle ATs specially.  They are not main declarations, but also not
-- implict things (in particular, adding them to `implicitTyThings' would mess
-- things up in the renaming/type checking of source programs).
-----------------------------------------------------

addDeclsToPTE :: PackageTypeEnv -> [(Name,TyThing)] -> PackageTypeEnv
addDeclsToPTE pte things = extendNameEnvList pte things

loadDecls :: Bool
	  -> [(Version, IfaceDecl)]
	  -> IfL [(Name,TyThing)]
loadDecls ignore_prags ver_decls
   = do { mod <- getIfModule
 	; thingss <- mapM (loadDecl ignore_prags mod) ver_decls
	; return (concat thingss)
	}

loadDecl :: Bool		    -- Don't load pragmas into the decl pool
	 -> Module
	  -> (Version, IfaceDecl)
	  -> IfL [(Name,TyThing)]   -- The list can be poked eagerly, but the
				    -- TyThings are forkM'd thunks
loadDecl ignore_prags mod (_version, decl)
  = do 	{ 	-- Populate the name cache with final versions of all 
		-- the names associated with the decl
	  main_name      <- mk_new_bndr mod (ifName decl)
        ; traceIf (text "Loading decl for " <> ppr main_name)
	; implicit_names <- mapM (mk_new_bndr mod) (ifaceDeclSubBndrs decl)

	-- Typecheck the thing, lazily
	-- NB. Firstly, the laziness is there in case we never need the
	-- declaration (in one-shot mode), and secondly it is there so that 
	-- we don't look up the occurrence of a name before calling mk_new_bndr
	-- on the binder.  This is important because we must get the right name
	-- which includes its nameParent.

	; thing <- forkM doc $ do { bumpDeclStats main_name
				  ; tcIfaceDecl ignore_prags decl }

	-- Populate the type environment with the implicitTyThings too
	; let mini_env = mkOccEnv [(getOccName t, t) | t <- implicitTyThings thing]
	      lookup n = case lookupOccEnv mini_env (getOccName n) of
			   Just thing -> thing
			   Nothing    -> 
			     pprPanic "loadDecl" (ppr main_name <+> ppr n $$ ppr (decl))

	; returnM $ (main_name, thing) :  [(n, lookup n) | n <- implicit_names]
	}
		-- We build a list from the *known* names, with (lookup n) thunks
		-- as the TyThings.  That way we can extend the PTE without poking the
		-- thunks
  where
	-- mk_new_bndr allocates in the name cache the final canonical
	-- name for the thing, with the correct 
	--	* parent
	--	* location
	-- imported name, to fix the module correctly in the cache
    mk_new_bndr mod occ 
	= newGlobalBinder mod occ 
			  (importedSrcLoc (showSDoc (ppr (moduleName mod))))
			-- ToDo: qualify with the package name if necessary

    doc = ptext SLIT("Declaration for") <+> ppr (ifName decl)

bumpDeclStats :: Name -> IfL ()		-- Record that one more declaration has actually been used
bumpDeclStats name
  = do	{ traceIf (text "Loading decl for" <+> ppr name)
	; updateEps_ (\eps -> let stats = eps_stats eps
			      in eps { eps_stats = stats { n_decls_out = n_decls_out stats + 1 } })
	}
\end{code}


%*********************************************************
%*							*
\subsection{Reading an interface file}
%*							*
%*********************************************************

\begin{code}
findAndReadIface :: SDoc -> Module
		 -> IsBootInterface	-- True  <=> Look for a .hi-boot file
					-- False <=> Look for .hi file
		 -> TcRnIf gbl lcl (MaybeErr Message (ModIface, FilePath))
	-- Nothing <=> file not found, or unreadable, or illegible
	-- Just x  <=> successfully found and parsed 

	-- It *doesn't* add an error to the monad, because 
	-- sometimes it's ok to fail... see notes with loadInterface

findAndReadIface doc_str mod hi_boot_file
  = do	{ traceIf (sep [hsep [ptext SLIT("Reading"), 
			      if hi_boot_file 
				then ptext SLIT("[boot]") 
				else empty,
			      ptext SLIT("interface for"), 
			      ppr mod <> semi],
		        nest 4 (ptext SLIT("reason:") <+> doc_str)])

	-- Check for GHC.Prim, and return its static interface
	; dflags <- getDOpts
	; if mod == gHC_PRIM
	  then returnM (Succeeded (ghcPrimIface, 
				   "<built in interface for GHC.Prim>"))
	  else do

	-- Look for the file
	; hsc_env <- getTopEnv
	; mb_found <- ioToIOEnv (findHiFile hsc_env mod hi_boot_file)
	; case mb_found of {
	      Failed err -> do
		{ traceIf (ptext SLIT("...not found"))
		; dflags <- getDOpts
		; returnM (Failed (cannotFindInterface dflags 
					(moduleName mod) err)) } ;

	      Succeeded file_path -> do 

	-- Found file, so read it
	{ traceIf (ptext SLIT("readIFace") <+> text file_path)
	; read_result <- readIface mod file_path hi_boot_file
	; case read_result of
	    Failed err -> returnM (Failed (badIfaceFile file_path err))
	    Succeeded iface 
		| mi_module iface /= mod ->
		  return (Failed (wrongIfaceModErr iface mod file_path))
		| otherwise ->
		  returnM (Succeeded (iface, file_path))
			-- Don't forget to fill in the package name...
	}}}

findHiFile :: HscEnv -> Module -> IsBootInterface
	   -> IO (MaybeErr FindResult FilePath)
findHiFile hsc_env mod hi_boot_file
  = do
      maybe_found <- findExactModule hsc_env mod
      case maybe_found of
	Found loc mod -> return (Succeeded path)
		where
		   path = addBootSuffix_maybe hi_boot_file (ml_hi_file loc)
	err -> return (Failed err)
\end{code}

@readIface@ tries just the one file.

\begin{code}
readIface :: Module -> FilePath -> IsBootInterface 
	  -> TcRnIf gbl lcl (MaybeErr Message ModIface)
	-- Failed err    <=> file not found, or unreadable, or illegible
	-- Succeeded iface <=> successfully found and parsed 

readIface wanted_mod file_path is_hi_boot_file
  = do	{ dflags <- getDOpts
        ; res <- tryMostM $ readBinIface file_path
	; case res of
	    Right iface 
		| wanted_mod == actual_mod -> return (Succeeded iface)
		| otherwise	  	   -> return (Failed err)
		where
		  actual_mod = mi_module iface
		  err = hiModuleNameMismatchWarn wanted_mod actual_mod

	    Left exn    -> return (Failed (text (showException exn)))
    }
\end{code}


%*********************************************************
%*						 	 *
	Wired-in interface for GHC.Prim
%*							 *
%*********************************************************

\begin{code}
initExternalPackageState :: ExternalPackageState
initExternalPackageState
  = EPS { 
      eps_is_boot      = emptyUFM,
      eps_PIT          = emptyPackageIfaceTable,
      eps_PTE          = emptyTypeEnv,
      eps_inst_env     = emptyInstEnv,
      eps_fam_inst_env = emptyFamInstEnv,
      eps_rule_base    = mkRuleBase builtinRules,
	-- Initialise the EPS rule pool with the built-in rules
      eps_stats = EpsStats { n_ifaces_in = 0, n_decls_in = 0, n_decls_out = 0
			   , n_insts_in = 0, n_insts_out = 0
			   , n_rules_in = length builtinRules, n_rules_out = 0 }
    }
\end{code}


%*********************************************************
%*						 	 *
	Wired-in interface for GHC.Prim
%*							 *
%*********************************************************

\begin{code}
ghcPrimIface :: ModIface
ghcPrimIface
  = (emptyModIface gHC_PRIM) {
	mi_exports  = [(gHC_PRIM, ghcPrimExports)],
	mi_decls    = [],
	mi_fixities = fixities,
	mi_fix_fn  = mkIfaceFixCache fixities
    }		
  where
    fixities = [(getOccName seqId, Fixity 0 InfixR)]
			-- seq is infixr 0
\end{code}

%*********************************************************
%*							*
\subsection{Statistics}
%*							*
%*********************************************************

\begin{code}
ifaceStats :: ExternalPackageState -> SDoc
ifaceStats eps 
  = hcat [text "Renamer stats: ", msg]
  where
    stats = eps_stats eps
    msg = vcat 
    	[int (n_ifaces_in stats) <+> text "interfaces read",
    	 hsep [ int (n_decls_out stats), text "type/class/variable imported, out of", 
    	        int (n_decls_in stats), text "read"],
    	 hsep [ int (n_insts_out stats), text "instance decls imported, out of",  
    	        int (n_insts_in stats), text "read"],
    	 hsep [ int (n_rules_out stats), text "rule decls imported, out of",  
    	        int (n_rules_in stats), text "read"]
	]
\end{code}    


%************************************************************************
%*				 					*
		Printing interfaces
%*				 					*
%************************************************************************

\begin{code}
-- | Read binary interface, and print it out
showIface :: HscEnv -> FilePath -> IO ()
showIface hsc_env filename = do
   -- skip the version check; we don't want to worry about profiled vs.
   -- non-profiled interfaces, for example.
   writeIORef v_IgnoreHiWay True
   iface <- initTcRnIf 's' hsc_env () () $ readBinIface  filename
   printDump (pprModIface iface)
\end{code}

\begin{code}
pprModIface :: ModIface -> SDoc
-- Show a ModIface
pprModIface iface
 = vcat [ ptext SLIT("interface")
		<+> ppr (mi_module iface) <+> pp_boot 
		<+> ppr (mi_mod_vers iface) <+> pp_sub_vers
		<+> (if mi_orphan iface then ptext SLIT("[orphan module]") else empty)
		<+> (if mi_finsts iface then ptext SLIT("[family instance module]") else empty)
		<+> int opt_HiVersion
		<+> ptext SLIT("where")
	, vcat (map pprExport (mi_exports iface))
	, pprDeps (mi_deps iface)
	, vcat (map pprUsage (mi_usages iface))
	, pprFixities (mi_fixities iface)
	, vcat (map pprIfaceDecl (mi_decls iface))
	, vcat (map ppr (mi_insts iface))
	, vcat (map ppr (mi_rules iface))
	, pprDeprecs (mi_deprecs iface)
	]
  where
    pp_boot | mi_boot iface = ptext SLIT("[boot]")
	    | otherwise     = empty

    exp_vers  = mi_exp_vers iface
    rule_vers = mi_rule_vers iface

    pp_sub_vers | exp_vers == initialVersion && rule_vers == initialVersion = empty
		| otherwise = brackets (ppr exp_vers <+> ppr rule_vers)
\end{code}

When printing export lists, we print like this:
	Avail   f		f
	AvailTC C [C, x, y]	C(x,y)
	AvailTC C [x, y]	C!(x,y)		-- Exporting x, y but not C

\begin{code}
pprExport :: IfaceExport -> SDoc
pprExport (mod, items)
 = hsep [ ptext SLIT("export"), ppr mod, hsep (map pp_avail items) ]
  where
    pp_avail :: GenAvailInfo OccName -> SDoc
    pp_avail (Avail occ)    = ppr occ
    pp_avail (AvailTC _ []) = empty
    pp_avail (AvailTC n (n':ns)) 
	| n==n'     = ppr n <> pp_export ns
 	| otherwise = ppr n <> char '|' <> pp_export (n':ns)
    
    pp_export []    = empty
    pp_export names = braces (hsep (map ppr names))

pprUsage :: Usage -> SDoc
pprUsage usage
  = hsep [ptext SLIT("import"), ppr (usg_name usage), 
	  int (usg_mod usage), 
	  pp_export_version (usg_exports usage),
	  int (usg_rules usage),
	  pp_versions (usg_entities usage) ]
  where
    pp_versions nvs = hsep [ ppr n <+> int v | (n,v) <- nvs ]
    pp_export_version Nothing  = empty
    pp_export_version (Just v) = int v

pprDeps :: Dependencies -> SDoc
pprDeps (Deps { dep_mods = mods, dep_pkgs = pkgs, dep_orphs = orphs,
		dep_finsts = finsts })
  = vcat [ptext SLIT("module dependencies:") <+> fsep (map ppr_mod mods),
	  ptext SLIT("package dependencies:") <+> fsep (map ppr pkgs), 
	  ptext SLIT("orphans:") <+> fsep (map ppr orphs),
	  ptext SLIT("family instance modules:") <+> fsep (map ppr finsts)
	]
  where
    ppr_mod (mod_name, boot) = ppr mod_name <+> ppr_boot boot
    ppr_boot True  = text "[boot]"
    ppr_boot False = empty

pprIfaceDecl :: (Version, IfaceDecl) -> SDoc
pprIfaceDecl (ver, decl)
  = ppr_vers ver <+> ppr decl
  where
	-- Print the version for the decl
    ppr_vers v | v == initialVersion = empty
	       | otherwise	     = int v

pprFixities :: [(OccName, Fixity)] -> SDoc
pprFixities []    = empty
pprFixities fixes = ptext SLIT("fixities") <+> pprWithCommas pprFix fixes
		  where
		    pprFix (occ,fix) = ppr fix <+> ppr occ 

pprDeprecs NoDeprecs	    = empty
pprDeprecs (DeprecAll txt)  = ptext SLIT("Deprecate all") <+> doubleQuotes (ftext txt)
pprDeprecs (DeprecSome prs) = ptext SLIT("Deprecate") <+> vcat (map pprDeprec prs)
			    where
			      pprDeprec (name, txt) = ppr name <+> doubleQuotes (ftext txt)
\end{code}


%*********************************************************
%*						 	 *
\subsection{Errors}
%*							 *
%*********************************************************

\begin{code}
badIfaceFile file err
  = vcat [ptext SLIT("Bad interface file:") <+> text file, 
	  nest 4 err]

hiModuleNameMismatchWarn :: Module -> Module -> Message
hiModuleNameMismatchWarn requested_mod read_mod = 
  withPprStyle defaultUserStyle $
    -- we want the Modules below to be qualified with package names,
    -- so reset the PrintUnqualified setting.
    hsep [ ptext SLIT("Something is amiss; requested module ")
	 , ppr requested_mod
	 , ptext SLIT("differs from name found in the interface file")
   	 , ppr read_mod
  	 ]

wrongIfaceModErr iface mod_name file_path 
  = sep [ptext SLIT("Interface file") <+> iface_file,
         ptext SLIT("contains module") <+> quotes (ppr (mi_module iface)) <> comma,
         ptext SLIT("but we were expecting module") <+> quotes (ppr mod_name),
	 sep [ptext SLIT("Probable cause: the source code which generated"),
	     nest 2 iface_file,
	     ptext SLIT("has an incompatible module name")
	    ]
	]
  where iface_file = doubleQuotes (text file_path)
\end{code}

