%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Dealing with interface files}

\begin{code}
module RnHiFiles (
	readIface, loadInterface, loadHomeInterface, 
	loadOrphanModules,
	loadOldIface,
	ParsedIface(..)
   ) where

#include "HsVersions.h"

import DriverState	( v_GhcMode, isCompManagerMode )
import DriverUtil	( splitFilename )
import CmdLineOpts	( opt_IgnoreIfacePragmas )
import Parser		( parseIface )
import HscTypes		( ModIface(..), emptyModIface,
			  ExternalPackageState(..), 
			  VersionInfo(..), ImportedModuleInfo,
			  lookupIfaceByModName, RdrExportItem, WhatsImported(..),
			  ImportVersion, WhetherHasOrphans, IsBootInterface,
			  DeclsMap, GatedDecl, IfaceInsts, IfaceRules, mkIfaceDecls,
			  AvailInfo, GenAvailInfo(..), ParsedIface(..), IfaceDeprecs,
			  Avails, availNames, availName, Deprecations(..)
			 )
import HsSyn		( TyClDecl(..), InstDecl(..), RuleDecl(..), ConDecl(..),
			  hsTyVarNames, splitHsInstDeclTy, tyClDeclName, tyClDeclNames
			)
import RdrHsSyn		( RdrNameTyClDecl, RdrNameInstDecl, RdrNameRuleDecl )
import RnHsSyn		( RenamedInstDecl, RenamedRuleDecl, RenamedTyClDecl,
			  extractHsTyNames_s )
import BasicTypes	( Version, FixitySig(..), Fixity(..), FixityDirection(..) )
import RnSource		( rnIfaceRuleDecl, rnTyClDecl, rnInstDecl )
import RnTypes		( rnHsType )
import RnEnv
import TcRnMonad

import PrelNames	( gHC_PRIM_Name, gHC_PRIM )
import PrelInfo		( ghcPrimExports, cCallableClassDecl, cReturnableClassDecl, assertDecl )
import Name		( Name {-instance NamedThing-}, 
			  nameModule, isInternalName )
import NameEnv
import NameSet
import Id		( idName )
import MkId		( seqId )
import Packages		( preludePackage )
import Module		( Module, ModuleName, ModLocation(ml_hi_file),
			  moduleName, isHomeModule, mkVanillaModule,
			  extendModuleEnv
			)
import RdrName		( RdrName, mkRdrUnqual, rdrNameOcc, nameRdrName )
import OccName		( OccName, mkWorkerOcc, mkClassTyConOcc, mkClassDataConOcc,
			  mkSuperDictSelOcc, mkGenOcc1, mkGenOcc2 )
import TyCon		( DataConDetails(..) )
import SrcLoc		( noSrcLoc, mkSrcLoc )
import Maybes		( maybeToBool )
import StringBuffer     ( hGetStringBuffer )
import FastString	( mkFastString )
import ErrUtils         ( Message )
import Finder		( findModule, findPackageModule )
import Lex
import FiniteMap
import ListSetOps	( minusList )
import Outputable
import Bag
import BinIface		( readBinIface )
import Panic
import Config

import EXCEPTION as Exception
import DATA_IOREF	( readIORef )

import Directory
\end{code}


%*********************************************************
%*							*
\subsection{Loading a new interface file}
%*							*
%*********************************************************

\begin{code}
loadHomeInterface :: SDoc -> Name -> TcRn m ModIface
loadHomeInterface doc_str name
  = ASSERT2( not (isInternalName name), ppr name <+> parens doc_str )
    loadInterface doc_str (moduleName (nameModule name)) ImportBySystem

loadOrphanModules :: [ModuleName] -> TcRn m ()
loadOrphanModules mods
  | null mods = returnM ()
  | otherwise = traceRn (text "Loading orphan modules:" <+> 
			 fsep (map ppr mods))			`thenM_` 
		mappM_ load mods				`thenM_`
		returnM ()
  where
    load mod   = loadInterface (mk_doc mod) mod ImportBySystem
    mk_doc mod = ppr mod <+> ptext SLIT("is a orphan-instance module")

loadInterface :: SDoc -> ModuleName -> WhereFrom -> TcRn m ModIface
  -- Returns Nothing if failed
  -- If we can't find an interface file, and we are doing ImportForUsage,
  --	just fail in the monad, and modify anything else
  -- Otherwise, if we can't find an interface file, 
  --	add an error message to the monad (the first time only) 
  --	and return emptyIface
  -- The "first time only" part is done by modifying the PackageIfaceTable
  --		to have an empty entry
  --
  -- The ImportForUsage case is because when we read the usage information from 
  -- an interface file, we try to read the interfaces it mentions.  
  -- But it's OK to fail; perhaps the module has changed, and that interface 
  -- is no longer used.
  
  -- tryLoadInterface guarantees to return with eps_mod_info m --> (..., True)
  -- (If the load fails, we plug in a vanilla placeholder)
loadInterface doc_str mod_name from
 = getHpt	`thenM` \ hpt ->
   getModule	`thenM` \ this_mod ->
   getEps 	`thenM` \ eps@(EPS { eps_PIT = pit }) ->

	-- CHECK WHETHER WE HAVE IT ALREADY
   case lookupIfaceByModName hpt pit mod_name of {
	Just iface |  case from of
			ImportByUser   src_imp -> src_imp == mi_boot iface
			ImportForUsage src_imp -> src_imp == mi_boot iface
			ImportBySystem 	       -> True
		   -> returnM iface ;		-- Already loaded
			-- The not (mi_boot iface) test checks that the already-loaded
			-- interface isn't a boot iface.  This can conceivably happen,
			-- if the version checking happened to load a boot interface
			-- before we got to real imports.  
	other	    -> 

   let
	mod_map  = eps_imp_mods eps
	mod_info = lookupFM mod_map mod_name

	hi_boot_file 
	  = case (from, mod_info) of
		(ImportByUser   is_boot, _)  	    -> is_boot
		(ImportForUsage is_boot, _)	    -> is_boot
		(ImportBySystem, Just (_, is_boot)) -> is_boot
		(ImportBySystem, Nothing)	    -> False
			-- We're importing a module we know absolutely
			-- nothing about, so we assume it's from
			-- another package, where we aren't doing 
			-- dependency tracking. So it won't be a hi-boot file.

	redundant_source_import 
	  = case (from, mod_info) of 
		(ImportByUser True, Just (_,False)) -> True
		other			            -> False
   in

	-- Issue a warning for a redundant {- SOURCE -} import
	-- NB that we arrange to read all the ordinary imports before 
	-- any of the {- SOURCE -} imports
   warnIf	redundant_source_import
		(warnRedundantSourceImport mod_name)	`thenM_`

	-- Check that we aren't importing ourselves. 
	-- That only happens in Rename.checkOldIface, 
	-- which doesn't call loadInterface
   warnIf
	(isHomeModule this_mod && moduleName this_mod == mod_name)
	(warnSelfImport this_mod)		`thenM_`

	-- READ THE MODULE IN
   findAndReadIface doc_str mod_name hi_boot_file
					    `thenM` \ read_result ->
   case read_result of {
	Left err
	  | case from of { ImportForUsage _ -> True ; other -> False }
	  -> failM	-- Fail with no error messages

	  |  otherwise  
	  -> let  	-- Not found, so add an empty export env to 
			-- the EPS map so that we don't look again
		fake_mod   = mkVanillaModule mod_name
		fake_iface = emptyModIface fake_mod
		new_eps    = eps { eps_PIT = extendModuleEnv pit fake_mod fake_iface }
	     in
	     setEps new_eps		`thenM_`
	     addErr (elaborate err)	`thenM_`
	     returnM fake_iface 
	  where
	    elaborate err = hang (ptext SLIT("Failed to load interface for") <+> 
				  quotes (ppr mod_name) <> colon) 4 err
	  ;

	-- Found and parsed!
	Right (mod, iface) ->

	-- LOAD IT INTO EPS

	-- NB: *first* we do loadDecl, so that the provenance of all the locally-defined
	---    names is done correctly (notably, whether this is an .hi file or .hi-boot file).
	--     If we do loadExport first the wrong info gets into the cache (unless we
	-- 	explicitly tag each export which seems a bit of a bore)


	-- Sanity check.  If we're system-importing a module we know nothing at all
	-- about, it should be from a different package to this one
    WARN( not (maybeToBool mod_info) && 
	  case from of { ImportBySystem -> True; other -> False } &&
	  isHomeModule mod,
	  ppr mod )

    initRn (InterfaceMode mod)					$
	-- Set the module, for use when looking up occurrences
	-- of names in interface decls and rules
    loadDecls mod	(eps_decls eps)	  (pi_decls iface)	`thenM` \ (decls_vers, new_decls) ->
    loadRules     mod	(eps_rules eps)   (pi_rules iface)	`thenM` \ (rule_vers, new_rules) ->
    loadInstDecls mod	(eps_insts eps)   (pi_insts iface)	`thenM` \ new_insts ->
    loadExports 		 	  (pi_exports iface)	`thenM` \ (export_vers, avails) ->
    loadFixDecls  	 		  (pi_fixity iface)	`thenM` \ fix_env ->
    loadDeprecs				  (pi_deprecs iface)	`thenM` \ deprec_env ->
   let
	version	= VersionInfo { vers_module  = pi_vers iface, 
				vers_exports = export_vers,
				vers_rules = rule_vers,
				vers_decls = decls_vers }

	-- For an explicit user import, add to mod_map info about
	-- the things the imported module depends on, extracted
	-- from its usage info; and delete the module itself, which is now in the PIT
	usages   = pi_usages iface
	mod_map1 = case from of
			ImportByUser _ -> addModDeps mod is_loaded usages mod_map
			other          -> mod_map
	mod_map2 = delFromFM mod_map1 mod_name

	-- mod_deps is a pruned version of usages that records only what 
	-- module imported, but nothing about versions.
	-- This info is used when demand-linking the dependencies
	mod_deps = [ (mod,orph,boot,NothingAtAll) | (mod,orph,boot,_) <- usages]

 	this_mod_name = moduleName this_mod
	is_loaded m   =  m == this_mod_name 
		      || maybeToBool (lookupIfaceByModName hpt pit m)
		-- We treat the currently-being-compiled module as 'loaded' because
		-- even though it isn't yet in the HIT or PIT; otherwise it gets
		-- put into iImpModInfo, and then spat out into its own interface
		-- file as a dependency

	-- Now add info about this module to the PIT
	has_orphans = pi_orphan iface
	new_pit   = extendModuleEnv pit mod mod_iface
 	mod_iface = ModIface { mi_module = mod, mi_package = pi_pkg iface,
			       mi_version = version,
			       mi_orphan = has_orphans, mi_boot = hi_boot_file,
			       mi_exports = avails, 
			       mi_fixities = fix_env, mi_deprecs = deprec_env,
			       mi_usages   = mod_deps,	-- Used for demand-loading,
							-- not for version info
			       mi_decls    = panic "No mi_decls in PIT",
			       mi_globals  = Nothing
		    }

	new_eps = eps { eps_PIT	     = new_pit,
			eps_decls    = new_decls,
			eps_insts    = new_insts,
			eps_rules    = new_rules,
			eps_imp_mods = mod_map2  }
    in
    setEps new_eps		`thenM_`
    returnM mod_iface
    }}

-----------------------------------------------------
--	Adding module dependencies from the 
--	import decls in the interface file
-----------------------------------------------------

addModDeps :: Module 
	   -> (ModuleName -> Bool)	-- True for modules that are already loaded
	   -> [ImportVersion a] 
	   -> ImportedModuleInfo -> ImportedModuleInfo
-- (addModDeps M ivs deps)
-- We are importing module M, and M.hi contains 'import' decls given by ivs
addModDeps mod is_loaded new_deps mod_deps
  = foldr add mod_deps filtered_new_deps
  where
	-- Don't record dependencies when importing a module from another package
	-- Except for its descendents which contain orphans,
	-- and in that case, forget about the boot indicator
    filtered_new_deps :: [(ModuleName, (WhetherHasOrphans, IsBootInterface))]
    filtered_new_deps
	| isHomeModule mod  = [ (imp_mod, (has_orphans, is_boot))
			      | (imp_mod, has_orphans, is_boot, _) <- new_deps,
				not (is_loaded imp_mod)
			      ]			      
	| otherwise	    = [ (imp_mod, (True, False))
			      | (imp_mod, has_orphans, _, _) <- new_deps,
				not (is_loaded imp_mod) && has_orphans
			      ]
    add (imp_mod, dep) deps = addToFM_C combine deps imp_mod dep

    combine old@(old_has_orphans, old_is_boot) new@(new_has_orphans, new_is_boot)
	| old_is_boot = new	-- Record the best is_boot info
	| otherwise   = old

-----------------------------------------------------
--	Loading the export list
-----------------------------------------------------

loadExports :: (Version, [RdrExportItem]) -> TcRn m (Version, [(ModuleName,Avails)])
loadExports (vers, items)
  = mappM loadExport items	`thenM` \ avails_s ->
    returnM (vers, avails_s)


loadExport :: RdrExportItem -> TcRn m (ModuleName, Avails)
loadExport (mod, entities)
  = mappM (load_entity mod) entities	`thenM` \ avails ->
    returnM (mod, avails)
  where
    load_entity mod (Avail occ)
      =	newGlobalName mod occ	`thenM` \ name ->
	returnM (Avail name)
    load_entity mod (AvailTC occ occs)
      =	newGlobalName mod occ	      	`thenM` \ name ->
        mappM (newGlobalName mod) occs	`thenM` \ names ->
        returnM (AvailTC name names)


-----------------------------------------------------
--	Loading type/class/value decls
-----------------------------------------------------

loadDecls :: Module 
	  -> DeclsMap
	  -> [(Version, RdrNameTyClDecl)]
	  -> TcRn m (NameEnv Version, DeclsMap)
loadDecls mod (decls_map, n_slurped) decls
  = foldlM (loadDecl mod) (emptyNameEnv, decls_map) decls	`thenM` \ (vers, decls_map') -> 
    returnM (vers, (decls_map', n_slurped))

loadDecl mod (version_map, decls_map) (version, decl)
  = getTyClDeclBinders mod decl		`thenM` \ avail ->
    getSysBinders mod decl		`thenM` \ sys_names ->
    let
	full_avail    = case avail of
			  Avail n -> avail
			  AvailTC n ns -> AvailTC n (sys_names ++ ns)
	main_name     = availName full_avail
	new_decls_map = extendNameEnvList decls_map stuff
	stuff  	      = [ (name, (full_avail, name==main_name, (mod, decl))) 
			| name <- availNames full_avail]

	new_version_map = extendNameEnv version_map main_name version
    in
    traceRn (text "Loading" <+> ppr full_avail) `thenM_`
    returnM (new_version_map, new_decls_map)



-----------------
getTyClDeclBinders :: Module -> RdrNameTyClDecl -> TcRn m AvailInfo	

getTyClDeclBinders mod (IfaceSig {tcdName = var, tcdLoc = src_loc})
  = newTopBinder mod var src_loc			`thenM` \ var_name ->
    returnM (Avail var_name)

getTyClDeclBinders mod tycl_decl
  = mapM new (tyClDeclNames tycl_decl)	`thenM` \ names@(main_name:_) ->
    returnM (AvailTC main_name names)
  where
    new (nm,loc) = newTopBinder mod nm loc

--------------------------------
-- The "system names" are extra implicit names *bound* by the decl.

getSysBinders :: Module -> TyClDecl RdrName -> TcRn m [Name]
-- Similar to tyClDeclNames, but returns the "implicit" 
-- or "system" names of the declaration.  And it only works
-- on RdrNames, returning OccNames

getSysBinders mod (ClassDecl {tcdName = cname, tcdCtxt = cxt, tcdLoc = loc})
  = sequenceM [new_sys_bndr mod n loc | n <- sys_occs]
  where
	-- C.f. TcClassDcl.tcClassDecl1
    sys_occs	= tc_occ : data_occ : dw_occ : sc_sel_occs
    cls_occ  	= rdrNameOcc cname
    data_occ 	= mkClassDataConOcc cls_occ
    dw_occ  	= mkWorkerOcc data_occ
    tc_occ    	= mkClassTyConOcc   cls_occ
    sc_sel_occs = [mkSuperDictSelOcc n cls_occ | n <- [1..length cxt]]

getSysBinders mod (TyData {tcdName = tc_name, tcdCons = DataCons cons, 	
			   tcdGeneric = Just want_generic, tcdLoc = loc})
	-- The 'Just' is because this is an interface-file decl
	-- so it will say whether to derive generic stuff for it or not
  = sequenceM ([new_sys_bndr mod n loc | n <- gen_occs] ++ 
	       map con_sys_occ cons)
  where
	-- c.f. TcTyDecls.tcTyDecl
    tc_occ = rdrNameOcc tc_name
    gen_occs | want_generic = [mkGenOcc1 tc_occ, mkGenOcc2 tc_occ]
	     | otherwise    = []
    con_sys_occ (ConDecl name _ _ _ loc) 
	= new_sys_bndr mod (mkWorkerOcc (rdrNameOcc name)) loc
    
getSysBinders mod decl = returnM []

new_sys_bndr mod occ loc = newTopBinder mod (mkRdrUnqual occ) loc


-----------------------------------------------------
--	Loading fixity decls
-----------------------------------------------------

loadFixDecls decls
  = mappM loadFixDecl decls	`thenM` \ to_add ->
    returnM (mkNameEnv to_add)

loadFixDecl (FixitySig rdr_name fixity loc)
  = lookupGlobalOccRn rdr_name	 	`thenM` \ name ->
    returnM (name, FixitySig name fixity loc)


-----------------------------------------------------
--	Loading instance decls
-----------------------------------------------------

loadInstDecls :: Module -> IfaceInsts
	      -> [RdrNameInstDecl]
	      -> RnM IfaceInsts
loadInstDecls mod (insts, n_slurped) decls
  = foldlM (loadInstDecl mod) insts decls 	`thenM` \ insts' ->
    returnM (insts', n_slurped)


loadInstDecl mod insts decl@(InstDecl inst_ty _ _ _ _)
  = 	-- Find out what type constructors and classes are "gates" for the
	-- instance declaration.  If all these "gates" are slurped in then
	-- we should slurp the instance decl too.
	-- 
	-- We *don't* want to count names in the context part as gates, though.
	-- For example:
	--		instance Foo a => Baz (T a) where ...
	--
	-- Here the gates are Baz and T, but *not* Foo.
	-- 
	-- HOWEVER: functional dependencies make things more complicated
	--	class C a b | a->b where ...
	--	instance C Foo Baz where ...
	-- Here, the gates are really only C and Foo, *not* Baz.
	-- That is, if C and Foo are visible, even if Baz isn't, we must
	-- slurp the decl.
	--
	-- Rather than take fundeps into account "properly", we just slurp
	-- if C is visible and *any one* of the Names in the types
	-- This is a slightly brutal approximation, but most instance decls
	-- are regular H98 ones and it's perfect for them.
	--
	-- NOTICE that we rename the type before extracting its free
	-- variables.  The free-variable finder for a renamed HsType 
	-- does the Right Thing for built-in syntax like [] and (,).
    rnHsType (text "In an interface instance decl") inst_ty	`thenM` \ inst_ty' ->
    let 
	(tvs,_,cls,tys) = splitHsInstDeclTy inst_ty'
	free_tcs  = nameSetToList (extractHsTyNames_s tys) `minusList` hsTyVarNames tvs

	gate_fn vis_fn = vis_fn cls && (null free_tcs || any vis_fn free_tcs)
	-- The 'vis_fn' returns True for visible names
	-- Here is the implementation of HOWEVER above
	-- (Note that we do let the inst decl in if it mentions 
	--  no tycons at all.  Hence the null free_ty_names.)
    in
    traceRn ((text "Load instance for" <+> ppr inst_ty') $$ ppr free_tcs)	`thenM_`
    returnM ((gate_fn, (mod, decl)) `consBag` insts)



-----------------------------------------------------
--	Loading Rules
-----------------------------------------------------

loadRules :: Module
	  -> IfaceRules 
	  -> (Version, [RdrNameRuleDecl])
	  -> RnM (Version, IfaceRules)
loadRules mod (rule_bag, n_slurped) (version, rules)
  | null rules || opt_IgnoreIfacePragmas 
  = returnM (version, (rule_bag, n_slurped))
  | otherwise
  = mappM (loadRule mod) rules		`thenM` \ new_rules ->
    returnM (version, (rule_bag `unionBags` listToBag new_rules, n_slurped))

loadRule :: Module -> RdrNameRuleDecl -> RnM (GatedDecl RdrNameRuleDecl)
-- "Gate" the rule simply by whether the rule variable is
-- needed.  We can refine this later.
loadRule mod decl@(IfaceRule _ _ _ var _ _ src_loc)
  = lookupGlobalOccRn var		`thenM` \ var_name ->
    returnM (\vis_fn -> vis_fn var_name, (mod, decl))


-----------------------------------------------------
--	Loading Deprecations
-----------------------------------------------------

loadDeprecs :: IfaceDeprecs -> RnM Deprecations
loadDeprecs Nothing	       = returnM NoDeprecs
loadDeprecs (Just (Left txt))  = returnM (DeprecAll txt)
loadDeprecs (Just (Right prs)) = foldlM loadDeprec emptyNameEnv prs	`thenM` \ env ->
				 returnM (DeprecSome env)
loadDeprec deprec_env (n, txt)
  = lookupGlobalOccRn n 	`thenM` \ name ->
    traceRn (text "Loaded deprecation(s) for" <+> ppr name <> colon <+> ppr txt) `thenM_`
    returnM (extendNameEnv deprec_env name (name,txt))
\end{code}


%********************************************************
%*							*
	Load the ParsedIface for the *current* module
	into a ModIface; then it can be checked
	for up-to-date-ness
%*							*
%********************************************************

\begin{code}
loadOldIface :: ParsedIface -> RnM ModIface

loadOldIface iface
  = loadHomeDecls 	(pi_decls iface)	`thenM` \ (decls_vers, new_decls) ->
    loadHomeRules 	(pi_rules iface)	`thenM` \ (rule_vers, new_rules) -> 
    loadHomeInsts 	(pi_insts iface)	`thenM` \ new_insts ->
    mappM loadHomeUsage	(pi_usages iface)	`thenM` \ usages ->
    loadExports         (pi_exports iface)	`thenM` \ (export_vers, avails) ->
    loadFixDecls 	(pi_fixity iface)	`thenM` \ fix_env ->
    loadDeprecs 	(pi_deprecs iface)	`thenM` \ deprec_env ->

    getModeRn					`thenM` \ (InterfaceMode mod) ->
		-- Caller sets the module before the call; also needed
		-- by the newGlobalName stuff in some of the loadHomeX calls
    let
	version	= VersionInfo { vers_module  = pi_vers iface, 
				vers_exports = export_vers,
				vers_rules   = rule_vers,
				vers_decls   = decls_vers }

	decls = mkIfaceDecls new_decls new_rules new_insts

 	mod_iface = ModIface { mi_module = mod, mi_package = pi_pkg iface,
			       mi_version = version,
			       mi_exports = avails, mi_usages = usages,
			       mi_boot = False, mi_orphan = pi_orphan iface, 
			       mi_fixities = fix_env, mi_deprecs = deprec_env,
			       mi_decls   = decls,
			       mi_globals = Nothing
		    }
    in
    returnM mod_iface
\end{code}

\begin{code}
loadHomeDecls :: [(Version, RdrNameTyClDecl)]
	      -> RnM (NameEnv Version, [RenamedTyClDecl])
loadHomeDecls decls = foldlM loadHomeDecl (emptyNameEnv, []) decls

loadHomeDecl :: (NameEnv Version, [RenamedTyClDecl])
	     -> (Version, RdrNameTyClDecl)
	     -> RnM (NameEnv Version, [RenamedTyClDecl])
loadHomeDecl (version_map, decls) (version, decl)
  = rnTyClDecl decl	`thenM` \ decl' ->
    returnM (extendNameEnv version_map (tyClDeclName decl') version, decl':decls)

------------------
loadHomeRules :: (Version, [RdrNameRuleDecl])
	      -> RnM (Version, [RenamedRuleDecl])
loadHomeRules (version, rules)
  = mappM rnIfaceRuleDecl rules	`thenM` \ rules' ->
    returnM (version, rules')

------------------
loadHomeInsts :: [RdrNameInstDecl]
	      -> RnM [RenamedInstDecl]
loadHomeInsts insts = mappM rnInstDecl insts

------------------
loadHomeUsage :: ImportVersion OccName
	      -> TcRn m (ImportVersion Name)
loadHomeUsage (mod_name, orphans, is_boot, whats_imported)
  = rn_imps whats_imported	`thenM` \ whats_imported' ->
    returnM (mod_name, orphans, is_boot, whats_imported')
  where
    rn_imps NothingAtAll	   	  = returnM NothingAtAll
    rn_imps (Everything v)		  = returnM (Everything v)
    rn_imps (Specifically mv ev items rv) = mappM rn_imp items 	`thenM` \ items' ->
					    returnM (Specifically mv ev items' rv)
    rn_imp (occ,vers) = newGlobalName mod_name occ	`thenM` \ name ->
			returnM (name,vers)
\end{code}


%*********************************************************
%*							*
\subsection{Reading an interface file}
%*							*
%*********************************************************

\begin{code}
findAndReadIface :: SDoc -> ModuleName 
		 -> IsBootInterface	-- True  <=> Look for a .hi-boot file
					-- False <=> Look for .hi file
		 -> TcRn m (Either Message (Module, ParsedIface))
	-- Nothing <=> file not found, or unreadable, or illegible
	-- Just x  <=> successfully found and parsed 

	-- It *doesn't* add an error to the monad, because 
	-- sometimes it's ok to fail... see notes with loadInterface

findAndReadIface doc_str mod_name hi_boot_file
  = traceRn trace_msg			`thenM_`

    -- Check for GHC.Prim, and return its static interface
    if mod_name == gHC_PRIM_Name
	then returnM (Right (gHC_PRIM, ghcPrimIface))
	else

    ioToTcRn (findHiFile mod_name hi_boot_file)	`thenM` \ maybe_found ->

    case maybe_found of
      Nothing -> 
	traceRn (ptext SLIT("...not found"))	`thenM_`
	returnM (Left (noIfaceErr mod_name hi_boot_file))

      Just (wanted_mod, file_path) -> 
	traceRn (ptext SLIT("readIFace") <+> text file_path) 	`thenM_` 

	readIface wanted_mod file_path hi_boot_file	`thenM` \ read_result ->
		-- Catch exceptions here 

	case read_result of
          Left exn    -> returnM (Left (badIfaceFile file_path 
					  (text (showException exn))))

          Right iface -> returnM (Right (wanted_mod, iface))

  where
    trace_msg = sep [hsep [ptext SLIT("Reading"), 
			   if hi_boot_file then ptext SLIT("[boot]") else empty,
			   ptext SLIT("interface for"), 
			   ppr mod_name <> semi],
		     nest 4 (ptext SLIT("reason:") <+> doc_str)]

findHiFile :: ModuleName -> IsBootInterface -> IO (Maybe (Module, FilePath))
findHiFile mod_name hi_boot_file
 = do { 
	-- In interactive or --make mode, we are *not allowed* to demand-load
	-- a home package .hi file.  So don't even look for them.
	-- This helps in the case where you are sitting in eg. ghc/lib/std
	-- and start up GHCi - it won't complain that all the modules it tries
	-- to load are found in the home location.
	ghci_mode <- readIORef v_GhcMode ;
	let { home_allowed = hi_boot_file || 
			     not (isCompManagerMode ghci_mode) } ;
	maybe_found <-	if home_allowed 
			then findModule mod_name
			else findPackageModule mod_name ;

	case maybe_found of {
	  Nothing -> return Nothing ;

	  Just (mod,loc) -> do {

	-- Return the path to M.hi, M.hi-boot, or M.hi-boot-n as appropriate
	let { hi_path            = ml_hi_file loc ;
	      (hi_base, _hi_suf) = splitFilename hi_path ;
    	      hi_boot_path       = hi_base ++ ".hi-boot" ;
    	      hi_boot_ver_path   = hi_base ++ ".hi-boot-" ++ cHscIfaceFileVersion } ;

	if not hi_boot_file then
	   return (Just (mod, hi_path))
	else do {
		hi_ver_exists <- doesFileExist hi_boot_ver_path ;
		if hi_ver_exists then return (Just (mod, hi_boot_ver_path))
				 else return (Just (mod, hi_boot_path))
	}}}}
\end{code}

@readIface@ tries just the one file.

\begin{code}
readIface :: Module -> String -> IsBootInterface -> TcRn m (Either IOError ParsedIface)
	-- Nothing <=> file not found, or unreadable, or illegible
	-- Just x  <=> successfully found and parsed 

readIface mod file_path is_hi_boot_file
  = ioToTcRn_no_fail (read_iface mod file_path is_hi_boot_file)

read_iface mod file_path is_hi_boot_file
 | is_hi_boot_file		-- Read ascii
 = do { buffer <- hGetStringBuffer file_path ;
        case parseIface buffer (mkPState loc exts) of
	  POk _ iface | wanted_mod_name == actual_mod_name
		      -> return iface
		      | otherwise
		      -> throwDyn (ProgramError (showSDoc err)) 
				-- 'showSDoc' is a bit yukky
		where
		  wanted_mod_name = moduleName mod
		  actual_mod_name = pi_mod iface
		  err = hiModuleNameMismatchWarn wanted_mod_name actual_mod_name

	  PFailed err -> throwDyn (ProgramError (showSDoc err))
     }

 | otherwise		-- Read binary
 = readBinIface file_path

 where
    exts = ExtFlags {glasgowExtsEF = True,
		     ffiEF	   = True,
		     withEF	   = True,
		     parrEF	   = True}
    loc  = mkSrcLoc (mkFastString file_path) 1
\end{code}


%*********************************************************
%*						 	 *
	Wired-in interface for GHC.Prim
%*							 *
%*********************************************************

\begin{code}
ghcPrimIface :: ParsedIface
ghcPrimIface = ParsedIface {
      pi_mod	 = gHC_PRIM_Name,
      pi_pkg     = preludePackage,
      pi_vers    = 1,
      pi_orphan  = False,
      pi_usages  = [],
      pi_exports = (1, [(gHC_PRIM_Name, ghcPrimExports)]),
      pi_decls   = [(1,cCallableClassDecl), 
		    (1,cReturnableClassDecl), 
		    (1,assertDecl)],
      pi_fixity  = [FixitySig (nameRdrName (idName seqId)) 
			      (Fixity 0 InfixR) noSrcLoc],
		-- seq is infixr 0
      pi_insts   = [],
      pi_rules   = (1,[]),
      pi_deprecs = Nothing
 }
\end{code}

%*********************************************************
%*						 	 *
\subsection{Errors}
%*							 *
%*********************************************************

\begin{code}
noIfaceErr mod_name boot_file
  = ptext SLIT("Could not find interface file for") <+> quotes (ppr mod_name)
	-- We used to print the search path, but we can't do that
	-- now, because it's hidden inside the finder.
	-- Maybe the finder should expose more functions.

badIfaceFile file err
  = vcat [ptext SLIT("Bad interface file:") <+> text file, 
	  nest 4 err]

hiModuleNameMismatchWarn :: ModuleName -> ModuleName -> Message
hiModuleNameMismatchWarn requested_mod read_mod = 
    hsep [ ptext SLIT("Something is amiss; requested module name")
	 , ppr requested_mod
	 , ptext SLIT("differs from name found in the interface file")
   	 , ppr read_mod
  	 ]

warnRedundantSourceImport mod_name
  = ptext SLIT("Unnecessary {- SOURCE -} in the import of module")
          <+> quotes (ppr mod_name)

warnSelfImport mod
  = ptext SLIT("Importing my own interface: module") <+> ppr mod
\end{code}
