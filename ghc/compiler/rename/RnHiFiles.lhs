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
import DriverUtil	( replaceFilenameSuffix )
import CmdLineOpts	( DynFlag(..) )
import Parser		( parseIface )
import HscTypes		( ModIface(..), emptyModIface,
			  ExternalPackageState(..), noDependencies,
			  VersionInfo(..), Usage(..),
			  lookupIfaceByModName, RdrExportItem, 
			  IsBootInterface,
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
import PrelInfo		( ghcPrimExports )
import Name		( Name {-instance NamedThing-}, 
			  nameModule, isInternalName )
import NameEnv
import NameSet
import Id		( idName )
import MkId		( seqId )
import Packages		( basePackage )
import Module		( Module, ModuleName, ModLocation(ml_hi_file),
			  moduleName, isHomeModule, mkPackageModule,
			  extendModuleEnv, lookupModuleEnvByName
			)
import RdrName		( RdrName, mkRdrUnqual, rdrNameOcc, nameRdrName )
import OccName		( OccName, mkClassTyConOcc, mkClassDataConOcc,
			  mkSuperDictSelOcc, mkGenOcc1, mkGenOcc2, 
			  mkDataConWrapperOcc, mkDataConWorkerOcc )
import TyCon		( DataConDetails(..) )
import SrcLoc		( noSrcLoc, mkSrcLoc )
import Maybes		( maybeToBool )
import StringBuffer     ( hGetStringBuffer )
import FastString	( mkFastString )
import ErrUtils         ( Message )
import Finder		( findModule, findPackageModule, 
			  hiBootExt, hiBootVerExt )
import Lexer
import FiniteMap
import ListSetOps	( minusList )
import Outputable
import Bag
import BinIface		( readBinIface )
import Panic

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
  
loadInterface doc_str mod_name from
 = getHpt		`thenM` \ hpt ->
   getModule		`thenM` \ this_mod ->
   getImports		`thenM` \ import_avails ->
   getEps 		`thenM` \ eps@(EPS { eps_PIT = pit }) ->

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
	mod_map  = imp_dep_mods import_avails
	mod_info = lookupModuleEnvByName mod_map mod_name

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
		(ImportByUser True, Just (_, False)) -> True
		other			             -> False
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
		fake_mod   = mkPackageModule mod_name
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

	-- Now add info about this module to the PIT
	-- Even home modules loaded by this route (which only 
	-- happens in OneShot mode) are put in the PIT
	has_orphans = pi_orphan iface
	new_pit   = extendModuleEnv pit mod mod_iface
 	mod_iface = ModIface { mi_module = mod, mi_package = pi_pkg iface,
			       mi_version = version,
			       mi_orphan = has_orphans, mi_boot = hi_boot_file,
			       mi_exports = avails, 
			       mi_fixities = fix_env, mi_deprecs = deprec_env,
			       mi_deps     = pi_deps iface,
			       mi_usages   = panic "No mi_usages in PIT",
			       mi_decls    = panic "No mi_decls in PIT",
			       mi_globals  = Nothing
		    }

	new_eps = eps { eps_PIT	     = new_pit,
			eps_decls    = new_decls,
			eps_insts    = new_insts,
			eps_rules    = new_rules }
    in
    setEps new_eps		`thenM_`
    returnM mod_iface
    }}

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
      =	newGlobalName2 mod occ	`thenM` \ name ->
	returnM (Avail name)
    load_entity mod (AvailTC occ occs)
      =	newGlobalName2 mod occ	      	`thenM` \ name ->
        mappM (newGlobalName2 mod) occs	`thenM` \ names ->
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
  = maybeStripPragmas decl		`thenM` \ decl ->
    getTyClDeclBinders mod decl		`thenM` \ avail ->
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
--    traceRn (text "Loading" <+> ppr full_avail) `thenM_`
    returnM (new_version_map, new_decls_map)

maybeStripPragmas sig@(IfaceSig {tcdIdInfo = idinfo})
  = doptM Opt_IgnoreInterfacePragmas 	`thenM` \ ignore_prags ->
    if ignore_prags 
	then returnM sig{ tcdIdInfo = [] }
	else returnM sig
maybeStripPragmas other
  = returnM other

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
  = mapM (new_sys_bndr mod loc) sys_occs
  where
	-- C.f. TcClassDcl.tcClassDecl1
    sys_occs	= tc_occ : data_occ : dwrap_occ : dwork_occ : sc_sel_occs
    cls_occ  	= rdrNameOcc cname
    data_occ 	= mkClassDataConOcc cls_occ
    dwrap_occ  	= mkDataConWrapperOcc data_occ
    dwork_occ  	= mkDataConWorkerOcc data_occ
    tc_occ    	= mkClassTyConOcc   cls_occ
    sc_sel_occs = [mkSuperDictSelOcc n cls_occ | n <- [1..length cxt]]

getSysBinders mod (TyData {tcdName = tc_name, tcdCons = DataCons cons, 	
			   tcdGeneric = Just want_generic, tcdLoc = loc})
	-- The 'Just' is because this is an interface-file decl
	-- so it will say whether to derive generic stuff for it or not
  = mapM (new_sys_bndr mod loc) (gen_occs ++ concatMap mk_con_occs cons)
  where
    new = new_sys_bndr
	-- c.f. TcTyDecls.tcTyDecl
    tc_occ = rdrNameOcc tc_name
    gen_occs | want_generic = [mkGenOcc1 tc_occ, mkGenOcc2 tc_occ]
	     | otherwise    = []
    mk_con_occs (ConDecl name _ _ _ _) 
	= [mkDataConWrapperOcc con_occ, mkDataConWorkerOcc con_occ]
	where
	  con_occ = rdrNameOcc name	-- The "source name"
    
getSysBinders mod decl = returnM []

new_sys_bndr mod loc occ = newTopBinder mod (mkRdrUnqual occ) loc


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
--    traceRn ((text "Load instance for" <+> ppr inst_ty') $$ ppr free_tcs)	`thenM_`
    returnM ((gate_fn, (mod, decl)) `consBag` insts)



-----------------------------------------------------
--	Loading Rules
-----------------------------------------------------

loadRules :: Module
	  -> IfaceRules 
	  -> (Version, [RdrNameRuleDecl])
	  -> RnM (Version, IfaceRules)
loadRules mod (rule_bag, n_slurped) (version, rules)
  = doptM Opt_IgnoreInterfacePragmas 	`thenM` \ ignore_prags ->
    if null rules || ignore_prags
	then returnM (version, (rule_bag, n_slurped))
	else mappM (loadRule mod) rules		`thenM` \ new_rules ->
    	     returnM (version, (rule_bag `unionBags` 
				  listToBag new_rules, n_slurped))

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
--    traceRn (text "Loaded deprecation(s) for" <+> ppr name <> colon <+> ppr txt) `thenM_`
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
			       mi_version = version, mi_deps = pi_deps iface,
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
loadHomeUsage :: Usage OccName -> TcRn m (Usage Name)
loadHomeUsage usage
  = mappM rn_imp (usg_entities usage)	`thenM` \ entities' ->
    returnM (usage { usg_entities = entities' })
  where
    mod_name = usg_name usage 
    rn_imp (occ,vers) = newGlobalName2 mod_name occ	`thenM` \ name ->
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
      Left files -> 
	traceRn (ptext SLIT("...not found"))	`thenM_`
	getDOpts				`thenM` \ dflags ->
	returnM (Left (noIfaceErr dflags mod_name hi_boot_file files))

      Right (wanted_mod, file_path) -> 
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

findHiFile :: ModuleName -> IsBootInterface
	   -> IO (Either [FilePath] (Module, FilePath))
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
	  Left files -> return (Left files) ;

	  Right (mod,loc) -> do {

	-- Return the path to M.hi, M.hi-boot, or M.hi-boot-n as appropriate
	let { hi_path            = ml_hi_file loc ;
    	      hi_boot_path       = replaceFilenameSuffix hi_path hiBootExt ;
    	      hi_boot_ver_path   = replaceFilenameSuffix hi_path hiBootVerExt 
	    };

	if not hi_boot_file then
	   return (Right (mod, hi_path))
	else do {
		hi_ver_exists <- doesFileExist hi_boot_ver_path ;
		if hi_ver_exists then return (Right (mod, hi_boot_ver_path))
				 else return (Right (mod, hi_boot_path))
	}}}}
\end{code}

@readIface@ tries just the one file.

\begin{code}
readIface :: Module -> String -> IsBootInterface -> TcRn m (Either Exception ParsedIface)
	-- Nothing <=> file not found, or unreadable, or illegible
	-- Just x  <=> successfully found and parsed 

readIface mod file_path is_hi_boot_file
  = do dflags <- getDOpts
       ioToTcRn (tryMost (read_iface mod dflags file_path is_hi_boot_file))

read_iface mod dflags file_path is_hi_boot_file
 | is_hi_boot_file		-- Read ascii
 = do { buffer <- hGetStringBuffer file_path ;
        case unP parseIface (mkPState buffer loc dflags) of
	  POk _ iface | wanted_mod_name == actual_mod_name
		      -> return iface
		      | otherwise
		      -> throwDyn (ProgramError (showSDoc err)) 
				-- 'showSDoc' is a bit yukky
		where
		  wanted_mod_name = moduleName mod
		  actual_mod_name = pi_mod iface
		  err = hiModuleNameMismatchWarn wanted_mod_name actual_mod_name

	  PFailed loc1 loc2  err -> 
		throwDyn (ProgramError (showPFailed loc1 loc2 err))
     }

 | otherwise		-- Read binary
 = readBinIface file_path

 where
    loc  = mkSrcLoc (mkFastString file_path) 1 0
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
      pi_pkg     = basePackage,
      pi_deps    = noDependencies,
      pi_vers    = 1,
      pi_orphan  = False,
      pi_usages  = [],
      pi_exports = (1, [(gHC_PRIM_Name, ghcPrimExports)]),
      pi_decls   = [],
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
