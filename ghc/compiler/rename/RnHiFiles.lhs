%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Dealing with interface files}

\begin{code}
module RnHiFiles (
	readIface, findAndReadIface, loadInterface, loadHomeInterface, 
	tryLoadInterface, loadOrphanModules,
	loadExports, loadFixDecls, loadDeprecs,

	lookupFixityRn, 

	getTyClDeclBinders
   ) where

#include "HsVersions.h"

import DriverState	( GhcMode(..), v_GhcMode )
import DriverUtil	( splitFilename )
import CmdLineOpts	( opt_IgnoreIfacePragmas )
import HscTypes		( ModuleLocation(..),
			  ModIface(..), emptyModIface,
			  VersionInfo(..), ImportedModuleInfo,
			  lookupIfaceByModName, 
			  ImportVersion, WhetherHasOrphans, IsBootInterface,
			  DeclsMap, GatedDecl, IfaceInsts, IfaceRules,
			  AvailInfo, GenAvailInfo(..), Avails, Deprecations(..)
			 )
import HsSyn		( TyClDecl(..), InstDecl(..),
			  HsType(..), HsPred(..), FixitySig(..), RuleDecl(..),
			  tyClDeclNames, tyClDeclSysNames, hsTyVarNames, getHsInstHead,
			)
import RdrHsSyn		( RdrNameTyClDecl, RdrNameInstDecl, RdrNameRuleDecl )
import RnHsSyn		( extractHsTyNames_s )
import BasicTypes	( Version, defaultFixity )
import RnTypes		( rnHsType )
import RnEnv
import RnMonad
import ParseIface	( parseIface )

import Name		( Name {-instance NamedThing-}, 
			  nameModule, isLocalName, nameIsLocalOrFrom
			 )
import NameEnv
import NameSet
import Module
import RdrName		( rdrNameOcc )
import SrcLoc		( mkSrcLoc )
import Maybes		( maybeToBool, orElse )
import StringBuffer     ( hGetStringBuffer )
import FastString	( mkFastString )
import ErrUtils         ( Message )
import Finder		( findModule, findPackageModule )
import Lex
import FiniteMap
import ListSetOps	( minusList )
import Outputable
import Bag
import Config

import IOExts
import Directory
\end{code}


%*********************************************************
%*							*
\subsection{Loading a new interface file}
%*							*
%*********************************************************

\begin{code}
loadHomeInterface :: SDoc -> Name -> RnM d ModIface
loadHomeInterface doc_str name
  = ASSERT2( not (isLocalName name), ppr name <+> parens doc_str )
    loadInterface doc_str (moduleName (nameModule name)) ImportBySystem

loadOrphanModules :: [ModuleName] -> RnM d ()
loadOrphanModules mods
  | null mods = returnRn ()
  | otherwise = traceRn (text "Loading orphan modules:" <+> 
			 fsep (map ppr mods))			`thenRn_` 
		mapRn_ load mods				`thenRn_`
		returnRn ()
  where
    load mod   = loadInterface (mk_doc mod) mod ImportBySystem
    mk_doc mod = ppr mod <+> ptext SLIT("is a orphan-instance module")

loadInterface :: SDoc -> ModuleName -> WhereFrom -> RnM d ModIface
loadInterface doc mod from 
  = tryLoadInterface doc mod from	`thenRn` \ (ifaces, maybe_err) ->
    case maybe_err of
	Nothing  -> returnRn ifaces
	Just err -> failWithRn ifaces (elaborate err)
  where
    elaborate err = hang (ptext SLIT("failed to load interface for") <+> quotes (ppr mod) <> colon)
			 4 err

tryLoadInterface :: SDoc -> ModuleName -> WhereFrom -> RnM d (ModIface, Maybe Message)
  -- Returns (Just err) if an error happened
  -- It *doesn't* add an error to the monad, because sometimes it's ok to fail...
  -- Specifically, when we read the usage information from an interface file,
  -- we try to read the interfaces it mentions.  But it's OK to fail; perhaps
  -- the module has changed, and that interface is no longer used.
  
  -- tryLoadInterface guarantees to return with iImpModInfo m --> (..., True)
  -- (If the load fails, we plug in a vanilla placeholder)
tryLoadInterface doc_str mod_name from
 = getHomeIfaceTableRn		`thenRn` \ hit ->
   getModuleRn			`thenRn` \ this_mod ->
   getIfacesRn 			`thenRn` \ ifaces@(Ifaces { iPIT = pit }) ->

	-- CHECK WHETHER WE HAVE IT ALREADY
   case lookupIfaceByModName hit pit mod_name of {
	Just iface |  case from of
			ImportByUser	   -> not (mi_boot iface)
			ImportByUserSource -> mi_boot iface
			ImportBySystem 	   -> True
		   -> returnRn (iface, Nothing) ;	-- Already loaded
			-- The not (mi_boot iface) test checks that the already-loaded
			-- interface isn't a boot iface.  This can conceivably happen,
			-- if the version checking happened to load a boot interface
			-- before we got to real imports.  
	other	    -> 

   let
	mod_map  = iImpModInfo ifaces
	mod_info = lookupFM mod_map mod_name

	hi_boot_file 
	  = case (from, mod_info) of
		(ImportByUser,       _)    	    -> False 	-- Not hi-boot
		(ImportByUserSource, _)		    -> True 	-- hi-boot
		(ImportBySystem, Just (_, is_boot)) -> is_boot
		(ImportBySystem, Nothing)	    -> False
			-- We're importing a module we know absolutely
			-- nothing about, so we assume it's from
			-- another package, where we aren't doing 
			-- dependency tracking. So it won't be a hi-boot file.

	redundant_source_import 
	  = case (from, mod_info) of 
		(ImportByUserSource, Just (_,False)) -> True
		other				     -> False
   in

	-- Issue a warning for a redundant {- SOURCE -} import
	-- NB that we arrange to read all the ordinary imports before 
	-- any of the {- SOURCE -} imports
   warnCheckRn	(not redundant_source_import)
		(warnRedundantSourceImport mod_name)	`thenRn_`

	-- Check that we aren't importing ourselves. 
	-- That only happens in Rename.checkOldIface, 
	-- which doesn't call tryLoadInterface
   warnCheckRn	
	(not (isHomeModule this_mod) || moduleName this_mod /= mod_name)
	(warnSelfImport this_mod)		`thenRn_`

	-- READ THE MODULE IN
   findAndReadIface doc_str mod_name hi_boot_file
					    `thenRn` \ read_result ->
   case read_result of {
	Left err -> 	-- Not found, so add an empty export env to the Ifaces map
			-- so that we don't look again
	   let
		fake_mod    = mkVanillaModule mod_name
		fake_iface  = emptyModIface fake_mod
		new_ifaces  = ifaces { iPIT = extendModuleEnv pit fake_mod fake_iface }
	   in
	   setIfacesRn new_ifaces		`thenRn_`
	   returnRn (fake_iface, Just err) ;

	-- Found and parsed!
	Right (mod, iface) ->

	-- LOAD IT INTO Ifaces

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

    loadDecls mod		(iDecls ifaces)	  (pi_decls iface)	`thenRn` \ (decls_vers, new_decls) ->
    loadRules mod		(iRules ifaces)   (pi_rules iface)	`thenRn` \ (rule_vers, new_rules) ->
    loadInstDecls mod		(iInsts ifaces)   (pi_insts iface)	`thenRn` \ new_insts ->
    loadExports 			 	  (pi_exports iface)	`thenRn` \ (export_vers, avails) ->
    loadFixDecls mod		 		  (pi_fixity iface)	`thenRn` \ fix_env ->
    loadDeprecs mod				  (pi_deprecs iface)	`thenRn` \ deprec_env ->
    let
	version	= VersionInfo { vers_module  = pi_vers iface, 
				vers_exports = export_vers,
				vers_rules = rule_vers,
				vers_decls = decls_vers }

	-- For an explicit user import, add to mod_map info about
	-- the things the imported module depends on, extracted
	-- from its usage info; and delete the module itself, which is now in the PIT
	mod_map1 = case from of
			ImportByUser -> addModDeps mod is_loaded (pi_usages iface) mod_map
			other        -> mod_map
	mod_map2 = delFromFM mod_map1 mod_name

 	this_mod_name = moduleName this_mod
	is_loaded m   =  m == this_mod_name 
		      || maybeToBool (lookupIfaceByModName hit pit m)
		-- We treat the currently-being-compiled module as 'loaded' because
		-- even though it isn't yet in the HIT or PIT; otherwise it gets
		-- put into iImpModInfo, and then spat out into its own interface
		-- file as a dependency

	-- Now add info about this module to the PIT
	has_orphans = pi_orphan iface
	new_pit   = extendModuleEnv pit mod mod_iface
 	mod_iface = ModIface { mi_module = mod, mi_version = version,
			       mi_orphan = has_orphans, mi_boot = hi_boot_file,
			       mi_exports = avails, 
			       mi_fixities = fix_env, mi_deprecs = deprec_env,
			       mi_usages  = [],	-- Will be filled in later
			       mi_decls   = panic "No mi_decls in PIT",
			       mi_globals = mkIfaceGlobalRdrEnv avails
		    }

	new_ifaces = ifaces { iPIT	  = new_pit,
			      iDecls      = new_decls,
			      iInsts      = new_insts,
			      iRules	  = new_rules,
			      iImpModInfo = mod_map2  }
    in
    setIfacesRn new_ifaces		`thenRn_`
    returnRn (mod_iface, Nothing)
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

loadExports :: (Version, [ExportItem]) -> RnM d (Version, [(ModuleName,Avails)])
loadExports (vers, items)
  = mapRn loadExport items	`thenRn` \ avails_s ->
    returnRn (vers, avails_s)


loadExport :: ExportItem -> RnM d (ModuleName, Avails)
loadExport (mod, entities)
  = mapRn (load_entity mod) entities	`thenRn` \ avails ->
    returnRn (mod, avails)
  where
    load_entity mod (Avail occ)
      =	newGlobalName mod occ	`thenRn` \ name ->
	returnRn (Avail name)
    load_entity mod (AvailTC occ occs)
      =	newGlobalName mod occ	      	`thenRn` \ name ->
        mapRn (newGlobalName mod) occs	`thenRn` \ names ->
        returnRn (AvailTC name names)


-----------------------------------------------------
--	Loading type/class/value decls
-----------------------------------------------------

loadDecls :: Module 
	  -> DeclsMap
	  -> [(Version, RdrNameTyClDecl)]
	  -> RnM d (NameEnv Version, DeclsMap)
loadDecls mod (decls_map, n_slurped) decls
  = foldlRn (loadDecl mod) (emptyNameEnv, decls_map) decls	`thenRn` \ (vers, decls_map') -> 
    returnRn (vers, (decls_map', n_slurped))

loadDecl mod (version_map, decls_map) (version, decl)
  = getTyClDeclBinders mod decl	`thenRn` \ (avail, sys_names) ->
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
    traceRn (text "Loading" <+> ppr full_avail) `thenRn_`
    returnRn (new_version_map, new_decls_map)

-----------------------------------------------------
--	Loading fixity decls
-----------------------------------------------------

loadFixDecls mod decls
  = mapRn (loadFixDecl mod_name) decls	`thenRn` \ to_add ->
    returnRn (mkNameEnv to_add)
  where
    mod_name = moduleName mod

loadFixDecl mod_name sig@(FixitySig rdr_name fixity loc)
  = newGlobalName mod_name (rdrNameOcc rdr_name) 	`thenRn` \ name ->
    returnRn (name, fixity)


-----------------------------------------------------
--	Loading instance decls
-----------------------------------------------------

loadInstDecls :: Module
	      -> IfaceInsts
	      -> [RdrNameInstDecl]
	      -> RnM d IfaceInsts
loadInstDecls mod (insts, n_slurped) decls
  = setModuleRn mod $
    foldlRn (loadInstDecl mod) insts decls 	`thenRn` \ insts' ->
    returnRn (insts', n_slurped)


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
    initIfaceRnMS mod (
	rnHsType (text "In an interface instance decl") inst_ty
    )					`thenRn` \ inst_ty' ->
    let 
	(tvs,(cls,tys)) = getHsInstHead inst_ty'
	free_tcs  = nameSetToList (extractHsTyNames_s tys) `minusList` hsTyVarNames tvs

	gate_fn vis_fn = vis_fn cls && (null free_tcs || any vis_fn free_tcs)
	-- Here is the implementation of HOWEVER above
	-- (Note that we do let the inst decl in if it mentions 
	--  no tycons at all.  Hence the null free_ty_names.)
    in
    returnRn ((gate_fn, (mod, decl)) `consBag` insts)



-----------------------------------------------------
--	Loading Rules
-----------------------------------------------------

loadRules :: Module -> IfaceRules 
	  -> (Version, [RdrNameRuleDecl])
	  -> RnM d (Version, IfaceRules)
loadRules mod (rule_bag, n_slurped) (version, rules)
  | null rules || opt_IgnoreIfacePragmas 
  = returnRn (version, (rule_bag, n_slurped))
  | otherwise
  = setModuleRn mod		 	$
    mapRn (loadRule mod) rules		`thenRn` \ new_rules ->
    returnRn (version, (rule_bag `unionBags` listToBag new_rules, n_slurped))

loadRule :: Module -> RdrNameRuleDecl -> RnM d (GatedDecl RdrNameRuleDecl)
-- "Gate" the rule simply by whether the rule variable is
-- needed.  We can refine this later.
loadRule mod decl@(IfaceRule _ _ _ var _ _ src_loc)
  = lookupIfaceName var		`thenRn` \ var_name ->
    returnRn (\vis_fn -> vis_fn var_name, (mod, decl))


-----------------------------------------------------
--	Loading Deprecations
-----------------------------------------------------

loadDeprecs :: Module -> IfaceDeprecs -> RnM d Deprecations
loadDeprecs m Nothing				       = returnRn NoDeprecs
loadDeprecs m (Just (Left txt))  = returnRn (DeprecAll txt)
loadDeprecs m (Just (Right prs)) = setModuleRn m 				$
    				   foldlRn loadDeprec emptyNameEnv prs	`thenRn` \ env ->
				   returnRn (DeprecSome env)
loadDeprec deprec_env (n, txt)
  = lookupIfaceName n 		`thenRn` \ name ->
    traceRn (text "Loaded deprecation(s) for" <+> ppr name <> colon <+> ppr txt) `thenRn_`
    returnRn (extendNameEnv deprec_env name (name,txt))
\end{code}


%*********************************************************
%*							*
\subsection{Getting binders out of a declaration}
%*							*
%*********************************************************

@getDeclBinders@ returns the names for a @RdrNameHsDecl@.
It's used for both source code (from @availsFromDecl@) and interface files
(from @loadDecl@).

It doesn't deal with source-code specific things: @ValD@, @DefD@.  They
are handled by the sourc-code specific stuff in @RnNames@.

	*** See "THE NAMING STORY" in HsDecls ****


\begin{code}
getTyClDeclBinders
	:: Module
	-> RdrNameTyClDecl
	-> RnM d (AvailInfo, [Name])	-- The [Name] are the system names

-----------------
getTyClDeclBinders mod (IfaceSig {tcdName = var, tcdLoc = src_loc})
  = newTopBinder mod var src_loc			`thenRn` \ var_name ->
    returnRn (Avail var_name, [])

getTyClDeclBinders mod tycl_decl
  = new_top_bndrs mod (tyClDeclNames tycl_decl)		`thenRn` \ names@(main_name:_) ->
    new_top_bndrs mod (tyClDeclSysNames tycl_decl)	`thenRn` \ sys_names ->
    returnRn (AvailTC main_name names, sys_names)

-----------------
new_top_bndrs mod names_w_locs
  = sequenceRn [newTopBinder mod name loc | (name,loc) <- names_w_locs]
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
		 -> RnM d (Either Message (Module, ParsedIface))
	-- Nothing <=> file not found, or unreadable, or illegible
	-- Just x  <=> successfully found and parsed 

findAndReadIface doc_str mod_name hi_boot_file
  = traceRn trace_msg			`thenRn_`

    -- In interactive or --make mode, we are *not allowed* to demand-load
    -- a home package .hi file.  So don't even look for them.
    -- This helps in the case where you are sitting in eg. ghc/lib/std
    -- and start up GHCi - it won't complain that all the modules it tries
    -- to load are found in the home location.
    ioToRnM_no_fail (readIORef v_GhcMode) `thenRn` \ mode ->
    let home_allowed = hi_boot_file ||
		       mode `notElem` [ DoInteractive, DoMake ]
    in

    ioToRnM (if home_allowed 
		then findModule mod_name
		else findPackageModule mod_name) `thenRn` \ maybe_found ->

    case maybe_found of

      Right (Just (wanted_mod,locn))
        -> mkHiPath hi_boot_file locn `thenRn` \ file -> 
	   readIface file `thenRn` \ read_result ->
	   case read_result of
                Left bad -> returnRn (Left bad)
                Right iface 
                   -> let read_mod = pi_mod iface
		      in -- check that the module names agree
			 checkRn
			   (wanted_mod == read_mod)
		   	   (hiModuleNameMismatchWarn wanted_mod read_mod)
					`thenRn_`
			 -- check that the package names agree
			 warnCheckRn 
			   (modulePackage wanted_mod == modulePackage read_mod)
		   	   (packageNameMismatchWarn wanted_mod read_mod)
					 `thenRn_`
		         returnRn (Right (wanted_mod, iface))
	-- Can't find it
      other   -> traceRn (ptext SLIT("...not found"))	`thenRn_`
		 returnRn (Left (noIfaceErr mod_name hi_boot_file))

  where
    trace_msg = sep [hsep [ptext SLIT("Reading"), 
			   if hi_boot_file then ptext SLIT("[boot]") else empty,
			   ptext SLIT("interface for"), 
			   ppr mod_name <> semi],
		     nest 4 (ptext SLIT("reason:") <+> doc_str)]

mkHiPath hi_boot_file locn
  | hi_boot_file = 
	ioToRnM_no_fail (doesFileExist hi_boot_ver_path) `thenRn` \ b ->
	if b then returnRn hi_boot_ver_path
	     else returnRn hi_boot_path
  | otherwise    = returnRn hi_path
	where hi_path            = ml_hi_file locn
	      (hi_base, _hi_suf) = splitFilename hi_path
    	      hi_boot_path       = hi_base ++ ".hi-boot"
    	      hi_boot_ver_path   = hi_base ++ ".hi-boot-" ++ cHscIfaceFileVersion
\end{code}

@readIface@ tries just the one file.

\begin{code}
readIface :: String -> RnM d (Either Message ParsedIface)
	-- Nothing <=> file not found, or unreadable, or illegible
	-- Just x  <=> successfully found and parsed 
readIface file_path
  = --ioToRnM (putStrLn ("reading iface " ++ file_path)) `thenRn_`
    traceRn (ptext SLIT("readIFace") <+> text file_path) 	`thenRn_` 

    ioToRnM (hGetStringBuffer False file_path)			`thenRn` \ read_result ->
    case read_result of {
	Left io_error  -> bale_out (text (show io_error)) ;
	Right contents -> 

    case parseIface contents init_parser_state of
	POk _ iface          -> returnRn (Right iface)
	PFailed err 	     -> bale_out err
    }
  where
    init_parser_state = PState{ bol = 0#, atbol = 1#,
				context = [],
				glasgow_exts = 1#,
				loc = mkSrcLoc (mkFastString file_path) 1 }

    bale_out err = returnRn (Left (badIfaceFile file_path err))
\end{code}

%*********************************************************
%*							*
\subsection{Looking up fixities}
%*							*
%*********************************************************

@lookupFixityRn@ has to be in RnIfaces (or RnHiFiles), instead of
its obvious home in RnEnv,  because it calls @loadHomeInterface@.

lookupFixity is a bit strange.  

* Nested local fixity decls are put in the local fixity env, which we
  find with getFixtyEnv

* Imported fixities are found in the HIT or PIT

* Top-level fixity decls in this module may be for Names that are
    either  Global	   (constructors, class operations)
    or 	    Local/Exported (everything else)
  (See notes with RnNames.getLocalDeclBinders for why we have this split.)
  We put them all in the local fixity environment

\begin{code}
lookupFixityRn :: Name -> RnMS Fixity
lookupFixityRn name
  = getModuleRn				`thenRn` \ this_mod ->
    if nameIsLocalOrFrom this_mod name
    then	-- It's defined in this module
	getFixityEnv			`thenRn` \ local_fix_env ->
	returnRn (lookupLocalFixity local_fix_env name)

    else	-- It's imported
      -- For imported names, we have to get their fixities by doing a
      -- loadHomeInterface, and consulting the Ifaces that comes back
      -- from that, because the interface file for the Name might not
      -- have been loaded yet.  Why not?  Suppose you import module A,
      -- which exports a function 'f', thus;
      --        module CurrentModule where
      --	  import A( f )
      -- 	module A( f ) where
      --	  import B( f )
      -- Then B isn't loaded right away (after all, it's possible that
      -- nothing from B will be used).  When we come across a use of
      -- 'f', we need to know its fixity, and it's then, and only
      -- then, that we load B.hi.  That is what's happening here.
  	loadHomeInterface doc name		`thenRn` \ iface ->
	returnRn (lookupNameEnv (mi_fixities iface) name `orElse` defaultFixity)
  where
    doc      = ptext SLIT("Checking fixity for") <+> ppr name
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

hiModuleNameMismatchWarn :: Module -> Module  -> Message
hiModuleNameMismatchWarn requested_mod read_mod = 
    hsep [ ptext SLIT("Something is amiss; requested module name")
	 , ppr (moduleName requested_mod)
	 , ptext SLIT("differs from name found in the interface file")
   	 , ppr read_mod
  	 ]

packageNameMismatchWarn :: Module -> Module  -> Message
packageNameMismatchWarn requested_mod read_mod = 
    fsep [ ptext SLIT("Module"), quotes (ppr requested_mod), 
	  ptext SLIT("is located in package"), 
	  quotes (ptext (modulePackage requested_mod)),
	  ptext SLIT("but its interface file claims it is part of package"),
	  quotes (ptext (modulePackage read_mod))
	]

warnRedundantSourceImport mod_name
  = ptext SLIT("Unnecessary {- SOURCE -} in the import of module")
          <+> quotes (ppr mod_name)

warnSelfImport mod
  = ptext SLIT("Importing my own interface: module") <+> ppr mod
\end{code}
