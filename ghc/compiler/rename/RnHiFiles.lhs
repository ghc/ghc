%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Dealing with interface files}

\begin{code}
module RnHiFiles (
	findAndReadIface, loadInterface, loadHomeInterface, 
	tryLoadInterface, loadOrphanModules,
	loadExports, loadFixDecls, loadDeprecs,

	lookupFixityRn, 

	getTyClDeclBinders, 
	removeContext	 	-- removeContext probably belongs somewhere else
   ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_IgnoreIfacePragmas )
import HscTypes
import HsSyn		( HsDecl(..), TyClDecl(..), InstDecl(..),
			  HsType(..), ConDecl(..), 
			  FixitySig(..), RuleDecl(..),
			  tyClDeclNames
			)
import RdrHsSyn		( RdrNameTyClDecl, RdrNameInstDecl, RdrNameRuleDecl,
			  extractHsTyRdrNames 
			)
import BasicTypes	( Version )
import RnEnv
import RnMonad
import ParseIface	( parseIface, IfaceStuff(..) )

import Name		( Name {-instance NamedThing-}, nameOccName,
			  nameModule,
			  NamedThing(..),
			  mkNameEnv, extendNameEnv
			 )
import Module		( Module,
			  moduleName, isModuleInThisPackage,
			  ModuleName, WhereFrom(..),
			  extendModuleEnv, lookupModuleEnvByName,
			)
import RdrName		( RdrName, rdrNameOcc )
import NameSet
import SrcLoc		( mkSrcLoc, SrcLoc )
import Maybes		( maybeToBool )
import StringBuffer     ( hGetStringBuffer )
import FastString	( mkFastString )
import ErrUtils         ( Message )
import Lex
import FiniteMap
import Outputable
import Bag
\end{code}


%*********************************************************
%*							*
\subsection{Loading a new interface file}
%*							*
%*********************************************************

\begin{code}
loadHomeInterface :: SDoc -> Name -> RnM d Ifaces
loadHomeInterface doc_str name
  = loadInterface doc_str (moduleName (nameModule name)) ImportBySystem

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

loadInterface :: SDoc -> ModuleName -> WhereFrom -> RnM d Ifaces
loadInterface doc mod from 
  = tryLoadInterface doc mod from	`thenRn` \ (ifaces, maybe_err) ->
    case maybe_err of
	Nothing  -> returnRn ifaces
	Just err -> failWithRn ifaces err

tryLoadInterface :: SDoc -> ModuleName -> WhereFrom -> RnM d (Ifaces, Maybe Message)
	-- Returns (Just err) if an error happened
	-- Guarantees to return with iImpModInfo m --> (..., True)
	-- (If the load fails, we plug in a vanilla placeholder)
tryLoadInterface doc_str mod_name from
 = getHomeIfaceTableRn		`thenRn` \ hit ->
   getIfacesRn 			`thenRn` \ ifaces ->
	
	-- Check whether we have it already in the home package
   case lookupModuleEnvByName hit mod_name of {
	Just _  -> returnRn (ifaces, Nothing) ;	-- In the home package
	Nothing -> 

   let
	mod_map  = iImpModInfo ifaces
	mod_info = lookupFM mod_map mod_name

	hi_boot_file 
	  = case (from, mod_info) of
		(ImportByUser,       _)    	       -> False 	-- Not hi-boot
		(ImportByUserSource, _)		       -> True 		-- hi-boot
		(ImportBySystem, Just (_, is_boot, _)) -> is_boot 	-- 
		(ImportBySystem, Nothing)	       -> False
			-- We're importing a module we know absolutely
			-- nothing about, so we assume it's from
			-- another package, where we aren't doing 
			-- dependency tracking. So it won't be a hi-boot file.

	redundant_source_import 
	  = case (from, mod_info) of 
		(ImportByUserSource, Just (_,False,_)) -> True
		other				       -> False
   in
	-- CHECK WHETHER WE HAVE IT ALREADY
   case mod_info of {
	Just (_, _, True)
		-> 	-- We're read it already so don't re-read it
		    returnRn (ifaces, Nothing) ;

	_ ->

	-- Issue a warning for a redundant {- SOURCE -} import
	-- NB that we arrange to read all the ordinary imports before 
	-- any of the {- SOURCE -} imports
   warnCheckRn	(not redundant_source_import)
		(warnRedundantSourceImport mod_name)	`thenRn_`

	-- READ THE MODULE IN
   findAndReadIface doc_str mod_name hi_boot_file   `thenRn` \ read_result ->
   case read_result of {
	Left err -> 	-- Not found, so add an empty export env to the Ifaces map
			-- so that we don't look again
	   let
		new_mod_map = addToFM mod_map mod_name (False, False, True)
		new_ifaces  = ifaces { iImpModInfo = new_mod_map }
	   in
	   setIfacesRn new_ifaces		`thenRn_`
	   returnRn (new_ifaces, Just err) ;

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
	  isModuleInThisPackage mod,
	  ppr mod )

    loadDecls mod		(iDecls ifaces)	  (pi_decls iface)	`thenRn` \ (decls_vers, new_decls) ->
    loadRules mod		(iRules ifaces)   (pi_rules iface)	`thenRn` \ (rule_vers, new_rules) ->
    foldlRn (loadInstDecl mod)	(iInsts ifaces)   (pi_insts iface)	`thenRn` \ new_insts ->
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
	-- from its usage info.
	mod_map1 = case from of
			ImportByUser -> addModDeps mod (pi_usages iface) mod_map
			other        -> mod_map
	mod_map2 = addToFM mod_map1 mod_name (has_orphans, hi_boot_file, True)

	-- Now add info about this module to the PIT
	has_orphans = pi_orphan iface
	new_pit   = extendModuleEnv (iPIT ifaces) mod mod_iface
 	mod_iface = ModIface { mi_module = mod, mi_version = version,
			       mi_exports = avails, mi_orphan = has_orphans,
			       mi_fixities = fix_env, mi_deprecs = deprec_env,
			       mi_usages  = [],	-- Will be filled in later
			       mi_decls   = panic "No mi_decls in PIT",
			       mi_globals = panic "No mi_globals in PIT"
		    }

	new_ifaces = ifaces { iPIT	  = new_pit,
			      iDecls      = new_decls,
			      iInsts      = new_insts,
			      iRules	  = new_rules,
			      iImpModInfo = mod_map2  }
    in
    setIfacesRn new_ifaces		`thenRn_`
    returnRn (new_ifaces, Nothing)
    }}}

-----------------------------------------------------
--	Adding module dependencies from the 
--	import decls in the interface file
-----------------------------------------------------

addModDeps :: Module -> [ImportVersion a] 
	   -> ImportedModuleInfo -> ImportedModuleInfo
-- (addModDeps M ivs deps)
-- We are importing module M, and M.hi contains 'import' decls given by ivs
addModDeps mod new_deps mod_deps
  = foldr add mod_deps filtered_new_deps
  where
	-- Don't record dependencies when importing a module from another package
	-- Except for its descendents which contain orphans,
	-- and in that case, forget about the boot indicator
    filtered_new_deps :: [(ModuleName, (WhetherHasOrphans, IsBootInterface, IsLoaded))]
    filtered_new_deps
	| isModuleInThisPackage mod 
			    = [ (imp_mod, (has_orphans, is_boot, False))
			      | (imp_mod, has_orphans, is_boot, _) <- new_deps 
			      ]			      
	| otherwise	    = [ (imp_mod, (True, False, False))
			      | (imp_mod, has_orphans, _, _) <- new_deps, 
				has_orphans
			      ]
    add (imp_mod, dep) deps = addToFM_C combine deps imp_mod dep

    combine old@(_, old_is_boot, old_is_loaded) new
	| old_is_loaded || not old_is_boot = old	-- Keep the old info if it's already loaded
							-- or if it's a non-boot pending load
	| otherwise			    = new	-- Otherwise pick new info


-----------------------------------------------------
--	Loading the export list
-----------------------------------------------------

loadExports :: (Version, [ExportItem]) -> RnM d (Version, Avails)
loadExports (vers, items)
  = getModuleRn 				`thenRn` \ this_mod ->
    mapRn (loadExport this_mod) items		`thenRn` \ avails_s ->
    returnRn (vers, concat avails_s)


loadExport :: Module -> ExportItem -> RnM d [AvailInfo]
loadExport this_mod (mod, entities)
  | mod == moduleName this_mod = returnRn []
	-- If the module exports anything defined in this module, just ignore it.
	-- Reason: otherwise it looks as if there are two local definition sites
	-- for the thing, and an error gets reported.  Easiest thing is just to
	-- filter them out up front. This situation only arises if a module
	-- imports itself, or another module that imported it.  (Necessarily,
	-- this invoves a loop.)  Consequence: if you say
	--	module A where
	--	   import B( AType )
	--	   type AType = ...
	--
	--	module B( AType ) where
	--	   import {-# SOURCE #-} A( AType )
	--
	-- then you'll get a 'B does not export AType' message.  A bit bogus
	-- but it's a bogus thing to do!

  | otherwise
  = mapRn (load_entity mod) entities
  where
    new_name mod occ = newGlobalName mod occ

    load_entity mod (Avail occ)
      =	new_name mod occ	`thenRn` \ name ->
	returnRn (Avail name)
    load_entity mod (AvailTC occ occs)
      =	new_name mod occ	      `thenRn` \ name ->
        mapRn (new_name mod) occs     `thenRn` \ names ->
        returnRn (AvailTC name names)


-----------------------------------------------------
--	Loading type/class/value decls
-----------------------------------------------------

loadDecls :: Module 
	  -> DeclsMap
	  -> [(Version, RdrNameTyClDecl)]
	  -> RnM d (NameEnv Version, DeclsMap)
loadDecls mod decls_map decls
  = foldlRn (loadDecl mod) (emptyNameEnv, decls_map) decls

loadDecl :: Module 
	 -> (NameEnv Version, DeclsMap)
	 -> (Version, RdrNameTyClDecl)
	 -> RnM d (NameEnv Version, DeclsMap)
loadDecl mod (version_map, decls_map) (version, decl)
  = getIfaceDeclBinders new_name decl	`thenRn` \ full_avail ->
    let
	main_name     = availName full_avail
	new_decls_map = extendNameEnvList decls_map stuff
	stuff  	      = [ (name, (full_avail, name==main_name, (mod, decl))) 
			| name <- availNames full_avail]

	new_version_map = extendNameEnv version_map main_name version
    in
    returnRn (new_version_map, new_decls_map)
  where
	-- newTopBinder puts into the cache the binder with the
	-- module information set correctly.  When the decl is later renamed,
	-- the binding site will thereby get the correct module.
	-- There maybe occurrences that don't have the correct Module, but
	-- by the typechecker will propagate the binding definition to all 
	-- the occurrences, so that doesn't matter
    new_name rdr_name loc = newTopBinder mod rdr_name loc


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

loadInstDecl :: Module
	     -> IfaceInsts
	     -> RdrNameInstDecl
	     -> RnM d IfaceInsts
loadInstDecl mod insts decl@(InstDecl inst_ty binds uprags dfun_name src_loc)
  = 
	-- Find out what type constructors and classes are "gates" for the
	-- instance declaration.  If all these "gates" are slurped in then
	-- we should slurp the instance decl too.
	-- 
	-- We *don't* want to count names in the context part as gates, though.
	-- For example:
	--		instance Foo a => Baz (T a) where ...
	--
	-- Here the gates are Baz and T, but *not* Foo.
    let 
	munged_inst_ty = removeContext inst_ty
	free_names     = extractHsTyRdrNames munged_inst_ty
    in
    setModuleRn mod $
    mapRn lookupOrigName free_names	`thenRn` \ gate_names ->
    returnRn ((mkNameSet gate_names, (mod, InstD decl)) `consBag` insts)


-- In interface files, the instance decls now look like
--	forall a. Foo a -> Baz (T a)
-- so we have to strip off function argument types as well
-- as the bit before the '=>' (which is always empty in interface files)
removeContext (HsForAllTy tvs cxt ty) = HsForAllTy tvs [] (removeFuns ty)
removeContext ty		      = removeFuns ty

removeFuns (HsFunTy _ ty) = removeFuns ty
removeFuns ty		    = ty


-----------------------------------------------------
--	Loading Rules
-----------------------------------------------------

loadRules :: Module -> IfaceRules 
	  -> (Version, [RdrNameRuleDecl])
	  -> RnM d (Version, IfaceRules)
loadRules mod rule_bag (version, rules)
  | null rules || opt_IgnoreIfacePragmas 
  = returnRn (version, rule_bag)
  | otherwise
  = setModuleRn mod		 	$
    mapRn (loadRule mod) rules		`thenRn` \ new_rules ->
    returnRn (version, rule_bag `unionBags` listToBag new_rules)

loadRule :: Module -> RdrNameRuleDecl -> RnM d GatedDecl
-- "Gate" the rule simply by whether the rule variable is
-- needed.  We can refine this later.
loadRule mod decl@(IfaceRule _ _ var _ _ src_loc)
  = lookupOrigName var		`thenRn` \ var_name ->
    returnRn (unitNameSet var_name, (mod, RuleD decl))


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
  = lookupOrigName n 		`thenRn` \ name ->
    traceRn (text "Loaded deprecation(s) for" <+> ppr name <> colon <+> ppr txt) `thenRn_`
    returnRn (extendNameEnv deprec_env name txt)
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

\begin{code}
getIfaceDeclBinders, getTyClDeclBinders
	:: (RdrName -> SrcLoc -> RnM d Name)	-- New-name function
	-> RdrNameTyClDecl
	-> RnM d AvailInfo

getIfaceDeclBinders new_name tycl_decl
  = getTyClDeclBinders    new_name tycl_decl	`thenRn` \ avail ->
    getSysTyClDeclBinders new_name tycl_decl	`thenRn` \ extras ->
    returnRn (addSysAvails avail extras)
		-- Add the sys-binders to avail.  When we import the decl,
		-- it's full_avail that will get added to the 'already-slurped' set (iSlurp)
		-- If we miss out sys-binders, we'll read the decl multiple times!

getTyClDeclBinders new_name (IfaceSig var ty prags src_loc)
  = new_name var src_loc			`thenRn` \ var_name ->
    returnRn (Avail var_name)

getTyClDeclBinders new_name tycl_decl
  = mapRn do_one (tyClDeclNames tycl_decl)	`thenRn` \ (main_name:sub_names) ->
    returnRn (AvailTC main_name (main_name : sub_names))
  where
    do_one (name,loc) = new_name name loc
\end{code}

@getDeclSysBinders@ gets the implicit binders introduced by a decl.
A the moment that's just the tycon and datacon that come with a class decl.
They aren't returned by @getDeclBinders@ because they aren't in scope;
but they {\em should} be put into the @DeclsMap@ of this module.

Note that this excludes the default-method names of a class decl,
and the dict fun of an instance decl, because both of these have 
bindings of their own elsewhere.

\begin{code}
getSysTyClDeclBinders new_name (ClassDecl _ cname _ _ sigs _ names src_loc)
  = sequenceRn [new_name n src_loc | n <- names]

getSysTyClDeclBinders new_name (TyData _ _ _ _ cons _ _ _ _ _)
  = sequenceRn [new_name wkr_name src_loc | ConDecl _ wkr_name _ _ _ src_loc <- cons]

getSysTyClDeclBinders new_name other_decl
  = returnRn []
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

    getFinderRn				`thenRn` \ finder ->
    ioToRnM (finder mod_name)		`thenRn` \ maybe_found ->

    case maybe_found of
      Right (Just (mod,locn))
	| hi_boot_file -> readIface mod (hi_file locn ++ "-hi-boot")
	| otherwise    -> readIface mod (hi_file locn)
	
	-- Can't find it
      other   -> traceRn (ptext SLIT("...not found"))	`thenRn_`
		 returnRn (Left (noIfaceErr mod_name hi_boot_file))

  where
    trace_msg = sep [hsep [ptext SLIT("Reading"), 
			   if hi_boot_file then ptext SLIT("[boot]") else empty,
			   ptext SLIT("interface for"), 
			   ppr mod_name <> semi],
		     nest 4 (ptext SLIT("reason:") <+> doc_str)]
\end{code}

@readIface@ tries just the one file.

\begin{code}
readIface :: Module -> String -> RnM d (Either Message (Module, ParsedIface))
	-- Nothing <=> file not found, or unreadable, or illegible
	-- Just x  <=> successfully found and parsed 
readIface wanted_mod file_path
  = traceRn (ptext SLIT("...reading from") <+> text file_path)	`thenRn_`
    ioToRnM (hGetStringBuffer False file_path)      		 `thenRn` \ read_result ->
    case read_result of
	Right contents	  -> 
             case parseIface contents
			PState{ bol = 0#, atbol = 1#,
				context = [],
				glasgow_exts = 1#,
				loc = mkSrcLoc (mkFastString file_path) 1 } of
		  POk _  (PIface iface) ->
		      warnCheckRn (wanted_mod == read_mod)
		    		  (hiModuleNameMismatchWarn wanted_mod read_mod) `thenRn_`
		      returnRn (Right (wanted_mod, iface))
		    where
		      read_mod = pi_mod iface

		  PFailed err   -> bale_out err
	          parse_result 	-> bale_out empty
		 	-- This last case can happen if the interface file is (say) empty
			-- in which case the parser thinks it looks like an IdInfo or
			-- something like that.  Just an artefact of the fact that the
			-- parser is used for several purposes at once.

        Left io_err -> bale_out (text (show io_err))
  where
    bale_out err = returnRn (Left (badIfaceFile file_path err))
\end{code}


%*********************************************************
%*							*
\subsection{Looking up fixities}
%*							*
%*********************************************************

This has to be in RnIfaces (or RnHiFiles) because it calls loadHomeInterface

\begin{code}
lookupFixityRn :: Name -> RnMS Fixity
lookupFixityRn name
  | isLocallyDefined name
  = getFixityEnv			`thenRn` \ local_fix_env ->
    returnRn (lookupLocalFixity local_fix_env name)

  | otherwise	-- Imported
      -- For imported names, we have to get their fixities by doing a loadHomeInterface,
      -- and consulting the Ifaces that comes back from that, because the interface
      -- file for the Name might not have been loaded yet.  Why not?  Suppose you import module A,
      -- which exports a function 'f', which is defined in module B.  Then B isn't loaded
      -- right away (after all, it's possible that nothing from B will be used).
      -- When we come across a use of 'f', we need to know its fixity, and it's then,
      -- and only then, that we load B.hi.  That is what's happening here.
  = getHomeIfaceTableRn 		`thenRn` \ hit ->
    loadHomeInterface doc name		`thenRn` \ ifaces ->
    case lookupTable hit (iPIT ifaces) name of
	Just iface -> returnRn (lookupNameEnv (mi_fixities iface) name `orElse` defaultFixity)
	Nothing	   -> returnRn defaultFixity
  where
    doc = ptext SLIT("Checking fixity for") <+> ppr name
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

warnRedundantSourceImport mod_name
  = ptext SLIT("Unnecessary {- SOURCE -} in the import of module")
          <+> quotes (ppr mod_name)
\end{code}

