%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnNames]{Extracting imported and top-level names in scope}

\begin{code}
module RnNames (
	rnImports, importsFromLocalDecls, exportsFromAvail,
	reportUnusedNames, mkModDeps
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} RnHiFiles	( loadInterface )

import CmdLineOpts	( DynFlag(..) )

import HsSyn		( IE(..), ieName, ImportDecl(..),
			  ForeignDecl(..), HsGroup(..),
			  collectLocatedHsBinders, tyClDeclNames 
			)
import RdrHsSyn		( RdrNameIE, RdrNameImportDecl, main_RDR_Unqual )
import RnEnv
import TcRnMonad

import FiniteMap
import PrelNames	( pRELUDE_Name, isBuiltInSyntaxName )
import Module		( Module, ModuleName, ModuleEnv, moduleName, 
			  moduleNameUserString, isHomeModule,
			  emptyModuleEnv, unitModuleEnvByName, unitModuleEnv, 
			  lookupModuleEnvByName, extendModuleEnvByName, moduleEnvElts )
import Name		( Name, nameSrcLoc, nameOccName, nameModule, isExternalName )
import NameSet
import NameEnv
import OccName		( OccName, srcDataName, isTcOcc )
import HscTypes		( Provenance(..), ImportReason(..), GlobalRdrEnv,
			  GenAvailInfo(..), AvailInfo, Avails, GhciMode(..),
			  IsBootInterface,
			  availName, availNames, availsToNameSet, 
			  Deprecations(..), ModIface(..), Dependencies(..),
		  	  GlobalRdrElt(..), unQualInScope, isLocalGRE, pprNameProvenance
			)
import RdrName		( RdrName, rdrNameOcc, setRdrNameSpace, lookupRdrEnv, rdrEnvToList,
			  emptyRdrEnv, foldRdrEnv, rdrEnvElts, mkRdrUnqual, isQual )
import Outputable
import Maybe		( isJust, isNothing, catMaybes )
import Maybes		( orElse )
import ListSetOps	( removeDups )
import Util		( sortLt, notNull )
import List		( partition, insert )
import IO		( openFile, IOMode(..) )
\end{code}



%************************************************************************
%*									*
		rnImports
%*									*
%************************************************************************

\begin{code}
rnImports :: [RdrNameImportDecl]
	  -> TcRn m (GlobalRdrEnv, ImportAvails)

rnImports imports
  = 		-- PROCESS IMPORT DECLS
		-- Do the non {- SOURCE -} ones first, so that we get a helpful
		-- warning for {- SOURCE -} ones that are unnecessary
	getModule				`thenM` \ this_mod ->
 	getSrcLocM				`thenM` \ loc ->
	doptM Opt_NoImplicitPrelude		`thenM` \ opt_no_prelude -> 
	let
	  all_imports	     = mk_prel_imports this_mod loc opt_no_prelude ++ imports
	  (source, ordinary) = partition is_source_import all_imports
	  is_source_import (ImportDecl _ is_boot _ _ _ _) = is_boot

	  get_imports = importsFromImportDecl this_mod
	in
	mappM get_imports ordinary	`thenM` \ stuff1 ->
	mappM get_imports source	`thenM` \ stuff2 ->

		-- COMBINE RESULTS
	let
	    (imp_gbl_envs, imp_avails) = unzip (stuff1 ++ stuff2)
	    gbl_env :: GlobalRdrEnv
	    gbl_env = foldr plusGlobalRdrEnv emptyRdrEnv imp_gbl_envs

	    all_avails :: ImportAvails
	    all_avails = foldr plusImportAvails emptyImportAvails imp_avails
	in
		-- ALL DONE
	returnM (gbl_env, all_avails)
  where
	-- NB: opt_NoImplicitPrelude is slightly different to import Prelude ();
	-- because the former doesn't even look at Prelude.hi for instance 
	-- declarations, whereas the latter does.
    mk_prel_imports this_mod loc no_prelude
	|  moduleName this_mod == pRELUDE_Name
	|| explicit_prelude_import
	|| no_prelude
	= []

	| otherwise = [preludeImportDecl loc]

    explicit_prelude_import
      = notNull [ () | (ImportDecl mod _ _ _ _ _) <- imports, 
		       mod == pRELUDE_Name ]

preludeImportDecl loc
  = ImportDecl pRELUDE_Name
	       False {- Not a boot interface -}
	       False	{- Not qualified -}
	       Nothing	{- No "as" -}
	       Nothing	{- No import list -}
	       loc
\end{code}
	
\begin{code}
importsFromImportDecl :: Module
		      -> RdrNameImportDecl
		      -> TcRn m (GlobalRdrEnv, ImportAvails)

importsFromImportDecl this_mod
	(ImportDecl imp_mod_name is_boot qual_only as_mod imp_spec iloc)
  = addSrcLoc iloc $
    let
	doc = ppr imp_mod_name <+> ptext SLIT("is directly imported")
    in

	-- If there's an error in loadInterface, (e.g. interface
	-- file not found) we get lots of spurious errors from 'filterImports'
    tryM (loadInterface doc imp_mod_name (ImportByUser is_boot))	`thenM` \ mb_iface ->

    case mb_iface of {
	Left exn    -> returnM (emptyRdrEnv, emptyImportAvails ) ;
	Right iface ->    

    let
	imp_mod	     	 = mi_module iface
	avails_by_module = mi_exports iface
	deprecs	     	 = mi_deprecs iface
	is_orph		 = mi_orphan iface 
	deps 		 = mi_deps iface

	avails :: Avails
	avails = [ avail | (mod_name, avails) <- avails_by_module,
			   mod_name /= this_mod_name,
			   avail <- avails ]
	this_mod_name = moduleName this_mod
	-- If the module exports anything defined in this module, just ignore it.
	-- Reason: otherwise it looks as if there are two local definition sites
	-- for the thing, and an error gets reported.  Easiest thing is just to
	-- filter them out up front. This situation only arises if a module
	-- imports itself, or another module that imported it.  (Necessarily,
	-- this invoves a loop.)  
	--
	-- Tiresome consequence: if you say
	--	module A where
	--	   import B( AType )
	--	   type AType = ...
	--
	--	module B( AType ) where
	--	   import {-# SOURCE #-} A( AType )
	--
	-- then you'll get a 'B does not export AType' message.  Oh well.

    in
	-- Filter the imports according to the import list
    filterImports imp_mod is_boot imp_spec avails    `thenM` \ (filtered_avails, explicits) ->

    let
	-- Compute new transitive dependencies
 	orphans | is_orph   = insert imp_mod_name (dep_orphs deps)
		| otherwise = dep_orphs deps

	(dependent_mods, dependent_pkgs) 
	   | isHomeModule imp_mod 
	   = 	-- Imported module is from the home package
		-- Take its dependent modules and
		--	(a) remove this_mod (might be there as a hi-boot)
		--	(b) add imp_mod itself
		-- Take its dependent packages unchanged
	     ((imp_mod_name, is_boot) : filter not_self (dep_mods deps), dep_pkgs deps)

	   | otherwise	
 	   = 	-- Imported module is from another package
		-- Dump the dependent modules
		-- Add the package imp_mod comes from to the dependent packages
		-- from imp_mod
	     ([], insert (mi_package iface) (dep_pkgs deps))

	not_self (m, _) = m /= this_mod_name

	import_all = case imp_spec of
			(Just (False, _)) -> False	-- Imports are spec'd explicitly
			other	   	  -> True	-- Everything is imported, 
							-- (or almost everything [hiding])

	qual_mod_name = case as_mod of
			  Nothing  	    -> imp_mod_name
			  Just another_name -> another_name
	
	-- unqual_avails is the Avails that are visible in *unqualified* form
	-- We need to know this so we know what to export when we see
	--	module M ( module P ) where ...
	-- Then we must export whatever came from P unqualified.
	avail_env = mkAvailEnv filtered_avails

	mk_prov name = NonLocalDef (UserImport imp_mod iloc (name `elemNameSet` explicits)) 
	gbl_env      = mkGlobalRdrEnv qual_mod_name (not qual_only) 
				      mk_prov filtered_avails deprecs
	imports      = ImportAvails { 
			imp_qual     = unitModuleEnvByName qual_mod_name avail_env,
			imp_env      = avail_env,
			imp_mods     = unitModuleEnv imp_mod (imp_mod, import_all),
			imp_orphs    = orphans,
			imp_dep_mods = mkModDeps dependent_mods,
			imp_dep_pkgs = dependent_pkgs }

    in
	-- Complain if we import a deprecated module
    ifOptM Opt_WarnDeprecations	(
       case deprecs of	
	  DeprecAll txt -> addWarn (moduleDeprec imp_mod_name txt)
	  other	        -> returnM ()
    )							`thenM_`

    returnM (gbl_env, imports)
    }

mkModDeps :: [(ModuleName, IsBootInterface)]
	  -> ModuleEnv (ModuleName, IsBootInterface)
mkModDeps deps = foldl add emptyModuleEnv deps
	       where
		 add env elt@(m,_) = extendModuleEnvByName env m elt
\end{code}


%************************************************************************
%*									*
		importsFromLocalDecls
%*									*
%************************************************************************

From the top-level declarations of this module produce
  	* the lexical environment
	* the ImportAvails
created by its bindings.  
	
Complain about duplicate bindings

\begin{code}
importsFromLocalDecls :: HsGroup RdrName
		      -> TcRn m (GlobalRdrEnv, ImportAvails)
importsFromLocalDecls group
  = getModule				`thenM` \ this_mod ->
    getLocalDeclBinders this_mod group	`thenM` \ avails ->
	-- The avails that are returned don't include the "system" names
    let
	all_names :: [Name]	-- All the defns; no dups eliminated
	all_names = [name | avail <- avails, name <- availNames avail]

	dups :: [[Name]]
	(_, dups) = removeDups compare all_names
    in
	-- Check for duplicate definitions
	-- The complaint will come out as "Multiple declarations of Foo.f" because
	-- since 'f' is in the env twice, the unQualInScope used by the error-msg
	-- printer returns False.  It seems awkward to fix, unfortunately.
    mappM_ (addErr . dupDeclErr) dups			`thenM_` 

    doptM Opt_NoImplicitPrelude 		`thenM` \ implicit_prelude ->
    let
	mod_name   = moduleName this_mod
	mk_prov n  = LocalDef	-- Provenance is local

	unqual_imp = True	-- Want unqualified names in scope
	gbl_env = mkGlobalRdrEnv mod_name unqual_imp mk_prov avails NoDeprecs
	    -- NoDeprecs: don't complain about locally defined names
	    -- For a start, we may be exporting a deprecated thing
	    -- Also we may use a deprecated thing in the defn of another
	    -- deprecated things.  We may even use a deprecated thing in
	    -- the defn of a non-deprecated thing, when changing a module's 
	    -- interface


	    -- Optimisation: filter out names for built-in syntax
	    -- They just clutter up the environment (esp tuples), and the parser
	    -- will generate Exact RdrNames for them, so the cluttered
	    -- envt is no use.  To avoid doing this filter all the time,
	    -- we use -fno-implicit-prelude as a clue that the filter is
	    -- worth while.  Really, it's only useful for GHC.Base and GHC.Tuple.
	    --
	    -- It's worth doing because it makes the environment smaller for
	    -- every module that imports the Prelude
	    --
	    -- Note: don't filter the gbl_env (hence avails, not avails' in
	    -- defn of gbl_env above).      Stupid reason: when parsing 
	    -- data type decls, the constructors start as Exact tycon-names,
	    -- and then get turned into data con names by zapping the name space;
	    -- but that stops them being Exact, so they get looked up.  Sigh.
	    -- It doesn't matter because it only affects the Data.Tuple really.
	    -- The important thing is to trim down the exports.

 	avails' | implicit_prelude = filter not_built_in_syntax avails
		| otherwise	   = avails
	not_built_in_syntax a = not (all isBuiltInSyntaxName (availNames a))
		-- Only filter it if all the names of the avail are built-in
		-- In particular, lists have (:) which is not built in syntax
		-- so we don't filter it out.

	avail_env = mkAvailEnv avails'
	imports   = emptyImportAvails {
			imp_qual = unitModuleEnv this_mod avail_env,
			imp_env  = avail_env
		    }
    in
    returnM (gbl_env, imports)
\end{code}


%*********************************************************
%*							*
\subsection{Getting binders out of a declaration}
%*							*
%*********************************************************

@getLocalDeclBinders@ returns the names for a @RdrNameHsDecl@.  It's
used for both source code (from @importsFromLocalDecls@) and interface
files (@loadDecl@ calls @getTyClDeclBinders@).

	*** See "THE NAMING STORY" in HsDecls ****

\begin{code}
getLocalDeclBinders :: Module -> HsGroup RdrName -> TcRn m [AvailInfo]
getLocalDeclBinders mod (HsGroup {hs_valds = val_decls, 
				  hs_tyclds = tycl_decls, 
				  hs_fords = foreign_decls })
  =	-- For type and class decls, we generate Global names, with
	-- no export indicator.  They need to be global because they get
	-- permanently bound into the TyCons and Classes.  They don't need
	-- an export indicator because they are all implicitly exported.

    mappM new_tc tycl_decls				`thenM` \ tc_avails ->
    mappM new_bndr (for_hs_bndrs ++ val_hs_bndrs)	`thenM` \ simple_bndrs ->

    returnM (tc_avails ++ map Avail simple_bndrs)
  where
    new_bndr (rdr_name,loc) = newTopBinder mod rdr_name loc

    val_hs_bndrs = collectLocatedHsBinders val_decls
    for_hs_bndrs = [(nm,loc) | ForeignImport nm _ _ _ loc <- foreign_decls]

    new_tc tc_decl = mappM new_bndr (tyClDeclNames tc_decl)	`thenM` \ names@(main_name:_) ->
		     returnM (AvailTC main_name names)
\end{code}


%************************************************************************
%*									*
\subsection{Filtering imports}
%*									*
%************************************************************************

@filterImports@ takes the @ExportEnv@ telling what the imported module makes
available, and filters it through the import spec (if any).

\begin{code}
filterImports :: Module				-- The module being imported
	      -> IsBootInterface		-- Tells whether it's a {-# SOURCE #-} import
	      -> Maybe (Bool, [RdrNameIE])	-- Import spec; True => hiding
	      -> [AvailInfo]			-- What's available
	      -> TcRn m ([AvailInfo],		-- What's imported
		       NameSet)			-- What was imported explicitly

	-- Complains if import spec mentions things that the module doesn't export
        -- Warns/informs if import spec contains duplicates.
filterImports mod from Nothing imports
  = returnM (imports, emptyNameSet)

filterImports mod from (Just (want_hiding, import_items)) total_avails
  = mappM get_item import_items		`thenM` \ avails_w_explicits_s ->
    let
	(item_avails, explicits_s) = unzip (concat avails_w_explicits_s)
	explicits		   = foldl addListToNameSet emptyNameSet explicits_s
    in
    if want_hiding then
	let	-- All imported; item_avails to be hidden
	   hidden = availsToNameSet item_avails
	   keep n = not (n `elemNameSet` hidden)
  	in
	returnM (pruneAvails keep total_avails, emptyNameSet)
    else
	-- Just item_avails imported; nothing to be hidden
	returnM (item_avails, explicits)
  where
    import_fm :: FiniteMap OccName AvailInfo
    import_fm = listToFM [ (nameOccName name, avail) 
			 | avail <- total_avails,
			   name  <- availNames avail]
	-- Even though availNames returns data constructors too,
	-- they won't make any difference because naked entities like T
	-- in an import list map to TcOccs, not VarOccs.

    bale_out item = addErr (badImportItemErr mod from item)	`thenM_`
		    returnM []

    get_item :: RdrNameIE -> TcRn m [(AvailInfo, [Name])]
	-- Empty list for a bad item.
	-- Singleton is typical case.
	-- Can have two when we are hiding, and mention C which might be
	--	both a class and a data constructor.  
	-- The [Name] is the list of explicitly-mentioned names
    get_item item@(IEModuleContents _) = bale_out item

    get_item item@(IEThingAll _)
      = case check_item item of
	  Nothing    		     -> bale_out item
	  Just avail@(AvailTC _ [n]) -> 	-- This occurs when you import T(..), but
						-- only export T abstractly.  The single [n]
						-- in the AvailTC is the type or class itself
					ifOptM Opt_WarnMisc (addWarn (dodgyImportWarn mod item))	`thenM_`
		     	 		returnM [(avail, [availName avail])]
	  Just avail 		     -> returnM [(avail, [availName avail])]

    get_item item@(IEThingAbs n)
      | want_hiding	-- hiding( C ) 
			-- Here the 'C' can be a data constructor *or* a type/class
      = case catMaybes [check_item item, check_item (IEVar data_n)] of
		[]     -> bale_out item
		avails -> returnM [(a, []) | a <- avails]
				-- The 'explicits' list is irrelevant when hiding
      where
	data_n = setRdrNameSpace n srcDataName

    get_item item
      = case check_item item of
	  Nothing    -> bale_out item
	  Just avail -> returnM [(avail, availNames avail)]

    check_item item
      | isNothing maybe_in_import_avails ||
	isNothing maybe_filtered_avail
      = Nothing

      | otherwise    
      = Just filtered_avail
		
      where
 	wanted_occ	       = rdrNameOcc (ieName item)
	maybe_in_import_avails = lookupFM import_fm wanted_occ

	Just avail	       = maybe_in_import_avails
	maybe_filtered_avail   = filterAvail item avail
	Just filtered_avail    = maybe_filtered_avail
\end{code}

\begin{code}
filterAvail :: RdrNameIE	-- Wanted
	    -> AvailInfo	-- Available
	    -> Maybe AvailInfo	-- Resulting available; 
				-- Nothing if (any of the) wanted stuff isn't there

filterAvail ie@(IEThingWith want wants) avail@(AvailTC n ns)
  | sub_names_ok = Just (AvailTC n (filter is_wanted ns))
  | otherwise    = Nothing
  where
    is_wanted name = nameOccName name `elem` wanted_occs
    sub_names_ok   = all (`elem` avail_occs) wanted_occs
    avail_occs	   = map nameOccName ns
    wanted_occs    = map rdrNameOcc (want:wants)

filterAvail (IEThingAbs _) (AvailTC n ns)       = ASSERT( n `elem` ns ) 
						  Just (AvailTC n [n])

filterAvail (IEThingAbs _) avail@(Avail n)      = Just avail		-- Type synonyms

filterAvail (IEVar _)      avail@(Avail n)      = Just avail
filterAvail (IEVar v)      avail@(AvailTC n ns) = Just (AvailTC n (filter wanted ns))
						where
						  wanted n = nameOccName n == occ
						  occ      = rdrNameOcc v
	-- The second equation happens if we import a class op, thus
	-- 	import A( op ) 
	-- where op is a class operation

filterAvail (IEThingAll _) avail@(AvailTC _ _)   = Just avail
	-- We don't complain even if the IE says T(..), but
	-- no constrs/class ops of T are available
	-- Instead that's caught with a warning by the caller

filterAvail ie avail = Nothing
\end{code}


%************************************************************************
%*									*
\subsection{Export list processing}
%*									*
%************************************************************************

Processing the export list.

You might think that we should record things that appear in the export
list as ``occurrences'' (using @addOccurrenceName@), but you'd be
wrong.  We do check (here) that they are in scope, but there is no
need to slurp in their actual declaration (which is what
@addOccurrenceName@ forces).

Indeed, doing so would big trouble when compiling @PrelBase@, because
it re-exports @GHC@, which includes @takeMVar#@, whose type includes
@ConcBase.StateAndSynchVar#@, and so on...

\begin{code}
type ExportAccum	-- The type of the accumulating parameter of
			-- the main worker function in exportsFromAvail
     = ([ModuleName], 		-- 'module M's seen so far
	ExportOccMap,		-- Tracks exported occurrence names
	AvailEnv)		-- The accumulated exported stuff, kept in an env
				--   so we can common-up related AvailInfos
emptyExportAccum = ([], emptyFM, emptyAvailEnv) 

type ExportOccMap = FiniteMap OccName (Name, RdrNameIE)
	-- Tracks what a particular exported OccName
	--   in an export list refers to, and which item
	--   it came from.  It's illegal to export two distinct things
	--   that have the same occurrence name


exportsFromAvail :: Maybe Module 	-- Nothing => no 'module M(..) where' header at all
		 -> Maybe [RdrNameIE] 	-- Nothing => no explicit export list
		 -> TcRn m Avails
	-- Complains if two distinct exports have same OccName
        -- Warns about identical exports.
	-- Complains about exports items not in scope

exportsFromAvail maybe_mod exports
 = do { TcGblEnv { tcg_rdr_env = rdr_env, 
		   tcg_imports = imports } <- getGblEnv ;

	-- If the module header is omitted altogether, then behave
	-- as if the user had written "module Main(main) where..."
	-- EXCEPT in interactive mode, when we behave as if he had
	-- written "module Main where ..."
	-- Reason: don't want to complain about 'main' not in scope
	--	   in interactive mode
	ghci_mode <- getGhciMode ;
	let { real_exports 
	        = case maybe_mod of
		    Just mod -> exports
		    Nothing | ghci_mode == Interactive -> Nothing
			    | otherwise		     -> Just [IEVar main_RDR_Unqual] } ;

	exports_from_avail exports rdr_env imports }

exports_from_avail Nothing rdr_env
		   imports@(ImportAvails { imp_env = entity_avail_env })
 =  	-- Export all locally-defined things
	-- We do this by filtering the global RdrEnv,
	-- keeping only things that are (a) qualified,
	-- (b) locally defined, (c) a 'main' name
	-- Then we look up in the entity-avail-env
   return [ lookupAvailEnv entity_avail_env name
	       | (rdr_name, gres) <- rdrEnvToList rdr_env,
		 isQual rdr_name,	-- Avoid duplicates
		 GRE { gre_name   = name, 
		       gre_parent = Nothing,	-- Main things only
		       gre_prov   = LocalDef } <- gres
	       ]

exports_from_avail (Just export_items) rdr_env
		   (ImportAvails { imp_qual = mod_avail_env, 
				   imp_env  = entity_avail_env }) 
  = foldlM exports_from_item emptyExportAccum
	    export_items			`thenM` \ (_, _, export_avail_map) ->
    returnM (nameEnvElts export_avail_map)

  where
    exports_from_item :: ExportAccum -> RdrNameIE -> TcRn m ExportAccum

    exports_from_item acc@(mods, occs, avails) ie@(IEModuleContents mod)
	| mod `elem` mods 	-- Duplicate export of M
	= do { warn_dup_exports <- doptM Opt_WarnDuplicateExports ;
	       warnIf warn_dup_exports (dupModuleExport mod) ;
	       returnM acc }

	| otherwise
	= case lookupModuleEnvByName mod_avail_env mod of
	    Nothing -> addErr (modExportErr mod)	`thenM_`
		       returnM acc

	    Just avail_env
		-> let
			mod_avails = [ filtered_avail
				     | avail <- availEnvElts avail_env,
				       let mb_avail = filter_unqual rdr_env avail,
				       isJust mb_avail,
				       let Just filtered_avail = mb_avail]
						
			avails' = foldl addAvail avails mod_avails
		   in
		-- This check_occs not only finds conflicts between this item
		-- and others, but also internally within this item.  That is,
		-- if 'M.x' is in scope in several ways, we'll have several
		-- members of mod_avails with the same OccName.

		   foldlM (check_occs ie) occs mod_avails	`thenM` \ occs' ->
		   returnM (mod:mods, occs', avails')

    exports_from_item acc@(mods, occs, avails) ie
	= lookupGRE (ieName ie)	 		`thenM` \ mb_gre -> 
	  case mb_gre of {
	    Nothing  -> addErr (unknownNameErr (ieName ie))	`thenM_`
			returnM acc ;
	    Just gre ->		

		-- Get the AvailInfo for the parent of the specified name
	  let
	    parent = gre_parent gre `orElse` gre_name gre
	    avail  = lookupAvailEnv entity_avail_env parent
	  in
		-- Filter out the bits we want
	  case filterAvail ie avail of {
	    Nothing -> 	-- Not enough availability
			addErr (exportItemErr ie) `thenM_`
			returnM acc ;

	    Just export_avail -> 	

		-- Phew!  It's OK!  Now to check the occurrence stuff!
	  warnIf (not (ok_item ie avail)) (dodgyExportWarn ie)	`thenM_`
          check_occs ie occs export_avail			`thenM` \ occs' ->
	  returnM (mods, occs', addAvail avails export_avail)
	  }}


-------------------------------
filter_unqual :: GlobalRdrEnv -> AvailInfo -> Maybe AvailInfo
-- Filter the Avail by what's in scope unqualified
filter_unqual env (Avail n)
  | in_scope env n = Just (Avail n)
  | otherwise	   = Nothing
filter_unqual env (AvailTC n ns)
  | not (null ns') = Just (AvailTC n ns')
  | otherwise	   = Nothing
  where
    ns' = filter (in_scope env) ns

in_scope :: GlobalRdrEnv -> Name -> Bool
-- Checks whether the Name is in scope unqualified, 
-- regardless of whether it's ambiguous or not
in_scope env n 
  = case lookupRdrEnv env (mkRdrUnqual (nameOccName n)) of
	Nothing   -> False
	Just gres -> or [n == gre_name g | g <- gres]


-------------------------------
ok_item (IEThingAll _) (AvailTC _ [n]) = False
  -- This occurs when you import T(..), but
  -- only export T abstractly.  The single [n]
  -- in the AvailTC is the type or class itself
ok_item _ _ = True

-------------------------------
check_occs :: RdrNameIE -> ExportOccMap -> AvailInfo -> TcRn m ExportOccMap
check_occs ie occs avail 
  = foldlM check occs (availNames avail)
  where
    check occs name
      = case lookupFM occs name_occ of
	  Nothing -> returnM (addToFM occs name_occ (name, ie))

	  Just (name', ie') 
	    | name == name'  	-- Duplicate export
	    ->	do { warn_dup_exports <- doptM Opt_WarnDuplicateExports ;
		     warnIf warn_dup_exports (dupExportWarn name_occ ie ie') ;
		     returnM occs }

	    | otherwise		-- Same occ name but different names: an error
	    ->	do { global_env <- getGlobalRdrEnv ;
  		     addErr (exportClashErr global_env name name' ie ie') ;
		     returnM occs }
      where
	name_occ = nameOccName name
\end{code}

%*********************************************************
%*						 	 *
\subsection{Unused names}
%*							 *
%*********************************************************

\begin{code}
reportUnusedNames :: TcGblEnv -> DefUses -> TcRn m ()
reportUnusedNames gbl_env dus
  = warnUnusedModules unused_imp_mods	`thenM_`
    warnUnusedTopBinds bad_locals	`thenM_`
    warnUnusedImports bad_imports	`thenM_`
    printMinimalImports minimal_imports
  where
    used_names :: NameSet
    used_names = findUses dus emptyNameSet

	-- Collect the defined names from the in-scope environment
	-- Look for the qualified ones only, else get duplicates
    defined_names :: [GlobalRdrElt]
    defined_names = foldRdrEnv add [] (tcg_rdr_env gbl_env)
    add rdr_name ns acc | isQual rdr_name = ns ++ acc
			| otherwise	  = acc

    defined_and_used, defined_but_not_used :: [GlobalRdrElt]
    (defined_and_used, defined_but_not_used) = partition is_used defined_names

    is_used gre = n `elemNameSet` used_names || any (`elemNameSet` used_names) kids
	-- The 'kids' part is because a use of C implies a use of T,
	-- if C was brought into scope by T(..) or T(C)
	     where
	       n    = gre_name gre
	       kids = case lookupAvailEnv_maybe avail_env n of
			Just (AvailTC n ns) -> ns
			other	            -> []	-- Ids, class ops and datacons
							-- (The latter two give Nothing)
    
    -- Filter out the ones that are 
    --  (a) defined in this module, and
    --	(b) not defined by a 'deriving' clause 
    -- The latter have an Internal Name, so we can filter them out easily
    bad_locals :: [GlobalRdrElt]
    bad_locals = filter is_bad defined_but_not_used
    is_bad :: GlobalRdrElt -> Bool
    is_bad gre = isLocalGRE gre && isExternalName (gre_name gre)
    
    bad_imports :: [GlobalRdrElt]
    bad_imports = filter bad_imp defined_but_not_used
    bad_imp (GRE {gre_prov = NonLocalDef (UserImport mod _ True)}) = not (module_unused mod)
    bad_imp other						   = False
    
    -- To figure out the minimal set of imports, start with the things
    -- that are in scope (i.e. in gbl_env).  Then just combine them
    -- into a bunch of avails, so they are properly grouped
    minimal_imports :: FiniteMap ModuleName AvailEnv
    minimal_imports0 = emptyFM
    minimal_imports1 = foldr add_name     minimal_imports0 defined_and_used
    minimal_imports  = foldr add_inst_mod minimal_imports1 direct_import_mods
 	-- The last line makes sure that we retain all direct imports
    	-- even if we import nothing explicitly.
    	-- It's not necessarily redundant to import such modules. Consider 
    	--	      module This
    	--		import M ()
    	--
    	-- The import M() is not *necessarily* redundant, even if
    	-- we suck in no instance decls from M (e.g. it contains 
    	-- no instance decls, or This contains no code).  It may be 
    	-- that we import M solely to ensure that M's orphan instance 
    	-- decls (or those in its imports) are visible to people who 
    	-- import This.  Sigh. 
    	-- There's really no good way to detect this, so the error message 
    	-- in RnEnv.warnUnusedModules is weakened instead
    

	-- We've carefully preserved the provenance so that we can
	-- construct minimal imports that import the name by (one of)
	-- the same route(s) as the programmer originally did.
    add_name (GRE {gre_name = n, gre_parent = p,
		   gre_prov = NonLocalDef (UserImport m _ _)}) acc 
	= addToFM_C plusAvailEnv acc (moduleName m) 
		    (unitAvailEnv (mk_avail n p))
    add_name other acc 
	= acc

	-- n is the name of the thing, p is the name of its parent
    mk_avail n (Just p)			 	 = AvailTC p [p,n]
    mk_avail n Nothing | isTcOcc (nameOccName n) = AvailTC n [n]
		       | otherwise		 = Avail n
    
    add_inst_mod m acc 
      | m `elemFM` acc = acc	-- We import something already
      | otherwise      = addToFM acc m emptyAvailEnv
    	-- Add an empty collection of imports for a module
    	-- from which we have sucked only instance decls
   
    imports   = tcg_imports gbl_env
    avail_env = imp_env imports

    direct_import_mods :: [ModuleName]
    direct_import_mods = map (moduleName . fst) 
			     (moduleEnvElts (imp_mods imports))

    -- unused_imp_mods are the directly-imported modules 
    -- that are not mentioned in minimal_imports1
    -- [Note: not 'minimal_imports', because that includes direcly-imported
    --	      modules even if we use nothing from them; see notes above]
    unused_imp_mods = [m | m <- direct_import_mods,
    		       isNothing (lookupFM minimal_imports1 m),
    		       m /= pRELUDE_Name]
    
    module_unused :: Module -> Bool
    module_unused mod = moduleName mod `elem` unused_imp_mods


-- ToDo: deal with original imports with 'qualified' and 'as M' clauses
printMinimalImports :: FiniteMap ModuleName AvailEnv	-- Minimal imports
		    -> TcRn m ()
printMinimalImports imps
 = ifOptM Opt_D_dump_minimal_imports $ do {

   mod_ies  <-  mappM to_ies (fmToList imps) ;
   this_mod <- getModule ;
   rdr_env  <- getGlobalRdrEnv ;
   ioToTcRn (do { h <- openFile (mkFilename this_mod) WriteMode ;
		  printForUser h (unQualInScope rdr_env) 
				 (vcat (map ppr_mod_ie mod_ies)) })
   }
  where
    mkFilename this_mod = moduleNameUserString (moduleName this_mod) ++ ".imports"
    ppr_mod_ie (mod_name, ies) 
	| mod_name == pRELUDE_Name 
	= empty
	| null ies	-- Nothing except instances comes from here
	= ptext SLIT("import") <+> ppr mod_name <> ptext SLIT("()    -- Instances only")
	| otherwise
	= ptext SLIT("import") <+> ppr mod_name <> 
		    parens (fsep (punctuate comma (map ppr ies)))

    to_ies (mod, avail_env) = mappM to_ie (availEnvElts avail_env)	`thenM` \ ies ->
			      returnM (mod, ies)

    to_ie :: AvailInfo -> TcRn m (IE Name)
	-- The main trick here is that if we're importing all the constructors
	-- we want to say "T(..)", but if we're importing only a subset we want
	-- to say "T(A,B,C)".  So we have to find out what the module exports.
    to_ie (Avail n)       = returnM (IEVar n)
    to_ie (AvailTC n [m]) = ASSERT( n==m ) 
			    returnM (IEThingAbs n)
    to_ie (AvailTC n ns)  
	= loadInterface (text "Compute minimal imports from" <+> ppr n_mod) 
			n_mod ImportBySystem				`thenM` \ iface ->
	  case [xs | (m,as) <- mi_exports iface,
		     m == n_mod,
		     AvailTC x xs <- as, 
		     x == n] of
	      [xs] | all (`elem` ns) xs -> returnM (IEThingAll n)
		   | otherwise	        -> returnM (IEThingWith n (filter (/= n) ns))
	      other			-> pprTrace "to_ie" (ppr n <+> ppr (nameModule n) <+> ppr other) $
					   returnM (IEVar n)
	where
	  n_mod = moduleName (nameModule n)
\end{code}


%************************************************************************
%*									*
\subsection{Errors}
%*									*
%************************************************************************

\begin{code}
badImportItemErr mod from ie
  = sep [ptext SLIT("Module"), quotes (ppr mod), source_import,
	 ptext SLIT("does not export"), quotes (ppr ie)]
  where
    source_import = case from of
		      True  -> ptext SLIT("(hi-boot interface)")
		      other -> empty

dodgyImportWarn mod item = dodgyMsg (ptext SLIT("import")) item
dodgyExportWarn     item = dodgyMsg (ptext SLIT("export")) item

dodgyMsg kind item@(IEThingAll tc)
  = sep [ ptext SLIT("The") <+> kind <+> ptext SLIT("item") <+> quotes (ppr item),
	  ptext SLIT("suggests that") <+> quotes (ppr tc) <+> ptext SLIT("has constructor or class methods"),
	  ptext SLIT("but it has none; it is a type synonym or abstract type or class") ]
	  
modExportErr mod
  = hsep [ ptext SLIT("Unknown module in export list: module"), quotes (ppr mod)]

exportItemErr export_item
  = sep [ ptext SLIT("The export item") <+> quotes (ppr export_item),
	  ptext SLIT("attempts to export constructors or class methods that are not visible here") ]

exportClashErr global_env name1 name2 ie1 ie2
  = vcat [ ptext SLIT("Conflicting exports for") <+> quotes (ppr occ) <> colon
	 , ppr_export ie1 name1 
	 , ppr_export ie2 name2  ]
  where
    occ = nameOccName name1
    ppr_export ie name = nest 2 (quotes (ppr ie) <+> ptext SLIT("exports") <+> 
			 	 quotes (ppr name) <+> pprNameProvenance (get_gre name))

	-- get_gre finds a GRE for the Name, in a very inefficient way
	-- There isn't a more efficient way to do it, because we don't necessarily
	-- know the RdrName under which this Name is in scope.  So we just
	-- search linearly.  Shouldn't matter because this only happens
	-- in an error message.
    get_gre name
	= case [gre | gres <- rdrEnvElts global_env,
		      gre  <- gres,
		      gre_name gre == name] of
	     (gre:_) -> gre
	     []	     -> pprPanic "exportClashErr" (ppr name)

dupDeclErr (n:ns)
  = vcat [ptext SLIT("Multiple declarations of") <+> quotes (ppr n),
	  nest 4 (vcat (map ppr sorted_locs))]
  where
    sorted_locs = sortLt occ'ed_before (map nameSrcLoc (n:ns))
    occ'ed_before a b = LT == compare a b

dupExportWarn occ_name ie1 ie2
  = hsep [quotes (ppr occ_name), 
          ptext SLIT("is exported by"), quotes (ppr ie1),
          ptext SLIT("and"),            quotes (ppr ie2)]

dupModuleExport mod
  = hsep [ptext SLIT("Duplicate"),
	  quotes (ptext SLIT("Module") <+> ppr mod), 
          ptext SLIT("in export list")]

moduleDeprec mod txt
  = sep [ ptext SLIT("Module") <+> quotes (ppr mod) <+> ptext SLIT("is deprecated:"), 
	  nest 4 (ppr txt) ]	  
\end{code}
