%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnNames]{Extracting imported and top-level names in scope}

\begin{code}
module RnNames (
	rnImports, importsFromLocalDecls, exportsFromAvail,
	reportUnusedNames, reportDeprecations, 
	mkModDeps, exportsToAvails
    ) where

#include "HsVersions.h"

import CmdLineOpts	( DynFlag(..) )
import HsSyn		( IE(..), ieName, ImportDecl(..), LImportDecl,
			  ForeignDecl(..), HsGroup(..),
			  collectGroupBinders, tyClDeclNames 
			)
import RnEnv
import IfaceEnv		( lookupOrig, newGlobalBinder )
import LoadIface	( loadSrcInterface )
import TcRnMonad

import FiniteMap
import PrelNames	( pRELUDE_Name, isBuiltInSyntaxName, isUnboundName,
			  main_RDR_Unqual )
import Module		( Module, ModuleName, moduleName, mkPackageModule,
			  moduleNameUserString, isHomeModule,
			  unitModuleEnvByName, unitModuleEnv, 
			  lookupModuleEnvByName, moduleEnvElts )
import Name		( Name, nameSrcLoc, nameOccName, nameModuleName,
			  nameParent, nameParent_maybe, isExternalName, nameModule )
import NameSet
import NameEnv
import OccName		( srcDataName, isTcOcc, occNameFlavour, OccEnv, 
			  mkOccEnv, lookupOccEnv, emptyOccEnv, extendOccEnv )
import HscTypes		( GenAvailInfo(..), AvailInfo, Avails, GhciMode(..),
			  IfaceExport, HomePackageTable, PackageIfaceTable, 
			  availName, availNames, availsToNameSet, unQualInScope, 
			  Deprecs(..), ModIface(..), Dependencies(..), lookupIface,
			  ExternalPackageState(..)
			)
import RdrName		( RdrName, rdrNameOcc, setRdrNameSpace, 
		  	  GlobalRdrEnv, mkGlobalRdrEnv, GlobalRdrElt(..), 
			  emptyGlobalRdrEnv, plusGlobalRdrEnv, globalRdrEnvElts,
			  unQualOK, lookupGRE_Name,
			  Provenance(..), ImportSpec(..), 
			  isLocalGRE, pprNameProvenance )
import Outputable
import Maybes		( isJust, isNothing, catMaybes, mapCatMaybes, seqMaybe )
import SrcLoc		( noSrcLoc, Located(..), mkGeneralSrcSpan,
			  unLoc, noLoc, srcLocSpan, SrcSpan )
import BasicTypes	( DeprecTxt )
import ListSetOps	( removeDups )
import Util		( sortLt, notNull, isSingleton )
import List		( partition )
import IO		( openFile, IOMode(..) )
\end{code}



%************************************************************************
%*									*
		rnImports
%*									*
%************************************************************************

\begin{code}
rnImports :: [LImportDecl RdrName]
	  -> RnM (GlobalRdrEnv, ImportAvails)

rnImports imports
  = 		-- PROCESS IMPORT DECLS
		-- Do the non {- SOURCE -} ones first, so that we get a helpful
		-- warning for {- SOURCE -} ones that are unnecessary
	getModule				`thenM` \ this_mod ->
	doptM Opt_NoImplicitPrelude		`thenM` \ opt_no_prelude -> 
	let
	  all_imports	     = mk_prel_imports this_mod opt_no_prelude ++ imports
	  (source, ordinary) = partition is_source_import all_imports
	  is_source_import (L _ (ImportDecl _ is_boot _ _ _)) = is_boot

	  get_imports = importsFromImportDecl this_mod
	in
	mappM get_imports ordinary	`thenM` \ stuff1 ->
	mappM get_imports source	`thenM` \ stuff2 ->

		-- COMBINE RESULTS
	let
	    (imp_gbl_envs, imp_avails) = unzip (stuff1 ++ stuff2)
	    gbl_env :: GlobalRdrEnv
	    gbl_env = foldr plusGlobalRdrEnv emptyGlobalRdrEnv imp_gbl_envs

	    all_avails :: ImportAvails
	    all_avails = foldr plusImportAvails emptyImportAvails imp_avails
	in
		-- ALL DONE
	returnM (gbl_env, all_avails)
  where
	-- NB: opt_NoImplicitPrelude is slightly different to import Prelude ();
	-- because the former doesn't even look at Prelude.hi for instance 
	-- declarations, whereas the latter does.
    mk_prel_imports this_mod no_prelude
	|  moduleName this_mod == pRELUDE_Name
	|| explicit_prelude_import
	|| no_prelude
	= []

	| otherwise = [preludeImportDecl]

    explicit_prelude_import
      = notNull [ () | L _ (ImportDecl mod _ _ _ _) <- imports, 
		       unLoc mod == pRELUDE_Name ]

preludeImportDecl
  = L loc $
	ImportDecl (L loc pRELUDE_Name)
	       False {- Not a boot interface -}
	       False	{- Not qualified -}
	       Nothing	{- No "as" -}
	       Nothing	{- No import list -}
  where
    loc = mkGeneralSrcSpan FSLIT("Implicit import declaration")
\end{code}
	
\begin{code}
importsFromImportDecl :: Module
		      -> LImportDecl RdrName
		      -> RnM (GlobalRdrEnv, ImportAvails)

importsFromImportDecl this_mod
	(L loc (ImportDecl loc_imp_mod_name want_boot qual_only as_mod imp_details))
  = 
    addSrcSpan loc $

	-- If there's an error in loadInterface, (e.g. interface
	-- file not found) we get lots of spurious errors from 'filterImports'
    let
	imp_mod_name = unLoc loc_imp_mod_name
	this_mod_name = moduleName this_mod
	doc = ppr imp_mod_name <+> ptext SLIT("is directly imported")
    in
    loadSrcInterface doc imp_mod_name want_boot	`thenM` \ iface ->

	-- Compiler sanity check: if the import didn't say
	-- {-# SOURCE #-} we should not get a hi-boot file
    WARN( not want_boot && mi_boot iface, ppr imp_mod_name )

	-- Issue a user warning for a redundant {- SOURCE -} import
	-- NB that we arrange to read all the ordinary imports before 
	-- any of the {- SOURCE -} imports
    warnIf (want_boot && not (mi_boot iface))
	   (warnRedundantSourceImport imp_mod_name)	`thenM_`

    let
	imp_mod	= mi_module iface
	deprecs	= mi_deprecs iface
	is_orph	= mi_orphan iface 
	deps 	= mi_deps iface

	filtered_exports = filter not_this_mod (mi_exports iface)
	not_this_mod (mod,_) = mod /= this_mod_name
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

	qual_mod_name = case as_mod of
			  Nothing  	    -> imp_mod_name
			  Just another_name -> another_name
	imp_spec  = ImportSpec { is_mod = imp_mod_name, is_qual = qual_only,  
		  		 is_loc = loc, is_as = qual_mod_name }
    in

	-- Get the total imports, and filter them according to the import list
    exportsToAvails filtered_exports		`thenM` \ total_avails ->
    filterImports iface imp_spec
		  imp_details total_avails	`thenM` \ (avail_env, gbl_env) ->

    let
	-- Compute new transitive dependencies

 	orphans | is_orph   = ASSERT( not (imp_mod_name `elem` dep_orphs deps) )
			      imp_mod_name : dep_orphs deps
		| otherwise = dep_orphs deps

	(dependent_mods, dependent_pkgs) 
	   | isHomeModule imp_mod 
	   = 	-- Imported module is from the home package
		-- Take its dependent modules and
		--	(a) remove this_mod (might be there as a hi-boot)
		--	(b) add imp_mod itself
		-- Take its dependent packages unchanged
	     ((imp_mod_name, want_boot) : filter not_self (dep_mods deps), dep_pkgs deps)

	   | otherwise	
 	   = 	-- Imported module is from another package
		-- Dump the dependent modules
		-- Add the package imp_mod comes from to the dependent packages
	     ASSERT( not (mi_package iface `elem` dep_pkgs deps) )
	     ([], mi_package iface : dep_pkgs deps)

	not_self (m, _) = m /= this_mod_name

	import_all = case imp_details of
			Just (is_hiding, ls)	 -- Imports are spec'd explicitly
			  | not is_hiding -> Just (not (null ls))
			_ -> Nothing		-- Everything is imported, 
						-- (or almost everything [hiding])

	-- unqual_avails is the Avails that are visible in *unqualified* form
	-- We need to know this so we know what to export when we see
	--	module M ( module P ) where ...
	-- Then we must export whatever came from P unqualified.
	imports   = ImportAvails { 
			imp_qual     = unitModuleEnvByName qual_mod_name avail_env,
			imp_env      = avail_env,
			imp_mods     = unitModuleEnv imp_mod (imp_mod, import_all, loc),
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

exportsToAvails :: [IfaceExport] -> TcRnIf gbl lcl Avails
exportsToAvails exports 
  = do 	{ avails_by_module <- mappM do_one exports
	; return (concat avails_by_module) }
  where
    do_one (mod_name, exports) = mapM (do_avail mod_name) exports
    do_avail mod_nm (Avail n)      = do { n' <- lookupOrig mod_nm n; 
					; return (Avail n') }
    do_avail mod_nm (AvailTC n ns) = do { n' <- lookupOrig mod_nm n
					; ns' <- mappM (lookup_sub n') ns
					; return (AvailTC n' ns') }
	where
	  mod = mkPackageModule mod_nm	-- Not necessarily right yet
	  lookup_sub parent occ = newGlobalBinder mod occ (Just parent) noSrcLoc
		-- Hack alert! Notice the newGlobalBinder.  It ensures that the subordinate 
		-- names record their parent; and that in turn ensures that the GlobalRdrEnv
		-- has the correct parent for all the names in its range.
		-- For imported things, we only suck in the binding site later, if ever.
	-- Reason for all this:
	--   Suppose module M exports type A.T, and constructor A.MkT
	--   Then, we know that A.MkT is a subordinate name of A.T,
	--   even though we aren't at the binding site of A.T
	--   And it's important, because we may simply re-export A.T
	--   without ever sucking in the declaration itself.

warnRedundantSourceImport mod_name
  = ptext SLIT("Unnecessary {- SOURCE -} in the import of module")
          <+> quotes (ppr mod_name)
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
		      -> RnM (GlobalRdrEnv, ImportAvails)
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
    mappM_ addDupDeclErr dups			`thenM_` 

    doptM Opt_NoImplicitPrelude 		`thenM` \ implicit_prelude ->
    let
	mod_name = moduleName this_mod
	prov     = LocalDef mod_name
	gbl_env  = mkGlobalRdrEnv gres
	gres     = [ GRE { gre_name = name, gre_prov = prov}
		   | name <- all_names]

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
	    -- but that stops them being Exact, so they get looked up.  
	    -- Ditto in fixity decls; e.g. 	infix 5 :
	    -- Sigh. It doesn't matter because it only affects the Data.Tuple really.
	    -- The important thing is to trim down the exports.

 	avails' | implicit_prelude = filter not_built_in_syntax avails
		| otherwise	   = avails
	not_built_in_syntax a = not (all isBuiltInSyntaxName (availNames a))
		-- Only filter it if all the names of the avail are built-in
		-- In particular, lists have (:) which is not built in syntax
		-- so we don't filter it out.  [Sept 03: wrong: see isBuiltInSyntaxName]

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

@getLocalDeclBinders@ returns the names for an @HsDecl@.  It's
used for source code.

	*** See "THE NAMING STORY" in HsDecls ****

\begin{code}
getLocalDeclBinders :: Module -> HsGroup RdrName -> RnM [AvailInfo]
getLocalDeclBinders mod (HsGroup {hs_valds = val_decls, 
				  hs_tyclds = tycl_decls, 
				  hs_fords = foreign_decls })
  =	-- For type and class decls, we generate Global names, with
	-- no export indicator.  They need to be global because they get
	-- permanently bound into the TyCons and Classes.  They don't need
	-- an export indicator because they are all implicitly exported.

    mappM new_tc     tycl_decls				`thenM` \ tc_avails ->
    mappM new_simple (for_hs_bndrs ++ val_hs_bndrs)	`thenM` \ simple_avails ->
    returnM (tc_avails ++ simple_avails)
  where
    new_simple rdr_name = newTopSrcBinder mod Nothing rdr_name `thenM` \ name ->
			  returnM (Avail name)

    val_hs_bndrs = collectGroupBinders val_decls
    for_hs_bndrs = [nm | L _ (ForeignImport nm _ _ _) <- foreign_decls]

    new_tc tc_decl 
	= newTopSrcBinder mod Nothing main_rdr			`thenM` \ main_name ->
	  mappM (newTopSrcBinder mod (Just main_name)) sub_rdrs	`thenM` \ sub_names ->
	  returnM (AvailTC main_name (main_name : sub_names))
	where
	  (main_rdr : sub_rdrs) = tyClDeclNames (unLoc tc_decl)
\end{code}


%************************************************************************
%*									*
\subsection{Filtering imports}
%*									*
%************************************************************************

@filterImports@ takes the @ExportEnv@ telling what the imported module makes
available, and filters it through the import spec (if any).

\begin{code}
filterImports :: ModIface
	      -> ImportSpec			-- The span for the entire import decl
	      -> Maybe (Bool, [Located (IE RdrName)])	-- Import spec; True => hiding
	      -> [AvailInfo]			-- What's available
	      -> RnM (AvailEnv,			-- What's imported (qualified or unqualified)
		      GlobalRdrEnv)		-- Same again, but in GRE form

	-- Complains if import spec mentions things that the module doesn't export
        -- Warns/informs if import spec contains duplicates.
			
mkGenericRdrEnv imp_spec avails
  = mkGlobalRdrEnv [ GRE { gre_name = name, gre_prov = Imported [imp_spec] False }
		   | avail <- avails, name <- availNames avail ]

filterImports iface imp_spec Nothing total_avails
  = returnM (mkAvailEnv total_avails, 
	     mkGenericRdrEnv imp_spec total_avails)

filterImports iface imp_spec (Just (want_hiding, import_items)) total_avails
  = mapAndUnzipM (addLocM get_item) import_items 	`thenM` \ (avails_s, gres) ->
    let
	avails = concat avails_s
    in
    if not want_hiding then
      return (mkAvailEnv avails,
	      foldr plusGlobalRdrEnv emptyGlobalRdrEnv gres)
    else
      let
	hidden = availsToNameSet avails
	keep n = not (n `elemNameSet` hidden)
	pruned_avails = pruneAvails keep total_avails
      in
      return (mkAvailEnv pruned_avails,
	      mkGenericRdrEnv imp_spec pruned_avails)

  where
    import_fm :: OccEnv AvailInfo
    import_fm = mkOccEnv [ (nameOccName name, avail) 
			 | avail <- total_avails,
			   name  <- availNames avail]
	-- Even though availNames returns data constructors too,
	-- they won't make any difference because naked entities like T
	-- in an import list map to TcOccs, not VarOccs.

    bale_out item = addErr (badImportItemErr iface imp_spec item)	`thenM_`
		    returnM ([], emptyGlobalRdrEnv)

    succeed_with :: Bool -> AvailInfo -> RnM ([AvailInfo], GlobalRdrEnv)
    succeed_with all_explicit avail
      = do { loc <- getSrcSpanM
	   ; returnM ([avail], 
		      mkGlobalRdrEnv (map (mk_gre loc) (availNames avail))) }
      where
	mk_gre loc name = GRE { gre_name = name, 
				gre_prov = Imported [this_imp_spec loc] (explicit name) }
	this_imp_spec loc = imp_spec { is_loc = loc }
	explicit name = all_explicit || name == main_name
	main_name = availName avail

    get_item :: IE RdrName -> RnM ([AvailInfo], GlobalRdrEnv)
	-- Empty result for a bad item.
	-- Singleton result is typical case.
	-- Can have two when we are hiding, and mention C which might be
	--	both a class and a data constructor.  
    get_item item@(IEModuleContents _) 
      = bale_out item

    get_item item@(IEThingAll tc)
      = case check_item item of
	  Nothing    		     -> bale_out item
	  Just avail@(AvailTC _ [n]) -> 	-- This occurs when you import T(..), but
						-- only export T abstractly.  The single [n]
						-- in the AvailTC is the type or class itself
					ifOptM Opt_WarnDodgyImports (addWarn (dodgyImportWarn tc)) `thenM_`
		     	 		succeed_with False avail
	  Just avail 		     -> succeed_with False avail

    get_item item@(IEThingAbs n)
      | want_hiding	-- hiding( C ) 
			-- Here the 'C' can be a data constructor 
			-- *or* a type/class, or even both
      = case catMaybes [check_item item, check_item (IEVar data_n)] of
	  []     -> bale_out item
	  avails -> returnM (avails, emptyGlobalRdrEnv)
			-- The GlobalRdrEnv result is irrelevant when hiding
      where
	data_n = setRdrNameSpace n srcDataName

    get_item item
      = case check_item item of
	  Nothing    -> bale_out item
	  Just avail -> succeed_with True avail

    check_item item
      | isNothing maybe_in_import_avails ||
	isNothing maybe_filtered_avail
      = Nothing

      | otherwise    
      = Just filtered_avail
		
      where
 	wanted_occ	       = rdrNameOcc (ieName item)
	maybe_in_import_avails = lookupOccEnv import_fm wanted_occ

	Just avail	       = maybe_in_import_avails
	maybe_filtered_avail   = filterAvail item avail
	Just filtered_avail    = maybe_filtered_avail
\end{code}

\begin{code}
filterAvail :: IE RdrName	-- Wanted
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
	NameSet)		-- The accumulated exported stuff
emptyExportAccum = ([], emptyOccEnv, emptyNameSet) 

type ExportOccMap = OccEnv (Name, IE RdrName)
	-- Tracks what a particular exported OccName
	--   in an export list refers to, and which item
	--   it came from.  It's illegal to export two distinct things
	--   that have the same occurrence name


exportsFromAvail :: Bool  -- False => no 'module M(..) where' header at all
		 -> Maybe [Located (IE RdrName)] -- Nothing => no explicit export list
		 -> RnM NameSet
	-- Complains if two distinct exports have same OccName
        -- Warns about identical exports.
	-- Complains about exports items not in scope

exportsFromAvail explicit_mod exports
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
		| explicit_mod   	   = exports
		| ghci_mode == Interactive = Nothing
		| otherwise 		   = Just [noLoc (IEVar main_RDR_Unqual)] } ;
	exports_from_avail real_exports rdr_env imports }


exports_from_avail Nothing rdr_env imports
 =  	-- Export all locally-defined things
	-- We do this by filtering the global RdrEnv,
	-- keeping only things that are locally-defined
   return (mkNameSet [ gre_name gre 
		     | gre <- globalRdrEnvElts rdr_env,
		       isLocalGRE gre ])

exports_from_avail (Just export_items) rdr_env
		   (ImportAvails { imp_qual = mod_avail_env, 
				   imp_env  = entity_avail_env }) 
  = foldlM (exports_from_litem) emptyExportAccum
	    export_items			`thenM` \ (_, _, exports) ->
    returnM exports

  where
    exports_from_litem :: ExportAccum -> Located (IE RdrName) -> RnM ExportAccum
    exports_from_litem acc = addLocM (exports_from_item acc)

    exports_from_item :: ExportAccum -> IE RdrName -> RnM ExportAccum
    exports_from_item acc@(mods, occs, exports) ie@(IEModuleContents mod)
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
			new_exports = [ name | avail <- availEnvElts avail_env,
					       name  <- availNames avail,
				               inScopeUnqual rdr_env name ]
		   in

		-- This check_occs not only finds conflicts between this item
		-- and others, but also internally within this item.  That is,
		-- if 'M.x' is in scope in several ways, we'll have several
		-- members of mod_avails with the same OccName.
		   check_occs ie occs new_exports	`thenM` \ occs' ->
		   returnM (mod:mods, occs', addListToNameSet exports new_exports)

    exports_from_item acc@(mods, occs, exports) ie
	= lookupGlobalOccRn (ieName ie)	 		`thenM` \ name -> 
	  if isUnboundName name then
		returnM acc 	-- Avoid error cascade
	  else
		-- Get the AvailInfo for the parent of the specified name
	  let
	    parent = nameParent name 
	    avail  = lookupAvailEnv entity_avail_env parent
	  in
		-- Filter out the bits we want
	  case filterAvail ie avail of {
	    Nothing -> 	-- Not enough availability
			addErr (exportItemErr ie) `thenM_`
			returnM acc ;

	    Just export_avail -> 	

		-- Phew!  It's OK!  Now to check the occurrence stuff!
	
	  let 
 	      new_exports = availNames export_avail 
	  in
	  checkForDodgyExport ie new_exports		`thenM_`
          check_occs ie occs new_exports		`thenM` \ occs' ->
	  returnM (mods, occs', addListToNameSet exports new_exports)
	  }


-------------------------------
inScopeUnqual :: GlobalRdrEnv -> Name -> Bool
-- Checks whether the Name is in scope unqualified, 
-- regardless of whether it's ambiguous or not
inScopeUnqual env n = any unQualOK (lookupGRE_Name env n)

-------------------------------
checkForDodgyExport :: IE RdrName -> [Name] -> RnM ()
checkForDodgyExport (IEThingAll tc) [n] = addWarn (dodgyExportWarn tc)
  -- This occurs when you import T(..), but
  -- only export T abstractly.  The single [n]
  -- in the AvailTC is the type or class itself
checkForDodgyExport _ _ = return ()

-------------------------------
check_occs :: IE RdrName -> ExportOccMap -> [Name] -> RnM ExportOccMap
check_occs ie occs names
  = foldlM check occs names
  where
    check occs name
      = case lookupOccEnv occs name_occ of
	  Nothing -> returnM (extendOccEnv occs name_occ (name, ie))

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
		Deprecations
%*							 *
%*********************************************************

\begin{code}
reportDeprecations :: TcGblEnv -> RnM ()
reportDeprecations tcg_env
  = ifOptM Opt_WarnDeprecations	$
    do	{ (eps,hpt) <- getEpsAndHpt
	; mapM_ (check hpt (eps_PIT eps)) all_gres }
  where
    used_names = findUses (tcg_dus tcg_env) emptyNameSet
    all_gres   = globalRdrEnvElts (tcg_rdr_env tcg_env)

    check hpt pit (GRE {gre_name = name, gre_prov = Imported (imp_spec:_) _})
      | name `elemNameSet` used_names
      ,	Just deprec_txt <- lookupDeprec hpt pit name
      = addSrcSpan (is_loc imp_spec) $
	addWarn (sep [ptext SLIT("Deprecated use of") <+> 
			text (occNameFlavour (nameOccName name)) <+> 
		 	quotes (ppr name),
		      (parens imp_msg),
		      (ppr deprec_txt) ])
	where
	  name_mod = nameModuleName name
	  imp_mod  = is_mod imp_spec
	  imp_msg  = ptext SLIT("imported from") <+> ppr imp_mod <> extra
	  extra | imp_mod == name_mod = empty
		| otherwise = ptext SLIT(", but defined in") <+> ppr name_mod

    check hpt pit ok_gre = returnM ()	-- Local, or not used, or not deprectated
	    -- The Imported pattern-match: don't deprecate locally defined names
	    -- For a start, we may be exporting a deprecated thing
	    -- Also we may use a deprecated thing in the defn of another
	    -- deprecated things.  We may even use a deprecated thing in
	    -- the defn of a non-deprecated thing, when changing a module's 
	    -- interface

lookupDeprec :: HomePackageTable -> PackageIfaceTable 
	     -> Name -> Maybe DeprecTxt
lookupDeprec hpt pit n 
  = case lookupIface hpt pit (nameModule n) of
	Just iface -> mi_dep_fn iface n `seqMaybe` 	-- Bleat if the thing, *or
		      mi_dep_fn iface (nameParent n)	-- its parent*, is deprec'd
	Nothing    -> pprPanic "lookupDeprec" (ppr n)	
		-- By now all the interfaces should have been loaded

gre_is_used :: NameSet -> GlobalRdrElt -> Bool
gre_is_used used_names gre = gre_name gre `elemNameSet` used_names
\end{code}

%*********************************************************
%*						 	 *
		Unused names
%*							 *
%*********************************************************

\begin{code}
reportUnusedNames :: TcGblEnv -> RnM ()
reportUnusedNames gbl_env 
  = do	{ warnUnusedTopBinds   unused_locals
	; warnUnusedModules    unused_imp_mods
	; warnUnusedImports    unused_imports	
	; warnDuplicateImports dup_imps
	; printMinimalImports  minimal_imports }
  where
    used_names, all_used_names :: NameSet
    used_names = findUses (tcg_dus gbl_env) emptyNameSet
    all_used_names = used_names `unionNameSets` 
		     mkNameSet (mapCatMaybes nameParent_maybe (nameSetToList used_names))
			-- A use of C implies a use of T,
			-- if C was brought into scope by T(..) or T(C)

	-- Collect the defined names from the in-scope environment
    defined_names :: [GlobalRdrElt]
    defined_names = globalRdrEnvElts (tcg_rdr_env gbl_env)

	-- Note that defined_and_used, defined_but_not_used
	-- are both [GRE]; that's why we need defined_and_used
	-- rather than just all_used_names
    defined_and_used, defined_but_not_used :: [GlobalRdrElt]
    (defined_and_used, defined_but_not_used) 
	= partition (gre_is_used all_used_names) defined_names
    
	-- Find the duplicate imports
    dup_imps = filter is_dup defined_and_used
    is_dup (GRE {gre_prov = Imported imp_spec True}) = not (isSingleton imp_spec)
    is_dup other				     = False

	-- Filter out the ones that are 
	--  (a) defined in this module, and
	--  (b) not defined by a 'deriving' clause 
	-- The latter have an Internal Name, so we can filter them out easily
    unused_locals :: [GlobalRdrElt]
    unused_locals = filter is_unused_local defined_but_not_used
    is_unused_local :: GlobalRdrElt -> Bool
    is_unused_local gre = isLocalGRE gre && isExternalName (gre_name gre)
    
    unused_imports :: [GlobalRdrElt]
    unused_imports = filter unused_imp defined_but_not_used
    unused_imp (GRE {gre_prov = Imported imp_specs True}) 
	= not (all (module_unused . is_mod) imp_specs)
		-- Don't complain about unused imports if we've already said the
		-- entire import is unused
    unused_imp other = False
    
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
    add_name (GRE {gre_name = n, gre_prov = Imported imp_specs _}) acc 
	= addToFM_C plusAvailEnv acc (is_mod (head imp_specs))
		    (unitAvailEnv (mk_avail n (nameParent_maybe n)))
    add_name other acc 
	= acc

	-- n is the name of the thing, p is the name of its parent
    mk_avail n (Just p)			 	 = AvailTC p [p,n]
    mk_avail n Nothing | isTcOcc (nameOccName n) = AvailTC n [n]
		       | otherwise		 = Avail n
    
    add_inst_mod (mod,_,_) acc 
      | mod_name `elemFM` acc = acc	-- We import something already
      | otherwise             = addToFM acc mod_name emptyAvailEnv
      where
	mod_name = moduleName mod
    	-- Add an empty collection of imports for a module
    	-- from which we have sucked only instance decls
   
    imports = tcg_imports gbl_env

    direct_import_mods :: [(Module, Maybe Bool, SrcSpan)]
	-- See the type of the imp_mods for this triple
    direct_import_mods = moduleEnvElts (imp_mods imports)

    -- unused_imp_mods are the directly-imported modules 
    -- that are not mentioned in minimal_imports1
    -- [Note: not 'minimal_imports', because that includes directly-imported
    --	      modules even if we use nothing from them; see notes above]
    unused_imp_mods = [(mod_name,loc) | (mod,imp,loc) <- direct_import_mods,
		       let mod_name = moduleName mod,
    		       not (mod_name `elemFM` minimal_imports1),
    		       mod_name /= pRELUDE_Name,
		       imp /= Just False]
	-- The Just False part is not to complain about
	-- import M (), which is an idiom for importing
	-- instance declarations
    
    module_unused :: ModuleName -> Bool
    module_unused mod = any (((==) mod) . fst) unused_imp_mods

---------------------
warnDuplicateImports :: [GlobalRdrElt] -> RnM ()
warnDuplicateImports gres
  = ifOptM Opt_WarnUnusedImports (mapM_ warn gres)
  where
    warn (GRE { gre_name = name, gre_prov = Imported imps _ })
	= addWarn ((quotes (ppr name) <+> ptext SLIT("is imported more than once:")) 
	       $$ nest 2 (vcat (map ppr imps)))
			      

-- ToDo: deal with original imports with 'qualified' and 'as M' clauses
printMinimalImports :: FiniteMap ModuleName AvailEnv	-- Minimal imports
		    -> RnM ()
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

    to_ie :: AvailInfo -> RnM (IE Name)
	-- The main trick here is that if we're importing all the constructors
	-- we want to say "T(..)", but if we're importing only a subset we want
	-- to say "T(A,B,C)".  So we have to find out what the module exports.
    to_ie (Avail n)       = returnM (IEVar n)
    to_ie (AvailTC n [m]) = ASSERT( n==m ) 
			    returnM (IEThingAbs n)
    to_ie (AvailTC n ns)  
	= loadSrcInterface doc n_mod False			`thenM` \ iface ->
	  case [xs | (m,as) <- mi_exports iface,
		     m == n_mod,
		     AvailTC x xs <- as, 
		     x == nameOccName n] of
	      [xs] | all_used xs -> returnM (IEThingAll n)
		   | otherwise	 -> returnM (IEThingWith n (filter (/= n) ns))
	      other		 -> pprTrace "to_ie" (ppr n <+> ppr n_mod <+> ppr other) $
				    returnM (IEVar n)
	where
	  all_used avail_occs = all (`elem` map nameOccName ns) avail_occs
	  doc = text "Compute minimal imports from" <+> ppr n
	  n_mod = nameModuleName n
\end{code}


%************************************************************************
%*									*
\subsection{Errors}
%*									*
%************************************************************************

\begin{code}
badImportItemErr iface imp_spec ie
  = sep [ptext SLIT("Module"), quotes (ppr (is_mod imp_spec)), source_import,
	 ptext SLIT("does not export"), quotes (ppr ie)]
  where
    source_import | mi_boot iface = ptext SLIT("(hi-boot interface)")
		  | otherwise     = empty

dodgyImportWarn item = dodgyMsg (ptext SLIT("import")) item
dodgyExportWarn item = dodgyMsg (ptext SLIT("export")) item

dodgyMsg kind tc
  = sep [ ptext SLIT("The") <+> kind <+> ptext SLIT("item") <+> quotes (ppr (IEThingAll tc)),
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

	-- get_gre finds a GRE for the Name, so that we can show its provenance
    get_gre name
	= case lookupGRE_Name global_env name of
	     (gre:_) -> gre
	     []	     -> pprPanic "exportClashErr" (ppr name)

addDupDeclErr :: [Name] -> TcRn ()
addDupDeclErr (n:ns)
  = addErrAt (srcLocSpan (nameSrcLoc n)) $
    vcat [ptext SLIT("Multiple declarations of") <+> quotes (ppr n),
	  nest 2 (ptext SLIT("other declarations at:")),
	  nest 4 (vcat (map ppr sorted_locs))]
  where
    sorted_locs = sortLt occ'ed_before (map nameSrcLoc ns)
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
