%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnNames]{Extracting imported and top-level names in scope}

\begin{code}
module RnNames (
	getGlobalNames, exportsFromAvail
    ) where

#include "HsVersions.h"

import CmdLineOpts	( DynFlag(..) )

import HsSyn		( HsModule(..), HsDecl(..), IE(..), ieName, ImportDecl(..),
			  ForeignDecl(..), ForKind(..), isDynamicExtName,
			  collectTopBinders
			)
import RdrHsSyn		( RdrNameIE, RdrNameImportDecl,
			  RdrNameHsModule, RdrNameHsDecl
			)
import RnIfaces		( getInterfaceExports, recordLocalSlurps )
import RnHiFiles	( getTyClDeclBinders )
import RnEnv
import RnMonad

import FiniteMap
import PrelNames	( pRELUDE_Name, mAIN_Name, main_RDR_Unqual, isUnboundName )
import UniqFM		( lookupUFM )
import Bag		( bagToList )
import Module		( ModuleName, moduleName, WhereFrom(..) )
import NameSet
import Name		( Name, nameSrcLoc, nameOccName,  nameEnvElts )
import HscTypes		( Provenance(..), ImportReason(..), GlobalRdrEnv,
			  GenAvailInfo(..), AvailInfo, Avails, AvailEnv )
import RdrName		( RdrName, rdrNameOcc, setRdrNameOcc, mkRdrQual, mkRdrUnqual )
import OccName		( setOccNameSpace, dataName )
import NameSet		( elemNameSet, emptyNameSet )
import Outputable
import Maybes		( maybeToBool, catMaybes, mapMaybe )
import UniqFM		( emptyUFM, listToUFM )
import ListSetOps	( removeDups )
import Util		( sortLt )
import List		( partition )
\end{code}



%************************************************************************
%*									*
\subsection{Get global names}
%*									*
%************************************************************************

\begin{code}
getGlobalNames :: Module -> RdrNameHsModule
	       -> RnMG (GlobalRdrEnv,	-- Maps all in-scope things
			GlobalRdrEnv,	-- Maps just *local* things
			ExportAvails)	-- The exported stuff

getGlobalNames this_mod (HsModule _ _ _ imports decls _ mod_loc)
  = 		-- PROCESS LOCAL DECLS
		-- Do these *first* so that the correct provenance gets
		-- into the global name cache.
	importsFromLocalDecls this_mod decls		`thenRn` \ (local_gbl_env, local_mod_avails) ->

		-- PROCESS IMPORT DECLS
		-- Do the non {- SOURCE -} ones first, so that we get a helpful
		-- warning for {- SOURCE -} ones that are unnecessary
	doptRn Opt_NoImplicitPrelude				`thenRn` \ opt_no_prelude -> 
	let
	  all_imports	     = mk_prel_imports opt_no_prelude ++ imports
	  (source, ordinary) = partition is_source_import all_imports
	  is_source_import (ImportDecl _ ImportByUserSource _ _ _ _) = True
	  is_source_import other				     = False

	  get_imports = importsFromImportDecl this_mod_name
	in
	mapAndUnzipRn get_imports ordinary	`thenRn` \ (imp_gbl_envs1, imp_avails_s1) ->
	mapAndUnzipRn get_imports source	`thenRn` \ (imp_gbl_envs2, imp_avails_s2) ->

		-- COMBINE RESULTS
		-- We put the local env second, so that a local provenance
		-- "wins", even if a module imports itself.
	let
	    gbl_env :: GlobalRdrEnv
	    imp_gbl_env = foldr plusGlobalRdrEnv emptyRdrEnv (imp_gbl_envs2 ++ imp_gbl_envs1)
	    gbl_env     = imp_gbl_env `plusGlobalRdrEnv` local_gbl_env

	    all_avails :: ExportAvails
	    all_avails = foldr plusExportAvails local_mod_avails (imp_avails_s2 ++ imp_avails_s1)
	in

		-- ALL DONE
	returnRn (gbl_env, local_gbl_env, all_avails)
  where
    this_mod_name = moduleName this_mod

	-- NB: opt_NoImplicitPrelude is slightly different to import Prelude ();
	-- because the former doesn't even look at Prelude.hi for instance declarations,
	-- whereas the latter does.
    mk_prel_imports no_prelude
	| this_mod_name == pRELUDE_Name ||
	  explicit_prelude_import ||
	  no_prelude
	= []

	| otherwise = [ImportDecl pRELUDE_Name
				  ImportByUser
				  False	{- Not qualified -}
				  Nothing	{- No "as" -}
				  Nothing	{- No import list -}
				  mod_loc]
    
    explicit_prelude_import
      = not (null [ () | (ImportDecl mod _ _ _ _ _) <- imports, mod == pRELUDE_Name ])
\end{code}
	
\begin{code}
importsFromImportDecl :: ModuleName
		      -> RdrNameImportDecl
		      -> RnMG (GlobalRdrEnv, 
			       ExportAvails) 

importsFromImportDecl this_mod_name (ImportDecl imp_mod_name from qual_only as_mod import_spec iloc)
  = pushSrcLocRn iloc $
    getInterfaceExports imp_mod_name from	`thenRn` \ (imp_mod, avails_by_module) ->

    if null avails_by_module then
	-- If there's an error in getInterfaceExports, (e.g. interface
	-- file not found) we get lots of spurious errors from 'filterImports'
	returnRn (emptyRdrEnv, mkEmptyExportAvails imp_mod_name)
    else

    let
	avails :: Avails
	avails = [ avail | (mod_name, avails) <- avails_by_module,
			   mod_name /= this_mod_name,
			   avail <- avails ]
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
    filterImports imp_mod_name from import_spec avails	`thenRn` \ (filtered_avails, hides, explicits) ->

    let
	mk_provenance name = NonLocalDef (UserImport imp_mod iloc (name `elemNameSet` explicits)) 
    in

    qualifyImports imp_mod_name
		   (not qual_only)	-- Maybe want unqualified names
		   as_mod hides
		   mk_provenance
		   filtered_avails
\end{code}


\begin{code}
importsFromLocalDecls this_mod decls
  = mapRn (getLocalDeclBinders this_mod) decls	`thenRn` \ avails_s ->

    let
	avails = concat avails_s

	all_names :: [Name]	-- All the defns; no dups eliminated
	all_names = [name | avail <- avails, name <- availNames avail]

	dups :: [[Name]]
	(_, dups) = removeDups compare all_names
    in
	-- Check for duplicate definitions
    mapRn_ (addErrRn . dupDeclErr) dups			`thenRn_` 

	-- Record that locally-defined things are available
    recordLocalSlurps (availsToNameSet avails)		`thenRn_`

	-- Build the environment
    qualifyImports (moduleName this_mod)
		   True			-- Want unqualified names
		   Nothing		-- no 'as M'
		   []			-- Hide nothing
		   (\n -> LocalDef)	-- Provenance is local
		   avails

---------------------------
getLocalDeclBinders :: Module 
		    -> RdrNameHsDecl -> RnMG Avails
getLocalDeclBinders mod (TyClD tycl_decl)
  =	-- For type and class decls, we generate Global names, with
	-- no export indicator.  They need to be global because they get
	-- permanently bound into the TyCons and Classes.  They don't need
	-- an export indicator because they are all implicitly exported.
    getTyClDeclBinders mod tycl_decl	`thenRn` \ avail ->
    returnRn [avail]

getLocalDeclBinders mod (ValD binds)
  = mapRn new (bagToList (collectTopBinders binds))
  where
    new (rdr_name, loc) = newTopBinder mod rdr_name loc 	`thenRn` \ name ->
			  returnRn (Avail name)

getLocalDeclBinders mod (ForD (ForeignDecl nm kind _ ext_nm _ loc))
  | binds_haskell_name kind
  = newTopBinder mod nm loc	    `thenRn` \ name ->
    returnRn [Avail name]

  | otherwise 		-- a foreign export
  = returnRn []
  where
    binds_haskell_name (FoImport _) = True
    binds_haskell_name FoLabel      = True
    binds_haskell_name FoExport     = isDynamicExtName ext_nm

getLocalDeclBinders mod (FixD _)    = returnRn []
getLocalDeclBinders mod (DeprecD _) = returnRn []
getLocalDeclBinders mod (DefD _)    = returnRn []
getLocalDeclBinders mod (InstD _)   = returnRn []
getLocalDeclBinders mod (RuleD _)   = returnRn []
\end{code}


%************************************************************************
%*									*
\subsection{Filtering imports}
%*									*
%************************************************************************

@filterImports@ takes the @ExportEnv@ telling what the imported module makes
available, and filters it through the import spec (if any).

\begin{code}
filterImports :: ModuleName			-- The module being imported
	      -> WhereFrom			-- Tells whether it's a {-# SOURCE #-} import
	      -> Maybe (Bool, [RdrNameIE])	-- Import spec; True => hiding
	      -> [AvailInfo]			-- What's available
	      -> RnMG ([AvailInfo],		-- What's actually imported
		       [AvailInfo],		-- What's to be hidden
						-- (the unqualified version, that is)
			-- (We need to return both the above sets, because
			--  the qualified version is never hidden; so we can't
			--  implement hiding by reducing what's imported.)
		       NameSet)			-- What was imported explicitly

	-- Complains if import spec mentions things that the module doesn't export
        -- Warns/informs if import spec contains duplicates.
filterImports mod from Nothing imports
  = returnRn (imports, [], emptyNameSet)

filterImports mod from (Just (want_hiding, import_items)) total_avails
  = flatMapRn get_item import_items		`thenRn` \ avails_w_explicits ->
    let
	(item_avails, explicits_s) = unzip avails_w_explicits
	explicits		   = foldl addListToNameSet emptyNameSet explicits_s
    in
    if want_hiding 
    then	
	-- All imported; item_avails to be hidden
	returnRn (total_avails, item_avails, emptyNameSet)
    else
	-- Just item_avails imported; nothing to be hidden
	returnRn (item_avails, [], explicits)
  where
    import_fm :: FiniteMap OccName AvailInfo
    import_fm = listToFM [ (nameOccName name, avail) 
			 | avail <- total_avails,
			   name  <- availNames avail]
	-- Even though availNames returns data constructors too,
	-- they won't make any difference because naked entities like T
	-- in an import list map to TcOccs, not VarOccs.

    bale_out item = addErrRn (badImportItemErr mod from item)	`thenRn_`
		    returnRn []

    get_item item@(IEModuleContents _) = bale_out item

    get_item item@(IEThingAll _)
      = case check_item item of
	  Nothing    		     -> bale_out item
	  Just avail@(AvailTC _ [n]) -> 	-- This occurs when you import T(..), but
						-- only export T abstractly.  The single [n]
						-- in the AvailTC is the type or class itself
					addWarnRn (dodgyImportWarn mod item)	`thenRn_`
		     	 		returnRn [(avail, [availName avail])]
	  Just avail 		     -> returnRn [(avail, [availName avail])]

    get_item item@(IEThingAbs n)
      | want_hiding	-- hiding( C ) 
			-- Here the 'C' can be a data constructor *or* a type/class
      = case catMaybes [check_item item, check_item (IEThingAbs data_n)] of
		[]     -> bale_out item
		avails -> returnRn [(a, []) | a <- avails]
				-- The 'explicits' list is irrelevant when hiding
      where
	data_n = setRdrNameOcc n (setOccNameSpace (rdrNameOcc n) dataName)

    get_item item
      = case check_item item of
	  Nothing    -> bale_out item
	  Just avail -> returnRn [(avail, availNames avail)]

    check_item item
      | not (maybeToBool maybe_in_import_avails) ||
	not (maybeToBool maybe_filtered_avail)
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



%************************************************************************
%*									*
\subsection{Qualifiying imports}
%*									*
%************************************************************************

@qualifyImports@ takes the @ExportEnv@ after filtering through the import spec
of an import decl, and deals with producing an @RnEnv@ with the 
right qualified names.  It also turns the @Names@ in the @ExportEnv@ into
fully fledged @Names@.

\begin{code}
qualifyImports :: ModuleName		-- Imported module
	       -> Bool			-- True <=> want unqualified import
	       -> Maybe ModuleName	-- Optional "as M" part 
	       -> [AvailInfo]		-- What's to be hidden (but only the unqualified 
					-- 	version is hidden)
	       -> (Name -> Provenance)
	       -> Avails		-- Whats imported and how
	       -> RnMG (GlobalRdrEnv, ExportAvails)

qualifyImports this_mod unqual_imp as_mod hides mk_provenance avails
  = 
 	-- Make the name environment.  We're talking about a 
	-- single module here, so there must be no name clashes.
	-- In practice there only ever will be if it's the module
	-- being compiled.
    let
	-- Add the things that are available
	name_env1 = foldl add_avail emptyRdrEnv avails

	-- Delete things that are hidden
	name_env2 = foldl del_avail name_env1 hides

	-- Create the export-availability info
	export_avails = mkExportAvails qual_mod unqual_imp name_env2 avails
    in
    returnRn (name_env2, export_avails)

  where
    qual_mod = case as_mod of
		  Nothing  	    -> this_mod
		  Just another_name -> another_name

    add_avail :: GlobalRdrEnv -> AvailInfo -> GlobalRdrEnv
    add_avail env avail = foldl add_name env (availNames avail)

    add_name env name
	| unqual_imp = env2
	| otherwise  = env1
	where
	  env1 = addOneToGlobalRdrEnv env  (mkRdrQual qual_mod occ) (name,prov)
	  env2 = addOneToGlobalRdrEnv env1 (mkRdrUnqual occ) 	    (name,prov)
	  occ  = nameOccName name
	  prov = mk_provenance name

    del_avail env avail = foldl delOneFromGlobalRdrEnv env rdr_names
			where
			  rdr_names = map (mkRdrUnqual . nameOccName) (availNames avail)


mkEmptyExportAvails :: ModuleName -> ExportAvails
mkEmptyExportAvails mod_name = (unitFM mod_name [], emptyUFM)

mkExportAvails :: ModuleName -> Bool -> GlobalRdrEnv -> [AvailInfo] -> ExportAvails
mkExportAvails mod_name unqual_imp name_env avails
  = (mod_avail_env, entity_avail_env)
  where
    mod_avail_env = unitFM mod_name unqual_avails 

	-- unqual_avails is the Avails that are visible in *unqualfied* form
	-- (1.4 Report, Section 5.1.1)
	-- For example, in 
	--	import T hiding( f )
	-- we delete f from avails

    unqual_avails | not unqual_imp = []	-- Short cut when no unqualified imports
		  | otherwise      = mapMaybe prune avails

    prune (Avail n) | unqual_in_scope n = Just (Avail n)
    prune (Avail n) | otherwise		= Nothing
    prune (AvailTC n ns) | null uqs     = Nothing
			 | otherwise    = Just (AvailTC n uqs)
			 where
			   uqs = filter unqual_in_scope ns

    unqual_in_scope n = unQualInScope name_env n

    entity_avail_env = listToUFM [ (name,avail) | avail <- avails, 
			  	   		  name  <- availNames avail]

plusExportAvails ::  ExportAvails ->  ExportAvails ->  ExportAvails
plusExportAvails (m1, e1) (m2, e2)
  = (plusFM_C (++) m1 m2, plusAvailEnv e1 e2)
	-- ToDo: wasteful: we do this once for each constructor!
\end{code}


%************************************************************************
%*									*
\subsection{Export list processing}
%*									*
%************************************************************************

Processing the export list.

You might think that we should record things that appear in the export list
as ``occurrences'' (using @addOccurrenceName@), but you'd be wrong.
We do check (here) that they are in scope,
but there is no need to slurp in their actual declaration
(which is what @addOccurrenceName@ forces).

Indeed, doing so would big trouble when
compiling @PrelBase@, because it re-exports @GHC@, which includes @takeMVar#@,
whose type includes @ConcBase.StateAndSynchVar#@, and so on...

\begin{code}
type ExportAccum	-- The type of the accumulating parameter of
			-- the main worker function in exportsFromAvail
     = ([ModuleName], 		-- 'module M's seen so far
	ExportOccMap,		-- Tracks exported occurrence names
	AvailEnv)		-- The accumulated exported stuff, kept in an env
				--   so we can common-up related AvailInfos

type ExportOccMap = FiniteMap OccName (Name, RdrNameIE)
	-- Tracks what a particular exported OccName
	--   in an export list refers to, and which item
	--   it came from.  It's illegal to export two distinct things
	--   that have the same occurrence name


exportsFromAvail :: ModuleName
		 -> Maybe [RdrNameIE]	-- Export spec
		 -> ExportAvails
		 -> GlobalRdrEnv 
		 -> RnMG Avails
	-- Complains if two distinct exports have same OccName
        -- Warns about identical exports.
	-- Complains about exports items not in scope
exportsFromAvail this_mod Nothing export_avails global_name_env
  = exportsFromAvail this_mod true_exports export_avails global_name_env
  where
    true_exports = Just $ if this_mod == mAIN_Name
                          then [IEVar main_RDR_Unqual]
                               -- export Main.main *only* unless otherwise specified,
                          else [IEModuleContents this_mod]
                               -- but for all other modules export everything.

exportsFromAvail this_mod (Just export_items) 
		 (mod_avail_env, entity_avail_env)
	         global_name_env
  = doptRn Opt_WarnDuplicateExports 		`thenRn` \ warn_dup_exports ->
    foldlRn (exports_from_item warn_dup_exports)
	    ([], emptyFM, emptyAvailEnv) export_items
						`thenRn` \ (_, _, export_avail_map) ->
    let
	export_avails :: [AvailInfo]
	export_avails   = nameEnvElts export_avail_map
    in
    returnRn export_avails

  where
    exports_from_item :: Bool -> ExportAccum -> RdrNameIE -> RnMG ExportAccum

    exports_from_item warn_dups acc@(mods, occs, avails) ie@(IEModuleContents mod)
	| mod `elem` mods 	-- Duplicate export of M
	= warnCheckRn warn_dups (dupModuleExport mod)	`thenRn_`
	  returnRn acc

	| otherwise
	= case lookupFM mod_avail_env mod of
		Nothing	        -> failWithRn acc (modExportErr mod)
		Just mod_avails -> foldlRn (check_occs ie) occs mod_avails
			 	   `thenRn` \ occs' ->
				   let
					avails' = foldl addAvail avails mod_avails
				   in
				   returnRn (mod:mods, occs', avails')

    exports_from_item warn_dups acc@(mods, occs, avails) ie
	= lookupSrcName global_name_env (ieName ie)	`thenRn` \ name -> 

		-- See what's available in the current environment
	  case lookupUFM entity_avail_env name of {
	    Nothing -> 	-- Presumably this happens because lookupSrcName didn't find
			-- the name and returned an unboundName, which won't be in
			-- the entity_avail_env, of course
			WARN( not (isUnboundName name), ppr name )
			returnRn acc ;

	    Just avail ->

		-- Filter out the bits we want
	  case filterAvail ie avail of {
	    Nothing -> 	-- Not enough availability
			   failWithRn acc (exportItemErr ie) ;

	    Just export_avail -> 	

		-- Phew!  It's OK!  Now to check the occurrence stuff!
	  warnCheckRn (ok_item ie avail) (dodgyExportWarn ie)	`thenRn_`
          check_occs ie occs export_avail			`thenRn` \ occs' ->
	  returnRn (mods, occs', addAvail avails export_avail)
	  }}



ok_item (IEThingAll _) (AvailTC _ [n]) = False
  -- This occurs when you import T(..), but
  -- only export T abstractly.  The single [n]
  -- in the AvailTC is the type or class itself
ok_item _ _ = True

check_occs :: RdrNameIE -> ExportOccMap -> AvailInfo -> RnMG ExportOccMap
check_occs ie occs avail 
  = doptRn Opt_WarnDuplicateExports	`thenRn` \ warn_dup_exports ->
    foldlRn (check warn_dup_exports) occs (availNames avail)
  where
    check warn_dup occs name
      = case lookupFM occs name_occ of
	  Nothing	    -> returnRn (addToFM occs name_occ (name, ie))
	  Just (name', ie') 
	    | name == name' -> 	-- Duplicate export
				warnCheckRn warn_dup
					    (dupExportWarn name_occ ie ie')
				`thenRn_` returnRn occs

	    | otherwise	    ->	-- Same occ name but different names: an error
				failWithRn occs (exportClashErr name_occ ie ie')
      where
	name_occ = nameOccName name
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
		      ImportByUserSource -> ptext SLIT("(hi-boot interface)")
		      other		 -> empty

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

exportClashErr occ_name ie1 ie2
  = hsep [ptext SLIT("The export items"), quotes (ppr ie1)
         ,ptext SLIT("and"), quotes (ppr ie2)
	 ,ptext SLIT("create conflicting exports for"), quotes (ppr occ_name)]

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
\end{code}
