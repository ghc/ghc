%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnNames]{Extracting imported and top-level names in scope}

\begin{code}
module RnNames (
	getGlobalNames
    ) where

#include "HsVersions.h"

import CmdLineOpts    ( opt_NoImplicitPrelude, opt_WarnDuplicateExports, 
			opt_SourceUnchanged, opt_WarnUnusedBinds
		      )

import HsSyn	( HsModule(..), HsDecl(..), TyClDecl(..),
		  IE(..), ieName, 
		  ForeignDecl(..), ForKind(..), isDynamicExtName,
		  FixitySig(..), Sig(..), ImportDecl(..),
		  collectTopBinders
		)
import RdrHsSyn	( RdrNameIE, RdrNameImportDecl,
		  RdrNameHsModule, RdrNameHsDecl
		)
import RnIfaces	( getInterfaceExports, getDeclBinders, getDeclSysBinders,
		  recordSlurp, checkUpToDate
		)
import RnEnv
import RnMonad

import FiniteMap
import PrelMods
import PrelInfo ( main_RDR )
import UniqFM	( lookupUFM )
import Bag	( bagToList )
import Maybes	( maybeToBool, catMaybes )
import Module	( ModuleName, mkThisModule, pprModuleName, WhereFrom(..) )
import NameSet
import Name	( Name, ExportFlag(..), ImportReason(..), Provenance(..),
		  isLocallyDefined, setNameProvenance,
		  nameOccName, getSrcLoc, pprProvenance, getNameProvenance
		)
import RdrName	( RdrName, rdrNameOcc, setRdrNameOcc, mkRdrQual, mkRdrUnqual, isQual )
import OccName	( setOccNameSpace, dataName )
import SrcLoc	( SrcLoc )
import NameSet	( elemNameSet, emptyNameSet )
import Outputable
import Unique	( getUnique )
import Util	( removeDups, equivClassesByUniq, sortLt )
import List	( partition )
\end{code}



%************************************************************************
%*									*
\subsection{Get global names}
%*									*
%************************************************************************

\begin{code}
getGlobalNames :: RdrNameHsModule
	       -> RnMG (Maybe (ExportEnv, 
			       GlobalRdrEnv,
			       FixityEnv,	 -- Fixities for local decls only
			       AvailEnv		 -- Maps a name to its parent AvailInfo
						 -- Just for in-scope things only
			       ))
			-- Nothing => no need to recompile

getGlobalNames (HsModule this_mod _ exports imports decls _ mod_loc)
  = 	-- These two fix-loops are to get the right
	-- provenance information into a Name
    fixRn (\ ~(rec_gbl_env, rec_exported_avails, _) ->

	let
	   rec_unqual_fn :: Name -> Bool	-- Is this chap in scope unqualified?
	   rec_unqual_fn = unQualInScope rec_gbl_env

	   rec_exp_fn :: Name -> ExportFlag
	   rec_exp_fn = mk_export_fn (availsToNameSet rec_exported_avails)
	in
	setModuleRn this_mod			$

		-- PROCESS LOCAL DECLS
		-- Do these *first* so that the correct provenance gets
		-- into the global name cache.
	importsFromLocalDecls this_mod rec_exp_fn decls
	`thenRn` \ (local_gbl_env, local_mod_avails) ->

		-- PROCESS IMPORT DECLS
		-- Do the non {- SOURCE -} ones first, so that we get a helpful
		-- warning for {- SOURCE -} ones that are unnecessary
	let
	  (source, ordinary) = partition is_source_import all_imports
	  is_source_import (ImportDecl _ ImportByUserSource _ _ _ _) = True
	  is_source_import other				     = False
	in
	mapAndUnzipRn (importsFromImportDecl rec_unqual_fn) ordinary
	`thenRn` \ (imp_gbl_envs1, imp_avails_s1) ->
	mapAndUnzipRn (importsFromImportDecl rec_unqual_fn) source
	`thenRn` \ (imp_gbl_envs2, imp_avails_s2) ->

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

	-- TRY FOR EARLY EXIT
	-- We can't go for an early exit before this because we have to check
	-- for name clashes.  Consider:
	--
	--	module A where		module B where
	--  	   import B		   h = True
	--   	   f = h
	--
	-- Suppose I've compiled everything up, and then I add a
	-- new definition to module B, that defines "f".
	--
	-- Then I must detect the name clash in A before going for an early
	-- exit.  The early-exit code checks what's actually needed from B
	-- to compile A, and of course that doesn't include B.f.  That's
	-- why we wait till after the plusEnv stuff to do the early-exit.
      checkEarlyExit this_mod			`thenRn` \ up_to_date ->
      if up_to_date then
	returnRn (gbl_env, junk_exp_fn, Nothing)
      else
 
	-- RECORD BETTER PROVENANCES IN THE CACHE
 	-- The names in the envirnoment have better provenances (e.g. imported on line x)
	-- than the names in the name cache.  We update the latter now, so that we
	-- we start renaming declarations we'll get the good names
	-- The isQual is because the qualified name is always in scope
      updateProvenances (concat [names | (rdr_name, names) <- rdrEnvToList imp_gbl_env, 
					  isQual rdr_name])	`thenRn_`

	-- PROCESS EXPORT LISTS
      exportsFromAvail this_mod exports all_avails gbl_env      `thenRn` \ exported_avails ->

	-- DONE
      returnRn (gbl_env, exported_avails, Just all_avails)
    )		`thenRn` \ (gbl_env, exported_avails, maybe_stuff) ->

    case maybe_stuff of {
	Nothing -> returnRn Nothing ;
	Just all_avails ->

	-- DEAL WITH FIXITIES
   fixitiesFromLocalDecls gbl_env decls		`thenRn` \ local_fixity_env ->
   let
	-- Export only those fixities that are for names that are
	--	(a) defined in this module
	--	(b) exported
	exported_fixities :: [(Name,Fixity)]
	exported_fixities = [(name,fixity)
			    | FixitySig name fixity _ <- nameEnvElts local_fixity_env,
			      isLocallyDefined name
			    ]

	-- CONSTRUCT RESULTS
	export_mods = case exports of
			Nothing -> []
			Just es -> [mod | IEModuleContents mod <- es, mod /= this_mod]

	export_env	      = ExportEnv exported_avails exported_fixities export_mods
	(_, global_avail_env) = all_avails
   in
   traceRn (text "fixity env" <+> vcat (map ppr (nameEnvElts local_fixity_env)))	`thenRn_`

   returnRn (Just (export_env, gbl_env, local_fixity_env, global_avail_env))
   }
  where
    junk_exp_fn = error "RnNames:export_fn"

    all_imports = prel_imports ++ imports

	-- NB: opt_NoImplicitPrelude is slightly different to import Prelude ();
	-- because the former doesn't even look at Prelude.hi for instance declarations,
	-- whereas the latter does.
    prel_imports | this_mod == pRELUDE_Name ||
		   explicit_prelude_import ||
		   opt_NoImplicitPrelude
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
checkEarlyExit mod
  = checkErrsRn				`thenRn` \ no_errs_so_far ->
    if not no_errs_so_far then
	-- Found errors already, so exit now
	returnRn True
    else

    traceRn (text "Considering whether compilation is required...")	`thenRn_`
    if not opt_SourceUnchanged then
	-- Source code changed and no errors yet... carry on 
	traceRn (nest 4 (text "source file changed or recompilation check turned off"))	`thenRn_` 
	returnRn False
    else

	-- Unchanged source, and no errors yet; see if usage info
	-- up to date, and exit if so
    checkUpToDate mod						`thenRn` \ up_to_date ->
    (if up_to_date 
	then putDocRn (text "Compilation IS NOT required")
	else returnRn ())					`thenRn_`
    returnRn up_to_date
\end{code}
	
\begin{code}
importsFromImportDecl :: (Name -> Bool)		-- OK to omit qualifier
		      -> RdrNameImportDecl
		      -> RnMG (GlobalRdrEnv, 
			       ExportAvails) 

importsFromImportDecl is_unqual (ImportDecl imp_mod_name from qual_only as_mod import_spec iloc)
  = pushSrcLocRn iloc $
    getInterfaceExports imp_mod_name from	`thenRn` \ (imp_mod, avails) ->

    if null avails then
	-- If there's an error in getInterfaceExports, (e.g. interface
	-- file not found) we get lots of spurious errors from 'filterImports'
	returnRn (emptyRdrEnv, mkEmptyExportAvails imp_mod_name)
    else

    filterImports imp_mod_name import_spec avails
    `thenRn` \ (filtered_avails, hides, explicits) ->

	-- We 'improve' the provenance by setting
	--	(a) the import-reason field, so that the Name says how it came into scope
	--		including whether it's explicitly imported
	--	(b) the print-unqualified field
	-- But don't fiddle with wired-in things or we get in a twist
    let
	improve_prov name =
	 setNameProvenance name (NonLocalDef (UserImport imp_mod iloc (is_explicit name)) 
					     (is_unqual name))
	is_explicit name  = name `elemNameSet` explicits
    in
    qualifyImports imp_mod_name
		   (not qual_only)	-- Maybe want unqualified names
		   as_mod hides
		   filtered_avails improve_prov
    `thenRn` \ (rdr_name_env, mod_avails) ->

    returnRn (rdr_name_env, mod_avails)
\end{code}


\begin{code}
importsFromLocalDecls mod_name rec_exp_fn decls
  = mapRn (getLocalDeclBinders newLocalName) decls	`thenRn` \ avails_s ->

    let
	avails = concat avails_s

	all_names :: [Name]	-- All the defns; no dups eliminated
	all_names = [name | avail <- avails, name <- availNames avail]

	dups :: [[Name]]
	(_, dups) = removeDups compare all_names
    in
	-- Check for duplicate definitions
    mapRn_ (addErrRn . dupDeclErr) dups		`thenRn_` 

	-- Record that locally-defined things are available
    mapRn_ (recordSlurp Nothing) avails		`thenRn_`

	-- Build the environment
    qualifyImports mod_name 
		   True		-- Want unqualified names
		   Nothing	-- no 'as M'
		   []		-- Hide nothing
		   avails
		   (\n -> n)

  where
    mod = mkThisModule mod_name

    newLocalName rdr_name loc 
	= (if isQual rdr_name then
		qualNameErr (text "the binding for" <+> quotes (ppr rdr_name)) (rdr_name,loc)
		-- There should never be a qualified name in a binding position (except in instance decls)
		-- The parser doesn't check this because the same parser parses instance decls
	    else 
		returnRn ())			`thenRn_`

	  newLocalTopBinder mod (rdrNameOcc rdr_name) rec_exp_fn loc


getLocalDeclBinders :: (RdrName -> SrcLoc -> RnMG Name)	-- New-name function
		    -> RdrNameHsDecl
		    -> RnMG Avails
getLocalDeclBinders new_name (ValD binds)
  = mapRn do_one (bagToList (collectTopBinders binds))
  where
    do_one (rdr_name, loc) = new_name rdr_name loc	`thenRn` \ name ->
			     returnRn (Avail name)

getLocalDeclBinders new_name decl
  = getDeclBinders new_name decl	`thenRn` \ maybe_avail ->
    case maybe_avail of
	Nothing    -> returnRn []		-- Instance decls and suchlike
	Just avail -> getDeclSysBinders new_sys_name decl		`thenRn_`  
		      returnRn [avail]
  where
	-- The getDeclSysBinders is just to get the names of superclass selectors
	-- etc, into the cache
    new_sys_name rdr_name loc = newImplicitBinder (rdrNameOcc rdr_name) loc

fixitiesFromLocalDecls :: GlobalRdrEnv -> [RdrNameHsDecl] -> RnMG FixityEnv
fixitiesFromLocalDecls gbl_env decls
  = foldlRn getFixities emptyNameEnv decls
  where
    getFixities :: FixityEnv -> RdrNameHsDecl -> RnMG FixityEnv
    getFixities acc (FixD fix)
      = fix_decl acc fix

    getFixities acc (TyClD (ClassDecl _ _ _ _ sigs _ _ _ _ _ _ _))
      = foldlRn fix_decl acc [sig | FixSig sig <- sigs]
		-- Get fixities from class decl sigs too.
    getFixities acc other_decl
      = returnRn acc

    fix_decl acc sig@(FixitySig rdr_name fixity loc)
	= 	-- Check for fixity decl for something not declared
	  case lookupRdrEnv gbl_env rdr_name of {
	    Nothing | opt_WarnUnusedBinds 
		    -> pushSrcLocRn loc (addWarnRn (unusedFixityDecl rdr_name fixity))
		       `thenRn_` returnRn acc 
		    | otherwise -> returnRn acc ;
	
	    Just (name:_) ->

		-- Check for duplicate fixity decl
	  case lookupNameEnv acc name of {
	    Just (FixitySig _ _ loc') -> addErrRn (dupFixityDecl rdr_name loc loc')
					 `thenRn_` returnRn acc ;

	    Nothing -> returnRn (addToNameEnv acc name (FixitySig name fixity loc))
	  }}
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
filterImports mod Nothing imports
  = returnRn (imports, [], emptyNameSet)

filterImports mod (Just (want_hiding, import_items)) avails
  = flatMapRn get_item import_items		`thenRn` \ avails_w_explicits ->
    let
	(item_avails, explicits_s) = unzip avails_w_explicits
	explicits		   = foldl addListToNameSet emptyNameSet explicits_s
    in
    if want_hiding 
    then	
	-- All imported; item_avails to be hidden
	returnRn (avails, item_avails, emptyNameSet)
    else
	-- Just item_avails imported; nothing to be hidden
	returnRn (item_avails, [], explicits)
  where
    import_fm :: FiniteMap OccName AvailInfo
    import_fm = listToFM [ (nameOccName name, avail) 
			 | avail <- avails,
			   name  <- availNames avail]
	-- Even though availNames returns data constructors too,
	-- they won't make any difference because naked entities like T
	-- in an import list map to TcOccs, not VarOccs.

    bale_out item = addErrRn (badImportItemErr mod item)	`thenRn_`
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

    ok_dotdot_item (AvailTC _ [n]) = False
    ok_dotdot_item other = True

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
	       -> [AvailInfo]		-- What's to be hidden
	       -> Avails		-- Whats imported and how
	       -> (Name -> Name) 	-- Improves the provenance on imported things
	       -> RnMG (GlobalRdrEnv, ExportAvails)
	-- NB: the Names in ExportAvails don't have the improve-provenance
	--     function applied to them
	-- We could fix that, but I don't think it matters

qualifyImports this_mod unqual_imp as_mod hides
	       avails improve_prov
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
	  env1 = addOneToGlobalRdrEnv env  (mkRdrQual qual_mod occ) better_name
	  env2 = addOneToGlobalRdrEnv env1 (mkRdrUnqual occ) 	    better_name
	  occ         = nameOccName name
	  better_name = improve_prov name

    del_avail env avail = foldl delOneFromGlobalRdrEnv env rdr_names
			where
			  rdr_names = map (mkRdrUnqual . nameOccName) (availNames avail)
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
                          then [IEVar main_RDR]
                               -- export Main.main *only* unless otherwise specified,
                          else [IEModuleContents this_mod]
                               -- but for all other modules export everything.

exportsFromAvail this_mod (Just export_items) 
		 (mod_avail_env, entity_avail_env)
	         global_name_env
  = foldlRn exports_from_item
	    ([], emptyFM, emptyAvailEnv) export_items	`thenRn` \ (_, _, export_avail_map) ->
    let
	export_avails :: [AvailInfo]
	export_avails   = nameEnvElts export_avail_map
    in
    returnRn export_avails

  where
    exports_from_item :: ExportAccum -> RdrNameIE -> RnMG ExportAccum

    exports_from_item acc@(mods, occs, avails) ie@(IEModuleContents mod)
	| mod `elem` mods 	-- Duplicate export of M
	= warnCheckRn opt_WarnDuplicateExports
		      (dupModuleExport mod)	`thenRn_`
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

    exports_from_item acc@(mods, occs, avails) ie
	| not (maybeToBool maybe_in_scope) 
	= failWithRn acc (unknownNameErr (ieName ie))

	| not (null dup_names)
	= addNameClashErrRn rdr_name (name:dup_names)	`thenRn_`
	  returnRn acc

#ifdef DEBUG
	-- I can't see why this should ever happen; if the thing is in scope
	-- at all it ought to have some availability
	| not (maybeToBool maybe_avail)
	= pprTrace "exportsFromAvail: curious Nothing:" (ppr name)
	  returnRn acc
#endif

	| not enough_avail
	= failWithRn acc (exportItemErr ie)

	| otherwise	-- Phew!  It's OK!  Now to check the occurrence stuff!


	= warnCheckRn (ok_item ie avail) (dodgyExportWarn ie)	`thenRn_`
          check_occs ie occs export_avail			`thenRn` \ occs' ->
	  returnRn (mods, occs', addAvail avails export_avail)

       where
	  rdr_name	  = ieName ie
          maybe_in_scope  = lookupFM global_name_env rdr_name
	  Just (name:dup_names) = maybe_in_scope
	  maybe_avail        = lookupUFM entity_avail_env name
	  Just avail         = maybe_avail
 	  maybe_export_avail = filterAvail ie avail
	  enough_avail	     = maybeToBool maybe_export_avail
	  Just export_avail  = maybe_export_avail

    ok_item (IEThingAll _) (AvailTC _ [n]) = False
		-- This occurs when you import T(..), but
		-- only export T abstractly.  The single [n]
		-- in the AvailTC is the type or class itself
    ok_item _ _ = True

check_occs :: RdrNameIE -> ExportOccMap -> AvailInfo -> RnMG ExportOccMap
check_occs ie occs avail 
  = foldlRn check occs (availNames avail)
  where
    check occs name
      = case lookupFM occs name_occ of
	  Nothing	    -> returnRn (addToFM occs name_occ (name, ie))
	  Just (name', ie') 
	    | name == name' -> 	-- Duplicate export
				warnCheckRn opt_WarnDuplicateExports
					    (dupExportWarn name_occ ie ie')
				`thenRn_` returnRn occs

	    | otherwise	    ->	-- Same occ name but different names: an error
				failWithRn occs (exportClashErr name_occ ie ie')
      where
	name_occ = nameOccName name
	
mk_export_fn :: NameSet -> (Name -> ExportFlag)
mk_export_fn exported_names
  = \name -> if name `elemNameSet` exported_names
	     then Exported
	     else NotExported
\end{code}

%************************************************************************
%*									*
\subsection{Errors}
%*									*
%************************************************************************

\begin{code}
badImportItemErr mod ie
  = sep [ptext SLIT("Module"), quotes (pprModuleName mod), 
	 ptext SLIT("does not export"), quotes (ppr ie)]

dodgyImportWarn mod item = dodgyMsg (ptext SLIT("import")) item
dodgyExportWarn     item = dodgyMsg (ptext SLIT("export")) item

dodgyMsg kind item@(IEThingAll tc)
  = sep [ ptext SLIT("The") <+> kind <+> ptext SLIT("item") <+> quotes (ppr item),
	  ptext SLIT("suggests that") <+> quotes (ppr tc) <+> ptext SLIT("has constructor or class methods"),
	  ptext SLIT("but it has none; it is a type synonym or abstract type or class") ]
	  
modExportErr mod
  = hsep [ ptext SLIT("Unknown module in export list: module"), quotes (pprModuleName mod)]

exportItemErr export_item
  = sep [ ptext SLIT("The export item") <+> quotes (ppr export_item),
	  ptext SLIT("attempts to export constructors or class methods that are not visible here") ]

exportClashErr occ_name ie1 ie2
  = hsep [ptext SLIT("The export items"), quotes (ppr ie1)
         ,ptext SLIT("and"), quotes (ppr ie2)
	 ,ptext SLIT("create conflicting exports for"), quotes (ppr occ_name)]

dupDeclErr (n:ns)
  = vcat [ptext SLIT("Multiple declarations of") <+> quotes (ppr n),
	  nest 4 (vcat (map pp sorted_ns))]
  where
    sorted_ns = sortLt occ'ed_before (n:ns)

    occ'ed_before a b = LT == compare (getSrcLoc a) (getSrcLoc b)

    pp n      = pprProvenance (getNameProvenance n)

dupExportWarn occ_name ie1 ie2
  = hsep [quotes (ppr occ_name), 
          ptext SLIT("is exported by"), quotes (ppr ie1),
          ptext SLIT("and"),            quotes (ppr ie2)]

dupModuleExport mod
  = hsep [ptext SLIT("Duplicate"),
	  quotes (ptext SLIT("Module") <+> pprModuleName mod), 
          ptext SLIT("in export list")]

unusedFixityDecl rdr_name fixity
  = hsep [ptext SLIT("Unused fixity declaration for"), quotes (ppr rdr_name)]

dupFixityDecl rdr_name loc1 loc2
  = vcat [ptext SLIT("Multiple fixity declarations for") <+> quotes (ppr rdr_name),
	  ptext SLIT("at ") <+> ppr loc1,
	  ptext SLIT("and") <+> ppr loc2]
\end{code}
