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

import HsSyn	( HsModule(..), ImportDecl(..), HsDecl(..), TyClDecl(..),
		  IE(..), ieName, 
		  ForeignDecl(..), ForKind(..), isDynamic,
		  FixitySig(..), Sig(..),
		  collectTopBinders
		)
import RdrHsSyn	( RdrNameIE, RdrNameImportDecl,
		  RdrNameHsModule, RdrNameHsDecl
		)
import RnIfaces	( getInterfaceExports, getDeclBinders, getImportedFixities, 
		  recordSlurp, checkUpToDate, loadHomeInterface
		)
import RnEnv
import RnMonad

import FiniteMap
import PrelMods
import UniqFM	( lookupUFM )
import Bag	( bagToList )
import Maybes	( maybeToBool )
import NameSet
import Name
import RdrName	( RdrName, rdrNameOcc, mkRdrQual, mkRdrUnqual )
import SrcLoc	( SrcLoc )
import NameSet	( elemNameSet, emptyNameSet )
import Outputable
import Unique	( getUnique )
import Util	( removeDups, equivClassesByUniq, sortLt )
import List	( nubBy )
\end{code}



%************************************************************************
%*									*
\subsection{Get global names}
%*									*
%************************************************************************

\begin{code}
getGlobalNames :: RdrNameHsModule
	       -> RnMG (Maybe (ExportEnv, 
			       RnEnv,
			       NameEnv AvailInfo	-- Maps a name to its parent AvailInfo
							-- Just for in-scope things only
			       ))
			-- Nothing => no need to recompile

getGlobalNames (HsModule this_mod _ exports imports decls mod_loc)
  = 	-- These two fix-loops are to get the right
	-- provenance information into a Name
    fixRn (\ ~(rec_exp_fn, _) ->

      fixRn (\ ~(rec_rn_env, _) ->
	let
	   rec_unqual_fn :: Name -> Bool	-- Is this chap in scope unqualified?
	   rec_unqual_fn = unQualInScope rec_rn_env
	in
	setOmitQualFn rec_unqual_fn		$

		-- PROCESS LOCAL DECLS
		-- Do these *first* so that the correct provenance gets
		-- into the global name cache.
	importsFromLocalDecls this_mod rec_exp_fn decls	`thenRn` \ (local_gbl_env, local_mod_avails) ->

		-- PROCESS IMPORT DECLS
	mapAndUnzipRn importsFromImportDecl all_imports	`thenRn` \ (imp_gbl_envs, imp_avails_s) ->

		-- COMBINE RESULTS
		-- We put the local env second, so that a local provenance
		-- "wins", even if a module imports itself.
	let
	    gbl_env :: GlobalRdrEnv
	    imp_gbl_env = foldr plusGlobalRdrEnv emptyRdrEnv imp_gbl_envs
	    gbl_env     = imp_gbl_env `plusGlobalRdrEnv` local_gbl_env

	    export_avails :: ExportAvails
	    export_avails = foldr plusExportAvails local_mod_avails imp_avails_s
	in
	returnRn (gbl_env, export_avails)
      )							`thenRn` \ (gbl_env, export_avails) ->

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
	-- why we wait till after the plusRnEnv stuff to do the early-exit.
      checkEarlyExit this_mod			`thenRn` \ up_to_date ->
      if up_to_date then
	returnRn (junk_exp_fn, Nothing)
      else
 
	-- FIXITIES
      fixitiesFromLocalDecls gbl_env decls		`thenRn` \ local_fixity_env ->
      getImportedFixities				`thenRn` \ imp_fixity_env ->
      let
	fixity_env = imp_fixity_env `plusNameEnv` local_fixity_env
	rn_env     = RnEnv gbl_env fixity_env
	(_, global_avail_env) = export_avails
      in
      traceRn (text "fixity env" <+> vcat (map ppr (nameEnvElts fixity_env)))	`thenRn_`

	-- PROCESS EXPORT LISTS
      exportsFromAvail this_mod exports export_avails rn_env	`thenRn` \ (export_fn, export_env) ->

	-- DONE
      returnRn (export_fn, Just (export_env, rn_env, global_avail_env))
    )							`thenRn` \ (_, result) ->
    returnRn result
  where
    junk_exp_fn = error "RnNames:export_fn"

    all_imports = prel_imports ++ imports

	-- NB: opt_NoImplicitPrelude is slightly different to import Prelude ();
	-- because the former doesn't even look at Prelude.hi for instance declarations,
	-- whereas the latter does.
    prel_imports | this_mod == pRELUDE ||
		   explicit_prelude_import ||
		   opt_NoImplicitPrelude
		 = []

		 | otherwise		   = [ImportDecl pRELUDE 
							 False		{- Not qualified -}
							 Nothing	{- No "as" -}
							 Nothing	{- No import list -}
							 mod_loc]
    
    explicit_prelude_import
      = not (null [ () | (ImportDecl mod qual _ _ _) <- imports, mod == pRELUDE ])
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
    putDocRn (text "Compilation" <+> 
	      text (if up_to_date then "IS NOT" else "IS") <+>
	      text "required")					`thenRn_`
    returnRn up_to_date
\end{code}
	
\begin{code}
importsFromImportDecl :: RdrNameImportDecl
		      -> RnMG (GlobalRdrEnv, 
			       ExportAvails) 

importsFromImportDecl (ImportDecl imp_mod qual_only as_mod import_spec iloc)
  = pushSrcLocRn iloc $
    getInterfaceExports imp_mod 	`thenRn` \ avails ->

    if null avails then
	-- If there's an error in getInterfaceExports, (e.g. interface
	-- file not found) we get lots of spurious errors from 'filterImports'
	returnRn (emptyRdrEnv, mkEmptyExportAvails imp_mod)
    else

    filterImports imp_mod import_spec avails	`thenRn` \ (filtered_avails, hides, explicits) ->

	-- Load all the home modules for the things being
	-- bought into scope.  This makes sure their fixities
	-- are loaded before we grab the FixityEnv from Ifaces
    let
	home_modules = [name | avail <- filtered_avails,
				-- Doesn't take account of hiding, but that doesn't matter
		
			       let name = availName avail,
			       not (isLocallyDefined name || nameModule name == imp_mod)
				-- Don't try to load the module being compiled
				--	(this can happen in mutual-recursion situations)
				-- or from the module being imported (it's already loaded)
			]
				
	same_module n1 n2 = nameModule n1 == nameModule n2
	load n		  = loadHomeInterface (doc_str n) n
	doc_str n	  = ptext SLIT("Need fixities from") <+> ppr (nameModule n) <+> parens (ppr n)
    in
    mapRn load (nubBy same_module home_modules)			`thenRn_`
    
	-- We 'improve' the provenance by setting
	--	(a) the import-reason field, so that the Name says how it came into scope
	--		including whether it's explicitly imported
	--	(b) the print-unqualified field
	-- But don't fiddle with wired-in things or we get in a twist
    let
	improve_prov name = setNameImportReason name (UserImport imp_mod iloc (is_explicit name))
	is_explicit name  = name `elemNameSet` explicits
    in
    qualifyImports imp_mod 
		   (not qual_only)	-- Maybe want unqualified names
		   as_mod hides
		   filtered_avails improve_prov		`thenRn` \ (rdr_name_env, mod_avails) ->

    returnRn (rdr_name_env, mod_avails)
\end{code}


\begin{code}
importsFromLocalDecls mod rec_exp_fn decls
  = mapRn (getLocalDeclBinders newLocalName) decls	`thenRn` \ avails_s ->

    let
	avails = concat avails_s

	all_names :: [Name]	-- All the defns; no dups eliminated
	all_names = [name | avail <- avails, name <- availNames avail]

	dups :: [[Name]]
	dups = filter non_singleton (equivClassesByUniq getUnique all_names)
	     where
		non_singleton (x1:x2:xs) = True
		non_singleton other      = False
    in
	-- Check for duplicate definitions
    mapRn (addErrRn . dupDeclErr) dups				`thenRn_` 

	-- Record that locally-defined things are available
    mapRn (recordSlurp Nothing Compulsory) avails	`thenRn_`

	-- Build the environment
    qualifyImports mod 
		   True		-- Want unqualified names
		   Nothing	-- no 'as M'
		   []		-- Hide nothing
		   avails
		   (\n -> n)

  where
    newLocalName rdr_name loc = newLocallyDefinedGlobalName mod (rdrNameOcc rdr_name)
							    rec_exp_fn loc

getLocalDeclBinders :: (RdrName -> SrcLoc -> RnMG Name)	-- New-name function
		    -> RdrNameHsDecl
		    -> RnMG Avails
getLocalDeclBinders new_name (ValD binds)
  = mapRn do_one (bagToList (collectTopBinders binds))
  where
    do_one (rdr_name, loc) = new_name rdr_name loc	`thenRn` \ name ->
			     returnRn (Avail name)

    -- foreign declarations
getLocalDeclBinders new_name (ForD (ForeignDecl nm kind _ dyn _ loc))
  | binds_haskell_name kind dyn
  = new_name nm loc		    `thenRn` \ name ->
    returnRn [Avail name]

  | otherwise
  = returnRn []

getLocalDeclBinders new_name decl
  = getDeclBinders new_name decl	`thenRn` \ maybe_avail ->
    case maybe_avail of
	Nothing    -> returnRn []		-- Instance decls and suchlike
	Just avail -> returnRn [avail]

binds_haskell_name (FoImport _) _   = True
binds_haskell_name FoLabel      _   = True
binds_haskell_name FoExport  ext_nm = isDynamic ext_nm

fixitiesFromLocalDecls :: GlobalRdrEnv -> [RdrNameHsDecl] -> RnMG FixityEnv
fixitiesFromLocalDecls gbl_env decls
  = foldlRn getFixities emptyNameEnv decls
  where
    getFixities :: FixityEnv -> RdrNameHsDecl -> RnMG FixityEnv
    getFixities acc (FixD fix)
      = fix_decl acc fix

    getFixities acc (TyClD (ClassDecl _ _ _ sigs _ _ _ _ _))
      = foldlRn fix_decl acc [sig | FixSig sig <- sigs]
		-- Get fixities from class decl sigs too

    getFixities acc other_decl
      = returnRn acc

    fix_decl acc (FixitySig rdr_name fixity loc)
	= 	-- Check for fixity decl for something not declared
	  case lookupRdrEnv gbl_env rdr_name of {
	    Nothing | opt_WarnUnusedBinds 
		    -> pushSrcLocRn loc (addWarnRn (unusedFixityDecl rdr_name fixity))	`thenRn_`
		       returnRn acc 
		    | otherwise -> returnRn acc ;
	
	    Just (name:_) ->

		-- Check for duplicate fixity decl
	  case lookupNameEnv acc name of {
	    Just (FixitySig _ _ loc') -> addErrRn (dupFixityDecl rdr_name loc loc')	`thenRn_`
					 returnRn acc ;


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
filterImports :: Module				-- The module being imported
	      -> Maybe (Bool, [RdrNameIE])	-- Import spec; True => hiding
	      -> [AvailInfo]			-- What's available
	      -> RnMG ([AvailInfo],		-- What's actually imported
		       [AvailInfo],		-- What's to be hidden (the unqualified version, that is)
		       NameSet)			-- What was imported explicitly

	-- Complains if import spec mentions things that the module doesn't export
        -- Warns/informs if import spec contains duplicates.
filterImports mod Nothing imports
  = returnRn (imports, [], emptyNameSet)

filterImports mod (Just (want_hiding, import_items)) avails
  = mapMaybeRn check_item import_items		`thenRn` \ avails_w_explicits ->
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

    check_item item@(IEModuleContents _)
      = addErrRn (badImportItemErr mod item)	`thenRn_`
	returnRn Nothing

    check_item item
      | not (maybeToBool maybe_in_import_avails) ||
	not (maybeToBool maybe_filtered_avail)
      = addErrRn (badImportItemErr mod item)	`thenRn_`
	returnRn Nothing

      | dodgy_import = addWarnRn (dodgyImportWarn mod item)	`thenRn_`
		       returnRn (Just (filtered_avail, explicits))

      | otherwise    = returnRn (Just (filtered_avail, explicits))
		
      where
 	wanted_occ	       = rdrNameOcc (ieName item)
	maybe_in_import_avails = lookupFM import_fm wanted_occ

	Just avail	       = maybe_in_import_avails
	maybe_filtered_avail   = filterAvail item avail
	Just filtered_avail    = maybe_filtered_avail
	explicits	       | dot_dot   = [availName filtered_avail]
			       | otherwise = availNames filtered_avail

	dot_dot = case item of 
		    IEThingAll _    -> True
		    other	    -> False

	dodgy_import = case (item, avail) of
			  (IEThingAll _, AvailTC _ [n]) -> True
				-- This occurs when you import T(..), but
				-- only export T abstractly.  The single [n]
				-- in the AvailTC is the type or class itself
					
			  other -> False
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
qualifyImports :: Module		-- Imported module
	       -> Bool			-- True <=> want unqualified import
	       -> Maybe Module		-- Optional "as M" part 
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
\subsection{Export list processing
%*									*
%************************************************************************

Processing the export list.

You might think that we should record things that appear in the export list as
``occurrences'' (using addOccurrenceName), but you'd be wrong.  We do check (here)
that they are in scope, but there is no need to slurp in their actual declaration
(which is what addOccurrenceName forces).  Indeed, doing so would big trouble when
compiling PrelBase, because it re-exports GHC, which includes takeMVar#, whose type
includes ConcBase.StateAndSynchVar#, and so on...

\begin{code}
type ExportAccum	-- The type of the accumulating parameter of
			-- the main worker function in exportsFromAvail
     = ([Module], 		-- 'module M's seen so far
	ExportOccMap,		-- Tracks exported occurrence names
	NameEnv AvailInfo)	-- The accumulated exported stuff, kept in an env
				--   so we can common-up related AvailInfos

type ExportOccMap = FiniteMap OccName (Name, RdrNameIE)
	-- Tracks what a particular exported OccName
	--   in an export list refers to, and which item
	--   it came from.  It's illegal to export two distinct things
	--   that have the same occurrence name


exportsFromAvail :: Module
		 -> Maybe [RdrNameIE]	-- Export spec
		 -> ExportAvails
		 -> RnEnv
		 -> RnMG (Name -> ExportFlag, ExportEnv)
	-- Complains if two distinct exports have same OccName
        -- Warns about identical exports.
	-- Complains about exports items not in scope
exportsFromAvail this_mod Nothing export_avails rn_env
  = exportsFromAvail this_mod (Just [IEModuleContents this_mod]) export_avails rn_env

exportsFromAvail this_mod (Just export_items) 
		 (mod_avail_env, entity_avail_env)
	         (RnEnv global_name_env fixity_env)
  = foldlRn exports_from_item
	    ([], emptyFM, emptyNameEnv) export_items	`thenRn` \ (_, _, export_avail_map) ->
    let
	export_avails :: [AvailInfo]
	export_avails   = nameEnvElts export_avail_map

	export_names :: NameSet
        export_names = availsToNameSet export_avails

	-- Export only those fixities that are for names that are
	--	(a) defined in this module
	--	(b) exported
	export_fixities :: [(Name,Fixity)]
	export_fixities = [ (name,fixity) 
			  | FixitySig name fixity _ <- nameEnvElts fixity_env,
			    name `elemNameSet` export_names,
			    isLocallyDefined name
			  ]

	export_fn :: Name -> ExportFlag
	export_fn = mk_export_fn export_names
    in
    returnRn (export_fn, ExportEnv export_avails export_fixities)

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
		Just mod_avails -> foldlRn (check_occs ie) occs mod_avails	`thenRn` \ occs' ->
				   let
					avails' = foldl add_avail avails mod_avails
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
	= check_occs ie occs export_avail	`thenRn` \ occs' ->
	  returnRn (mods, occs', add_avail avails export_avail)

       where
	  rdr_name	  = ieName ie
          maybe_in_scope  = lookupFM global_name_env rdr_name
	  Just (name:dup_names) = maybe_in_scope
	  maybe_avail        = lookupUFM entity_avail_env name
	  Just avail         = maybe_avail
 	  maybe_export_avail = filterAvail ie avail
	  enough_avail	     = maybeToBool maybe_export_avail
	  Just export_avail  = maybe_export_avail

add_avail avails avail = addToNameEnv_C plusAvail avails (availName avail) avail

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
					    (dupExportWarn name_occ ie ie')	`thenRn_`
				returnRn occs

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
  = sep [ptext SLIT("Module"), quotes (pprModule mod), 
	 ptext SLIT("does not export"), quotes (ppr ie)]

dodgyImportWarn mod (IEThingAll tc)
  = sep [ptext SLIT("Module") <+> quotes (pprModule mod) <+> ptext SLIT("exports") <+> quotes (ppr tc), 
	 ptext SLIT("with no constructors/class operations;"),
	 ptext SLIT("yet it is imported with a (..)")]

modExportErr mod
  = hsep [ ptext SLIT("Unknown module in export list: module"), quotes (pprModule mod)]

exportItemErr export_item
  = sep [ ptext SLIT("Bad export item"), quotes (ppr export_item)]

exportClashErr occ_name ie1 ie2
  = hsep [ptext SLIT("The export items"), quotes (ppr ie1), ptext SLIT("and"), quotes (ppr ie2),
	  ptext SLIT("create conflicting exports for"), quotes (ppr occ_name)]

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
	  quotes (ptext SLIT("Module") <+> pprModule mod), 
          ptext SLIT("in export list")]

unusedFixityDecl rdr_name fixity
  = hsep [ptext SLIT("Unused fixity declaration for"), quotes (ppr rdr_name)]

dupFixityDecl rdr_name loc1 loc2
  = vcat [ptext SLIT("Multiple fixity declarations for") <+> quotes (ppr rdr_name),
	  ptext SLIT("at ") <+> ppr loc1,
	  ptext SLIT("and") <+> ppr loc2]

\end{code}
