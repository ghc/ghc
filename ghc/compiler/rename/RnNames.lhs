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
			opt_SourceUnchanged
		      )

import HsSyn	( HsModule(..), ImportDecl(..), HsDecl(..), 
		  IE(..), ieName, 
		  ForeignDecl(..), ExtName(..), ForKind(..),
		  FixityDecl(..),
		  collectTopBinders
		)
import RdrHsSyn	( RdrName(..), RdrNameIE, RdrNameImportDecl,
		  RdrNameHsModule, RdrNameFixityDecl,
		  rdrNameOcc, ieOcc
		)
import RnIfaces	( getInterfaceExports, getDeclBinders, recordSlurp, checkUpToDate )
import BasicTypes ( IfaceFlavour(..) )
import RnEnv
import RnMonad

import FiniteMap
import PrelMods
import UniqFM	( lookupUFM )
import Bag	( bagToList )
import Maybes	( maybeToBool )
import Name
import NameSet	( elemNameSet )
import Outputable
import Util	( removeDups )
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
			       FiniteMap Name HowInScope, 	-- Locally defined or explicitly imported 
			       Name -> PrintUnqualified))
			-- Nothing => no need to recompile

getGlobalNames m@(HsModule this_mod _ exports imports _ _ mod_loc)
  = fixRn (\ ~(rec_exp_fn, _) ->

	-- PROCESS LOCAL DECLS
	-- Do these *first* so that the correct provenance gets
	-- into the global name cache.
      importsFromLocalDecls rec_exp_fn m	`thenRn` \ (local_rn_env, local_mod_avails, local_info) ->

	-- PROCESS IMPORT DECLS
      mapAndUnzip3Rn importsFromImportDecl all_imports
						`thenRn` \ (imp_rn_envs, imp_avails_s, explicit_imports_s) ->

	-- COMBINE RESULTS
	-- We put the local env second, so that a local provenance
	-- "wins", even if a module imports itself.
      foldlRn plusRnEnv emptyRnEnv imp_rn_envs		`thenRn` \ imp_rn_env ->
      plusRnEnv imp_rn_env local_rn_env	 		`thenRn` \ rn_env ->

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
      checkEarlyExit this_mod				`thenRn` \ up_to_date ->
      if up_to_date then
	returnRn (error "early exit", Nothing)
      else
 

	-- PROCESS EXPORT LISTS
      let
	 export_avails :: ExportAvails
	 export_avails = foldr plusExportAvails local_mod_avails imp_avails_s

	 explicit_info :: FiniteMap Name HowInScope  -- Locally defined or explicitly imported
	 explicit_info = foldr plusFM local_info explicit_imports_s
      in
      exportsFromAvail this_mod exports export_avails rn_env	
							`thenRn` \ (export_fn, export_env) ->

        -- BUILD THE "IMPORT FN".  It just tells whether a name is in
	-- scope in an unqualified form.
      let 
	  print_unqual = mkImportFn imp_rn_env
      in   

      returnRn (export_fn, Just (export_env, rn_env, explicit_info, print_unqual))
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
							 HiFile		{- Not source imported -}
							 Nothing	{- No "as" -}
							 Nothing	{- No import list -}
							 mod_loc]
    
    explicit_prelude_import
      = not (null [ () | (ImportDecl mod qual _ _ _ _) <- imports, mod == pRELUDE ])
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
		      -> RnMG (RnEnv, 
			       ExportAvails, 
			       FiniteMap Name HowInScope)  -- Records the explicitly-imported things

importsFromImportDecl (ImportDecl mod qual_only as_source as_mod import_spec loc)
  = pushSrcLocRn loc $
    getInterfaceExports mod as_source		`thenRn` \ (avails, fixities) ->
    filterImports mod import_spec avails	`thenRn` \ (filtered_avails, hides, explicits) ->
    let
	how_in_scope = FromImportDecl mod loc
	explicit_info = listToFM [(name, how_in_scope) 
				 | avail <- explicits,
				   name  <- availNames avail
				 ]
    in
    qualifyImports mod 
		   True 		-- Want qualified names
		   (not qual_only)	-- Maybe want unqualified names
		   as_mod
		   hides
		   filtered_avails (\n -> how_in_scope)
		   [ (occ,(fixity,how_in_scope)) | (occ,fixity) <- fixities ]
							`thenRn` \ (rn_env, mod_avails) ->
    returnRn (rn_env, mod_avails, explicit_info)
\end{code}


\begin{code}
importsFromLocalDecls rec_exp_fn (HsModule mod _ _ _ fix_decls decls _)
  = foldlRn getLocalDeclBinders [] decls		`thenRn` \ avails ->

	-- Record that locally-defined things are available
    mapRn (recordSlurp Nothing Compulsory) avails	`thenRn_`

	-- Fixities
    mapRn fixityFromFixDecl fix_decls			`thenRn` \ fixities ->

	-- Record where the available stuff came from
    let
	explicit_info = listToFM [(name, FromLocalDefn (getSrcLoc name))
				 | avail <- avails,
				   name  <- availNames avail
				 ]
    in
    qualifyImports mod 
		   False	-- Don't want qualified names
		   True		-- Want unqualified names
		   Nothing	-- No "as M" part
		   []		-- Hide nothing
		   avails (\n -> FromLocalDefn (getSrcLoc n))
		   fixities
							`thenRn` \ (rn_env, mod_avails) ->
    returnRn (rn_env, mod_avails, explicit_info)
  where
    newLocalName rdr_name loc
      = newLocallyDefinedGlobalName mod (rdrNameOcc rdr_name) rec_exp_fn loc

    getLocalDeclBinders avails (ValD binds)
      = mapRn do_one (bagToList (collectTopBinders binds))	`thenRn` \ val_avails ->
	returnRn (val_avails ++ avails)

    -- foreign import declaration
    getLocalDeclBinders avails (ForD (ForeignDecl nm (FoImport _) _ _ _ loc))
      = do_one (nm,loc)			    `thenRn` \ for_avail ->
	returnRn (for_avail : avails)

    -- foreign import declaration
    getLocalDeclBinders avails (ForD (ForeignDecl nm FoLabel _ _ _ loc))
      = do_one (nm,loc)			    `thenRn` \ for_avail ->
	returnRn (for_avail : avails)

    -- foreign export dynamic declaration
    getLocalDeclBinders avails (ForD (ForeignDecl nm FoExport _ Dynamic _ loc))
      = do_one (nm,loc)			    `thenRn` \ for_avail ->
	returnRn (for_avail : avails)

    getLocalDeclBinders avails decl
      = getDeclBinders newLocalName decl	`thenRn` \ avail ->
	case avail of
	   NotAvailable -> returnRn avails		-- Instance decls and suchlike
	   other	-> returnRn (avail : avails)

    do_one (rdr_name, loc)
      = newLocalName rdr_name loc 	`thenRn` \ name ->
        returnRn (Avail name)
\end{code}

%************************************************************************
%*									*
\subsection{Filtering imports}
%*									*
%************************************************************************

@filterImports@ takes the @ExportEnv@ telling what the imported module makes
available, and filters it through the import spec (if any).

\begin{code}
filterImports :: Module
	      -> Maybe (Bool, [RdrNameIE])		-- Import spec; True => hidin
	      -> [AvailInfo]				-- What's available
	      -> RnMG ([AvailInfo],			-- What's actually imported
		       [AvailInfo],			-- What's to be hidden (the unqualified version, that is)
		       [AvailInfo])			-- What was imported explicitly

	-- Complains if import spec mentions things that the module doesn't export
        -- Warns/informs if import spec contains duplicates.
filterImports mod Nothing imports
  = returnRn (imports, [], [])

filterImports mod (Just (want_hiding, import_items)) avails
  = mapRn check_item import_items		`thenRn` \ item_avails ->
    if want_hiding 
    then	
	returnRn (avails, item_avails, [])	-- All imported; item_avails to be hidden
    else
	returnRn (item_avails, [], item_avails)	-- Just item_avails imported; nothing to be hidden

  where
    import_fm :: FiniteMap OccName AvailInfo
    import_fm = listToFM [ (nameOccName name, avail) 
			 | avail <- avails,
			   name  <- availEntityNames avail]

    check_item item@(IEModuleContents _)
      = addErrRn (badImportItemErr mod item)	`thenRn_`
	returnRn NotAvailable

    check_item item
      | not (maybeToBool maybe_in_import_avails) ||
	(case filtered_avail of { NotAvailable -> True; other -> False })
      = addErrRn (badImportItemErr mod item)	`thenRn_`
	returnRn NotAvailable

      | dodgy_import = addWarnRn (dodgyImportWarn mod item)	`thenRn_`
		       returnRn filtered_avail

      | otherwise    = returnRn filtered_avail
		
      where
	maybe_in_import_avails = lookupFM import_fm (ieOcc item)
	Just avail	       = maybe_in_import_avails
	filtered_avail	       = filterAvail item avail
	dodgy_import 	       = case (item, avail) of
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
qualifyImports :: Module				-- Imported module
	       -> Bool					-- True <=> want qualified import
	       -> Bool					-- True <=> want unqualified import
	       -> Maybe Module				-- Optional "as M" part 
	       -> [AvailInfo]				-- What's to be hidden
	       -> Avails -> (Name -> HowInScope)	-- Whats imported and how
	       -> [(OccName, (Fixity, HowInScope))]	-- Ditto for fixities
	       -> RnMG (RnEnv, ExportAvails)

qualifyImports this_mod qual_imp unqual_imp as_mod hides
	       avails name_to_his fixities
  = 
 	-- Make the name environment.  Even though we're talking about a 
	-- single import module there might still be name clashes, 
	-- because it might be the module being compiled.
    foldlRn add_avail emptyGlobalNameEnv avails	`thenRn` \ name_env1 ->
    let
	-- Delete things that are hidden
	name_env2 = foldl del_avail name_env1 hides

	-- Create the fixity env
	fixity_env = foldl (add_fixity name_env2) emptyFixityEnv fixities

	-- Create the export-availability info
	export_avails = mkExportAvails qual_mod unqual_imp name_env2 avails
    in
    returnRn (RnEnv name_env2 fixity_env, export_avails)
  where
    qual_mod = case as_mod of
		  Nothing  	    -> this_mod
		  Just another_name -> another_name

    add_avail :: GlobalNameEnv -> AvailInfo -> RnMG GlobalNameEnv
    add_avail env avail = foldlRn add_name env (availNames avail)

    add_name env name   = add qual_imp   env  (Qual qual_mod occ err_hif) `thenRn` \ env1 ->
			  add unqual_imp env1 (Unqual occ)
			where
			  add False env rdr_name = returnRn env
			  add True  env rdr_name = addOneToGlobalNameEnv env rdr_name (name, name_to_his name)
			  occ  = nameOccName name

    del_avail env avail = foldl delOneFromGlobalNameEnv env rdr_names
			where
			  rdr_names = map (Unqual . nameOccName) (availNames avail)
			
    add_fixity name_env fix_env (occ_name, fixity)
	= add qual $ add unqual $ fix_env
 	where
	  qual   = Qual qual_mod occ_name err_hif
	  unqual = Unqual occ_name

	  add rdr_name fix_env | maybeToBool (lookupFM name_env rdr_name)
			       = addOneToFixityEnv fix_env rdr_name fixity
			       | otherwise
			       = fix_env

err_hif = error "qualifyImports: hif"	-- Not needed in key to mapping
\end{code}

unQualify adds an Unqual binding for every existing Qual binding.

\begin{code}
unQualify :: FiniteMap RdrName elt -> FiniteMap RdrName elt
unQualify fm = addListToFM fm [(Unqual occ, elt) | (Qual _ occ _, elt) <- fmToList fm]
\end{code}

%************************************************************************
%*									*
\subsection{Local declarations}
%*									*
%************************************************************************


\begin{code}
fixityFromFixDecl :: RdrNameFixityDecl -> RnMG (OccName, (Fixity, HowInScope))

fixityFromFixDecl (FixityDecl rdr_name fixity loc)
  = returnRn (rdrNameOcc rdr_name, (fixity, FromLocalDefn loc))
\end{code}


%************************************************************************
%*									*
\subsection{Export list processing
%*									*
%************************************************************************

The @AvailEnv@ type is just used internally in @exportsFromAvail@.
When exporting we need to combine the availabilities for a particular
exported thing, and we also need to check for name clashes -- that
is: two exported things must have different @OccNames@.

\begin{code}
type AvailEnv = FiniteMap OccName (RdrNameIE, AvailInfo, Int{-no. of clashes-})
	-- The FM maps each OccName to the RdrNameIE that gave rise to it,
	-- for error reporting, as well as to its AvailInfo

emptyAvailEnv = emptyFM

{-
 Add new entry to environment. Checks for name clashes, i.e.,
 plain duplicates or exported entity pairs that have different OccNames.
 (c.f. 5.1.1 of Haskell 1.4 report.)
-}
addAvailEnv :: Bool -> RdrNameIE -> AvailEnv -> AvailInfo -> RnM s d AvailEnv
addAvailEnv warn_dups ie env NotAvailable   = returnRn env
addAvailEnv warn_dups ie env (AvailTC _ []) = returnRn env
addAvailEnv warn_dups ie env avail
  | warn_dups = mapMaybeRn (addErrRn . availClashErr) () conflict `thenRn_`
                returnRn (addToFM_C addAvail env key elt)
  | otherwise = returnRn (addToFM_C addAvail env key elt)
  where
   occ_avail = nameOccName (availName avail)
   occ_ie    = ieOcc ie
   key
    | not warn_dups || occ_ie == occ_avail = occ_avail
    | otherwise                            = occ_ie 
        -- export item is a class method, use export occ name instead.
        -- (this is only needed to get more precise warnings about
	--  duplicates.)
   elt  = (ie,avail,reports_on)

   reports_on
    | maybeToBool dup = 1
    | otherwise       = 0

   conflict = conflictFM bad_avail env key elt
   dup 
    | warn_dups = conflictFM dup_avail env key elt
    | otherwise = Nothing

addListToAvailEnv :: AvailEnv -> RdrNameIE -> [AvailInfo] -> RnM s d AvailEnv
addListToAvailEnv env ie items 
  = foldlRn (addAvailEnv False{-don't warn about dups-} ie) env items

bad_avail  (ie1,avail1,r1) (ie2,avail2,r2) 
   = availName avail1 /= availName avail2  -- Same OccName, different Name
dup_avail  (ie1,avail1,r1) (ie2,avail2,r2) 
   = availName avail1 == availName avail2 -- Same OccName & avail.

addAvail (ie1,a1,r1) (ie2,a2,r2) = (ie1, a1 `plusAvail` a2, r1 + r2)
\end{code}

Processing the export list.

You might think that we should record things that appear in the export list as
``occurrences'' (using addOccurrenceName), but you'd be wrong.  We do check (here)
that they are in scope, but there is no need to slurp in their actual declaration
(which is what addOccurrenceName forces).  Indeed, doing so would big trouble when
compiling PrelBase, because it re-exports GHC, which includes takeMVar#, whose type
includes ConcBase.StateAndSynchVar#, and so on...

\begin{code}
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
  = checkForModuleExportDups export_items                 `thenRn` \ export_items' ->
    foldlRn exports_from_item emptyAvailEnv export_items' `thenRn` \ export_avail_env ->
    let
     dup_entries = fmToList (filterFM (\ _ (_,_,clashes) -> clashes > 0) export_avail_env)
    in
    mapRn (addWarnRn . dupExportWarn) dup_entries         `thenRn_`
    let
	export_avails   = map (\ (_,a,_) -> a) (eltsFM export_avail_env)
	export_fixities = mk_exported_fixities (availsToNameSet export_avails)
	export_fn	= mk_export_fn export_avails
    in
    returnRn (export_fn, ExportEnv export_avails export_fixities)

  where
    exports_from_item :: AvailEnv -> RdrNameIE -> RnMG AvailEnv
    exports_from_item export_avail_env ie@(IEModuleContents mod)
	= case lookupFM mod_avail_env mod of
		Nothing	    -> failWithRn export_avail_env (modExportErr mod)
		Just avails -> addListToAvailEnv export_avail_env ie avails

    exports_from_item export_avail_env ie
	| not (maybeToBool maybe_in_scope) 
	= failWithRn export_avail_env (unknownNameErr (ieName ie))

#ifdef DEBUG
	-- I can't see why this should ever happen; if the thing is in scope
	-- at all it ought to have some availability
	| not (maybeToBool maybe_avail)
	= pprTrace "exportsFromAvail: curious Nothing:" (ppr name)
	  returnRn export_avail_env
#endif

	| not enough_avail
	= failWithRn export_avail_env (exportItemErr ie export_avail)

	| otherwise	-- Phew!  It's OK!
	= addAvailEnv opt_WarnDuplicateExports ie export_avail_env export_avail
       where
          maybe_in_scope  = lookupFM global_name_env (ieName ie)
	  Just (name,_)	  = maybe_in_scope
	  maybe_avail     = lookupUFM entity_avail_env name
	  Just avail      = maybe_avail
 	  export_avail    = filterAvail ie avail
	  enough_avail	  = case export_avail of {NotAvailable -> False; other -> True}

	-- We export a fixity iff we export a thing with the same (qualified) RdrName
    mk_exported_fixities :: NameSet -> [(OccName, Fixity)]
    mk_exported_fixities exports
	= fmToList (foldr (perhaps_add_fixity exports) 
			  emptyFM
			  (fmToList fixity_env))

    perhaps_add_fixity :: NameSet -> (RdrName, (Fixity, HowInScope))
		       -> FiniteMap OccName Fixity
		       -> FiniteMap OccName Fixity
    perhaps_add_fixity exports (rdr_name, (fixity, how_in_scope)) fix_env
      =  let
	    do_nothing = fix_env		-- The default is to pass on the env unchanged
	 in
      	 	-- Step 1: check whether the rdr_name is in scope; if so find its Name
	 case lookupFM global_name_env rdr_name of {
	   Nothing 	        -> do_nothing;
	   Just (fixity_name,_) -> 

		-- Step 2: check whether the fixity thing is exported
	 if not (fixity_name `elemNameSet` exports) then
		do_nothing
	 else
	
		-- Step 3: check whether we already have a fixity for the
		-- Name's OccName in the fix_env we are building up.  This can easily
		-- happen.  the original fixity_env might contain bindings for
		--	M.a and N.a, if a was imported via M and N.
		-- If this does happen, we expect the fixity to be the same either way.
	let
	    occ_name = rdrNameOcc rdr_name
	in
	case lookupFM fix_env occ_name of {
	  Just fixity1 -> 	-- Got it already
			   ASSERT( fixity == fixity1 )
			   do_nothing;
	  Nothing -> 

		-- Step 3: add it to the outgoing fix_env
	addToFM fix_env occ_name fixity
	}}

{- warn and weed out duplicate module entries from export list. -}
checkForModuleExportDups :: [RdrNameIE] -> RnMG [RdrNameIE]
checkForModuleExportDups ls 
  | opt_WarnDuplicateExports = check_modules ls
  | otherwise                = returnRn ls
  where
   -- NOTE: reorders the export list by moving all module-contents
   -- exports to the end (removing duplicates in the process.)
   check_modules ls = 
     (case dups of
        [] -> returnRn ()
        ls -> mapRn (\ ds@(IEModuleContents x:_) -> 
                       addWarnRn (dupModuleExport x (length ds))) ls `thenRn_`
              returnRn ()) `thenRn_`
     returnRn (ls_no_modules ++ no_module_dups)
     where
      (ls_no_modules,modules) = foldr split_mods ([],[]) ls

      split_mods i@(IEModuleContents _) (no_ms,ms) = (no_ms,i:ms)
      split_mods i (no_ms,ms) = (i:no_ms,ms)

      (no_module_dups, dups) = removeDups cmp_mods modules

      cmp_mods (IEModuleContents m1) (IEModuleContents m2) = m1 `compare` m2
  
mk_export_fn :: [AvailInfo] -> (Name -> ExportFlag)
mk_export_fn avails
  = \name -> if name `elemNameSet` exported_names
	     then Exported
	     else NotExported
  where
    exported_names :: NameSet
    exported_names = availsToNameSet avails
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

exportItemErr export_item NotAvailable
  = sep [ ptext SLIT("Export item not in scope:"), quotes (ppr export_item)]

exportItemErr export_item avail
  = hang (ptext SLIT("Export item not fully in scope:"))
	   4 (vcat [hsep [ptext SLIT("Wanted:   "), ppr export_item],
		    hsep [ptext SLIT("Available:"), ppr (ieOcc export_item), pprAvail avail]])

availClashErr (occ_name, ((ie1,avail1,_), (ie2,avail2,_)))
  = hsep [ptext SLIT("The export items"), quotes (ppr ie1), ptext SLIT("and"), quotes (ppr ie2),
	  ptext SLIT("create conflicting exports for"), quotes (ppr occ_name)]

dupExportWarn (occ_name, (_,_,times))
  = hsep [quotes (ppr occ_name), 
          ptext SLIT("mentioned"), speakNTimes (times+1),
          ptext SLIT("in export list")]

dupModuleExport mod times
  = hsep [ptext SLIT("Module"), quotes (pprModule mod), 
          ptext SLIT("mentioned"), speakNTimes times,
          ptext SLIT("in export list")]
\end{code}

