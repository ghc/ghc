%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnNames]{Extracting imported and top-level names in scope}

\begin{code}
#include "HsVersions.h"

module RnNames (
	getGlobalNames
    ) where

IMP_Ubiq()

import CmdLineOpts	( opt_SourceUnchanged, opt_NoImplicitPrelude )
import HsSyn	( HsModule(..), HsDecl(..), FixityDecl(..), Fixity, Fake, InPat, IE(..), HsTyVar,
		  TyDecl, ClassDecl, InstDecl, DefaultDecl, ImportDecl(..), HsBinds, IfaceSig
		)
import HsBinds	( collectTopBinders )
import HsImpExp	( ieName )
import RdrHsSyn	( RdrNameHsDecl(..), RdrName(..), RdrNameIE(..), SYN_IE(RdrNameImportDecl),
		  SYN_IE(RdrNameHsModule), SYN_IE(RdrNameFixityDecl),
		  rdrNameOcc, ieOcc
		)
import RnHsSyn	( RenamedHsModule(..), RenamedFixityDecl(..) )
import RnIfaces	( getInterfaceExports, getDeclBinders, checkUpToDate, recordSlurp )
import RnEnv
import RnMonad
import FiniteMap
import PrelMods
import UniqFM	( UniqFM, emptyUFM, addListToUFM_C, lookupUFM )
import Bag	( Bag, bagToList )
import Maybes	( maybeToBool, expectJust )
import Name
import Pretty
import PprStyle	( PprStyle(..) )
import Util	( panic, pprTrace, assertPanic )
#if __GLASGOW_HASKELL__ >= 202
import Outputable
#endif
\end{code}



%************************************************************************
%*									*
\subsection{Get global names}
%*									*
%************************************************************************

\begin{code}
getGlobalNames :: RdrNameHsModule
	       -> RnMG (Maybe (ExportEnv, RnEnv, NameSet))
			-- Nothing <=> no need to recompile
			-- The NameSet is the set of names that are
			--	either locally defined,
			--	or explicitly imported

getGlobalNames m@(HsModule this_mod _ exports imports _ _ mod_loc)
  = fixRn (\ ~(rec_exp_fn, _) ->

	-- PROCESS LOCAL DECLS
	-- Do these *first* so that the correct provenance gets
	-- into the global name cache.
      importsFromLocalDecls rec_exp_fn m	`thenRn` \ (local_rn_env, local_mod_avails, local_avails) ->

	-- PROCESS IMPORT DECLS
      mapAndUnzip3Rn importsFromImportDecl all_imports
						`thenRn` \ (imp_rn_envs, imp_avails_s, explicit_imports_s) ->

	-- CHECK FOR EARLY EXIT
      checkEarlyExit this_mod			`thenRn` \ early_exit ->
      if early_exit then
		returnRn (junk_exp_fn, Nothing)
      else

	-- COMBINE RESULTS
	-- We put the local env first, so that a local provenance
	-- "wins", even if a module imports itself.
      foldlRn plusRnEnv emptyRnEnv imp_rn_envs		`thenRn` \ imp_rn_env ->
      plusRnEnv local_rn_env imp_rn_env	 		`thenRn` \ rn_env ->
      let
	 all_avails :: ModuleAvails
	 all_avails = foldr plusModuleAvails local_mod_avails imp_avails_s

	 explicit_names :: NameSet 	-- locally defined or explicitly imported
	 explicit_names = foldr add_on emptyNameSet (local_avails : explicit_imports_s)
	 add_on avails names = foldr (unionNameSets . mkNameSet . availNames) names avails
      in
  
	-- PROCESS EXPORT LISTS
      exportsFromAvail this_mod exports all_avails rn_env	
							`thenRn` \ (export_fn, export_env) ->

	-- RECORD THAT LOCALLY DEFINED THINGS ARE AVAILABLE
      mapRn (recordSlurp Nothing) local_avails		`thenRn_`

      returnRn (export_fn, Just (export_env, rn_env, explicit_names))
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
    if not opt_SourceUnchanged then
	-- Source code changed and no errors yet... carry on 
	returnRn False
    else
	-- Unchanged source, and no errors yet; see if usage info
	-- up to date, and exit if so
	checkUpToDate mod			`thenRn` \ up_to_date ->
	returnRn up_to_date
\end{code}
	

\begin{code}
importsFromImportDecl :: RdrNameImportDecl
		      -> RnMG (RnEnv, ModuleAvails, [AvailInfo])

importsFromImportDecl (ImportDecl mod qual_only as_mod import_spec loc)
  = pushSrcLocRn loc $
    getInterfaceExports mod			`thenRn` \ (avails, fixities) ->
    filterImports mod import_spec avails	`thenRn` \ (filtered_avails, hides, explicits) ->
    let
	filtered_avails' = map set_avail_prov filtered_avails
	fixities'        = [ (occ,(fixity,provenance)) | (occ,fixity) <- fixities ]
    in
    qualifyImports mod 
		   True 		-- Want qualified names
		   (not qual_only)	-- Maybe want unqualified names
		   as_mod
		   (ExportEnv filtered_avails' fixities')
		   hides
							`thenRn` \ (rn_env, mod_avails) ->
    returnRn (rn_env, mod_avails, explicits)
  where
    set_avail_prov NotAvailable   = NotAvailable
    set_avail_prov (Avail n)      = Avail (set_name_prov n) 
    set_avail_prov (AvailTC n ns) = AvailTC (set_name_prov n) (map set_name_prov ns)
    set_name_prov name = setNameProvenance name provenance
    provenance = Imported mod loc
\end{code}


\begin{code}
importsFromLocalDecls rec_exp_fn (HsModule mod _ _ _ fix_decls decls _)
  = foldlRn getLocalDeclBinders [] decls		`thenRn` \ avails ->
    mapRn fixityFromFixDecl fix_decls			`thenRn` \ fixities ->
    qualifyImports mod 
		   False	-- Don't want qualified names
		   True		-- Want unqualified names
		   Nothing	-- No "as M" part
		   (ExportEnv avails fixities)
		   []		-- Hide nothing
							`thenRn` \ (rn_env, mod_avails) ->
    returnRn (rn_env, mod_avails, avails)
  where
    newLocalName rdr_name loc
      = newLocallyDefinedGlobalName mod (rdrNameOcc rdr_name) rec_exp_fn loc

    getLocalDeclBinders avails (ValD binds)
      = mapRn do_one (bagToList (collectTopBinders binds))	`thenRn` \ val_avails ->
	returnRn (val_avails ++ avails)

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

      | otherwise   = returnRn filtered_avail
		
      where
	maybe_in_import_avails = lookupFM import_fm (ieOcc item)
	Just avail	       = maybe_in_import_avails
	filtered_avail	       = filterAvail item avail
\end{code}



%************************************************************************
%*									*
\subsection{Qualifiying imports}
%*									*
%************************************************************************

@qualifyImports@ takes the @ExportEnv@ after filtering through the import spec
of an import decl, and deals with producing an @RnEnv@ with the 
right qaulified names.  It also turns the @Names@ in the @ExportEnv@ into
fully fledged @Names@.

\begin{code}
qualifyImports :: Module				-- Imported module
	       -> Bool					-- True <=> want qualified import
	       -> Bool					-- True <=> want unqualified import
	       -> Maybe Module				-- Optional "as M" part 
	       -> ExportEnv				-- What's imported
	       -> [AvailInfo]				-- What's to be hidden
	       -> RnMG (RnEnv, ModuleAvails)

qualifyImports this_mod qual_imp unqual_imp as_mod (ExportEnv avails fixities) hides
  = let
 	-- Make the name environment.  Since we're talking about a single import module
	-- there can't be name clashes, so we don't need to be in the monad
	name_env1 = foldl add_avail emptyNameEnv avails

	-- Delete things that are hidden
	name_env2 = foldl del_avail name_env1 hides

	-- Create the fixity env
	fixity_env = foldl (add_fixity name_env2) emptyFixityEnv fixities

	-- The "module M" syntax only applies to *unqualified* imports (1.4 Report, Section 5.1.1)
	mod_avail_env | unqual_imp = unitFM qual_mod avails
		      | otherwise  = emptyFM
    in
    returnRn (RnEnv name_env2 fixity_env, mod_avail_env)
  where
    qual_mod = case as_mod of
		  Nothing  	    -> this_mod
		  Just another_name -> another_name

    add_avail env avail = foldl add_name env (availNames avail)
    add_name env name   = env2
			where
			  env1 | qual_imp   = addOneToNameEnv env  (Qual qual_mod occ) name
			       | otherwise  = env
			  env2 | unqual_imp = addOneToNameEnv env1 (Unqual occ)	       name
			       | otherwise  = env1
			  occ  = nameOccName name

    del_avail env avail = foldl delOneFromNameEnv env rdr_names
			where
			  rdr_names = map (Unqual . nameOccName) (availNames avail)
			
    add_fixity name_env fix_env (occ_name, (fixity, provenance))
	= add qual $ add unqual $ fix_env
 	where
	  qual   = Qual qual_mod occ_name
	  unqual = Unqual occ_name

	  add rdr_name fix_env | maybeToBool (lookupFM name_env rdr_name)
			       = addOneToFixityEnv fix_env rdr_name (fixity,provenance)
			       | otherwise
			       = fix_env
\end{code}

unQualify adds an Unqual binding for every existing Qual binding.

\begin{code}
unQualify :: FiniteMap RdrName elt -> FiniteMap RdrName elt
unQualify fm = addListToFM fm [(Unqual occ, elt) | (Qual _ occ, elt) <- fmToList fm]
\end{code}

%************************************************************************
%*									*
\subsection{Local declarations}
%*									*
%************************************************************************


\begin{code}
fixityFromFixDecl :: RdrNameFixityDecl -> RnMG (OccName, (Fixity, Provenance))

fixityFromFixDecl (FixityDecl rdr_name fixity loc)
  = returnRn (rdrNameOcc rdr_name, (fixity, LocalDef (panic "export-flag") loc))
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
type AvailEnv = FiniteMap OccName (RdrNameIE, AvailInfo)
	-- The FM maps each OccName to the RdrNameIE that gave rise to it,
	-- for error reporting, as well as to its AvailInfo

emptyAvailEnv = emptyFM

unitAvailEnv :: RdrNameIE -> AvailInfo -> AvailEnv
unitAvailEnv ie NotAvailable   = emptyFM
unitAvailEnv ie (AvailTC _ []) = emptyFM
unitAvailEnv ie avail	       = unitFM (nameOccName (availName avail)) (ie,avail)

plusAvailEnv a1 a2
  = mapRn (addErrRn.availClashErr) (conflictsFM bad_avail a1 a2)	`thenRn_`
    returnRn (plusFM_C plus_avail a1 a2)

listToAvailEnv :: RdrNameIE -> [AvailInfo] -> RnM s d AvailEnv
listToAvailEnv ie items
  = foldlRn plusAvailEnv emptyAvailEnv (map (unitAvailEnv ie) items)

bad_avail  (ie1,avail1) (ie2,avail2) = availName avail1 /= availName avail2	-- Same OccName, different Name
plus_avail (ie1,a1) (ie2,a2) = (ie1, a1 `plusAvail` a2)
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
		 -> ModuleAvails
		 -> RnEnv
		 -> RnMG (Name -> ExportFlag, ExportEnv)
	-- Complains if two distinct exports have same OccName
	-- Complains about exports items not in scope
exportsFromAvail this_mod Nothing all_avails rn_env
  = exportsFromAvail this_mod (Just [IEModuleContents this_mod]) all_avails rn_env

exportsFromAvail this_mod (Just export_items) all_avails (RnEnv name_env fixity_env)
  = mapRn exports_from_item export_items 		`thenRn` \ avail_envs ->
    foldlRn plusAvailEnv emptyAvailEnv avail_envs	`thenRn` \ export_avail_env -> 
    let
	export_avails   = map snd (eltsFM export_avail_env)
	export_fixities = mk_exported_fixities (availsToNameSet export_avails)
	export_fn	= mk_export_fn export_avails
    in
    returnRn (export_fn, ExportEnv export_avails export_fixities)

  where
    full_avail_env :: UniqFM AvailInfo
    full_avail_env = addListToUFM_C plusAvail emptyUFM
			   [(name, avail) | avail <- concat (eltsFM all_avails),
					    name  <- availEntityNames avail 
			   ]

	-- NB: full_avail_env will contain bindings for class ops but not constructors
	-- (see defn of availEntityNames)

    exports_from_item :: RdrNameIE -> RnMG AvailEnv
    exports_from_item ie@(IEModuleContents mod)
	= case lookupFM all_avails mod of
		Nothing	    -> failWithRn emptyAvailEnv (modExportErr mod)
		Just avails -> listToAvailEnv ie avails

    exports_from_item ie
	| not (maybeToBool maybe_in_scope) 
	= failWithRn emptyAvailEnv (unknownNameErr (ieName ie))

#ifdef DEBUG
	-- I can't see why this should ever happen; if the thing is in scope
	-- at all it ought to have some availability
	| not (maybeToBool maybe_avail)
	= pprTrace "exportsFromAvail: curious Nothing:" (ppr PprDebug name)
	  returnRn emptyAvailEnv
#endif

	| not enough_avail
	= failWithRn emptyAvailEnv (exportItemErr ie export_avail)

	| otherwise	-- Phew!  It's OK!
	= returnRn (unitAvailEnv ie export_avail)
       where
          maybe_in_scope  = lookupNameEnv name_env (ieName ie)
	  Just name	  = maybe_in_scope
	  maybe_avail     = lookupUFM full_avail_env name
	  Just avail      = maybe_avail
 	  export_avail    = filterAvail ie avail
	  enough_avail	  = case export_avail of {NotAvailable -> False; other -> True}

	-- We export a fixity iff we export a thing with the same (qualified) RdrName
    mk_exported_fixities :: NameSet -> [(OccName, (Fixity, Provenance))]
    mk_exported_fixities exports
	= fmToList (foldr (perhaps_add_fixity exports) 
			  emptyFM
			  (fmToList fixity_env))

    perhaps_add_fixity :: NameSet -> (RdrName, (Fixity, Provenance))
		       -> FiniteMap OccName (Fixity,Provenance)
		       -> FiniteMap OccName (Fixity,Provenance)
    perhaps_add_fixity exports (rdr_name, (fixity, prov)) fix_env
      =  let
	    do_nothing = fix_env		-- The default is to pass on the env unchanged
	 in
      	 	-- Step 1: check whether the rdr_name is in scope; if so find its Name
	 case lookupFM name_env rdr_name of {
	   Nothing 	    -> do_nothing;
	   Just fixity_name -> 

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
	  Just (fixity1, prov1) -> 	-- Got it already
				   ASSERT( fixity == fixity1 )
				   do_nothing;
	  Nothing -> 

		-- Step 3: add it to the outgoing fix_env
	addToFM fix_env occ_name (fixity,prov)
	}}

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
badImportItemErr mod ie sty
  = sep [ptext SLIT("Module"), pprModule sty mod, ptext SLIT("does not export"), ppr sty ie]

modExportErr mod sty
  = hsep [ ptext SLIT("Unknown module in export list: module"), ptext mod]

exportItemErr export_item NotAvailable sty
  = sep [ ptext SLIT("Export item not in scope:"), ppr sty export_item ]

exportItemErr export_item avail sty
  = hang (ptext SLIT("Export item not fully in scope:"))
	   4 (vcat [hsep [ptext SLIT("Wanted:    "), ppr sty export_item],
			hsep [ptext SLIT("Available: "), ppr sty (ieOcc export_item), pprAvail sty avail]])

availClashErr (occ_name, ((ie1,avail1), (ie2,avail2))) sty
  = hang (hsep [ptext SLIT("Conflicting exports for local name: "), ppr sty occ_name])
	4 (vcat [ppr sty ie1, ppr sty ie2])
\end{code}

