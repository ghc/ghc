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
		  rdrNameOcc
		)
import RnHsSyn	( RenamedHsModule(..), RenamedFixityDecl(..) )
import RnIfaces	( getInterfaceExports, getDeclBinders, checkUpToDate )
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
import Util	( panic, pprTrace )
\end{code}



%************************************************************************
%*									*
\subsection{Get global names}
%*									*
%************************************************************************

\begin{code}
getGlobalNames :: RdrNameHsModule
	       -> RnMG (Maybe (ExportEnv, RnEnv, [AvailInfo]))
			-- Nothing <=> no need to recompile

getGlobalNames m@(HsModule this_mod _ exports imports _ _ mod_loc)
  = fixRn (\ ~(rec_exp_fn, _) ->

	-- PROCESS LOCAL DECLS
	-- Do these *first* so that the correct provenance gets
	-- into the global name cache.
      importsFromLocalDecls rec_exp_fn m	`thenRn` \ (local_rn_env, local_mod_avails) ->

	-- PROCESS IMPORT DECLS
      mapAndUnzipRn importsFromImportDecl all_imports
						`thenRn` \ (imp_rn_envs, imp_avails_s) ->

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
	 local_avails = expectJust "getGlobalNames" (lookupModuleAvails local_mod_avails this_mod)
      in
  
	-- PROCESS EXPORT LISTS
      exportsFromAvail this_mod exports all_avails rn_env	
							`thenRn` \ (export_fn, export_env) ->

      returnRn (export_fn, Just (export_env, rn_env, local_avails))
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
  = if not opt_SourceUnchanged then
	-- Source code changed; look no further
	returnRn False
    else
	-- Unchanged source; look further
	-- We check for 
	-- 	(a) errors so far.  These can arise if a module imports
	--	    something that's no longer exported by the imported module
	--	(b) usage information up to date
	checkErrsRn				`thenRn` \ no_errs_so_far ->
	checkUpToDate mod			`thenRn` \ up_to_date ->
	returnRn (no_errs_so_far && up_to_date)
\end{code}
	

\begin{code}
importsFromImportDecl :: RdrNameImportDecl
		      -> RnMG (RnEnv, ModuleAvails)

importsFromImportDecl (ImportDecl mod qual_only as_mod import_spec loc)
  = pushSrcLocRn loc $
    getInterfaceExports mod			`thenRn` \ (avails, fixities) ->
    filterImports mod import_spec avails	`thenRn` \ filtered_avails ->
    let
	filtered_avails' = [ Avail (set_name_prov n) (map set_name_prov ns)
			   | Avail n ns <- filtered_avails
			   ]
	fixities'        = [ (occ,fixity,provenance) | (occ,fixity) <- fixities ]
    in
    qualifyImports mod 
		   True 		-- Want qualified names
		   (not qual_only)	-- Maybe want unqualified names
		   as_mod
		   (ExportEnv filtered_avails' fixities')
  where
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
  where
    newLocalName rdr_name loc
      = newLocallyDefinedGlobalName mod (rdrNameOcc rdr_name) rec_exp_fn loc

    getLocalDeclBinders avails (ValD binds)
      = mapRn do_one (bagToList (collectTopBinders binds))	`thenRn` \ val_avails ->
	returnRn (val_avails ++ avails)

    getLocalDeclBinders avails decl
      = getDeclBinders newLocalName decl	`thenRn` \ avail ->
	returnRn (avail : avails)

    do_one (rdr_name, loc)
      = newLocalName rdr_name loc 	`thenRn` \ name ->
        returnRn (Avail name [])
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
	      -> RnMG [AvailInfo]			-- What's actually imported
	-- Complains if import spec mentions things the
	-- module doesn't export

filterImports mod Nothing imports
  = returnRn imports

filterImports mod (Just (want_hiding, import_items)) avails
  = 	-- Check that each import item mentions things that are actually available
    mapRn check_import_item import_items	`thenRn_`

	-- Return filtered environment; no need to filter fixities
    returnRn (map new_avail avails)

  where
    import_fm :: FiniteMap OccName RdrNameIE
    import_fm = listToFM [(ieOcc ie, ie) | ie <- import_items]

    avail_fm :: FiniteMap OccName AvailInfo
    avail_fm = listToFM [(nameOccName name, avail) | avail@(Avail name ns) <- avails]

    new_avail NotAvailable = NotAvailable
    new_avail avail@(Avail name _)
	| not in_import_items && want_hiding     = avail
	| not in_import_items && not want_hiding = NotAvailable
	| in_import_items     && want_hiding     = NotAvailable
	| in_import_items     && not want_hiding = filtered_avail
	where
	  maybe_import_item = lookupFM import_fm (nameOccName name)
	  in_import_items   = maybeToBool maybe_import_item
	  Just import_item  = maybe_import_item
	  filtered_avail    = filterAvail import_item avail

    check_import_item  :: RdrNameIE -> RnMG ()
    check_import_item item
      = checkRn (maybeToBool maybe_matching_avail && sub_names_ok item avail)
	        (badImportItemErr mod item)
     where
       item_name            = ieOcc item
       maybe_matching_avail = lookupFM avail_fm item_name
       Just avail	    = maybe_matching_avail

    sub_names_ok (IEVar _)  		_	      = True
    sub_names_ok (IEThingAbs _)		_	      = True
    sub_names_ok (IEThingAll _)		_	      = True
    sub_names_ok (IEThingWith _ wanted) (Avail _ has) = all ((`elem` has_list) . rdrNameOcc) wanted
						      where
						        has_list = map nameOccName has
    sub_names_ok other1			other2	      = False
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
	       -> RnMG (RnEnv, ModuleAvails)

qualifyImports this_mod qual_imp unqual_imp as_mod (ExportEnv avails fixities)
  = 	-- Make the qualified-name environments, checking of course for clashes
    foldlRn add_name emptyNameEnv avails			`thenRn` \ name_env ->
    foldlRn (add_fixity name_env) emptyFixityEnv fixities	`thenRn` \ fixity_env ->
    returnRn (RnEnv name_env fixity_env, mod_avail_env)
  where
    qual_mod = case as_mod of
		  Nothing  	    -> this_mod
		  Just another_name -> another_name

    mod_avail_env  = unitFM qual_mod avails

    add_name name_env NotAvailable = returnRn name_env
    add_name name_env (Avail n ns) = foldlRn add_one name_env (n : ns)

    add_one :: NameEnv -> Name -> RnMG NameEnv
    add_one env name = add_to_env addOneToNameEnvRn env occ_name name
		     where
			occ_name = nameOccName name

    add_to_env add_fn env occ thing | qual_imp && unqual_imp = both
				    | qual_imp		     = qual_only
				    | unqual_imp	     = unqual_only
				where
				  unqual_only = add_fn env  (Unqual occ)        thing
				  qual_only   = add_fn env  (Qual qual_mod occ) thing
				  both	      = unqual_only 	`thenRn` \ env' ->
						add_fn env' (Qual qual_mod occ) thing
			
    add_fixity name_env fixity_env (occ_name, fixity, provenance)
	| maybeToBool (lookupFM name_env rdr_name)	-- It's imported
	= add_to_env addOneToFixityEnvRn fixity_env occ_name (fixity,provenance)
	| otherwise					-- It ain't imported
	= returnRn fixity_env
	where
		-- rdr_name is a name by which the thing is guaranteed to be known,
		-- *if it is imported at all*
	  rdr_name | qual_imp  = Qual qual_mod occ_name
		   | otherwise = Unqual occ_name
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
fixityFromFixDecl :: RdrNameFixityDecl -> RnMG (OccName, Fixity, Provenance)

fixityFromFixDecl (FixityDecl rdr_name fixity loc)
  = returnRn (rdrNameOcc rdr_name, fixity, LocalDef (panic "export-flag") loc)
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
unitAvailEnv ie NotAvailable
  = emptyFM
unitAvailEnv ie avail@(Avail n ns)
  = unitFM (nameOccName n) (ie,avail)

plusAvailEnv a1 a2
  = mapRn (addErrRn.availClashErr) (conflictsFM bad_avail a1 a2)	`thenRn_`
    returnRn (plusFM_C plus_avail a1 a2)

listToAvailEnv :: RdrNameIE -> [AvailInfo] -> RnM s d AvailEnv
listToAvailEnv ie items
  = foldlRn plusAvailEnv emptyAvailEnv (map (unitAvailEnv ie) items)

bad_avail  (ie1,Avail n1 _) (ie2,Avail n2 _) = n1 /= n2	-- Same OccName, different Name
plus_avail (ie1,a1) (ie2,a2) = (ie1, a1 `plusAvail` a2)
\end{code}


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
			   [(name,avail) | avail@(Avail name _) <- concat (eltsFM all_avails)]
	-- NB: full_avail_env won't contain bindings for data constructors and class ops,
	-- which is right and proper; attempts to export them on their own will provoke an error

    exports_from_item :: RdrNameIE -> RnMG AvailEnv
    exports_from_item ie@(IEModuleContents mod)
	= case lookupFM all_avails mod of
		Nothing	    -> failWithRn emptyAvailEnv (modExportErr mod)
		Just avails -> addOccurrenceNames Compulsory [n | Avail n _ <- avails]	`thenRn_`
			       listToAvailEnv ie avails

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
	= addOccurrenceName Compulsory name	`thenRn_`
	  returnRn (unitAvailEnv ie export_avail)
       where
          maybe_in_scope  = lookupNameEnv name_env (ieName ie)
	  Just name	  = maybe_in_scope
	  maybe_avail     = lookupUFM full_avail_env name
	  Just avail      = maybe_avail
 	  export_avail    = filterAvail ie avail
	  enough_avail	  = case export_avail of {NotAvailable -> False; other -> True}

	-- We export a fixity iff we export a thing with the same (qualified) RdrName
    mk_exported_fixities :: NameSet -> [(OccName, Fixity, Provenance)]
    mk_exported_fixities exports
	= [ (rdrNameOcc rdr_name, fixity, prov)
	  | (rdr_name, (fixity, prov)) <- fmToList fixity_env,
	     export_fixity name_env exports rdr_name
 	  ]

mk_export_fn :: [AvailInfo] -> (Name -> ExportFlag)
mk_export_fn avails
  = \name -> if name `elemNameSet` exported_names
	     then Exported
	     else NotExported
  where
    exported_names :: NameSet
    exported_names = availsToNameSet avails

export_fixity :: NameEnv -> NameSet -> RdrName -> Bool
export_fixity name_env exports (Unqual _)
  = False	-- The qualified fixity is always there as well
export_fixity name_env exports rdr_name@(Qual _ occ)
  = case lookupFM name_env rdr_name of
	Just fixity_name -> fixity_name `elemNameSet` exports
				-- Check whether the exported thing is
				-- the one to which the fixity attaches
	other   -> False	-- Not even in scope
\end{code}				  


%************************************************************************
%*									*
\subsection{Errors}
%*									*
%************************************************************************

\begin{code}
ieOcc ie = rdrNameOcc (ieName ie)

badImportItemErr mod ie sty
  = ppSep [ppStr "Module", pprModule sty mod, ppStr "does not export", ppr sty ie]

modExportErr mod sty
  = ppCat [ ppStr "Unknown module in export list: module", ppPStr mod]

exportItemErr export_item NotAvailable sty
  = ppSep [ ppStr "Export item not in scope:", ppr sty export_item ]

exportItemErr export_item avail sty
  = ppHang (ppStr "Export item not fully in scope:")
	   4 (ppAboves [ppCat [ppStr "Wanted:    ", ppr sty export_item],
			ppCat [ppStr "Available: ", ppr sty (ieOcc export_item), pprAvail sty avail]])

availClashErr (occ_name, ((ie1,avail1), (ie2,avail2))) sty
  = ppHang (ppCat [ppStr "Conflicting exports for local name: ", ppr sty occ_name])
	4 (ppAboves [ppr sty ie1, ppr sty ie2])
\end{code}

