%
% (c) The GRASP Project, Glasgow University, 1992-1998
%
\section[Rename]{Renaming and dependency analysis passes}

\begin{code}
module Rename ( renameModule, renameStmt, closeIfaceDecls, checkOldIface ) where

#include "HsVersions.h"

import HsSyn
import RdrHsSyn		( RdrNameHsModule, RdrNameHsDecl, RdrNameDeprecation, 
			  RdrNameTyClDecl, RdrNameRuleDecl, RdrNameInstDecl, RdrNameImportDecl,
			  RdrNameStmt
			)
import RnHsSyn		( RenamedHsDecl, RenamedTyClDecl, RenamedRuleDecl, RenamedInstDecl,
			  RenamedStmt,
			  instDeclFVs, tyClDeclFVs, ruleDeclFVs
			)

import CmdLineOpts	( DynFlags, DynFlag(..) )
import RnMonad
import RnExpr		( rnStmt )
import RnNames		( getGlobalNames, exportsFromAvail )
import RnSource		( rnSourceDecls, rnTyClDecl, rnIfaceRuleDecl, rnInstDecl )
import RnIfaces		( slurpImpDecls, mkImportInfo, recordLocalSlurps,
			  closeDecls,
			  RecompileRequired, outOfDate, recompileRequired
			)
import RnHiFiles	( readIface, loadInterface,
			  loadExports, loadFixDecls, loadDeprecs,
			)
import RnEnv		( availsToNameSet, mkIfaceGlobalRdrEnv,
			  emptyAvailEnv, unitAvailEnv, availEnvElts, 
			  plusAvailEnv, groupAvails, warnUnusedImports, 
			  warnUnusedLocalBinds, warnUnusedModules, 
			  lookupSrcName, getImplicitStmtFVs, getImplicitModuleFVs, rnSyntaxNames,
			  newGlobalName, unQualInScope,, ubiquitousNames
			)
import Module           ( Module, ModuleName, WhereFrom(..),
			  moduleNameUserString, moduleName,
			  moduleEnvElts
			)
import Name		( Name, nameModule )
import NameEnv
import NameSet
import RdrName		( foldRdrEnv, isQual )
import PrelNames	( SyntaxMap, vanillaSyntaxMap, pRELUDE_Name )
import ErrUtils		( dumpIfSet, dumpIfSet_dyn, showPass, 
			  printErrorsAndWarnings, errorsFound )
import Bag		( bagToList )
import FiniteMap	( FiniteMap, fmToList, emptyFM, lookupFM, 
			  addToFM_C, elemFM, addToFM
			)
import Maybes		( maybeToBool, catMaybes )
import Outputable
import IO		( openFile, IOMode(..) )
import HscTypes		( PersistentCompilerState, HomeIfaceTable, HomeSymbolTable, 
			  ModIface(..), WhatsImported(..), 
			  VersionInfo(..), ImportVersion, IsExported,
			  IfaceDecls, mkIfaceDecls, dcl_tycl, dcl_rules, dcl_insts,
			  GlobalRdrEnv, GlobalRdrElt(..), pprGlobalRdrEnv,
			  AvailEnv, GenAvailInfo(..), AvailInfo, 
			  Provenance(..), ImportReason(..), initialVersionInfo,
			  Deprecations(..),
			  LocalRdrEnv
			 )
import CmStaticInfo	( GhciMode(..) )
import List		( partition, nub )
\end{code}




%*********************************************************
%*						 	 *
\subsection{The two main wrappers}
%*							 *
%*********************************************************

\begin{code}
renameModule :: DynFlags
	     -> HomeIfaceTable -> HomeSymbolTable
	     -> PersistentCompilerState 
	     -> Module -> RdrNameHsModule 
	     -> IO (PersistentCompilerState, PrintUnqualified,
		    Maybe (IsExported, ModIface, (SyntaxMap, [RenamedHsDecl])))
	-- Nothing => some error occurred in the renamer

renameModule dflags hit hst pcs this_module rdr_module
  = renameSource dflags hit hst pcs this_module $
    rename this_module rdr_module
\end{code}


\begin{code}
renameStmt :: DynFlags
	   -> HomeIfaceTable -> HomeSymbolTable
	   -> PersistentCompilerState 
	   -> Module			-- current context (scope to compile in)
	   -> Module			-- current module
	   -> LocalRdrEnv		-- current context (temp bindings)
	   -> RdrNameStmt		-- parsed stmt
	   -> IO ( PersistentCompilerState, 
		   PrintUnqualified,
		   Maybe ([Name], (SyntaxMap, RenamedStmt, [RenamedHsDecl]))
                 )

renameStmt dflags hit hst pcs scope_module this_module local_env stmt
  = renameSource dflags hit hst pcs this_module $

	-- Load the interface for the context module, so 
	-- that we can get its top-level lexical environment
	-- Bale out if we fail to do this
    loadInterface doc (moduleName scope_module) ImportByUser `thenRn` \ iface ->
    let rdr_env       = mi_globals iface
	print_unqual  = unQualInScope rdr_env
    in 
    checkErrsRn				`thenRn` \ no_errs_so_far ->
    if not no_errs_so_far then
	returnRn (print_unqual, Nothing)
    else

	-- Rename it
    initRnMS rdr_env local_env emptyLocalFixityEnv CmdLineMode (
	rnStmt stmt	$ \ stmt' ->
	returnRn (([], stmt'), emptyFVs)
    )						`thenRn` \ ((binders, stmt), fvs) -> 

	-- Bale out if we fail
    checkErrsRn					`thenRn` \ no_errs_so_far ->
    if not no_errs_so_far then
        doDump [] stmt [] `thenRn_` returnRn (print_unqual, Nothing)
    else

	-- Add implicit free vars, and close decls
    getImplicitStmtFVs 				`thenRn` \ implicit_fvs ->
    let
	filtered_fvs = fvs `delListFromNameSet` rdrEnvElts local_env 
	source_fvs   = implicit_fvs `plusFV` filtered_fvs
    in
    slurpImpDecls source_fvs			`thenRn` \ decls ->

    doDump binders stmt decls  `thenRn_`
    returnRn (print_unqual, Just (binders, (vanillaSyntaxMap, stmt, decls)))

  where
     doc = text "context for compiling expression"

     doDump :: [Name] -> RenamedStmt -> [RenamedHsDecl] -> RnMG (Either IOError ())
     doDump bndrs stmt decls
	= getDOptsRn  `thenRn` \ dflags ->
	  ioToRnM (dumpIfSet_dyn dflags Opt_D_dump_rn "Renamer:" 
			(vcat [text "Binders:" <+> ppr bndrs,
			       ppr stmt, text "",
			       vcat (map ppr decls)]))
\end{code}


%*********************************************************
%*						 	 *
\subsection{The main function: rename}
%*							 *
%*********************************************************

\begin{code}
renameSource :: DynFlags
	     -> HomeIfaceTable -> HomeSymbolTable
	     -> PersistentCompilerState 
	     -> Module 
	     -> RnMG (PrintUnqualified, Maybe r)
	     -> IO (PersistentCompilerState, PrintUnqualified, Maybe r)
	-- Nothing => some error occurred in the renamer

renameSource dflags hit hst old_pcs this_module thing_inside
  = do	{ showPass dflags "Renamer"

		-- Initialise the renamer monad
	; (new_pcs, msgs, (print_unqual, maybe_rn_stuff)) 
		<- initRn dflags hit hst old_pcs this_module thing_inside

	 	-- Print errors from renaming
	;  printErrorsAndWarnings print_unqual msgs ;

		-- Return results.  No harm in updating the PCS
	; if errorsFound msgs then
	    return (new_pcs, print_unqual, Nothing)
          else	    
	    return (new_pcs, print_unqual, maybe_rn_stuff)
    }
\end{code}

\begin{code}
rename :: Module -> RdrNameHsModule 
       -> RnMG (PrintUnqualified, Maybe (IsExported, ModIface, (SyntaxMap, [RenamedHsDecl])))
rename this_module contents@(HsModule _ _ exports imports local_decls mod_deprec loc)
  = pushSrcLocRn loc		$

 	-- FIND THE GLOBAL NAME ENVIRONMENT
    getGlobalNames this_module contents 	`thenRn` \ (gbl_env, local_gbl_env, all_avails@(_, global_avail_env)) ->
    let
	print_unqualified = unQualInScope gbl_env
    in
	-- Exit if we've found any errors
    checkErrsRn				`thenRn` \ no_errs_so_far ->
    if not no_errs_so_far then
	-- Found errors already, so exit now
	rnDump [] []		`thenRn_`
	returnRn (print_unqualified, Nothing)
    else
	
	-- PROCESS EXPORT LIST 
    exportsFromAvail mod_name exports all_avails gbl_env	`thenRn` \ export_avails ->
	
    traceRn (text "Local top-level environment" $$ 
	     nest 4 (pprGlobalRdrEnv local_gbl_env))	`thenRn_`

	-- DEAL WITH DEPRECATIONS
    rnDeprecs local_gbl_env mod_deprec 
	      [d | DeprecD d <- local_decls]		`thenRn` \ my_deprecs ->

	-- DEAL WITH LOCAL FIXITIES
    fixitiesFromLocalDecls local_gbl_env local_decls	`thenRn` \ local_fixity_env ->

	-- RENAME THE SOURCE
    rnSourceDecls gbl_env local_fixity_env local_decls	`thenRn` \ (rn_local_decls, source_fvs) ->

	-- EXIT IF ERRORS FOUND
	-- We exit here if there are any errors in the source, *before*
	-- we attempt to slurp the decls from the interfaces, otherwise
	-- the slurped decls may get lost when we return up the stack
	-- to hscMain/hscExpr.
    checkErrsRn					`thenRn` \ no_errs_so_far ->
    if not no_errs_so_far then
	-- Found errors already, so exit now
        rnDump [] rn_local_decls		`thenRn_` 
	returnRn (print_unqualified, Nothing)
    else

	-- SLURP IN ALL THE NEEDED DECLARATIONS
   	-- Find out what re-bindable names to use for desugaring
    getImplicitModuleFVs mod_name rn_local_decls	`thenRn` \ implicit_fvs ->
    rnSyntaxNames gbl_env source_fvs			`thenRn` \ (source_fvs1, sugar_map) ->
    let
	export_fvs  = availsToNameSet export_avails
	source_fvs2 = source_fvs1 `plusFV` export_fvs
		-- The export_fvs make the exported names look just as if they
		-- occurred in the source program.  For the reasoning, see the
		-- comments with RnIfaces.mkImportInfo
		-- It also helps reportUnusedNames, which of course must not complain
		-- that 'f' isn't mentioned if it is mentioned in the export list

	source_fvs3 = implicit_fvs `plusFV` source_fvs2
		-- It's important to do the "plus" this way round, so that
		-- when compiling the prelude, locally-defined (), Bool, etc
		-- override the implicit ones. 

    in
    traceRn (text "Source FVs:" <+> fsep (map ppr (nameSetToList source_fvs3)))	`thenRn_`
    slurpImpDecls source_fvs3			`thenRn` \ rn_imp_decls ->
    rnDump rn_imp_decls rn_local_decls		`thenRn_` 

	-- GENERATE THE VERSION/USAGE INFO
    mkImportInfo mod_name imports 		`thenRn` \ my_usages ->

	-- BUILD THE MODULE INTERFACE
    let
	-- We record fixities even for things that aren't exported,
	-- so that we can change into the context of this moodule easily
	fixities = mkNameEnv [ (name, fixity)
			     | FixitySig name fixity loc <- nameEnvElts local_fixity_env
			     ]

	-- Sort the exports to make them easier to compare for versions
	my_exports = groupAvails this_module export_avails
	
	final_decls = rn_local_decls ++ rn_imp_decls

	mod_iface = ModIface {	mi_module   = this_module,
				mi_version  = initialVersionInfo,
				mi_usages   = my_usages,
				mi_boot	    = False,
				mi_orphan   = panic "is_orphan",
				mi_exports  = my_exports,
				mi_globals  = gbl_env,
				mi_fixities = fixities,
				mi_deprecs  = my_deprecs,
				mi_decls    = panic "mi_decls"
		    }

	is_exported name  = name `elemNameSet` exported_names
	exported_names    = availsToNameSet export_avails
    in

	-- REPORT UNUSED NAMES, AND DEBUG DUMP 
    reportUnusedNames mod_iface print_unqualified 
		      imports global_avail_env
		      source_fvs2 rn_imp_decls		`thenRn_`
		-- NB: source_fvs2: include exports (else we get bogus 
		--     warnings of unused things) but not implicit FVs.

    returnRn (print_unqualified, Just (is_exported, mod_iface, (sugar_map, final_decls)))
  where
    mod_name = moduleName this_module
\end{code}



%*********************************************************
%*						 	 *
\subsection{Fixities}
%*							 *
%*********************************************************

\begin{code}
fixitiesFromLocalDecls :: GlobalRdrEnv -> [RdrNameHsDecl] -> RnMG LocalFixityEnv
fixitiesFromLocalDecls gbl_env decls
  = foldlRn getFixities emptyNameEnv decls				`thenRn` \ env -> 
    traceRn (text "fixity env" <+> vcat (map ppr (nameEnvElts env)))	`thenRn_`
    returnRn env
  where
    getFixities :: LocalFixityEnv -> RdrNameHsDecl -> RnMG LocalFixityEnv
    getFixities acc (FixD fix)
      = fix_decl acc fix

    getFixities acc (TyClD (ClassDecl { tcdSigs = sigs}))
      = foldlRn fix_decl acc [sig | FixSig sig <- sigs]
		-- Get fixities from class decl sigs too.
    getFixities acc other_decl
      = returnRn acc

    fix_decl acc sig@(FixitySig rdr_name fixity loc)
	= 	-- Check for fixity decl for something not declared
	  pushSrcLocRn loc 			$
	  lookupSrcName gbl_env rdr_name	`thenRn` \ name ->

		-- Check for duplicate fixity decl
	  case lookupNameEnv acc name of
	    Just (FixitySig _ _ loc') -> addErrRn (dupFixityDecl rdr_name loc loc')	`thenRn_`
					 returnRn acc ;

	    Nothing		      -> returnRn (extendNameEnv acc name (FixitySig name fixity loc))
\end{code}


%*********************************************************
%*						 	 *
\subsection{Deprecations}
%*							 *
%*********************************************************

For deprecations, all we do is check that the names are in scope.
It's only imported deprecations, dealt with in RnIfaces, that we
gather them together.

\begin{code}
rnDeprecs :: GlobalRdrEnv -> Maybe DeprecTxt
	   -> [RdrNameDeprecation] -> RnMG Deprecations
rnDeprecs gbl_env Nothing []
 = returnRn NoDeprecs

rnDeprecs gbl_env (Just txt) decls
 = mapRn (addErrRn . badDeprec) decls 	`thenRn_` 
   returnRn (DeprecAll txt)

rnDeprecs gbl_env Nothing decls
  = mapRn rn_deprec decls	`thenRn` \ pairs ->
    returnRn (DeprecSome (mkNameEnv (catMaybes pairs)))
 where
   rn_deprec (Deprecation rdr_name txt loc)
     = pushSrcLocRn loc				$
       lookupSrcName gbl_env rdr_name		`thenRn` \ name ->
       returnRn (Just (name, (name,txt)))
\end{code}


%************************************************************************
%*									*
\subsection{Grabbing the old interface file and checking versions}
%*									*
%************************************************************************

\begin{code}
checkOldIface :: GhciMode
              -> DynFlags
	      -> HomeIfaceTable -> HomeSymbolTable
	      -> PersistentCompilerState
	      -> FilePath
	      -> Bool 			-- Source unchanged
	      -> Maybe ModIface 	-- Old interface from compilation manager, if any
	      -> IO (PersistentCompilerState, Bool, (RecompileRequired, Maybe ModIface))
				-- True <=> errors happened

checkOldIface ghci_mode dflags hit hst pcs iface_path source_unchanged maybe_iface
    = runRn dflags hit hst pcs (panic "Bogus module") $

	-- CHECK WHETHER THE SOURCE HAS CHANGED
    ( if not source_unchanged then
	traceHiDiffsRn (nest 4 (text "Source file changed or recompilation check turned off"))    
      else returnRn () )   `thenRn_`

     -- If the source has changed and we're in interactive mode, avoid reading
     -- an interface; just return the one we might have been supplied with.
    if ghci_mode == Interactive && not source_unchanged then
         returnRn (outOfDate, maybe_iface)
    else

    case maybe_iface of
       Just old_iface -> -- Use the one we already have
                         setModuleRn (mi_module old_iface) (check_versions old_iface)

       Nothing -- try and read it from a file
          -> readIface iface_path	`thenRn` \ read_result ->
             case read_result of
               Left err -> -- Old interface file not found, or garbled; give up
			   traceHiDiffsRn (
				text "Cannot read old interface file:"
			   	   $$ nest 4 err) `thenRn_`
	                   returnRn (outOfDate, Nothing)

               Right parsed_iface
                      -> setModuleRn (pi_mod parsed_iface) $
                         loadOldIface parsed_iface `thenRn` \ m_iface ->
                         check_versions m_iface
    where
       check_versions :: ModIface -> RnMG (RecompileRequired, Maybe ModIface)
       check_versions iface
 	  | not source_unchanged
          = returnRn (outOfDate, Just iface)
          | otherwise
          = -- Check versions
            recompileRequired iface_path iface	`thenRn` \ recompile ->
            returnRn (recompile, Just iface)
\end{code}

I think the following function should now have a more representative name,
but what?

\begin{code}
loadOldIface :: ParsedIface -> RnMG ModIface

loadOldIface parsed_iface
  = let iface = parsed_iface 
        mod = pi_mod iface
    in
    initIfaceRnMS mod (
	loadHomeDecls (pi_decls iface)	`thenRn` \ decls ->
	loadHomeRules (pi_rules iface)	`thenRn` \ rules -> 
	loadHomeInsts (pi_insts iface)	`thenRn` \ insts ->
	returnRn (decls, rules, insts)
    )	
	`thenRn` \ ((decls_vers, new_decls), (rule_vers, new_rules), new_insts) ->

    mapRn loadHomeUsage	(pi_usages iface)	`thenRn` \ usages ->
    loadExports         (pi_exports iface)	`thenRn` \ (export_vers, avails) ->
    loadFixDecls mod	(pi_fixity iface)	`thenRn` \ fix_env ->
    loadDeprecs mod	(pi_deprecs iface)	`thenRn` \ deprec_env ->
    let
	version	= VersionInfo { vers_module  = pi_vers iface, 
				vers_exports = export_vers,
				vers_rules   = rule_vers,
				vers_decls   = decls_vers }

	decls = mkIfaceDecls new_decls new_rules new_insts

 	mod_iface = ModIface { mi_module = mod, mi_version = version,
			       mi_exports = avails, mi_usages  = usages,
			       mi_boot = False, mi_orphan = pi_orphan iface, 
			       mi_fixities = fix_env, mi_deprecs = deprec_env,
			       mi_decls   = decls,
			       mi_globals = mkIfaceGlobalRdrEnv avails
		    }
    in
    returnRn mod_iface
\end{code}

\begin{code}
loadHomeDecls :: [(Version, RdrNameTyClDecl)]
	      -> RnMS (NameEnv Version, [RenamedTyClDecl])
loadHomeDecls decls = foldlRn loadHomeDecl (emptyNameEnv, []) decls

loadHomeDecl :: (NameEnv Version, [RenamedTyClDecl])
	     -> (Version, RdrNameTyClDecl)
	     -> RnMS (NameEnv Version, [RenamedTyClDecl])
loadHomeDecl (version_map, decls) (version, decl)
  = rnTyClDecl decl	`thenRn` \ decl' ->
    returnRn (extendNameEnv version_map (tyClDeclName decl') version, decl':decls)

------------------
loadHomeRules :: (Version, [RdrNameRuleDecl])
	      -> RnMS (Version, [RenamedRuleDecl])
loadHomeRules (version, rules)
  = mapRn rnIfaceRuleDecl rules	`thenRn` \ rules' ->
    returnRn (version, rules')

------------------
loadHomeInsts :: [RdrNameInstDecl]
	      -> RnMS [RenamedInstDecl]
loadHomeInsts insts = mapRn rnInstDecl insts

------------------
loadHomeUsage :: ImportVersion OccName
	      -> RnMG (ImportVersion Name)
loadHomeUsage (mod_name, orphans, is_boot, whats_imported)
  = rn_imps whats_imported	`thenRn` \ whats_imported' ->
    returnRn (mod_name, orphans, is_boot, whats_imported')
  where
    rn_imps NothingAtAll	   	  = returnRn NothingAtAll
    rn_imps (Everything v)		  = returnRn (Everything v)
    rn_imps (Specifically mv ev items rv) = mapRn rn_imp items 	`thenRn` \ items' ->
					    returnRn (Specifically mv ev items' rv)
    rn_imp (occ,vers) = newGlobalName mod_name occ	`thenRn` \ name ->
			returnRn (name,vers)
\end{code}



%*********************************************************
%*						 	 *
\subsection{Closing up the interface decls}
%*							 *
%*********************************************************

Suppose we discover we don't need to recompile.   Then we start from the
IfaceDecls in the ModIface, and fluff them up by sucking in all the decls they need.

\begin{code}
closeIfaceDecls :: DynFlags
	      	-> HomeIfaceTable -> HomeSymbolTable
	      	-> PersistentCompilerState
	      	-> ModIface 	-- Get the decls from here
	      	-> IO (PersistentCompilerState, Bool, [RenamedHsDecl])
				-- True <=> errors happened
closeIfaceDecls dflags hit hst pcs
		mod_iface@(ModIface { mi_module = mod, mi_decls = iface_decls })
  = runRn dflags hit hst pcs mod $

    let
	rule_decls = dcl_rules iface_decls
	inst_decls = dcl_insts iface_decls
	tycl_decls = dcl_tycl  iface_decls
	decls = map RuleD rule_decls ++
		map InstD inst_decls ++
		map TyClD tycl_decls
	needed = unionManyNameSets (map ruleDeclFVs rule_decls) `unionNameSets`
		 unionManyNameSets (map instDeclFVs inst_decls) `unionNameSets`
		 unionManyNameSets (map tyClDeclFVs tycl_decls)
	local_names    = foldl add emptyNameSet tycl_decls
	add names decl = addListToNameSet names (map fst (tyClDeclSysNames decl ++ tyClDeclNames decl))
    in

    recordLocalSlurps local_names	`thenRn_`

	-- Do the transitive closure
    closeDecls decls (needed `plusFV` implicit_fvs) `thenRn` \closed_decls ->
    rnDump [] closed_decls `thenRn_`
    returnRn closed_decls
  where
    implicit_fvs = ubiquitousNames	-- Data type decls with record selectors,
					-- which may appear in the decls, need unpackCString
					-- and friends. It's easier to just grab them right now.
\end{code}

%*********************************************************
%*						 	 *
\subsection{Unused names}
%*							 *
%*********************************************************

\begin{code}
reportUnusedNames :: ModIface -> PrintUnqualified
		  -> [RdrNameImportDecl] 
		  -> AvailEnv
		  -> NameSet 		-- Used in this module
		  -> [RenamedHsDecl] 
		  -> RnMG ()
reportUnusedNames my_mod_iface unqual imports avail_env 
		  used_names imported_decls
  = warnUnusedModules unused_imp_mods				`thenRn_`
    warnUnusedLocalBinds bad_locals				`thenRn_`
    warnUnusedImports bad_imp_names				`thenRn_`
    printMinimalImports this_mod unqual minimal_imports
  where
    this_mod   = mi_module my_mod_iface
    gbl_env    = mi_globals my_mod_iface
    
    -- Now, a use of C implies a use of T,
    -- if C was brought into scope by T(..) or T(C)
    really_used_names = used_names `unionNameSets`
      mkNameSet [ parent_name
	        | sub_name <- nameSetToList used_names
    
    		-- Usually, every used name will appear in avail_env, but there 
    		-- is one time when it doesn't: tuples and other built in syntax.  When you
    		-- write (a,b) that gives rise to a *use* of "(,)", so that the
    		-- instances will get pulled in, but the tycon "(,)" isn't actually
    		-- in scope.  Also, (-x) gives rise to an implicit use of 'negate'; 
    		-- similarly,   3.5 gives rise to an implcit use of :%
    		-- Hence the silent 'False' in all other cases
    	      
	        , Just parent_name <- [case lookupNameEnv avail_env sub_name of
			    		Just (AvailTC n _) -> Just n
			    		other		   -> Nothing]
    	    ]
    
	-- Collect the defined names from the in-scope environment
	-- Look for the qualified ones only, else get duplicates
    defined_names :: [GlobalRdrElt]
    defined_names = foldRdrEnv add [] gbl_env
    add rdr_name ns acc | isQual rdr_name = ns ++ acc
			| otherwise	  = acc

    defined_and_used, defined_but_not_used :: [GlobalRdrElt]
    (defined_and_used, defined_but_not_used) = partition used defined_names
    used (GRE name _ _)	  		     = name `elemNameSet` really_used_names
    
    -- Filter out the ones only defined implicitly
    bad_locals :: [Name]
    bad_locals = [n | (GRE n LocalDef _) <- defined_but_not_used]
    
    bad_imp_names :: [(Name,Provenance)]
    bad_imp_names  = [(n,p) | GRE n p@(NonLocalDef (UserImport mod _ True)) _ <- defined_but_not_used,
  	  		      not (module_unused mod)]
    
    -- inst_mods are directly-imported modules that 
    --	contain instance decl(s) that the renamer decided to suck in
    -- It's not necessarily redundant to import such modules.
    --
    -- NOTE: Consider 
    --	      module This
    --		import M ()
    --
    --	 The import M() is not *necessarily* redundant, even if
    -- 	 we suck in no instance decls from M (e.g. it contains 
    --	 no instance decls, or This contains no code).  It may be 
    --	 that we import M solely to ensure that M's orphan instance 
    --	 decls (or those in its imports) are visible to people who 
    --	 import This.  Sigh. 
    --	 There's really no good way to detect this, so the error message 
    --	 in RnEnv.warnUnusedModules is weakened instead
    inst_mods :: [ModuleName]
    inst_mods = [m | InstD (InstDecl _ _ _ (Just dfun) _) <- imported_decls,
    		 let m = moduleName (nameModule dfun),
    		 m `elem` direct_import_mods
    	    ]
    
    -- To figure out the minimal set of imports, start with the things
    -- that are in scope (i.e. in gbl_env).  Then just combine them
    -- into a bunch of avails, so they are properly grouped
    minimal_imports :: FiniteMap ModuleName AvailEnv
    minimal_imports0 = emptyFM
    minimal_imports1 = foldr add_name     minimal_imports0 defined_and_used
    minimal_imports  = foldr add_inst_mod minimal_imports1 inst_mods
    
	-- We've carefully preserved the provenance so that we can
	-- construct minimal imports that import the name by (one of)
	-- the same route(s) as the programmer originally did.
    add_name (GRE n (NonLocalDef (UserImport m _ _)) _) acc = addToFM_C plusAvailEnv acc (moduleName m)
						    			(unitAvailEnv (mk_avail n))
    add_name (GRE n other_prov _)			acc = acc

    mk_avail n = case lookupNameEnv avail_env n of
    		Just (AvailTC m _) | n==m      -> AvailTC n [n]
    				   | otherwise -> AvailTC m [n,m]
    		Just avail	   -> Avail n
    		Nothing		   -> pprPanic "mk_avail" (ppr n)
    
    add_inst_mod m acc 
      | m `elemFM` acc = acc	-- We import something already
      | otherwise      = addToFM acc m emptyAvailEnv
    	-- Add an empty collection of imports for a module
    	-- from which we have sucked only instance decls
   
    direct_import_mods :: [ModuleName]
    direct_import_mods = nub [m | ImportDecl m _ _ _ _ _ <- imports]

    -- unused_imp_mods are the directly-imported modules 
    -- that are not mentioned in minimal_imports
    unused_imp_mods = [m | m <- direct_import_mods,
    		       not (maybeToBool (lookupFM minimal_imports m)),
    		       m /= pRELUDE_Name]
    
    module_unused :: Module -> Bool
    module_unused mod = moduleName mod `elem` unused_imp_mods


-- ToDo: deal with original imports with 'qualified' and 'as M' clauses
printMinimalImports :: Module 	-- This module
		    -> PrintUnqualified
		    -> FiniteMap ModuleName AvailEnv	-- Minimal imports
		    -> RnMG ()
printMinimalImports this_mod unqual imps
  = ifOptRn Opt_D_dump_minimal_imports 		$

    mapRn to_ies (fmToList imps)		`thenRn` \ mod_ies ->
    ioToRnM (do { h <- openFile filename WriteMode ;
		  printForUser h unqual (vcat (map ppr_mod_ie mod_ies))
	})					`thenRn_`
    returnRn ()
  where
    filename = moduleNameUserString (moduleName this_mod) ++ ".imports"
    ppr_mod_ie (mod_name, ies) 
	| mod_name == pRELUDE_Name 
	= empty
	| otherwise
	= ptext SLIT("import") <+> ppr mod_name <> 
			    parens (fsep (punctuate comma (map ppr ies)))

    to_ies (mod, avail_env) = mapRn to_ie (availEnvElts avail_env)	`thenRn` \ ies ->
			      returnRn (mod, ies)

    to_ie :: AvailInfo -> RnMG (IE Name)
	-- The main trick here is that if we're importing all the constructors
	-- we want to say "T(..)", but if we're importing only a subset we want
	-- to say "T(A,B,C)".  So we have to find out what the module exports.
    to_ie (Avail n)       = returnRn (IEVar n)
    to_ie (AvailTC n [m]) = ASSERT( n==m ) 
			    returnRn (IEThingAbs n)
    to_ie (AvailTC n ns)  
	= loadInterface (text "Compute minimal imports from" <+> ppr n_mod) n_mod ImportBySystem	`thenRn` \ iface ->
	  case [xs | (m,as) <- mi_exports iface,
		     m == n_mod,
		     AvailTC x xs <- as, 
		     x == n] of
	      [xs] | all (`elem` ns) xs -> returnRn (IEThingAll n)
		   | otherwise	        -> returnRn (IEThingWith n (filter (/= n) ns))
	      other			-> pprTrace "to_ie" (ppr n <+> ppr (nameModule n) <+> ppr other) $
					   returnRn (IEVar n)
	where
	  n_mod = moduleName (nameModule n)

rnDump  :: [RenamedHsDecl] 	-- Renamed imported decls
	-> [RenamedHsDecl] 	-- Renamed local decls
	-> RnMG ()
rnDump imp_decls local_decls
  = doptRn Opt_D_dump_rn_trace 	`thenRn` \ dump_rn_trace ->
    doptRn Opt_D_dump_rn_stats 	`thenRn` \ dump_rn_stats ->
    doptRn Opt_D_dump_rn 	`thenRn` \ dump_rn ->
    getIfacesRn			`thenRn` \ ifaces ->

    ioToRnM (do { dumpIfSet (dump_rn_trace || dump_rn_stats || dump_rn)
			    "Renamer statistics"
			    (getRnStats imp_decls ifaces) ;

		  dumpIfSet dump_rn "Renamer:" 
			    (vcat (map ppr (local_decls ++ imp_decls)))
    })				`thenRn_`

    returnRn ()
\end{code}


%*********************************************************
%*							*
\subsection{Statistics}
%*							*
%*********************************************************

\begin{code}
getRnStats :: [RenamedHsDecl] -> Ifaces -> SDoc
getRnStats imported_decls ifaces
  = hcat [text "Renamer stats: ", stats]
  where
    n_mods = length [() | _ <- moduleEnvElts (iPIT ifaces)]
	-- This is really only right for a one-shot compile

    (decls_map, n_decls_slurped) = iDecls ifaces
    
    n_decls_left   = length [decl | (avail, True, (_,decl)) <- nameEnvElts decls_map
    			-- Data, newtype, and class decls are in the decls_fm
    			-- under multiple names; the tycon/class, and each
    			-- constructor/class op too.
    			-- The 'True' selects just the 'main' decl
    		     ]
    
    (insts_left, n_insts_slurped) = iInsts ifaces
    n_insts_left  = length (bagToList insts_left)
    
    (rules_left, n_rules_slurped) = iRules ifaces
    n_rules_left  = length (bagToList rules_left)
    
    stats = vcat 
    	[int n_mods <+> text "interfaces read",
    	 hsep [ int n_decls_slurped, text "type/class/variable imported, out of", 
    	        int (n_decls_slurped + n_decls_left), text "read"],
    	 hsep [ int n_insts_slurped, text "instance decls imported, out of",  
    	        int (n_insts_slurped + n_insts_left), text "read"],
    	 hsep [ int n_rules_slurped, text "rule decls imported, out of",  
    	        int (n_rules_slurped + n_rules_left), text "read"]
	]
\end{code}    


%************************************************************************
%*									*
\subsection{Errors and warnings}
%*									*
%************************************************************************

\begin{code}
dupFixityDecl rdr_name loc1 loc2
  = vcat [ptext SLIT("Multiple fixity declarations for") <+> quotes (ppr rdr_name),
	  ptext SLIT("at ") <+> ppr loc1,
	  ptext SLIT("and") <+> ppr loc2]

badDeprec d
  = sep [ptext SLIT("Illegal deprecation when whole module is deprecated"),
	 nest 4 (ppr d)]
\end{code}


