%
% (c) The GRASP Project, Glasgow University, 1992-1998
%
\section[Rename]{Renaming and dependency analysis passes}

\begin{code}
module Rename ( renameModule, renameExpr, closeIfaceDecls, checkOldIface ) where

#include "HsVersions.h"

import HsSyn
import RdrHsSyn		( RdrNameHsModule, RdrNameHsDecl, RdrNameDeprecation, RdrNameHsExpr,
			  RdrNameTyClDecl, RdrNameRuleDecl, RdrNameInstDecl, RdrNameImportDecl
			)
import RnHsSyn		( RenamedHsDecl, RenamedTyClDecl, RenamedRuleDecl, RenamedInstDecl,
			  extractHsTyNames, RenamedHsExpr,
			  instDeclFVs, tyClDeclFVs, ruleDeclFVs
			)

import CmdLineOpts	( DynFlags, DynFlag(..) )
import RnMonad
import RnExpr		( rnExpr )
import RnNames		( getGlobalNames, exportsFromAvail )
import RnSource		( rnSourceDecls, rnTyClDecl, rnIfaceRuleDecl, rnInstDecl )
import RnIfaces		( slurpImpDecls, mkImportInfo, recordLocalSlurps,
			  getInterfaceExports, closeDecls,
			  RecompileRequired, outOfDate, recompileRequired
			)
import RnHiFiles	( readIface, removeContext, loadInterface,
			  loadExports, loadFixDecls, loadDeprecs,
			  tryLoadInterface )
import RnEnv		( availsToNameSet, availName, mkIfaceGlobalRdrEnv,
			  emptyAvailEnv, unitAvailEnv, availEnvElts, 
			  plusAvailEnv, groupAvails, warnUnusedImports, 
			  warnUnusedLocalBinds, warnUnusedModules, 
			  lookupOrigNames, lookupSrcName, 
			  newGlobalName, unQualInScope
			)
import Module           ( Module, ModuleName, WhereFrom(..),
			  moduleNameUserString, moduleName,
			  moduleEnvElts
			)
import Name		( Name, NamedThing(..), getSrcLoc,
			  nameIsLocalOrFrom, nameOccName, nameModule,
			)
import Name		( mkNameEnv, nameEnvElts, extendNameEnv )
import RdrName		( elemRdrEnv, foldRdrEnv, isQual )
import OccName		( occNameFlavour )
import NameSet
import TysWiredIn	( unitTyCon, intTyCon, boolTyCon )
import PrelNames	( mAIN_Name, pREL_MAIN_Name, pRELUDE_Name,
			  ioTyCon_RDR, main_RDR_Unqual,
			  unpackCString_RDR, unpackCStringFoldr_RDR, unpackCStringUtf8_RDR,
			  eqString_RDR
			)
import PrelInfo		( derivingOccurrences )
import Type		( funTyCon )
import ErrUtils		( dumpIfSet, dumpIfSet_dyn, showPass, 
			  printErrorsAndWarnings, errorsFound )
import Bag		( bagToList )
import FiniteMap	( FiniteMap, fmToList, emptyFM, lookupFM, 
			  addToFM_C, elemFM, addToFM
			)
import UniqFM		( lookupUFM )
import Maybes		( maybeToBool, catMaybes )
import Outputable
import IO		( openFile, IOMode(..) )
import HscTypes		( PersistentCompilerState, HomeIfaceTable, HomeSymbolTable, 
			  ModIface(..), WhatsImported(..), 
			  VersionInfo(..), ImportVersion, IsExported,
			  IfaceDecls, mkIfaceDecls, dcl_tycl, dcl_rules, dcl_insts,
			  GlobalRdrEnv, pprGlobalRdrEnv,
			  AvailEnv, GenAvailInfo(..), AvailInfo, Avails,
			  Provenance(..), ImportReason(..), initialVersionInfo,
			  Deprecations(..), lookupDeprec, lookupIface
			 )
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
	     -> IO (PersistentCompilerState, Maybe (PrintUnqualified, (IsExported, ModIface, [RenamedHsDecl])))
	-- Nothing => some error occurred in the renamer

renameModule dflags hit hst pcs this_module rdr_module
  = renameSource dflags hit hst pcs this_module $
    rename this_module rdr_module
\end{code}


\begin{code}
renameExpr :: DynFlags
	   -> HomeIfaceTable -> HomeSymbolTable
	   -> PersistentCompilerState 
	   -> Module -> RdrNameHsExpr
	   -> IO ( PersistentCompilerState, 
		   Maybe (PrintUnqualified, (RenamedHsExpr, [RenamedHsDecl]))
                 )

renameExpr dflags hit hst pcs this_module expr
  = do	{ renameSource dflags hit hst pcs this_module $
	  tryLoadInterface doc (moduleName this_module) ImportByUser 
						`thenRn` \ (iface, maybe_err) ->
	  case maybe_err of {
	    Just msg -> ioToRnM (printErrs alwaysQualify 
			  	 (ptext SLIT("failed to load interface for") 
			   	  <+> quotes (ppr this_module) 
				  <>  char ':' <+> msg)) `thenRn_`
		        returnRn Nothing;
	    Nothing -> 

	  let rdr_env      = mi_globals iface
	      print_unqual = unQualInScope rdr_env
	  in 
 
	  initRnMS rdr_env emptyLocalFixityEnv SourceMode (rnExpr expr)	
						`thenRn` \ (e,fvs) -> 

	  checkErrsRn				`thenRn` \ no_errs_so_far ->
    	  if not no_errs_so_far then
		-- Found errors already, so exit now
	        doDump e [] `thenRn_` 
		returnRn Nothing
	  else

	  lookupOrigNames implicit_occs			`thenRn` \ implicit_names ->
	  slurpImpDecls (fvs `plusFV` implicit_names)	`thenRn` \ decls ->

	  doDump e decls  `thenRn_`
	  returnRn (Just (print_unqual, (e, decls)))
	}}
  where
     implicit_occs = string_occs
     doc = text "context for compiling expression"

     doDump :: RenamedHsExpr -> [RenamedHsDecl] -> RnMG (Either IOError ())
     doDump e decls = 
	getDOptsRn  `thenRn` \ dflags ->
	ioToRnM (dumpIfSet_dyn dflags Opt_D_dump_rn "Renamer:" 
			(vcat (ppr e : map ppr decls)))
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
	     -> RnMG (Maybe (PrintUnqualified, r))
	     -> IO (PersistentCompilerState, Maybe (PrintUnqualified, r))
	-- Nothing => some error occurred in the renamer

renameSource dflags hit hst old_pcs this_module thing_inside
  = do	{ showPass dflags "Renamer"

		-- Initialise the renamer monad
	; (new_pcs, msgs, maybe_rn_stuff) <- initRn dflags hit hst old_pcs this_module thing_inside

	 	-- Print errors from renaming
	;  let print_unqual = case maybe_rn_stuff of
				Just (unqual, _) -> unqual
				Nothing		 -> alwaysQualify

	;  printErrorsAndWarnings print_unqual msgs ;

		-- Return results.  No harm in updating the PCS
	; if errorsFound msgs then
	    return (new_pcs, Nothing)
          else	    
	    return (new_pcs, maybe_rn_stuff)
    }
\end{code}

\begin{code}
rename :: Module -> RdrNameHsModule -> RnMG (Maybe (PrintUnqualified, (IsExported, ModIface, [RenamedHsDecl])))
rename this_module contents@(HsModule _ _ exports imports local_decls mod_deprec loc)
  = pushSrcLocRn loc		$

 	-- FIND THE GLOBAL NAME ENVIRONMENT
    getGlobalNames this_module contents 	`thenRn` \ (gbl_env, local_gbl_env, all_avails@(_, global_avail_env)) ->

	-- Exit if we've found any errors
    checkErrsRn				`thenRn` \ no_errs_so_far ->
    if not no_errs_so_far then
	-- Found errors already, so exit now
	rnDump [] []		`thenRn_`
	returnRn Nothing 
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

	-- CHECK THAT main IS DEFINED, IF REQUIRED
    checkMain this_module local_gbl_env		`thenRn_`

	-- EXIT IF ERRORS FOUND
	-- We exit here if there are any errors in the source, *before*
	-- we attempt to slurp the decls from the interfaces, otherwise
	-- the slurped decls may get lost when we return up the stack
	-- to hscMain/hscExpr.
    checkErrsRn					`thenRn` \ no_errs_so_far ->
    if not no_errs_so_far then
	-- Found errors already, so exit now
        rnDump [] rn_local_decls		`thenRn_` 
	returnRn Nothing
    else

	-- SLURP IN ALL THE NEEDED DECLARATIONS
    implicitFVs mod_name rn_local_decls 	`thenRn` \ implicit_fvs -> 
    let
	slurp_fvs = implicit_fvs `plusFV` source_fvs
		-- It's important to do the "plus" this way round, so that
		-- when compiling the prelude, locally-defined (), Bool, etc
		-- override the implicit ones. 
    in
    traceRn (text "Source FVs:" <+> fsep (map ppr (nameSetToList slurp_fvs)))	`thenRn_`
    slurpImpDecls slurp_fvs		`thenRn` \ rn_imp_decls ->

    rnDump rn_imp_decls rn_local_decls		`thenRn_` 

	-- GENERATE THE VERSION/USAGE INFO
    mkImportInfo mod_name imports 			`thenRn` \ my_usages ->

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
	is_orphan   = any (isOrphanDecl this_module) rn_local_decls

	mod_iface = ModIface {	mi_module   = this_module,
				mi_version  = initialVersionInfo,
				mi_usages   = my_usages,
				mi_boot	    = False,
				mi_orphan   = is_orphan,
				mi_exports  = my_exports,
				mi_globals  = gbl_env,
				mi_fixities = fixities,
				mi_deprecs  = my_deprecs,
				mi_decls    = panic "mi_decls"
		    }

	print_unqualified = unQualInScope gbl_env
	is_exported name  = name `elemNameSet` exported_names
	exported_names    = availsToNameSet export_avails
    in

	-- REPORT UNUSED NAMES, AND DEBUG DUMP 
    reportUnusedNames mod_iface print_unqualified 
		      imports global_avail_env
		      source_fvs export_avails rn_imp_decls 	`thenRn_`

    returnRn (Just (print_unqualified, (is_exported, mod_iface, final_decls)))
  where
    mod_name = moduleName this_module
\end{code}

Checking that main is defined

\begin{code}
checkMain :: Module -> GlobalRdrEnv -> RnMG ()
checkMain this_mod local_env
  | moduleName this_mod == mAIN_Name 
  = checkRn (main_RDR_Unqual `elemRdrEnv` local_env) noMainErr
  | otherwise
  = returnRn ()
\end{code}

@implicitFVs@ forces the renamer to slurp in some things which aren't
mentioned explicitly, but which might be needed by the type checker.

\begin{code}
implicitFVs mod_name decls
  = lookupOrigNames implicit_occs			`thenRn` \ implicit_names ->
    returnRn (mkNameSet (map getName default_tycons)	`plusFV`
	      implicit_names)
  where
 	-- Add occurrences for Int, and (), because they
	-- are the types to which ambigious type variables may be defaulted by
	-- the type checker; so they won't always appear explicitly.
	-- [The () one is a GHC extension for defaulting CCall results.]
	-- ALSO: funTyCon, since it occurs implicitly everywhere!
	--  	 (we don't want to be bothered with making funTyCon a
	--	  free var at every function application!)
	-- Double is dealt with separately in getGates
    default_tycons = [unitTyCon, funTyCon, boolTyCon, intTyCon]

	-- Add occurrences for IO or PrimIO
    implicit_main |  mod_name == mAIN_Name
		  || mod_name == pREL_MAIN_Name = [ioTyCon_RDR]
		  |  otherwise 		        = []

	-- Now add extra "occurrences" for things that
	-- the deriving mechanism, or defaulting, will later need in order to
	-- generate code
    implicit_occs = string_occs ++ foldr ((++) . get) implicit_main decls


    get (TyClD (TyData {tcdDerivs = Just deriv_classes})) = concat (map get_deriv deriv_classes)
    get other						  = []

    get_deriv cls = case lookupUFM derivingOccurrences cls of
			Nothing   -> []
			Just occs -> occs

-- Virtually every program has error messages in it somewhere
string_occs = [unpackCString_RDR, unpackCStringFoldr_RDR, 
	       unpackCStringUtf8_RDR, eqString_RDR]
\end{code}

\begin{code}
isOrphanDecl this_mod (InstD (InstDecl inst_ty _ _ _ _))
  = not (foldNameSet ((||) . nameIsLocalOrFrom this_mod) False 
		     (extractHsTyNames (removeContext inst_ty)))
	-- The 'removeContext' is because of
	--	instance Foo a => Baz T where ...
	-- The decl is an orphan if Baz and T are both not locally defined,
	--	even if Foo *is* locally defined

isOrphanDecl this_mod (RuleD (HsRule _ _ _ lhs _ _))
  = check lhs
  where
	-- At the moment we just check for common LHS forms
	-- Expand as necessary.  Getting it wrong just means
	-- more orphans than necessary
    check (HsVar v)   	  = not (nameIsLocalOrFrom this_mod v)
    check (HsApp f a) 	  = check f && check a
    check (HsLit _)   	  = False
    check (HsOverLit _)	  = False
    check (OpApp l o _ r) = check l && check o && check r
    check (NegApp e _)    = check e
    check (HsPar e)	  = check e
    check (SectionL e o)  = check e && check o
    check (SectionR o e)  = check e && check o

    check other	      	  = True 	-- Safe fall through

isOrphanDecl _ _  = False
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
checkOldIface :: DynFlags
	      -> HomeIfaceTable -> HomeSymbolTable
	      -> PersistentCompilerState
	      -> FilePath
	      -> Bool 			-- Source unchanged
	      -> Maybe ModIface 	-- Old interface from compilation manager, if any
	      -> IO (PersistentCompilerState, Bool, (RecompileRequired, Maybe ModIface))
				-- True <=> errors happened

checkOldIface dflags hit hst pcs iface_path source_unchanged maybe_iface
  = runRn dflags hit hst pcs (panic "Bogus module") $
    case maybe_iface of
       Just old_iface -> -- Use the one we already have
                         setModuleRn (mi_module old_iface) (check_versions old_iface)

       Nothing -- try and read it from a file
          -> readIface iface_path	`thenRn` \ read_result ->
             case read_result of
               Left err -> -- Old interface file not found, or garbled; give up
			   traceRn (text "Bad old interface file" $$ nest 4 err) 	`thenRn_`
	                   returnRn (outOfDate, Nothing)

               Right parsed_iface
                      -> setModuleRn (pi_mod parsed_iface) $
                         loadOldIface parsed_iface `thenRn` \ m_iface ->
                         check_versions m_iface
    where
       check_versions :: ModIface -> RnMG (RecompileRequired, Maybe ModIface)
       check_versions iface
          = -- Check versions
            recompileRequired iface_path source_unchanged iface
	 						`thenRn` \ recompile ->
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
	-- Record that we have now got declarations for local_names
    recordLocalSlurps local_names	`thenRn_`

	-- Do the transitive closure
    lookupOrigNames implicit_occs	`thenRn` \ implicit_names ->
    closeDecls decls (needed `plusFV` implicit_names)
  where
    implicit_occs = string_occs	-- Data type decls with record selectors,
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
		  -> Avails		-- Exported by this module
		  -> [RenamedHsDecl] 
		  -> RnMG ()
reportUnusedNames my_mod_iface unqual imports avail_env 
		  source_fvs export_avails imported_decls
  = warnUnusedModules unused_imp_mods				`thenRn_`
    warnUnusedLocalBinds bad_locals				`thenRn_`
    warnUnusedImports bad_imp_names				`thenRn_`
    printMinimalImports this_mod unqual minimal_imports		`thenRn_`
    warnDeprecations this_mod export_avails my_deprecs 
		     really_used_names

  where
    this_mod   = mi_module my_mod_iface
    gbl_env    = mi_globals my_mod_iface
    my_deprecs = mi_deprecs my_mod_iface
    
	-- The export_fvs make the exported names look just as if they
	-- occurred in the source program.  
    export_fvs = availsToNameSet export_avails
    used_names = source_fvs `plusFV` export_fvs

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
    defined_names :: [(Name,Provenance)]
    defined_names = foldRdrEnv add [] gbl_env
    add rdr_name ns acc | isQual rdr_name = ns ++ acc
			| otherwise	  = acc

    defined_and_used, defined_but_not_used :: [(Name,Provenance)]
    (defined_and_used, defined_but_not_used) = partition used defined_names
    used (name,_)	  		     = name `elemNameSet` really_used_names
    
    -- Filter out the ones only defined implicitly
    bad_locals :: [Name]
    bad_locals     = [n     | (n,LocalDef) <- defined_but_not_used]
    
    bad_imp_names :: [(Name,Provenance)]
    bad_imp_names  = [(n,p) | (n,p@(NonLocalDef (UserImport mod _ True))) <- defined_but_not_used,
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
    
    add_name (n,NonLocalDef (UserImport m _ _)) acc = addToFM_C plusAvailEnv acc (moduleName (nameModule n))
					    			(unitAvailEnv (mk_avail n))
    add_name (n,other_prov)			acc = acc

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

warnDeprecations this_mod export_avails my_deprecs used_names
  = doptRn Opt_WarnDeprecations				`thenRn` \ warn_drs ->
    if not warn_drs then returnRn () else

	-- The home modules for things in the export list
	-- may not have been loaded yet; do it now, so 
	-- that we can see their deprecations, if any
    mapRn_ load_home export_mods		`thenRn_`

    getIfacesRn					`thenRn` \ ifaces ->
    getHomeIfaceTableRn				`thenRn` \ hit ->
    let
	pit     = iPIT ifaces
	deprecs = [ (n,txt)
                  | n <- nameSetToList used_names,
                    Just txt <- [lookup_deprec hit pit n] ]
    in			  
    mapRn_ warnDeprec deprecs

  where
    export_mods = nub [ moduleName (nameModule name) 
		      | avail <- export_avails,
			let name = availName avail,
			not (nameIsLocalOrFrom this_mod name) ]
  
    load_home m = loadInterface (text "Check deprecations for" <+> ppr m) m ImportBySystem

    lookup_deprec hit pit n
	| nameIsLocalOrFrom this_mod n
	= lookupDeprec my_deprecs n 
	| otherwise
	= case lookupIface hit pit n of
		Just iface -> lookupDeprec (mi_deprecs iface) n
		Nothing    -> pprPanic "warnDeprecations:" (ppr n)

-- ToDo: deal with original imports with 'qualified' and 'as M' clauses
printMinimalImports this_mod unqual imps
  = doptRn Opt_D_dump_minimal_imports		`thenRn` \ dump_minimal ->
    if not dump_minimal then returnRn () else

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
    to_ie (Avail n)       = returnRn (IEVar n)
    to_ie (AvailTC n [m]) = ASSERT( n==m ) 
			    returnRn (IEThingAbs n)
    to_ie (AvailTC n ns)  
	= getInterfaceExports n_mod ImportBySystem		`thenRn` \ (_, avails_by_module) ->
	  case [xs | (m,as) <- avails_by_module,
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
warnDeprec :: (Name, DeprecTxt) -> RnM d ()
warnDeprec (name, txt)
  = pushSrcLocRn (getSrcLoc name)	$
    addWarnRn				$
    sep [ text (occNameFlavour (nameOccName name)) <+> ppr name <+>
          text "is deprecated:", nest 4 (ppr txt) ]


dupFixityDecl rdr_name loc1 loc2
  = vcat [ptext SLIT("Multiple fixity declarations for") <+> quotes (ppr rdr_name),
	  ptext SLIT("at ") <+> ppr loc1,
	  ptext SLIT("and") <+> ppr loc2]

badDeprec d
  = sep [ptext SLIT("Illegal deprecation when whole module is deprecated"),
	 nest 4 (ppr d)]

noMainErr
  = hsep [ptext SLIT("Module"), quotes (ppr mAIN_Name), 
	  ptext SLIT("must include a definition for"), quotes (ptext SLIT("main"))]
\end{code}


