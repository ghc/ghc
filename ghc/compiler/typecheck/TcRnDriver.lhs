%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcModule]{Typechecking a whole module}

\begin{code}
module TcRnDriver (
#ifdef GHCI
	mkExportEnv, getModuleContents, tcRnStmt, 
	tcRnGetInfo, tcRnExpr, tcRnType,
#endif
	tcRnModule, 
	tcTopSrcDecls,
	tcRnExtCore
    ) where

#include "HsVersions.h"

#ifdef GHCI
import {-# SOURCE #-} TcSplice ( tcSpliceDecls )
#endif

import CmdLineOpts	( DynFlag(..), opt_PprStyle_Debug, dopt )
import Packages		( moduleToPackageConfig, mkPackageId, package,
			  isHomeModule )
import DriverState	( v_MainModIs, v_MainFunIs )
import HsSyn		( HsModule(..), HsExtCore(..), HsGroup(..), LHsDecl, SpliceDecl(..), HsBind(..),
			  nlHsApp, nlHsVar, pprLHsBinds )
import RdrHsSyn		( findSplice )

import PrelNames	( runIOName, rootMainName, mAIN,
			  main_RDR_Unqual )
import RdrName		( RdrName, mkRdrUnqual, emptyGlobalRdrEnv, 
			  plusGlobalRdrEnv )
import TcHsSyn		( zonkTopDecls )
import TcExpr 		( tcInferRho )
import TcRnMonad
import TcType		( tidyTopType, tcEqType, mkTyVarTys, substTyWith )
import Inst		( showLIE )
import TcBinds		( tcTopBinds )
import TcDefaults	( tcDefaults )
import TcEnv		( tcExtendGlobalValEnv )
import TcRules		( tcRules )
import TcForeign	( tcForeignImports, tcForeignExports )
import TcInstDcls	( tcInstDecls1, tcInstDecls2 )
import TcIface		( tcExtCoreBindings )
import TcSimplify	( tcSimplifyTop )
import TcTyClsDecls	( tcTyAndClassDecls )
import LoadIface	( loadOrphanModules, loadHiBootInterface )
import RnNames		( importsFromLocalDecls, rnImports, exportsFromAvail, 
			  reportUnusedNames, reportDeprecations )
import RnEnv		( lookupSrcOcc_maybe )
import RnSource		( rnSrcDecls, rnTyClDecls, checkModDeprec )
import PprCore		( pprIdRules, pprCoreBindings )
import CoreSyn		( IdCoreRule, bindersOfBinds )
import DataCon		( dataConWrapId )
import ErrUtils		( Messages, mkDumpDoc, showPass )
import Id		( mkExportedLocalId, isLocalId, idName, idType )
import Var		( Var )
import Module           ( mkModule, moduleEnvElts )
import OccName		( mkVarOcc )
import Name		( Name, isExternalName, getSrcLoc, getOccName )
import NameSet
import TyCon		( tyConHasGenerics, isSynTyCon, getSynTyConDefn, tyConKind )
import SrcLoc		( srcLocSpan, Located(..), noLoc )
import Outputable
import HscTypes		( ModGuts(..), HscEnv(..), ExternalPackageState(..),
			  GhciMode(..), noDependencies, isOneShot,
			  Deprecs( NoDeprecs ), ModIface(..), plusDeprecs,
			  ForeignStubs(NoStubs), TyThing(..), 
			  TypeEnv, lookupTypeEnv,
			  extendTypeEnvWithIds, typeEnvIds, typeEnvTyCons, 
			  emptyFixityEnv, availName
			)
#ifdef GHCI
import HsSyn		( HsStmtContext(..), Stmt(..), HsExpr(..), HsBindGroup(..), 
			  LStmt, LHsExpr, LHsType, mkMatchGroup,
			  collectStmtsBinders, mkSimpleMatch, placeHolderType,
			  nlLetStmt, nlExprStmt, nlBindStmt, nlResultStmt, nlVarPat )
import RdrName		( GlobalRdrEnv, mkGlobalRdrEnv, GlobalRdrElt(..),
			  Provenance(..), ImportSpec(..),
			  lookupLocalRdrEnv, extendLocalRdrEnv )
import RnSource		( addTcgDUs )
import TcHsSyn		( mkHsLet, zonkTopLExpr, zonkTopBndrs )
import TcHsType		( kcHsType )
import TcExpr		( tcCheckRho )
import TcIface		( loadImportedInsts )
import TcMType		( zonkTcType, zonkQuantifiedTyVar )
import TcUnify		( unifyTyConApp )
import TcMatches	( tcStmtsAndThen, TcStmtCtxt(..) )
import TcSimplify	( tcSimplifyInteractive, tcSimplifyInfer )
import TcType		( Type, mkForAllTys, mkFunTys, mkTyConApp, tyVarsOfType, 
			  isUnLiftedType, tyClsNamesOfDFunHead )
import TcEnv		( tcLookupTyCon, tcLookupId, tcLookupGlobal )
import RnTypes		( rnLHsType )
import Inst		( tcStdSyntaxName, tcGetInstEnvs )
import InstEnv		( DFunId, classInstances, instEnvElts )
import RnExpr		( rnStmts, rnLExpr )
import RnNames		( exportsToAvails )
import LoadIface	( loadSrcInterface )
import IfaceSyn		( IfaceDecl(..), IfaceClassOp(..), IfaceConDecl(..), 
			  IfaceExtName(..), IfaceConDecls(..), IfaceInst(..),
			  tyThingToIfaceDecl, dfunToIfaceInst )
import RnEnv		( lookupOccRn, dataTcOccs, lookupFixityRn )
import Id		( Id, isImplicitId, globalIdDetails )
import MkId		( unsafeCoerceId )
import DataCon		( dataConTyCon )
import TyCon		( tyConName )
import TysWiredIn	( mkListTy, unitTy )
import IdInfo		( GlobalIdDetails(..) )
import SrcLoc		( interactiveSrcLoc, unLoc )
import Kind		( Kind )
import Var		( globaliseId )
import Name		( nameOccName, nameModule )
import NameEnv		( delListFromNameEnv )
import PrelNames	( iNTERACTIVE, ioTyConName, printName, monadNames, itName, returnIOName )
import Module		( Module, lookupModuleEnv )
import HscTypes		( InteractiveContext(..), ExternalPackageState( eps_PTE ),
			  HomeModInfo(..), typeEnvElts, typeEnvClasses,
			  availNames, icPrintUnqual,
			  ModDetails(..), Dependencies(..) )
import BasicTypes	( RecFlag(..), Fixity )
import Bag		( unitBag )
import ListSetOps	( removeDups )
import Panic		( ghcError, GhcException(..) )
import SrcLoc		( SrcLoc )
#endif

import FastString	( mkFastString )
import Util		( sortLe )
import Bag		( unionBags, snocBag )

import Maybe		( isJust )
\end{code}



%************************************************************************
%*									*
	Typecheck and rename a module
%*									*
%************************************************************************


\begin{code}
tcRnModule :: HscEnv 
	   -> Located (HsModule RdrName)
	   -> IO (Messages, Maybe TcGblEnv)

tcRnModule hsc_env (L loc (HsModule maybe_mod exports 
				import_decls local_decls mod_deprec))
 = do { showPass (hsc_dflags hsc_env) "Renamer/typechecker" ;

   let { this_mod = case maybe_mod of
			Nothing  -> mAIN	
					-- 'module M where' is omitted
			Just (L _ mod) -> mod } ;		
					-- The normal case
		
   initTc hsc_env this_mod $ 
   setSrcSpan loc $
   do {
	checkForPackageModule (hsc_dflags hsc_env) this_mod;

		-- Deal with imports; sets tcg_rdr_env, tcg_imports
	(rdr_env, imports) <- rnImports import_decls ;

		-- Record boot-file info in the EPS, so that it's 
		-- visible to loadHiBootInterface in tcRnSrcDecls,
		-- and any other incrementally-performed imports
	updateEps_ (\eps -> eps { eps_is_boot = imp_dep_mods imports }) ;

		-- Update the gbl env
	updGblEnv ( \ gbl -> gbl { tcg_rdr_env = rdr_env,
				   tcg_imports = tcg_imports gbl `plusImportAvails` imports }) 
		     $ do {
	traceRn (text "rn1" <+> ppr (imp_dep_mods imports)) ;
		-- Fail if there are any errors so far
		-- The error printing (if needed) takes advantage 
		-- of the tcg_env we have now set
	failIfErrsM ;

		-- Load any orphan-module interfaces, so that
		-- their rules and instance decls will be found
	loadOrphanModules (imp_orphs imports) ;

	traceRn (text "rn1a") ;
		-- Rename and type check the declarations
	tcg_env <- tcRnSrcDecls local_decls ;
	setGblEnv tcg_env		$ do {

	traceRn (text "rn3") ;

		-- Report the use of any deprecated things
		-- We do this before processsing the export list so
		-- that we don't bleat about re-exporting a deprecated
		-- thing (especially via 'module Foo' export item)
		-- Only uses in the body of the module are complained about
	reportDeprecations tcg_env ;

		-- Process the export list
	exports <- exportsFromAvail (isJust maybe_mod) exports ;

		-- Check whether the entire module is deprecated
		-- This happens only once per module
	let { mod_deprecs = checkModDeprec mod_deprec } ;

		-- Add exports and deprecations to envt
	let { final_env  = tcg_env { tcg_exports = exports,
				     tcg_dus = tcg_dus tcg_env `plusDU` usesOnly exports,
				     tcg_deprecs = tcg_deprecs tcg_env `plusDeprecs` 
						   mod_deprecs }
		-- A module deprecation over-rides the earlier ones
	     } ;

		-- Report unused names
 	reportUnusedNames final_env ;

		-- Dump output and return
	tcDump final_env ;
	return final_env
    }}}}

-- This is really a sanity check that the user has given -package-name
-- if necessary.  -package-name is only necessary when the package database
-- already contains the current package, because then we can't tell
-- whether a given module is in the current package or not, without knowing
-- the name of the current package.
checkForPackageModule dflags this_mod
  | not (isHomeModule dflags this_mod),
    Just (pkg,_) <- moduleToPackageConfig dflags this_mod =
	let 
		ppr_pkg = ppr (mkPackageId (package pkg))
	in
	addErr (ptext SLIT("Module") <+> quotes (ppr this_mod) <+>
	        ptext SLIT("is a member of package") <+>  ppr_pkg <> char '.' $$
		ptext SLIT("To compile this module, please use -ignore-package") <+> ppr_pkg <> char '.')
  | otherwise = return ()
\end{code}


%************************************************************************
%*									*
	Type-checking external-core modules
%*									*
%************************************************************************

\begin{code}
tcRnExtCore :: HscEnv 
	    -> HsExtCore RdrName
	    -> IO (Messages, Maybe ModGuts)
	-- Nothing => some error occurred 

tcRnExtCore hsc_env (HsExtCore this_mod decls src_binds)
	-- The decls are IfaceDecls; all names are original names
 = do { showPass (hsc_dflags hsc_env) "Renamer/typechecker" ;

   initTc hsc_env this_mod $ do {

   let { ldecls  = map noLoc decls } ;

	-- Deal with the type declarations; first bring their stuff
	-- into scope, then rname them, then type check them
   (rdr_env, imports) <- importsFromLocalDecls (mkFakeGroup ldecls) ;

   updGblEnv (\gbl -> gbl { tcg_rdr_env = rdr_env `plusGlobalRdrEnv` tcg_rdr_env gbl,
			    tcg_imports = imports `plusImportAvails` tcg_imports gbl }) 
		  $ do {

   rn_decls <- rnTyClDecls ldecls ;
   failIfErrsM ;

	-- Dump trace of renaming part
   rnDump (ppr rn_decls) ;

	-- Typecheck them all together so that
	-- any mutually recursive types are done right
   tcg_env <- checkNoErrs (tcTyAndClassDecls [{- no boot names -}] rn_decls) ;
	-- Make the new type env available to stuff slurped from interface files

   setGblEnv tcg_env $ do {
   
	-- Now the core bindings
   core_binds <- initIfaceExtCore (tcExtCoreBindings this_mod src_binds) ;

	-- Wrap up
   let {
	bndrs 	   = bindersOfBinds core_binds ;
	my_exports = mkNameSet (map idName bndrs) ;
		-- ToDo: export the data types also?

	final_type_env = extendTypeEnvWithIds (tcg_type_env tcg_env) bndrs ;

	mod_guts = ModGuts {	mg_module   = this_mod,
				mg_usages   = [],		-- ToDo: compute usage
				mg_dir_imps = [],		-- ??
				mg_deps     = noDependencies,	-- ??
				mg_exports  = my_exports,
				mg_types    = final_type_env,
				mg_insts    = tcg_insts tcg_env,
				mg_rules    = [],
				mg_binds    = core_binds,

				-- Stubs
				mg_rdr_env  = emptyGlobalRdrEnv,
				mg_fix_env  = emptyFixityEnv,
				mg_deprecs  = NoDeprecs,
				mg_foreign  = NoStubs
		    } } ;

   tcCoreDump mod_guts ;

   return mod_guts
   }}}}

mkFakeGroup decls -- Rather clumsy; lots of unused fields
  = HsGroup {	hs_tyclds = decls, 	-- This is the one we want
		hs_valds = [], hs_fords = [],
		hs_instds = [], hs_fixds = [], hs_depds = [],
		hs_ruleds = [], hs_defds = [] }
\end{code}


%************************************************************************
%*									*
	Type-checking the top level of a module
%*									*
%************************************************************************

\begin{code}
tcRnSrcDecls :: [LHsDecl RdrName] -> TcM TcGblEnv
	-- Returns the variables free in the decls
	-- Reason: solely to report unused imports and bindings
tcRnSrcDecls decls
 = do { boot_names <- loadHiBootInterface ;

	  	-- Do all the declarations
	(tc_envs, lie) <- getLIE (tc_rn_src_decls boot_names decls) ;

	     -- tcSimplifyTop deals with constant or ambiguous InstIds.  
	     -- How could there be ambiguous ones?  They can only arise if a
	     -- top-level decl falls under the monomorphism
	     -- restriction, and no subsequent decl instantiates its
	     -- type.  (Usually, ambiguous type variables are resolved
	     -- during the generalisation step.)
        traceTc (text "Tc8") ;
	inst_binds <- setEnvs tc_envs (tcSimplifyTop lie) ;
		-- Setting the global env exposes the instances to tcSimplifyTop
		-- Setting the local env exposes the local Ids to tcSimplifyTop, 
		-- so that we get better error messages (monomorphism restriction)

	    -- Backsubstitution.  This must be done last.
	    -- Even tcSimplifyTop may do some unification.
        traceTc (text "Tc9") ;
	let { (tcg_env, _) = tc_envs ;
	      TcGblEnv { tcg_type_env = type_env, tcg_binds = binds, 
		         tcg_rules = rules, tcg_fords = fords } = tcg_env } ;

	(bind_ids, binds', fords', rules') <- zonkTopDecls (binds `unionBags` inst_binds)
							   rules fords ;

	let { final_type_env = extendTypeEnvWithIds type_env bind_ids } ;

	-- Compre the hi-boot iface (if any) with the real thing
 	checkHiBootIface final_type_env boot_names ;

	-- Make the new type env available to stuff slurped from interface files
	writeMutVar (tcg_type_env_var tcg_env) final_type_env ;

	return (tcg_env { tcg_type_env = final_type_env,
			  tcg_binds = binds', tcg_rules = rules', tcg_fords = fords' }) 
   }

tc_rn_src_decls :: [Name] -> [LHsDecl RdrName] -> TcM (TcGblEnv, TcLclEnv)
-- Loops around dealing with each top level inter-splice group 
-- in turn, until it's dealt with the entire module
tc_rn_src_decls boot_names ds
 = do { let { (first_group, group_tail) = findSplice ds } ;
		-- If ds is [] we get ([], Nothing)

	-- Type check the decls up to, but not including, the first splice
	tc_envs@(tcg_env,tcl_env) <- tcRnGroup boot_names first_group ;

	-- Bale out if errors; for example, error recovery when checking
	-- the RHS of 'main' can mean that 'main' is not in the envt for 
	-- the subsequent checkMain test
	failIfErrsM ;

	setEnvs tc_envs $

	-- If there is no splice, we're nearly done
	case group_tail of {
	   Nothing -> do { 	-- Last thing: check for `main'
			   tcg_env <- checkMain ;
			   return (tcg_env, tcl_env) 
		      } ;

	-- If there's a splice, we must carry on
	   Just (SpliceDecl splice_expr, rest_ds) -> do {
#ifndef GHCI
	failWithTc (text "Can't do a top-level splice; need a bootstrapped compiler")
#else

	-- Rename the splice expression, and get its supporting decls
	(rn_splice_expr, splice_fvs) <- rnLExpr splice_expr ;
	failIfErrsM ;	-- Don't typecheck if renaming failed

	-- Execute the splice
	spliced_decls <- tcSpliceDecls rn_splice_expr ;

	-- Glue them on the front of the remaining decls and loop
	setGblEnv (tcg_env `addTcgDUs` usesOnly splice_fvs) $
	tc_rn_src_decls boot_names (spliced_decls ++ rest_ds)
#endif /* GHCI */
    }}}
\end{code}

%************************************************************************
%*									*
	Comparing the hi-boot interface with the real thing
%*									*
%************************************************************************

In both one-shot mode and GHCi mode, hi-boot interfaces are demand-loaded
into the External Package Table.  Once we've typechecked the body of the
module, we want to compare what we've found (gathered in a TypeEnv) with
the hi-boot stuff in the EPT.  We do so here, using the export list of 
the hi-boot interface as our checklist.

\begin{code}
checkHiBootIface :: TypeEnv -> [Name] -> TcM ()
-- Compare the hi-boot file for this module (if there is one)
-- with the type environment we've just come up with
-- In the common case where there is no hi-boot file, the list
-- of boot_names is empty.
checkHiBootIface env boot_names
  = mapM_ (check_one env) boot_names

----------------
check_one local_env name
  = do	{ eps  <- getEps

		-- Look up the hi-boot one; 
		-- it should jolly well be there (else GHC bug)
       ; case lookupTypeEnv (eps_PTE eps) name of {
	    Nothing -> pprPanic "checkHiBootIface" (ppr name) ;
	    Just boot_thing ->

		-- Look it up in the local type env
		-- It should be there, but it's a programmer error if not
         case lookupTypeEnv local_env name of
	   Nothing 	   -> addErrTc (missingBootThing boot_thing)
	   Just real_thing -> check_thing boot_thing real_thing
    } }

----------------
check_thing (ATyCon boot_tc) (ATyCon real_tc)
  | isSynTyCon boot_tc && isSynTyCon real_tc,
    defn1 `tcEqType` substTyWith tvs2 (mkTyVarTys tvs1) defn2
  = return ()

  | tyConKind boot_tc == tyConKind real_tc
  = return ()
  where
    (tvs1, defn1) = getSynTyConDefn boot_tc
    (tvs2, defn2) = getSynTyConDefn boot_tc

check_thing (AnId boot_id) (AnId real_id)
  | idType boot_id `tcEqType` idType real_id
  = return ()

check_thing (ADataCon dc1) (ADataCon dc2)
  | idType (dataConWrapId dc1) `tcEqType` idType (dataConWrapId dc2)
  = return ()

	-- Can't declare a class in a hi-boot file

check_thing boot_thing real_thing	-- Default case; failure
  = addErrAt (srcLocSpan (getSrcLoc real_thing))
	     (bootMisMatch real_thing)

----------------
missingBootThing thing
  = ppr thing <+> ptext SLIT("is defined in the hi-boot file, but not in the module")
bootMisMatch thing
  = ppr thing <+> ptext SLIT("has conflicting definitions in the module and its hi-boot file")
\end{code}


%************************************************************************
%*									*
	Type-checking the top level of a module
%*									*
%************************************************************************

tcRnGroup takes a bunch of top-level source-code declarations, and
 * renames them
 * gets supporting declarations from interface files
 * typechecks them
 * zonks them
 * and augments the TcGblEnv with the results

In Template Haskell it may be called repeatedly for each group of
declarations.  It expects there to be an incoming TcGblEnv in the
monad; it augments it and returns the new TcGblEnv.

\begin{code}
tcRnGroup :: [Name] -> HsGroup RdrName -> TcM (TcGblEnv, TcLclEnv)
	-- Returns the variables free in the decls, for unused-binding reporting
tcRnGroup boot_names decls
 = do {		-- Rename the declarations
	(tcg_env, rn_decls) <- rnTopSrcDecls decls ;
	setGblEnv tcg_env $ do {

		-- Typecheck the declarations
	tcTopSrcDecls boot_names rn_decls 
  }}

------------------------------------------------
rnTopSrcDecls :: HsGroup RdrName -> TcM (TcGblEnv, HsGroup Name)
rnTopSrcDecls group
 = do { 	-- Bring top level binders into scope
	(rdr_env, imports) <- importsFromLocalDecls group ;
	updGblEnv (\gbl -> gbl { tcg_rdr_env = rdr_env `plusGlobalRdrEnv` tcg_rdr_env gbl,
				 tcg_imports = imports `plusImportAvails` tcg_imports gbl }) 
		  $ do {

	traceRn (ptext SLIT("rnTopSrcDecls") <+> ppr rdr_env) ;
	failIfErrsM ;	-- No point in continuing if (say) we have duplicate declarations

		-- Rename the source decls
	(tcg_env, rn_decls) <- rnSrcDecls group ;
	failIfErrsM ;

		-- Dump trace of renaming part
	rnDump (ppr rn_decls) ;

	return (tcg_env, rn_decls)
   }}

------------------------------------------------
tcTopSrcDecls :: [Name] -> HsGroup Name -> TcM (TcGblEnv, TcLclEnv)
tcTopSrcDecls boot_names
	(HsGroup { hs_tyclds = tycl_decls, 
		   hs_instds = inst_decls,
		   hs_fords  = foreign_decls,
		   hs_defds  = default_decls,
		   hs_ruleds = rule_decls,
		   hs_valds  = val_binds })
 = do {		-- Type-check the type and class decls, and all imported decls
		-- The latter come in via tycl_decls
        traceTc (text "Tc2") ;

	tcg_env <- checkNoErrs (tcTyAndClassDecls boot_names tycl_decls) ;
	-- tcTyAndClassDecls recovers internally, but if anything gave rise to
	-- an error we'd better stop now, to avoid a cascade
	
	-- Make these type and class decls available to stuff slurped from interface files
	writeMutVar (tcg_type_env_var tcg_env) (tcg_type_env tcg_env) ;


	setGblEnv tcg_env	$ do {
		-- Source-language instances, including derivings,
		-- and import the supporting declarations
        traceTc (text "Tc3") ;
	(tcg_env, inst_infos, deriv_binds) <- tcInstDecls1 tycl_decls inst_decls ;
	setGblEnv tcg_env	$ do {

	        -- Foreign import declarations next.  No zonking necessary
		-- here; we can tuck them straight into the global environment.
        traceTc (text "Tc4") ;
	(fi_ids, fi_decls) <- tcForeignImports foreign_decls ;
	tcExtendGlobalValEnv fi_ids	$ do {

		-- Default declarations
        traceTc (text "Tc4a") ;
	default_tys <- tcDefaults default_decls ;
	updGblEnv (\gbl -> gbl { tcg_default = default_tys }) $ do {
	
		-- Value declarations next
		-- We also typecheck any extra binds that came out 
		-- of the "deriving" process (deriv_binds)
        traceTc (text "Tc5") ;
	(tc_val_binds, lcl_env) <- tcTopBinds (val_binds ++ deriv_binds) ;
	setLclTypeEnv lcl_env 	$ do {

	     	-- Second pass over class and instance declarations, 
        traceTc (text "Tc6") ;
	(tcl_env, inst_binds) <- tcInstDecls2 tycl_decls inst_infos ;
	showLIE (text "after instDecls2") ;

		-- Foreign exports
		-- They need to be zonked, so we return them
        traceTc (text "Tc7") ;
	(foe_binds, foe_decls) <- tcForeignExports foreign_decls ;

		-- Rules
	rules <- tcRules rule_decls ;

		-- Wrap up
        traceTc (text "Tc7a") ;
	tcg_env <- getGblEnv ;
	let { all_binds = tc_val_binds	 `unionBags`
			  inst_binds	 `unionBags`
			  foe_binds  ;

		-- Extend the GblEnv with the (as yet un-zonked) 
		-- bindings, rules, foreign decls
	      tcg_env' = tcg_env {  tcg_binds = tcg_binds tcg_env `unionBags` all_binds,
				    tcg_rules = tcg_rules tcg_env ++ rules,
				    tcg_fords = tcg_fords tcg_env ++ foe_decls ++ fi_decls } } ;
  	return (tcg_env', lcl_env)
    }}}}}}
\end{code}


%************************************************************************
%*									*
	Checking for 'main'
%*									*
%************************************************************************

\begin{code}
checkMain 
  = do { ghci_mode <- getGhciMode ;
	 tcg_env   <- getGblEnv ;

	 mb_main_mod <- readMutVar v_MainModIs ;
	 mb_main_fn  <- readMutVar v_MainFunIs ;
	 let { main_mod = case mb_main_mod of {
				Just mod -> mkModule mod ;
				Nothing  -> mAIN } ;
	       main_fn  = case mb_main_fn of {
				Just fn -> mkRdrUnqual (mkVarOcc (mkFastString fn)) ;
				Nothing -> main_RDR_Unqual } } ;
	
	 check_main ghci_mode tcg_env main_mod main_fn
    }


check_main ghci_mode tcg_env main_mod main_fn
     -- If we are in module Main, check that 'main' is defined.
     -- It may be imported from another module!
     --
     -- 
     -- Blimey: a whole page of code to do this...
 | mod /= main_mod
 = return tcg_env

 | otherwise
 = addErrCtxt mainCtxt			$
   do	{ mb_main <- lookupSrcOcc_maybe main_fn
		-- Check that 'main' is in scope
		-- It might be imported from another module!
	; case mb_main of {
	     Nothing -> do { complain_no_main	
			   ; return tcg_env } ;
	     Just main_name -> do
	{ let { rhs = nlHsApp (nlHsVar runIOName) (nlHsVar main_name) }
		   	-- :Main.main :: IO () = runIO main 

	; (main_expr, ty) <- setSrcSpan (srcLocSpan (getSrcLoc main_name)) $
			     tcInferRho rhs

	; let { root_main_id = mkExportedLocalId rootMainName ty ;
	        main_bind    = noLoc (VarBind root_main_id main_expr) }

	; return (tcg_env { tcg_binds = tcg_binds tcg_env 
					`snocBag` main_bind,
			    tcg_dus   = tcg_dus tcg_env
				        `plusDU` usesOnly (unitFV main_name)
			-- Record the use of 'main', so that we don't 
			-- complain about it being defined but not used
		 }) 
    }}}
  where
    mod = tcg_mod tcg_env
 
    complain_no_main | ghci_mode == Interactive = return ()
		     | otherwise 		= failWithTc noMainMsg
	-- In interactive mode, don't worry about the absence of 'main'
	-- In other modes, fail altogether, so that we don't go on
	-- and complain a second time when processing the export list.

    mainCtxt  = ptext SLIT("When checking the type of the main function") <+> quotes (ppr main_fn)
    noMainMsg = ptext SLIT("The main function") <+> quotes (ppr main_fn) 
		<+> ptext SLIT("is not defined in module") <+> quotes (ppr main_mod)
\end{code}


%*********************************************************
%*						 	 *
		GHCi stuff
%*							 *
%*********************************************************

\begin{code}
#ifdef GHCI
setInteractiveContext :: InteractiveContext -> TcRn a -> TcRn a
setInteractiveContext icxt thing_inside 
  = traceTc (text "setIC" <+> ppr (ic_type_env icxt))	`thenM_`
    (updGblEnv (\env -> env {tcg_rdr_env  = ic_rn_gbl_env icxt,
			     tcg_type_env = ic_type_env   icxt}) $
     updLclEnv (\env -> env {tcl_rdr = ic_rn_local_env icxt})	$
	       thing_inside)
\end{code}


\begin{code}
tcRnStmt :: HscEnv
	 -> InteractiveContext
	 -> LStmt RdrName
	 -> IO (Maybe (InteractiveContext, [Name], LHsExpr Id))
		-- The returned [Name] is the same as the input except for
		-- ExprStmt, in which case the returned [Name] is [itName]
		--
		-- The returned TypecheckedHsExpr is of type IO [ () ],
		-- a list of the bound values, coerced to ().

tcRnStmt hsc_env ictxt rdr_stmt
  = initTcPrintErrors hsc_env iNTERACTIVE $ 
    setInteractiveContext ictxt $ do {

    -- Rename; use CmdLineMode because tcRnStmt is only used interactively
    ([rn_stmt], fvs) <- rnStmts DoExpr [rdr_stmt] ;
    traceRn (text "tcRnStmt" <+> vcat [ppr rdr_stmt, ppr rn_stmt, ppr fvs]) ;
    failIfErrsM ;
    
    -- The real work is done here
    (bound_ids, tc_expr) <- tcUserStmt rn_stmt ;
    
    traceTc (text "tcs 1") ;
    let {	-- Make all the bound ids "global" ids, now that
    		-- they're notionally top-level bindings.  This is
	    	-- important: otherwise when we come to compile an expression
	    	-- using these ids later, the byte code generator will consider
	    	-- the occurrences to be free rather than global.
	global_ids     = map (globaliseId VanillaGlobal) bound_ids ;
    
		-- Update the interactive context
	rn_env   = ic_rn_local_env ictxt ;
	type_env = ic_type_env ictxt ;

	bound_names = map idName global_ids ;
	new_rn_env  = extendLocalRdrEnv rn_env bound_names ;

		-- Remove any shadowed bindings from the type_env;
		-- they are inaccessible but might, I suppose, cause 
		-- a space leak if we leave them there
	shadowed = [ n | name <- bound_names,
			 let rdr_name = mkRdrUnqual (nameOccName name),
			 Just n <- [lookupLocalRdrEnv rn_env rdr_name] ] ;

	filtered_type_env = delListFromNameEnv type_env shadowed ;
	new_type_env = extendTypeEnvWithIds filtered_type_env global_ids ;

	new_ic = ictxt { ic_rn_local_env = new_rn_env, 
		  	 ic_type_env     = new_type_env }
    } ;

    dumpOptTcRn Opt_D_dump_tc 
    	(vcat [text "Bound Ids" <+> pprWithCommas ppr global_ids,
    	       text "Typechecked expr" <+> ppr tc_expr]) ;

    returnM (new_ic, bound_names, tc_expr)
    }
\end{code}


Here is the grand plan, implemented in tcUserStmt

	What you type			The IO [HValue] that hscStmt returns
	-------------			------------------------------------
	let pat = expr		==> 	let pat = expr in return [coerce HVal x, coerce HVal y, ...]
					bindings: [x,y,...]

	pat <- expr		==> 	expr >>= \ pat -> return [coerce HVal x, coerce HVal y, ...]
					bindings: [x,y,...]

	expr (of IO type)	==>	expr >>= \ it -> return [coerce HVal it]
	  [NB: result not printed]	bindings: [it]
	  
	expr (of non-IO type,	==>	let it = expr in print it >> return [coerce HVal it]
	  result showable)		bindings: [it]

	expr (of non-IO type, 
	  result not showable)	==>	error


\begin{code}
---------------------------
tcUserStmt :: LStmt Name -> TcM ([Id], LHsExpr Id)
tcUserStmt (L _ (ExprStmt expr _))
  = newUnique 		`thenM` \ uniq ->
    let 
	fresh_it = itName uniq
        the_bind = noLoc $ FunBind (noLoc fresh_it) False 
			     (mkMatchGroup [mkSimpleMatch [] expr])
    in
    tryTcLIE_ (do { 	-- Try this if the other fails
		traceTc (text "tcs 1b") ;
		tc_stmts [
		    nlLetStmt [HsBindGroup (unitBag the_bind) [] NonRecursive],
		    nlExprStmt (nlHsApp (nlHsVar printName) 
					      (nlHsVar fresh_it)) 	
	] })
	  (do { 	-- Try this first 
		traceTc (text "tcs 1a") ;
		tc_stmts [nlBindStmt (nlVarPat fresh_it) expr] })

tcUserStmt stmt = tc_stmts [stmt]

---------------------------
tc_stmts stmts
 = do { ioTyCon <- tcLookupTyCon ioTyConName ;
	let {
	    ret_ty    = mkListTy unitTy ;
	    io_ret_ty = mkTyConApp ioTyCon [ret_ty] ;

	    names = map unLoc (collectStmtsBinders stmts) ;

	    stmt_ctxt = SC { sc_what = DoExpr, 
			     sc_rhs  = infer_rhs,
			     sc_body = check_body,
			     sc_ty   = ret_ty } ;

	    infer_rhs rhs   = do { (rhs', rhs_ty) <- tcInferRho rhs
				 ; [pat_ty] <- unifyTyConApp ioTyCon rhs_ty
				 ; return (rhs', pat_ty) } ;
	    check_body body = tcCheckRho body io_ret_ty ;

		-- mk_return builds the expression
		--	returnIO @ [()] [coerce () x, ..,  coerce () z]
		--
		-- Despite the inconvenience of building the type applications etc,
		-- this *has* to be done in type-annotated post-typecheck form
		-- because we are going to return a list of *polymorphic* values
		-- coerced to type (). If we built a *source* stmt
		--	return [coerce x, ..., coerce z]
		-- then the type checker would instantiate x..z, and we wouldn't
		-- get their *polymorphic* values.  (And we'd get ambiguity errs
		-- if they were overloaded, since they aren't applied to anything.)
	    mk_return ret_id ids = nlHsApp (noLoc $ TyApp (nlHsVar ret_id) [ret_ty]) 
			      		   (noLoc $ ExplicitList unitTy (map mk_item ids)) ;
	    mk_item id = nlHsApp (noLoc $ TyApp (nlHsVar unsafeCoerceId) [idType id, unitTy])
		    	       (nlHsVar id) ;

	    io_ty = mkTyConApp ioTyCon []
	 } ;

	-- OK, we're ready to typecheck the stmts
	traceTc (text "tcs 2") ;
	((ids, tc_expr), lie) <- getLIE $ do {
	    (ids, tc_stmts) <- tcStmtsAndThen combine stmt_ctxt stmts	$ 
			do {
			    -- Look up the names right in the middle,
			    -- where they will all be in scope
			    ids <- mappM tcLookupId names ;
			    ret_id <- tcLookupId returnIOName ;		-- return @ IO
			    return (ids, [nlResultStmt (mk_return ret_id ids)]) } ;

	    io_ids <- mappM (tcStdSyntaxName DoOrigin io_ty) monadNames ;
	    return (ids, noLoc (HsDo DoExpr tc_stmts io_ids io_ret_ty))
	} ;

	-- Simplify the context right here, so that we fail
	-- if there aren't enough instances.  Notably, when we see
	--		e
	-- we use recoverTc_ to try	it <- e
	-- and then			let it = e
	-- It's the simplify step that rejects the first.
	traceTc (text "tcs 3") ;
	const_binds <- tcSimplifyInteractive lie ;

	-- Build result expression and zonk it
	let { expr = mkHsLet const_binds tc_expr } ;
	zonked_expr <- zonkTopLExpr expr ;
	zonked_ids  <- zonkTopBndrs ids ;

	-- None of the Ids should be of unboxed type, because we
	-- cast them all to HValues in the end!
	mappM bad_unboxed (filter (isUnLiftedType . idType) zonked_ids) ;

	return (zonked_ids, zonked_expr)
	}
  where
    combine stmt (ids, stmts) = (ids, stmt:stmts)
    bad_unboxed id = addErr (sep [ptext SLIT("GHCi can't bind a variable of unlifted type:"),
				  nest 2 (ppr id <+> dcolon <+> ppr (idType id))])
\end{code}


tcRnExpr just finds the type of an expression

\begin{code}
tcRnExpr :: HscEnv
	 -> InteractiveContext
	 -> LHsExpr RdrName
	 -> IO (Maybe Type)
tcRnExpr hsc_env ictxt rdr_expr
  = initTcPrintErrors hsc_env iNTERACTIVE $ 
    setInteractiveContext ictxt $ do {

    (rn_expr, fvs) <- rnLExpr rdr_expr ;
    failIfErrsM ;

	-- Now typecheck the expression; 
	-- it might have a rank-2 type (e.g. :t runST)
    ((tc_expr, res_ty), lie)	   <- getLIE (tcInferRho rn_expr) ;
    ((qtvs, _, dict_ids), lie_top) <- getLIE (tcSimplifyInfer smpl_doc (tyVarsOfType res_ty) lie)  ;
    tcSimplifyInteractive lie_top ;
    qtvs' <- mappM zonkQuantifiedTyVar qtvs ;

    let { all_expr_ty = mkForAllTys qtvs' $
    		        mkFunTys (map idType dict_ids)	$
    		        res_ty } ;
    zonkTcType all_expr_ty
    }
  where
    smpl_doc = ptext SLIT("main expression")
\end{code}

tcRnType just finds the kind of a type

\begin{code}
tcRnType :: HscEnv
	 -> InteractiveContext
	 -> LHsType RdrName
	 -> IO (Maybe Kind)
tcRnType hsc_env ictxt rdr_type
  = initTcPrintErrors hsc_env iNTERACTIVE $ 
    setInteractiveContext ictxt $ do {

    rn_type <- rnLHsType doc rdr_type ;
    failIfErrsM ;

	-- Now kind-check the type
    (ty', kind) <- kcHsType rn_type ;
    return kind
    }
  where
    doc = ptext SLIT("In GHCi input")

#endif /* GHCi */
\end{code}


%************************************************************************
%*									*
	More GHCi stuff, to do with browsing and getting info
%*									*
%************************************************************************

\begin{code}
#ifdef GHCI
mkExportEnv :: HscEnv -> [Module]	-- Expose these modules' exports only
 	    -> IO GlobalRdrEnv
mkExportEnv hsc_env exports
  = do	{ mb_envs <- initTcPrintErrors hsc_env iNTERACTIVE $
		     mappM getModuleExports exports 
	; case mb_envs of
	     Just envs -> return (foldr plusGlobalRdrEnv emptyGlobalRdrEnv envs)
	     Nothing   -> return emptyGlobalRdrEnv
			     -- Some error; initTc will have printed it
    }

getModuleExports :: Module -> TcM GlobalRdrEnv
getModuleExports mod 
  = do	{ iface <- load_iface mod
	; loadOrphanModules (dep_orphs (mi_deps iface))
			-- Load any orphan-module interfaces,
			-- so their instances are visible
	; avails <- exportsToAvails (mi_exports iface)
	; let { gres =  [ GRE  { gre_name = name, gre_prov = vanillaProv mod }
			| avail <- avails, name <- availNames avail ] }
	; returnM (mkGlobalRdrEnv gres) }

vanillaProv :: Module -> Provenance
-- We're building a GlobalRdrEnv as if the user imported
-- all the specified modules into the global interactive module
vanillaProv mod = Imported [ImportSpec mod mod False 
			     (srcLocSpan interactiveSrcLoc)] False
\end{code}

\begin{code}
getModuleContents
  :: HscEnv
  -> InteractiveContext
  -> Module			-- Module to inspect
  -> Bool			-- Grab just the exports, or the whole toplev
  -> IO (Maybe [IfaceDecl])

getModuleContents hsc_env ictxt mod exports_only
 = initTcPrintErrors hsc_env iNTERACTIVE (get_mod_contents exports_only)
 where
   get_mod_contents exports_only
      | not exports_only  -- We want the whole top-level type env
 			  -- so it had better be a home module
      = do { hpt <- getHpt
 	   ; case lookupModuleEnv hpt mod of
 	       Just mod_info -> return (map toIfaceDecl $
					filter wantToSee $
 				        typeEnvElts $
 				        md_types (hm_details mod_info))
 	       Nothing -> ghcError (ProgramError (showSDoc (noRdrEnvErr mod)))
 			  -- This is a system error; the module should be in the HPT
 	   }
  
      | otherwise		-- Want the exports only
      = do { iface <- load_iface mod
 	   ; avails <- exportsToAvails (mi_exports iface)
 	   ; mappM get_decl avails
    	}

   get_decl avail 
	= do { thing <- tcLookupGlobal (availName avail)
	     ; return (filter_decl (availOccs avail) (toIfaceDecl thing)) }

---------------------
filter_decl occs decl@(IfaceClass {ifSigs = sigs})
  = decl { ifSigs = filter (keep_sig occs) sigs }
filter_decl occs decl@(IfaceData {ifCons = IfDataTyCon th cons})
  = decl { ifCons = IfDataTyCon th (filter (keep_con occs) cons) }
filter_decl occs decl@(IfaceData {ifCons = IfNewTyCon con})
  | keep_con occs con = decl
  | otherwise	      = decl {ifCons = IfAbstractTyCon}	-- Hmm?
filter_decl occs decl
  = decl

keep_sig occs (IfaceClassOp occ _ _) = occ `elem` occs
keep_con occs con		     = ifConOcc con `elem` occs

availOccs avail = map nameOccName (availNames avail)

wantToSee (AnId id)    = not (isImplicitId id)
wantToSee (ADataCon _) = False	-- They'll come via their TyCon
wantToSee _ 	       = True

---------------------
load_iface mod = loadSrcInterface doc mod False {- Not boot iface -}
	       where
		 doc = ptext SLIT("context for compiling statements")

---------------------
noRdrEnvErr mod = ptext SLIT("No top-level environment available for module") 
		  <+> quotes (ppr mod)
\end{code}

\begin{code}
tcRnGetInfo :: HscEnv
	    -> InteractiveContext
	    -> RdrName
	    -> IO (Maybe [(IfaceDecl, 
			   Fixity, SrcLoc, 
			   [(IfaceInst, SrcLoc)])])
-- Used to implemnent :info in GHCi
--
-- Look up a RdrName and return all the TyThings it might be
-- A capitalised RdrName is given to us in the DataName namespace,
-- but we want to treat it as *both* a data constructor 
-- *and* as a type or class constructor; 
-- hence the call to dataTcOccs, and we return up to two results
tcRnGetInfo hsc_env ictxt rdr_name
  = initTcPrintErrors hsc_env iNTERACTIVE $ 
    setInteractiveContext ictxt $ do {

	-- If the identifier is a constructor (begins with an
	-- upper-case letter), then we need to consider both
	-- constructor and type class identifiers.
    let { rdr_names = dataTcOccs rdr_name } ;

	-- results :: [(Messages, Maybe Name)]
    results <- mapM (tryTc . lookupOccRn) rdr_names ;

    traceRn (text "xx" <+> vcat [ppr rdr_names, ppr (map snd results)]);
	-- The successful lookups will be (Just name)
    let { (warns_s, good_names) = unzip [ (msgs, name) 
					| (msgs, Just name) <- results] ;
	  errs_s = [msgs | (msgs, Nothing) <- results] } ;

	-- Fail if nothing good happened, else add warnings
    if null good_names then
		-- No lookup succeeded, so
		-- pick the first error message and report it
		-- ToDo: If one of the errors is "could be Foo.X or Baz.X",
		--	 while the other is "X is not in scope", 
		--	 we definitely want the former; but we might pick the latter
	do { addMessages (head errs_s) ; failM }
      else 			-- Add deprecation warnings
	mapM_ addMessages warns_s ;
	
	-- And lookup up the entities, avoiding duplicates, which arise
	-- because constructors and record selectors are represented by
	-- their parent declaration
    let { do_one name = do { thing <- tcLookupGlobal name
			   ; let decl = toIfaceDecl thing
		           ; fixity <- lookupFixityRn name
			   ; insts  <- lookupInsts thing
		           ; return (decl, fixity, getSrcLoc thing, 
				     map mk_inst insts) } ;
		-- For the SrcLoc, the 'thing' has better info than
		-- the 'name' because getting the former forced the
		-- declaration to be loaded into the cache
	  mk_inst dfun = (dfunToIfaceInst dfun, getSrcLoc dfun) ;
	  cmp (d1,_,_,_) (d2,_,_,_) = ifName d1 `compare` ifName d2 } ;
    results <- mapM do_one good_names ;
    return (fst (removeDups cmp results))
    }

lookupInsts :: TyThing -> TcM [DFunId]
lookupInsts (AClass cls)
  = do	{ loadImportedInsts cls []	-- [] means load all instances for cls
	; inst_envs <- tcGetInstEnvs
	; return [df | (_,_,df) <- classInstances inst_envs cls] }

lookupInsts (ATyCon tc)
  = do 	{ eps <- getEps	-- Load all instances for all classes that are
			-- in the type environment (which are all the ones
			-- we've seen in any interface file so far
	; mapM_ (\c -> loadImportedInsts c [])
		(typeEnvClasses (eps_PTE eps))
	; (pkg_ie, home_ie) <- tcGetInstEnvs	-- Search all
	; return (get home_ie ++ get pkg_ie) }
  where
    get ie = [df | (_,_,df) <- instEnvElts ie, relevant df]
    relevant df = tc_name `elemNameSet` tyClsNamesOfDFunHead (idType df)
    tc_name = tyConName tc		  

lookupInsts other = return []


toIfaceDecl :: TyThing -> IfaceDecl
toIfaceDecl thing
  = tyThingToIfaceDecl True 		-- Discard IdInfo
		       emptyNameSet	-- Show data cons
		       ext_nm (munge thing)
  where
    ext_nm n = ExtPkg (nameModule n) (nameOccName n)

	-- munge transforms a thing to it's "parent" thing
    munge (ADataCon dc) = ATyCon (dataConTyCon dc)
    munge (AnId id) = case globalIdDetails id of
			RecordSelId tc lbl -> ATyCon tc
			ClassOpId cls      -> AClass cls
			other		   -> AnId id
    munge other_thing = other_thing

#endif /* GHCI */
\end{code}

%************************************************************************
%*									*
		Degugging output
%*									*
%************************************************************************

\begin{code}
rnDump :: SDoc -> TcRn ()
-- Dump, with a banner, if -ddump-rn
rnDump doc = do { dumpOptTcRn Opt_D_dump_rn (mkDumpDoc "Renamer" doc) }

tcDump :: TcGblEnv -> TcRn ()
tcDump env
 = do { dflags <- getDOpts ;

	-- Dump short output if -ddump-types or -ddump-tc
	ifM (dopt Opt_D_dump_types dflags || dopt Opt_D_dump_tc dflags)
	    (dumpTcRn short_dump) ;

	-- Dump bindings if -ddump-tc
	dumpOptTcRn Opt_D_dump_tc (mkDumpDoc "Typechecker" full_dump)
   }
  where
    short_dump = pprTcGblEnv env
    full_dump  = pprLHsBinds (tcg_binds env)
	-- NB: foreign x-d's have undefined's in their types; 
	--     hence can't show the tc_fords

tcCoreDump mod_guts
 = do { dflags <- getDOpts ;
	ifM (dopt Opt_D_dump_types dflags || dopt Opt_D_dump_tc dflags)
 	    (dumpTcRn (pprModGuts mod_guts)) ;

	-- Dump bindings if -ddump-tc
	dumpOptTcRn Opt_D_dump_tc (mkDumpDoc "Typechecker" full_dump) }
  where
    full_dump = pprCoreBindings (mg_binds mod_guts)

-- It's unpleasant having both pprModGuts and pprModDetails here
pprTcGblEnv :: TcGblEnv -> SDoc
pprTcGblEnv (TcGblEnv { tcg_type_env = type_env, 
		        tcg_insts    = dfun_ids, 
		        tcg_rules    = rules,
			tcg_imports  = imports })
  = vcat [ ppr_types dfun_ids type_env
	 , ppr_insts dfun_ids
	 , vcat (map ppr rules)
	 , ppr_gen_tycons (typeEnvTyCons type_env)
	 , ptext SLIT("Dependent modules:") <+> ppr (moduleEnvElts (imp_dep_mods imports))
	 , ptext SLIT("Dependent packages:") <+> ppr (imp_dep_pkgs imports)]

pprModGuts :: ModGuts -> SDoc
pprModGuts (ModGuts { mg_types = type_env,
		      mg_rules = rules })
  = vcat [ ppr_types [] type_env,
	   ppr_rules rules ]


ppr_types :: [Var] -> TypeEnv -> SDoc
ppr_types dfun_ids type_env
  = text "TYPE SIGNATURES" $$ nest 4 (ppr_sigs ids)
  where
    ids = [id | id <- typeEnvIds type_env, want_sig id]
    want_sig id | opt_PprStyle_Debug = True
	        | otherwise	     = isLocalId id && 
				       isExternalName (idName id) && 
				       not (id `elem` dfun_ids)
	-- isLocalId ignores data constructors, records selectors etc.
	-- The isExternalName ignores local dictionary and method bindings
	-- that the type checker has invented.  Top-level user-defined things 
	-- have External names.

ppr_insts :: [Var] -> SDoc
ppr_insts []       = empty
ppr_insts dfun_ids = text "INSTANCES" $$ nest 4 (ppr_sigs dfun_ids)

ppr_sigs :: [Var] -> SDoc
ppr_sigs ids
	-- Print type signatures; sort by OccName 
  = vcat (map ppr_sig (sortLe le_sig ids))
  where
    le_sig id1 id2 = getOccName id1 <= getOccName id2
    ppr_sig id = ppr id <+> dcolon <+> ppr (tidyTopType (idType id))

ppr_rules :: [IdCoreRule] -> SDoc
ppr_rules [] = empty
ppr_rules rs = vcat [ptext SLIT("{-# RULES"),
		      nest 4 (pprIdRules rs),
		      ptext SLIT("#-}")]

ppr_gen_tycons []  = empty
ppr_gen_tycons tcs = vcat [ptext SLIT("Tycons with generics:"),
			   nest 2 (fsep (map ppr (filter tyConHasGenerics tcs)))]
\end{code}
