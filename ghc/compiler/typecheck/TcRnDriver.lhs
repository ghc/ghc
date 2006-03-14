%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcModule]{Typechecking a whole module}

\begin{code}
module TcRnDriver (
#ifdef GHCI
	tcRnStmt, tcRnExpr, tcRnType,
	tcRnLookupRdrName,
	tcRnLookupName,
	tcRnGetInfo,
	getModuleExports, 
#endif
	tcRnModule, 
	tcTopSrcDecls,
	tcRnExtCore
    ) where

#include "HsVersions.h"

import IO
#ifdef GHCI
import {-# SOURCE #-} TcSplice ( tcSpliceDecls )
#endif

import DynFlags		( DynFlag(..), DynFlags(..), dopt, GhcMode(..) )
import StaticFlags	( opt_PprStyle_Debug )
import Packages		( checkForPackageConflicts, mkHomeModules )
import HsSyn		( HsModule(..), HsExtCore(..), HsGroup(..), LHsDecl,
			  SpliceDecl(..), HsBind(..), LHsBinds,
			  emptyRdrGroup, emptyRnGroup, appendGroups, plusHsValBinds,
			  nlHsApp, nlHsVar, pprLHsBinds )
import RdrHsSyn		( findSplice )

import PrelNames	( runMainIOName, rootMainKey, rOOT_MAIN, mAIN,
			  main_RDR_Unqual )
import RdrName		( RdrName, mkRdrUnqual, emptyGlobalRdrEnv )
import TcHsSyn		( zonkTopDecls )
import TcExpr 		( tcInferRho )
import TcRnMonad
import TcType		( tidyTopType, tcEqType, mkTyVarTys, substTyWith )
import Inst		( showLIE )
import InstEnv		( extendInstEnvList, Instance, pprInstances, instanceDFunId )
import TcBinds		( tcTopBinds, tcHsBootSigs )
import TcDefaults	( tcDefaults )
import TcEnv		( tcExtendGlobalValEnv, iDFunId )
import TcRules		( tcRules )
import TcForeign	( tcForeignImports, tcForeignExports )
import TcInstDcls	( tcInstDecls1, tcInstDecls2 )
import TcIface		( tcExtCoreBindings, tcHiBootIface )
import TcSimplify	( tcSimplifyTop )
import TcTyClsDecls	( tcTyAndClassDecls )
import LoadIface	( loadOrphanModules )
import RnNames		( importsFromLocalDecls, rnImports, rnExports,
                          mkRdrEnvAndImports, mkExportNameSet,
			  reportUnusedNames, reportDeprecations )
import RnEnv		( lookupSrcOcc_maybe )
import RnSource		( rnSrcDecls, rnTyClDecls, checkModDeprec )
import PprCore		( pprRules, pprCoreBindings )
import CoreSyn		( CoreRule, bindersOfBinds )
import DataCon		( dataConWrapId )
import ErrUtils		( Messages, mkDumpDoc, showPass )
import Id		( Id, mkExportedLocalId, isLocalId, idName, idType )
import Var		( Var )
import Module           ( Module, ModuleEnv, moduleEnvElts, elemModuleEnv )
import OccName		( mkVarOccFS )
import Name		( Name, NamedThing(..), isExternalName, getSrcLoc, isWiredInName,
			  mkExternalName )
import NameSet
import TyCon		( tyConHasGenerics, isSynTyCon, synTyConDefn, tyConKind )
import SrcLoc		( srcLocSpan, Located(..), noLoc )
import DriverPhases	( HscSource(..), isHsBoot )
import HscTypes		( ModGuts(..), ModDetails(..), emptyModDetails,
			  HscEnv(..), ExternalPackageState(..),
			  IsBootInterface, noDependencies, 
			  Deprecs( NoDeprecs ), plusDeprecs,
			  ForeignStubs(NoStubs), TyThing(..), 
			  TypeEnv, lookupTypeEnv, hptInstances, 
			  extendTypeEnvWithIds, typeEnvIds, typeEnvTyCons, typeEnvElts,
			  emptyFixityEnv
			)
import Outputable

#ifdef GHCI
import HsSyn		( HsStmtContext(..), Stmt(..), HsExpr(..), 
			  HsLocalBinds(..), HsValBinds(..),
			  LStmt, LHsExpr, LHsType, mkMatch, emptyLocalBinds,
			  collectLStmtsBinders, collectLStmtBinders, nlVarPat,
		   	  mkFunBind, placeHolderType, noSyntaxExpr )
import RdrName		( GlobalRdrElt(..), globalRdrEnvElts,
			  unQualOK, lookupLocalRdrEnv, extendLocalRdrEnv )
import RnSource		( addTcgDUs )
import TcHsSyn		( mkHsDictLet, zonkTopLExpr, zonkTopBndrs )
import TcHsType		( kcHsType )
import TcMType		( zonkTcType, zonkQuantifiedTyVar )
import TcMatches	( tcStmts, tcDoStmt )
import TcSimplify	( tcSimplifyInteractive, tcSimplifyInfer )
import TcType		( Type, mkForAllTys, mkFunTys, mkTyConApp, tyVarsOfType, isTauTy,
			  isUnLiftedType, tyClsNamesOfDFunHead, tyClsNamesOfType, isUnitTy )
import TcEnv		( tcLookupTyCon, tcLookupId, tcLookupGlobal )
import RnTypes		( rnLHsType )
import Inst		( tcGetInstEnvs )
import InstEnv		( classInstances, instEnvElts )
import RnExpr		( rnStmts, rnLExpr )
import LoadIface	( loadSrcInterface, loadSysInterface )
import IfaceEnv		( ifaceExportNames )
import Module		( moduleSetElts, mkModuleSet )
import RnEnv		( lookupOccRn, dataTcOccs, lookupFixityRn )
import Id		( setIdType )
import MkId		( unsafeCoerceId )
import TyCon		( tyConName )
import TysWiredIn	( mkListTy, unitTy )
import IdInfo		( GlobalIdDetails(..) )
import Kind		( Kind )
import Var		( globaliseId )
import Name		( nameOccName, nameModule, isBuiltInSyntax )
import OccName		( isTcOcc )
import NameEnv		( delListFromNameEnv )
import PrelNames	( iNTERACTIVE, ioTyConName, printName, itName, 
			  bindIOName, thenIOName, returnIOName )
import HscTypes		( InteractiveContext(..),
			  ModIface(..), icPrintUnqual,
			  Dependencies(..) )
import BasicTypes	( Fixity, RecFlag(..) )
import SrcLoc		( unLoc )
#endif

import FastString	( mkFastString )
import Maybes		( MaybeErr(..) )
import Util		( sortLe )
import Bag		( unionBags, snocBag, emptyBag, unitBag, unionManyBags )

import Maybe		( isJust )
\end{code}



%************************************************************************
%*									*
	Typecheck and rename a module
%*									*
%************************************************************************


\begin{code}
tcRnModule :: HscEnv 
	   -> HscSource
	   -> Bool 		-- True <=> save renamed syntax
	   -> Located (HsModule RdrName)
	   -> IO (Messages, Maybe TcGblEnv)

tcRnModule hsc_env hsc_src save_rn_syntax
	 (L loc (HsModule maybe_mod export_ies 
			  import_decls local_decls mod_deprec))
 = do { showPass (hsc_dflags hsc_env) "Renamer/typechecker" ;

   let { this_mod = case maybe_mod of
			Nothing  -> mAIN	  -- 'module M where' is omitted
			Just (L _ mod) -> mod }	; -- The normal case
		
   initTc hsc_env hsc_src this_mod $ 
   setSrcSpan loc $
   do {
		-- Deal with imports;
	rn_imports <- rnImports import_decls ;
        (rdr_env, imports) <- mkRdrEnvAndImports rn_imports ;

	let { dep_mods :: ModuleEnv (Module, IsBootInterface)
	    ; dep_mods = imp_dep_mods imports

		-- We want instance declarations from all home-package
		-- modules below this one, including boot modules, except
		-- ourselves.  The 'except ourselves' is so that we don't
		-- get the instances from this module's hs-boot file
	    ; want_instances :: Module -> Bool
	    ; want_instances mod = mod `elemModuleEnv` dep_mods
				   && mod /= this_mod
	    ; home_insts = hptInstances hsc_env want_instances
	    } ;

		-- Record boot-file info in the EPS, so that it's 
		-- visible to loadHiBootInterface in tcRnSrcDecls,
		-- and any other incrementally-performed imports
	updateEps_ (\eps -> eps { eps_is_boot = dep_mods }) ;

	checkConflicts imports this_mod $ do {

		-- Update the gbl env
	updGblEnv ( \ gbl -> 
		gbl { tcg_rdr_env  = rdr_env,
		      tcg_inst_env = extendInstEnvList (tcg_inst_env gbl) home_insts,
		      tcg_imports  = tcg_imports gbl `plusImportAvails` imports,
                      tcg_rn_imports = if save_rn_syntax then
                                         Just rn_imports
                                       else
                                         Nothing,
		      tcg_rn_decls = if save_rn_syntax then
					Just emptyRnGroup
				     else
					Nothing })
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
	tcg_env <- if isHsBoot hsc_src then
			tcRnHsBootDecls local_decls
		   else	
			tcRnSrcDecls local_decls ;
	setGblEnv tcg_env		$ do {

	traceRn (text "rn3") ;

		-- Report the use of any deprecated things
		-- We do this before processsing the export list so
		-- that we don't bleat about re-exporting a deprecated
		-- thing (especially via 'module Foo' export item)
		-- Only uses in the body of the module are complained about
	reportDeprecations tcg_env ;

		-- Process the export list
	rn_exports <- rnExports export_ies ;
        let { liftM2' fn a b = do a' <- a; b' <- b; return (fn a' b') } ;
        exports <- mkExportNameSet (isJust maybe_mod) (liftM2' (,) rn_exports export_ies) ;

		-- Check whether the entire module is deprecated
		-- This happens only once per module
	let { mod_deprecs = checkModDeprec mod_deprec } ;

		-- Add exports and deprecations to envt
	let { final_env  = tcg_env { tcg_exports = exports,
                                     tcg_rn_exports = if save_rn_syntax then
                                                         rn_exports
                                                      else Nothing,
				     tcg_dus = tcg_dus tcg_env `plusDU` usesOnly exports,
				     tcg_deprecs = tcg_deprecs tcg_env `plusDeprecs` 
						   mod_deprecs }
		-- A module deprecation over-rides the earlier ones
	     } ;

		-- Report unused names
 	reportUnusedNames export_ies final_env ;

		-- Dump output and return
	tcDump final_env ;
	return final_env
    }}}}}


-- The program is not allowed to contain two modules with the same
-- name, and we check for that here.  It could happen if the home package
-- contains a module that is also present in an external package, for example.
checkConflicts imports this_mod and_then = do
   dflags <- getDOpts
   let 
	dep_mods = this_mod : map fst (moduleEnvElts (imp_dep_mods imports))
		-- don't forget to include the current module!

	mb_dep_pkgs = checkForPackageConflicts 
				dflags dep_mods (imp_dep_pkgs imports)
   --
   case mb_dep_pkgs of
     Failed msg -> 
	do addErr msg; failM
     Succeeded _ -> 
	updGblEnv (\gbl -> gbl{ tcg_home_mods = mkHomeModules dep_mods })
	   and_then
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

   initTc hsc_env ExtCoreFile this_mod $ do {

   let { ldecls  = map noLoc decls } ;

	-- Deal with the type declarations; first bring their stuff
	-- into scope, then rname them, then type check them
   tcg_env  <- importsFromLocalDecls (mkFakeGroup ldecls) ;

   setGblEnv tcg_env $ do {

   rn_decls <- rnTyClDecls ldecls ;
   failIfErrsM ;

	-- Dump trace of renaming part
   rnDump (ppr rn_decls) ;

	-- Typecheck them all together so that
	-- any mutually recursive types are done right
   tcg_env <- checkNoErrs (tcTyAndClassDecls emptyModDetails rn_decls) ;
	-- Make the new type env available to stuff slurped from interface files

   setGblEnv tcg_env $ do {
   
	-- Now the core bindings
   core_binds <- initIfaceExtCore (tcExtCoreBindings src_binds) ;

	-- Wrap up
   let {
	bndrs 	   = bindersOfBinds core_binds ;
	my_exports = mkNameSet (map idName bndrs) ;
		-- ToDo: export the data types also?

	final_type_env = extendTypeEnvWithIds (tcg_type_env tcg_env) bndrs ;

	mod_guts = ModGuts {	mg_module   = this_mod,
				mg_boot	    = False,
				mg_usages   = [],		-- ToDo: compute usage
				mg_dir_imps = [],		-- ??
				mg_deps     = noDependencies,	-- ??
				mg_home_mods = mkHomeModules [], -- ?? wrong!!
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
  = emptyRdrGroup { hs_tyclds = decls }
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
 = do { 	-- Load the hi-boot interface for this module, if any
		-- We do this now so that the boot_names can be passed
		-- to tcTyAndClassDecls, because the boot_names are 
		-- automatically considered to be loop breakers
	mod <- getModule ;
	boot_iface <- tcHiBootIface mod ;

	  	-- Do all the declarations
	(tc_envs, lie) <- getLIE (tc_rn_src_decls boot_iface decls) ;

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

	tcDump tcg_env ;
	(bind_ids, binds', fords', rules') <- zonkTopDecls (binds `unionBags` inst_binds)
							   rules fords ;

	let { final_type_env = extendTypeEnvWithIds type_env bind_ids
	    ; tcg_env' = tcg_env { tcg_type_env = final_type_env,
			 	   tcg_binds = binds',
				   tcg_rules = rules', 
				   tcg_fords = fords' } } ;

	-- Make the new type env available to stuff slurped from interface files
	writeMutVar (tcg_type_env_var tcg_env) final_type_env ;

	-- Compare the hi-boot iface (if any) with the real thing
 	dfun_binds <- checkHiBootIface tcg_env' boot_iface ;

	return (tcg_env' { tcg_binds = tcg_binds tcg_env' `unionBags` dfun_binds }) 
   }

tc_rn_src_decls :: ModDetails -> [LHsDecl RdrName] -> TcM (TcGblEnv, TcLclEnv)
-- Loops around dealing with each top level inter-splice group 
-- in turn, until it's dealt with the entire module
tc_rn_src_decls boot_details ds
 = do { let { (first_group, group_tail) = findSplice ds } ;
		-- If ds is [] we get ([], Nothing)

	-- Type check the decls up to, but not including, the first splice
	tc_envs@(tcg_env,tcl_env) <- tcRnGroup boot_details first_group ;

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
	tc_rn_src_decls boot_details (spliced_decls ++ rest_ds)
#endif /* GHCI */
    }}}
\end{code}

%************************************************************************
%*									*
	Compiling hs-boot source files, and
	comparing the hi-boot interface with the real thing
%*									*
%************************************************************************

\begin{code}
tcRnHsBootDecls :: [LHsDecl RdrName] -> TcM TcGblEnv
tcRnHsBootDecls decls
   = do { let { (first_group, group_tail) = findSplice decls }

	; case group_tail of
	     Just stuff -> spliceInHsBootErr stuff
	     Nothing    -> return ()

		-- Rename the declarations
	; (tcg_env, rn_group) <- rnTopSrcDecls first_group
	; setGblEnv tcg_env $ do {

	-- Todo: check no foreign decls, no rules, no default decls

		-- Typecheck type/class decls
	; traceTc (text "Tc2")
	; let tycl_decls = hs_tyclds rn_group
	; tcg_env <- checkNoErrs (tcTyAndClassDecls emptyModDetails tycl_decls)
	; setGblEnv tcg_env	$ do {

		-- Typecheck instance decls
	; traceTc (text "Tc3")
	; (tcg_env, inst_infos, _binds) <- tcInstDecls1 tycl_decls (hs_instds rn_group)
	; setGblEnv tcg_env	$ do {

		-- Typecheck value declarations
	; traceTc (text "Tc5") 
	; val_ids <- tcHsBootSigs (hs_valds rn_group)

		-- Wrap up
		-- No simplification or zonking to do
	; traceTc (text "Tc7a")
	; gbl_env <- getGblEnv 
	
		-- Make the final type-env
		-- Include the dfun_ids so that their type sigs get
		-- are written into the interface file
	; let { type_env0 = tcg_type_env gbl_env
	      ; type_env1 = extendTypeEnvWithIds type_env0 val_ids
	      ; type_env2 = extendTypeEnvWithIds type_env1 dfun_ids 
	      ; dfun_ids = map iDFunId inst_infos }
	; return (gbl_env { tcg_type_env = type_env2 }) 
   }}}}

spliceInHsBootErr (SpliceDecl (L loc _), _)
  = addErrAt loc (ptext SLIT("Splices are not allowed in hs-boot files"))
\end{code}

Once we've typechecked the body of the module, we want to compare what
we've found (gathered in a TypeEnv) with the hi-boot details (if any).

\begin{code}
checkHiBootIface :: TcGblEnv -> ModDetails -> TcM (LHsBinds Id)
-- Compare the hi-boot file for this module (if there is one)
-- with the type environment we've just come up with
-- In the common case where there is no hi-boot file, the list
-- of boot_names is empty.
--
-- The bindings we return give bindings for the dfuns defined in the
-- hs-boot file, such as 	$fbEqT = $fEqT

checkHiBootIface
	(TcGblEnv { tcg_insts = local_insts, tcg_type_env = local_type_env })
	(ModDetails { md_insts = boot_insts, md_types = boot_type_env })
  = do	{ mapM_ check_one (typeEnvElts boot_type_env)
	; dfun_binds <- mapM check_inst boot_insts
	; return (unionManyBags dfun_binds) }
  where
    check_one boot_thing
      | no_check name
      = return ()	
      | otherwise	
      = case lookupTypeEnv local_type_env name of
	  Nothing 	  -> addErrTc (missingBootThing boot_thing)
	  Just real_thing -> check_thing boot_thing real_thing
      where
	name = getName boot_thing

    no_check name = isWiredInName name	-- No checking for wired-in names.  In particular,
					-- 'error' is handled by a rather gross hack
					-- (see comments in GHC.Err.hs-boot)
		  || name `elem` dfun_names
    dfun_names = map getName boot_insts

    check_inst boot_inst
	= case [dfun | inst <- local_insts, 
		       let dfun = instanceDFunId inst,
		       idType dfun `tcEqType` boot_inst_ty ] of
	    [] -> do { addErrTc (instMisMatch boot_inst); return emptyBag }
	    (dfun:_) -> return (unitBag $ noLoc $ VarBind local_boot_dfun (nlHsVar dfun))
	where
	  boot_dfun = instanceDFunId boot_inst
	  boot_inst_ty = idType boot_dfun
	  local_boot_dfun = mkExportedLocalId (idName boot_dfun) boot_inst_ty

----------------
check_thing (ATyCon boot_tc) (ATyCon real_tc)
  | isSynTyCon boot_tc && isSynTyCon real_tc,
    defn1 `tcEqType` substTyWith tvs2 (mkTyVarTys tvs1) defn2
  = return ()

  | tyConKind boot_tc == tyConKind real_tc
  = return ()
  where
    (tvs1, defn1) = synTyConDefn boot_tc
    (tvs2, defn2) = synTyConDefn boot_tc

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
  = ppr thing <+> ptext SLIT("is defined in the hs-boot file, but not in the module")
bootMisMatch thing
  = ppr thing <+> ptext SLIT("has conflicting definitions in the module and its hs-boot file")
instMisMatch inst
  = hang (ppr inst)
       2 (ptext SLIT("is defined in the hs-boot file, but not in the module"))
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
tcRnGroup :: ModDetails -> HsGroup RdrName -> TcM (TcGblEnv, TcLclEnv)
	-- Returns the variables free in the decls, for unused-binding reporting
tcRnGroup boot_details decls
 = do {		-- Rename the declarations
	(tcg_env, rn_decls) <- rnTopSrcDecls decls ;
	setGblEnv tcg_env $ do {

		-- Typecheck the declarations
	tcTopSrcDecls boot_details rn_decls 
  }}

------------------------------------------------
rnTopSrcDecls :: HsGroup RdrName -> TcM (TcGblEnv, HsGroup Name)
rnTopSrcDecls group
 = do { 	-- Bring top level binders into scope
	tcg_env <- importsFromLocalDecls group ;
	setGblEnv tcg_env $ do {

	failIfErrsM ;	-- No point in continuing if (say) we have duplicate declarations

		-- Rename the source decls
	(tcg_env, rn_decls) <- rnSrcDecls group ;
	failIfErrsM ;

		-- save the renamed syntax, if we want it
	let { tcg_env'
	        | Just grp <- tcg_rn_decls tcg_env
	          = tcg_env{ tcg_rn_decls = Just (appendGroups grp rn_decls) }
	        | otherwise
	           = tcg_env };

		-- Dump trace of renaming part
	rnDump (ppr rn_decls) ;

	return (tcg_env', rn_decls)
   }}

------------------------------------------------
tcTopSrcDecls :: ModDetails -> HsGroup Name -> TcM (TcGblEnv, TcLclEnv)
tcTopSrcDecls boot_details
	(HsGroup { hs_tyclds = tycl_decls, 
		   hs_instds = inst_decls,
		   hs_fords  = foreign_decls,
		   hs_defds  = default_decls,
		   hs_ruleds = rule_decls,
		   hs_valds  = val_binds })
 = do {		-- Type-check the type and class decls, and all imported decls
		-- The latter come in via tycl_decls
        traceTc (text "Tc2") ;

	tcg_env <- checkNoErrs (tcTyAndClassDecls boot_details tycl_decls) ;
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
	(tc_val_binds, tcl_env) <- tcTopBinds (val_binds `plusHsValBinds` deriv_binds) ;
	setLclTypeEnv tcl_env 	$ do {

	     	-- Second pass over class and instance declarations, 
        traceTc (text "Tc6") ;
	(inst_binds, tcl_env) <- tcInstDecls2 tycl_decls inst_infos ;
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
  	return (tcg_env', tcl_env)
    }}}}}}
\end{code}


%************************************************************************
%*									*
	Checking for 'main'
%*									*
%************************************************************************

\begin{code}
checkMain :: TcM TcGblEnv
-- If we are in module Main, check that 'main' is defined.
checkMain 
  = do { ghc_mode <- getGhcMode ;
	 tcg_env   <- getGblEnv ;
	 dflags    <- getDOpts ;
	 let { main_mod = mainModIs dflags ;
	       main_fn  = case mainFunIs dflags of {
				Just fn -> mkRdrUnqual (mkVarOccFS (mkFastString fn)) ;
				Nothing -> main_RDR_Unqual } } ;
	
	 check_main ghc_mode tcg_env main_mod main_fn
    }


check_main ghc_mode tcg_env main_mod main_fn
 | mod /= main_mod
 = traceTc (text "checkMain not" <+> ppr main_mod <+> ppr mod) >>
   return tcg_env

 | otherwise
 = addErrCtxt mainCtxt			$
   do	{ mb_main <- lookupSrcOcc_maybe main_fn
		-- Check that 'main' is in scope
		-- It might be imported from another module!
	; case mb_main of {
	     Nothing -> do { traceTc (text "checkMain fail" <+> ppr main_mod <+> ppr main_fn)
			   ; complain_no_main	
			   ; return tcg_env } ;
	     Just main_name -> do
	{ traceTc (text "checkMain found" <+> ppr main_mod <+> ppr main_fn)
	; let { rhs = nlHsApp (nlHsVar runMainIOName) (nlHsVar main_name) }
		   	-- :Main.main :: IO () = runMainIO main 

	; (main_expr, ty) <- setSrcSpan (srcLocSpan (getSrcLoc main_name)) $
			     tcInferRho rhs

	-- The function that the RTS invokes is always :Main.main,
	-- which we call root_main_id.  
	-- (Because GHC allows the user to have a module not called 
	-- Main as the main module, we can't rely on the main function
	-- being called "Main.main".  That's why root_main_id has a fixed
	-- module ":Main".)
	-- We also make root_main_id an implicit Id, by making main_name
	-- its parent (hence (Just main_name)).  That has the effect
	-- of preventing its type and unfolding from getting out into
	-- the interface file. Otherwise we can end up with two defns
	-- for 'main' in the interface file!

	; let { root_main_name =  mkExternalName rootMainKey rOOT_MAIN 
				   (mkVarOccFS FSLIT("main")) 
				   (Just main_name) (getSrcLoc main_name)
	      ; root_main_id = mkExportedLocalId root_main_name ty
	      ; main_bind    = noLoc (VarBind root_main_id main_expr) }

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
 
    complain_no_main | ghc_mode == Interactive = return ()
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
setInteractiveContext :: HscEnv -> InteractiveContext -> TcRn a -> TcRn a
setInteractiveContext hsc_env icxt thing_inside 
  = let 
	-- Initialise the tcg_inst_env with instances 
	-- from all home modules.  This mimics the more selective
	-- call to hptInstances in tcRnModule
	dfuns = hptInstances hsc_env (\mod -> True)
    in
    updGblEnv (\env -> env { 
	tcg_rdr_env  = ic_rn_gbl_env icxt,
	tcg_type_env = ic_type_env   icxt,
	tcg_inst_env = extendInstEnvList (tcg_inst_env env) dfuns }) $

    updLclEnv (\env -> env { tcl_rdr = ic_rn_local_env icxt })	$

    do	{ traceTc (text "setIC" <+> ppr (ic_type_env icxt))
 	; thing_inside }
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
    setInteractiveContext hsc_env ictxt $ do {

    -- Rename; use CmdLineMode because tcRnStmt is only used interactively
    (([rn_stmt], _), fvs) <- rnStmts DoExpr [rdr_stmt] (return ((), emptyFVs)) ;
    traceRn (text "tcRnStmt" <+> vcat [ppr rdr_stmt, ppr rn_stmt, ppr fvs]) ;
    failIfErrsM ;
    
    -- The real work is done here
    (bound_ids, tc_expr) <- mkPlan rn_stmt ;
    zonked_expr <- zonkTopLExpr tc_expr ;
    zonked_ids  <- zonkTopBndrs bound_ids ;
    
	-- None of the Ids should be of unboxed type, because we
	-- cast them all to HValues in the end!
    mappM bad_unboxed (filter (isUnLiftedType . idType) zonked_ids) ;

    traceTc (text "tcs 1") ;
    let {	-- (a) Make all the bound ids "global" ids, now that
    		--     they're notionally top-level bindings.  This is
	    	--     important: otherwise when we come to compile an expression
	    	--     using these ids later, the byte code generator will consider
	    	--     the occurrences to be free rather than global.
		-- 
		-- (b) Tidy their types; this is important, because :info may
		--     ask to look at them, and :info expects the things it looks
		--     up to have tidy types
	global_ids = map globaliseAndTidy zonked_ids ;
    
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
    	       text "Typechecked expr" <+> ppr zonked_expr]) ;

    returnM (new_ic, bound_names, zonked_expr)
    }
  where
    bad_unboxed id = addErr (sep [ptext SLIT("GHCi can't bind a variable of unlifted type:"),
				  nest 2 (ppr id <+> dcolon <+> ppr (idType id))])

globaliseAndTidy :: Id -> Id
globaliseAndTidy id
-- Give the Id a Global Name, and tidy its type
  = setIdType (globaliseId VanillaGlobal id) tidy_type
  where
    tidy_type = tidyTopType (idType id)
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
type PlanResult = ([Id], LHsExpr Id)
type Plan = TcM PlanResult

runPlans :: [Plan] -> TcM PlanResult
-- Try the plans in order.  If one fails (by raising an exn), try the next.
-- If one succeeds, take it.
runPlans []     = panic "runPlans"
runPlans [p]    = p
runPlans (p:ps) = tryTcLIE_ (runPlans ps) p

--------------------
mkPlan :: LStmt Name -> TcM PlanResult
mkPlan (L loc (ExprStmt expr _ _))	-- An expression typed at the prompt 
  = do	{ uniq <- newUnique		-- is treated very specially
	; let fresh_it  = itName uniq
	      the_bind  = L loc $ mkFunBind (L loc fresh_it) matches
	      matches   = [mkMatch [] expr emptyLocalBinds]
	      let_stmt  = L loc $ LetStmt (HsValBinds (ValBindsOut [(NonRecursive,unitBag the_bind)] []))
	      bind_stmt = L loc $ BindStmt (nlVarPat fresh_it) expr
					   (HsVar bindIOName) noSyntaxExpr 
	      print_it  = L loc $ ExprStmt (nlHsApp (nlHsVar printName) (nlHsVar fresh_it))
			          	   (HsVar thenIOName) placeHolderType

	-- The plans are:
	--	[it <- e; print it]	but not if it::()
	--	[it <- e]		
	--	[let it = e; print it]	
	; runPlans [	-- Plan A
		    do { stuff@([it_id], _) <- tcGhciStmts [bind_stmt, print_it]
		       ; it_ty <- zonkTcType (idType it_id)
		       ; ifM (isUnitTy it_ty) failM
		       ; return stuff },

			-- Plan B; a naked bind statment
		    tcGhciStmts [bind_stmt],	

			-- Plan C; check that the let-binding is typeable all by itself.
			-- If not, fail; if so, try to print it.
			-- The two-step process avoids getting two errors: one from
			-- the expression itself, and one from the 'print it' part
			-- This two-step story is very clunky, alas
		    do { checkNoErrs (tcGhciStmts [let_stmt]) 
				--- checkNoErrs defeats the error recovery of let-bindings
		       ; tcGhciStmts [let_stmt, print_it] }
	  ]}

mkPlan stmt@(L loc (BindStmt {}))
  | [L _ v] <- collectLStmtBinders stmt		-- One binder, for a bind stmt 
  = do	{ let print_v  = L loc $ ExprStmt (nlHsApp (nlHsVar printName) (nlHsVar v))
			          	   (HsVar thenIOName) placeHolderType
	-- The plans are:
	--	[stmt; print v]		but not if v::()
	--	[stmt]
	; runPlans [do { stuff@([v_id], _) <- tcGhciStmts [stmt, print_v]
		       ; v_ty <- zonkTcType (idType v_id)
		       ; ifM (isUnitTy v_ty || not (isTauTy v_ty)) failM
		       ; return stuff },
		    tcGhciStmts [stmt]
	  ]}

mkPlan stmt
  = tcGhciStmts [stmt]

---------------------------
tcGhciStmts :: [LStmt Name] -> TcM PlanResult
tcGhciStmts stmts
 = do { ioTyCon <- tcLookupTyCon ioTyConName ;
	ret_id  <- tcLookupId returnIOName ;		-- return @ IO
	let {
	    io_ty     = mkTyConApp ioTyCon [] ;
	    ret_ty    = mkListTy unitTy ;
	    io_ret_ty = mkTyConApp ioTyCon [ret_ty] ;

	    names = map unLoc (collectLStmtsBinders stmts) ;

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
	    mk_return ids = nlHsApp (noLoc $ TyApp (nlHsVar ret_id) [ret_ty]) 
			 	    (noLoc $ ExplicitList unitTy (map mk_item ids)) ;
	    mk_item id = nlHsApp (noLoc $ TyApp (nlHsVar unsafeCoerceId) [idType id, unitTy])
		    	         (nlHsVar id) 
	 } ;

	-- OK, we're ready to typecheck the stmts
	traceTc (text "tcs 2") ;
	((tc_stmts, ids), lie) <- getLIE $ 
				  tcStmts DoExpr (tcDoStmt io_ty) stmts io_ret_ty $ \ _ ->
				  mappM tcLookupId names ;
					-- Look up the names right in the middle,
					-- where they will all be in scope

	-- Simplify the context
	const_binds <- checkNoErrs (tcSimplifyInteractive lie) ;
		-- checkNoErrs ensures that the plan fails if context redn fails

	return (ids, mkHsDictLet const_binds $
		     noLoc (HsDo DoExpr tc_stmts (mk_return ids) io_ret_ty))
    }
\end{code}


tcRnExpr just finds the type of an expression

\begin{code}
tcRnExpr :: HscEnv
	 -> InteractiveContext
	 -> LHsExpr RdrName
	 -> IO (Maybe Type)
tcRnExpr hsc_env ictxt rdr_expr
  = initTcPrintErrors hsc_env iNTERACTIVE $ 
    setInteractiveContext hsc_env ictxt $ do {

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
    setInteractiveContext hsc_env ictxt $ do {

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
-- ASSUMES that the module is either in the HomePackageTable or is
-- a package module with an interface on disk.  If neither of these is
-- true, then the result will be an error indicating the interface
-- could not be found.
getModuleExports :: HscEnv -> Module -> IO (Messages, Maybe NameSet)
getModuleExports hsc_env mod
  = initTc hsc_env HsSrcFile iNTERACTIVE (tcGetModuleExports mod)

tcGetModuleExports :: Module -> TcM NameSet
tcGetModuleExports mod = do
  iface <- load_iface mod
  loadOrphanModules (dep_orphs (mi_deps iface))
  		-- Load any orphan-module interfaces,
  		-- so their instances are visible
  ifaceExportNames (mi_exports iface)

load_iface mod = loadSrcInterface doc mod False {- Not boot iface -}
	       where
		 doc = ptext SLIT("context for compiling statements")


tcRnLookupRdrName :: HscEnv -> RdrName -> IO (Maybe [Name])
tcRnLookupRdrName hsc_env rdr_name 
  = initTcPrintErrors hsc_env iNTERACTIVE $ 
    setInteractiveContext hsc_env (hsc_IC hsc_env) $ 
    lookup_rdr_name rdr_name

lookup_rdr_name rdr_name = do {
	-- If the identifier is a constructor (begins with an
	-- upper-case letter), then we need to consider both
	-- constructor and type class identifiers.
    let { rdr_names = dataTcOccs rdr_name } ;

	-- results :: [Either Messages Name]
    results <- mapM (tryTcErrs . lookupOccRn) rdr_names ;

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
    
    return good_names
 }


tcRnLookupName :: HscEnv -> Name -> IO (Maybe TyThing)
tcRnLookupName hsc_env name
  = initTcPrintErrors hsc_env iNTERACTIVE $ 
    setInteractiveContext hsc_env (hsc_IC hsc_env) $
    tcLookupGlobal name


tcRnGetInfo :: HscEnv
	    -> Name
	    -> IO (Maybe (TyThing, Fixity, [Instance]))

-- Used to implemnent :info in GHCi
--
-- Look up a RdrName and return all the TyThings it might be
-- A capitalised RdrName is given to us in the DataName namespace,
-- but we want to treat it as *both* a data constructor 
--  *and* as a type or class constructor; 
-- hence the call to dataTcOccs, and we return up to two results
tcRnGetInfo hsc_env name
  = initTcPrintErrors hsc_env iNTERACTIVE $ 
    let ictxt = hsc_IC hsc_env in
    setInteractiveContext hsc_env ictxt $ do

	-- Load the interface for all unqualified types and classes
	-- That way we will find all the instance declarations
	-- (Packages have not orphan modules, and we assume that
	--  in the home package all relevant modules are loaded.)
    loadUnqualIfaces ictxt

    thing  <- tcLookupGlobal name
    fixity <- lookupFixityRn name
    ispecs <- lookupInsts (icPrintUnqual ictxt) thing
    return (thing, fixity, ispecs)


lookupInsts :: PrintUnqualified -> TyThing -> TcM [Instance]
-- Filter the instances by the ones whose tycons (or clases resp) 
-- are in scope unqualified.  Otherwise we list a whole lot too many!
lookupInsts print_unqual (AClass cls)
  = do	{ inst_envs <- tcGetInstEnvs
	; return [ ispec
		 | ispec <- classInstances inst_envs cls
		 , plausibleDFun print_unqual (instanceDFunId ispec) ] }

lookupInsts print_unqual (ATyCon tc)
  = do 	{ eps <- getEps	-- Load all instances for all classes that are
			-- in the type environment (which are all the ones
			-- we've seen in any interface file so far)
	; (pkg_ie, home_ie) <- tcGetInstEnvs	-- Search all
	; return [ ispec
		 | ispec <- instEnvElts home_ie ++ instEnvElts pkg_ie
		 , let dfun = instanceDFunId ispec
		 , relevant dfun
		 , plausibleDFun print_unqual dfun ] }
  where
    relevant df = tc_name `elemNameSet` tyClsNamesOfDFunHead (idType df)
    tc_name     = tyConName tc		  

lookupInsts print_unqual other = return []

plausibleDFun print_unqual dfun	-- Dfun involving only names that print unqualified
  = all ok (nameSetToList (tyClsNamesOfType (idType dfun)))
  where
    ok name | isBuiltInSyntax name = True
	    | isExternalName name  = print_unqual (nameModule name) (nameOccName name)
	    | otherwise		   = True

loadUnqualIfaces :: InteractiveContext -> TcM ()
-- Load the home module for everything that is in scope unqualified
-- This is so that we can accurately report the instances for 
-- something
loadUnqualIfaces ictxt
  = initIfaceTcRn $
    mapM_ (loadSysInterface doc) (moduleSetElts (mkModuleSet unqual_mods))
  where
    unqual_mods = [ nameModule name
		  | gre <- globalRdrEnvElts (ic_rn_gbl_env ictxt),
		    let name = gre_name gre,
		    isTcOcc (nameOccName name),  -- Types and classes only
		    unQualOK gre ]		 -- In scope unqualified
    doc = ptext SLIT("Need interface for module whose export(s) are in scope unqualified")
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


ppr_types :: [Instance] -> TypeEnv -> SDoc
ppr_types ispecs type_env
  = text "TYPE SIGNATURES" $$ nest 4 (ppr_sigs ids)
  where
    dfun_ids = map instanceDFunId ispecs
    ids = [id | id <- typeEnvIds type_env, want_sig id]
    want_sig id | opt_PprStyle_Debug = True
	        | otherwise	     = isLocalId id && 
				       isExternalName (idName id) && 
				       not (id `elem` dfun_ids)
	-- isLocalId ignores data constructors, records selectors etc.
	-- The isExternalName ignores local dictionary and method bindings
	-- that the type checker has invented.  Top-level user-defined things 
	-- have External names.

ppr_insts :: [Instance] -> SDoc
ppr_insts []     = empty
ppr_insts ispecs = text "INSTANCES" $$ nest 2 (pprInstances ispecs)

ppr_sigs :: [Var] -> SDoc
ppr_sigs ids
	-- Print type signatures; sort by OccName 
  = vcat (map ppr_sig (sortLe le_sig ids))
  where
    le_sig id1 id2 = getOccName id1 <= getOccName id2
    ppr_sig id = ppr id <+> dcolon <+> ppr (tidyTopType (idType id))

ppr_rules :: [CoreRule] -> SDoc
ppr_rules [] = empty
ppr_rules rs = vcat [ptext SLIT("{-# RULES"),
		      nest 4 (pprRules rs),
		      ptext SLIT("#-}")]

ppr_gen_tycons []  = empty
ppr_gen_tycons tcs = vcat [ptext SLIT("Tycons with generics:"),
			   nest 2 (fsep (map ppr (filter tyConHasGenerics tcs)))]
\end{code}
