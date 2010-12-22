%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcModule]{Typechecking a whole module}

\begin{code}
module TcRnDriver (
#ifdef GHCI
	tcRnStmt, tcRnExpr, tcRnType,
	tcRnLookupRdrName,
	getModuleExports, 
#endif
	tcRnLookupName,
	tcRnGetInfo,
	tcRnModule, 
	tcTopSrcDecls,
	tcRnExtCore
    ) where

#ifdef GHCI
import {-# SOURCE #-} TcSplice ( tcSpliceDecls )
#endif

import DynFlags
import StaticFlags
import HsSyn
import PrelNames
import RdrName
import TcHsSyn
import TcExpr
import TcRnMonad
import Coercion
import FamInst
import InstEnv
import FamInstEnv
import TcAnnotations
import TcBinds
import TcType	( tidyTopType )
import TcDefaults
import TcEnv
import TcRules
import TcForeign
import TcInstDcls
import TcIface
import TcMType
import MkIface
import IfaceSyn
import TcSimplify
import TcTyClsDecls
import LoadIface
import RnNames
import RnEnv
import RnSource
import PprCore
import CoreSyn
import ErrUtils
import Id
import VarEnv
import Var
import Module
import UniqFM
import Name
import NameEnv
import NameSet
import TyCon
import TysPrim
import SrcLoc
import HscTypes
import ListSetOps
import Outputable
import DataCon
import Type
import Class
import TcType   ( tyClsNamesOfDFunHead )
import Inst	( tcGetInstEnvs )
import Data.List ( sortBy )

#ifdef GHCI
import TcType   ( isUnitTy, isTauTy )
import CoreUtils( mkPiTypes )
import TcHsType
import TcMatches
import RnTypes
import RnExpr
import IfaceEnv
import MkId
import BasicTypes
import TidyPgm	  ( globaliseAndTidyId )
import TysWiredIn ( unitTy, mkListTy )
#endif

import FastString
import Maybes
import Util
import Bag

import Control.Monad

#include "HsVersions.h"
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
			  import_decls local_decls mod_deprec
			  maybe_doc_hdr))
 = do { showPass (hsc_dflags hsc_env) "Renamer/typechecker" ;

   let { this_pkg = thisPackage (hsc_dflags hsc_env) ;
	 this_mod = case maybe_mod of
			Nothing  -> mAIN	-- 'module M where' is omitted
			Just (L _ mod) -> mkModule this_pkg mod } ;
						-- The normal case
		
   initTc hsc_env hsc_src save_rn_syntax this_mod $ 
   setSrcSpan loc $
   do {		-- Deal with imports;
	tcg_env <- tcRnImports hsc_env this_mod import_decls ;
	setGblEnv tcg_env		$ do {

		-- Load the hi-boot interface for this module, if any
		-- We do this now so that the boot_names can be passed
		-- to tcTyAndClassDecls, because the boot_names are 
		-- automatically considered to be loop breakers
		--
		-- Do this *after* tcRnImports, so that we know whether
		-- a module that we import imports us; and hence whether to
		-- look for a hi-boot file
	boot_iface <- tcHiBootIface hsc_src this_mod ;

		-- Rename and type check the declarations
	traceRn (text "rn1a") ;
	tcg_env <- if isHsBoot hsc_src then
			tcRnHsBootDecls local_decls
		   else	
			tcRnSrcDecls boot_iface local_decls ;
	setGblEnv tcg_env		$ do {

		-- Report the use of any deprecated things
		-- We do this *before* processsing the export list so
		-- that we don't bleat about re-exporting a deprecated
		-- thing (especially via 'module Foo' export item)
		-- That is, only uses in the *body* of the module are complained about
	traceRn (text "rn3") ;
	failIfErrsM ;	-- finishWarnings crashes sometimes 
			-- as a result of typechecker repairs (e.g. unboundNames)
	tcg_env <- finishWarnings (hsc_dflags hsc_env) mod_deprec tcg_env ;

		-- Process the export list
        traceRn (text "rn4a: before exports");
	tcg_env <- rnExports (isJust maybe_mod) export_ies tcg_env ;
	traceRn (text "rn4b: after exportss") ;

                -- Check that main is exported (must be after rnExports)
        checkMainExported tcg_env ;

	-- Compare the hi-boot iface (if any) with the real thing
	-- Must be done after processing the exports
 	tcg_env <- checkHiBootIface tcg_env boot_iface ;

	-- The new type env is already available to stuff slurped from 
	-- interface files, via TcEnv.updateGlobalTypeEnv
	-- It's important that this includes the stuff in checkHiBootIface, 
	-- because the latter might add new bindings for boot_dfuns, 
	-- which may be mentioned in imported unfoldings

		-- Don't need to rename the Haddock documentation,
		-- it's not parsed by GHC anymore.
	tcg_env <- return (tcg_env { tcg_doc_hdr = maybe_doc_hdr }) ;

		-- Report unused names
 	reportUnusedNames export_ies tcg_env ;

		-- Dump output and return
	tcDump tcg_env ;
	return tcg_env
    }}}}
\end{code}


%************************************************************************
%*									*
		Import declarations
%*									*
%************************************************************************

\begin{code}
tcRnImports :: HscEnv -> Module -> [LImportDecl RdrName] -> TcM TcGblEnv
tcRnImports hsc_env this_mod import_decls
  = do	{ (rn_imports, rdr_env, imports,hpc_info) <- rnImports import_decls ;

	; let { dep_mods :: ModuleNameEnv (ModuleName, IsBootInterface)
	      ; dep_mods = imp_dep_mods imports

		-- We want instance declarations from all home-package
		-- modules below this one, including boot modules, except
		-- ourselves.  The 'except ourselves' is so that we don't
		-- get the instances from this module's hs-boot file
	      ; want_instances :: ModuleName -> Bool
	      ; want_instances mod = mod `elemUFM` dep_mods
				   && mod /= moduleName this_mod
	      ; (home_insts, home_fam_insts) = hptInstances hsc_env 
                                                            want_instances
	      } ;

		-- Record boot-file info in the EPS, so that it's 
		-- visible to loadHiBootInterface in tcRnSrcDecls,
		-- and any other incrementally-performed imports
	; updateEps_ (\eps -> eps { eps_is_boot = dep_mods }) ;

		-- Update the gbl env
	; updGblEnv ( \ gbl -> 
	    gbl { 
              tcg_rdr_env      = plusOccEnv (tcg_rdr_env gbl) rdr_env,
	      tcg_imports      = tcg_imports gbl `plusImportAvails` imports,
              tcg_rn_imports   = rn_imports,
	      tcg_inst_env     = extendInstEnvList (tcg_inst_env gbl) home_insts,
	      tcg_fam_inst_env = extendFamInstEnvList (tcg_fam_inst_env gbl) 
                                                      home_fam_insts,
	      tcg_hpc          = hpc_info
	    }) $ do {

	; traceRn (text "rn1" <+> ppr (imp_dep_mods imports))
		-- Fail if there are any errors so far
		-- The error printing (if needed) takes advantage 
		-- of the tcg_env we have now set
-- 	; traceIf (text "rdr_env: " <+> ppr rdr_env)
	; failIfErrsM

		-- Load any orphan-module and family instance-module
		-- interfaces, so that their rules and instance decls will be
		-- found.
	; loadOrphanModules (imp_orphs  imports) False
	; loadOrphanModules (imp_finsts imports) True 

		-- Check type-familily consistency
	; traceRn (text "rn1: checking family instance consistency")
	; let { dir_imp_mods = moduleEnvKeys
			     . imp_mods 
			     $ imports }
	; checkFamInstConsistency (imp_finsts imports) dir_imp_mods ;

	; getGblEnv } }
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

   initTc hsc_env ExtCoreFile False this_mod $ do {

   let { ldecls  = map noLoc decls } ;

       -- bring the type and class decls into scope
       -- ToDo: check that this doesn't need to extract the val binds.
       --       It seems that only the type and class decls need to be in scope below because
       --          (a) tcTyAndClassDecls doesn't need the val binds, and 
       --          (b) tcExtCoreBindings doesn't need anything
       --              (in fact, it might not even need to be in the scope of
       --               this tcg_env at all)
   avails  <- getLocalNonValBinders (mkFakeGroup ldecls) ;
   tc_envs <- extendGlobalRdrEnvRn avails emptyFsEnv {- no fixity decls -} ;

   setEnvs tc_envs $ do {

   rn_decls <- checkNoErrs $ rnTyClDecls ldecls ;

	-- Dump trace of renaming part
   rnDump (ppr rn_decls) ;

	-- Typecheck them all together so that
	-- any mutually recursive types are done right
	-- Just discard the auxiliary bindings; they are generated 
	-- only for Haskell source code, and should already be in Core
   (tcg_env, _aux_binds, _dm_ids) <- tcTyAndClassDecls emptyModDetails rn_decls ;

   setGblEnv tcg_env $ do {
	-- Make the new type env available to stuff slurped from interface files
   
	-- Now the core bindings
   core_binds <- initIfaceExtCore (tcExtCoreBindings src_binds) ;

	-- Wrap up
   let {
	bndrs 	   = bindersOfBinds core_binds ;
	my_exports = map (Avail . idName) bndrs ;
		-- ToDo: export the data types also?

	final_type_env = 
             extendTypeEnvWithIds (tcg_type_env tcg_env) bndrs ;

	mod_guts = ModGuts {	mg_module    = this_mod,
				mg_boot	     = False,
				mg_used_names = emptyNameSet, -- ToDo: compute usage
				mg_dir_imps  = emptyModuleEnv, -- ??
				mg_deps      = noDependencies,	-- ??
				mg_exports   = my_exports,
				mg_types     = final_type_env,
				mg_insts     = tcg_insts tcg_env,
				mg_fam_insts = tcg_fam_insts tcg_env,
				mg_inst_env  = tcg_inst_env tcg_env,
				mg_fam_inst_env = tcg_fam_inst_env tcg_env,
				mg_rules     = [],
				mg_anns      = [],
				mg_binds     = core_binds,

				-- Stubs
				mg_rdr_env   = emptyGlobalRdrEnv,
				mg_fix_env   = emptyFixityEnv,
				mg_warns     = NoWarnings,
				mg_foreign   = NoStubs,
				mg_hpc_info  = emptyHpcInfo False,
                                mg_modBreaks = emptyModBreaks,
                                mg_vect_info = noVectInfo
		    } } ;

   tcCoreDump mod_guts ;

   return mod_guts
   }}}}

mkFakeGroup :: [LTyClDecl a] -> HsGroup a
mkFakeGroup decls -- Rather clumsy; lots of unused fields
  = emptyRdrGroup { hs_tyclds = decls }
\end{code}


%************************************************************************
%*									*
	Type-checking the top level of a module
%*									*
%************************************************************************

\begin{code}
tcRnSrcDecls :: ModDetails -> [LHsDecl RdrName] -> TcM TcGblEnv
	-- Returns the variables free in the decls
	-- Reason: solely to report unused imports and bindings
tcRnSrcDecls boot_iface decls
 = do {   	-- Do all the declarations
	(tc_envs, lie) <- captureConstraints $ tc_rn_src_decls boot_iface decls ;
      ; traceTc "Tc8" empty ;
      ; setEnvs tc_envs $ 
   do { 

	     -- 	Finish simplifying class constraints
	     -- 
	     -- simplifyTop deals with constant or ambiguous InstIds.  
	     -- How could there be ambiguous ones?  They can only arise if a
	     -- top-level decl falls under the monomorphism restriction
	     -- and no subsequent decl instantiates its type.
	     --
	     -- We do this after checkMain, so that we use the type info 
	     -- thaat checkMain adds
	     -- 
	     -- We do it with both global and local env in scope:
	     --	 * the global env exposes the instances to simplifyTop
	     --  * the local env exposes the local Ids to simplifyTop, 
	     --    so that we get better error messages (monomorphism restriction)
	new_ev_binds <- simplifyTop lie ;
        traceTc "Tc9" empty ;

	failIfErrsM ;	-- Don't zonk if there have been errors
			-- It's a waste of time; and we may get debug warnings
			-- about strangely-typed TyCons!

	-- Zonk the final code.  This must be done last.
	-- Even simplifyTop may do some unification.
        -- This pass also warns about missing type signatures
	let { (tcg_env, _) = tc_envs
	    ; TcGblEnv { tcg_type_env  = type_env,
		         tcg_binds     = binds,
		         tcg_sigs      = sig_ns,
		         tcg_ev_binds  = cur_ev_binds,
		         tcg_imp_specs = imp_specs,
		         tcg_rules     = rules,
		         tcg_fords     = fords } = tcg_env
            ; all_ev_binds = cur_ev_binds `unionBags` new_ev_binds } ;

	(bind_ids, ev_binds', binds', fords', imp_specs', rules') 
            <- zonkTopDecls all_ev_binds binds sig_ns rules imp_specs fords ;
	
	let { final_type_env = extendTypeEnvWithIds type_env bind_ids
	    ; tcg_env' = tcg_env { tcg_binds    = binds',
				   tcg_ev_binds = ev_binds',
				   tcg_imp_specs = imp_specs',
				   tcg_rules 	= rules', 
				   tcg_fords 	= fords' } } ;

        setGlobalTypeEnv tcg_env' final_type_env				   
   } }

tc_rn_src_decls :: ModDetails -> [LHsDecl RdrName] -> TcM (TcGblEnv, TcLclEnv)
-- Loops around dealing with each top level inter-splice group 
-- in turn, until it's dealt with the entire module
tc_rn_src_decls boot_details ds
 = do { (first_group, group_tail) <- findSplice ds  ;
		-- If ds is [] we get ([], Nothing)

	-- Deal with decls up to, but not including, the first splice
	(tcg_env, rn_decls) <- rnTopSrcDecls first_group ;
		-- rnTopSrcDecls fails if there are any errors

	(tcg_env, tcl_env) <- setGblEnv tcg_env $ 
			      tcTopSrcDecls boot_details rn_decls ;

	-- If there is no splice, we're nearly done
	setEnvs (tcg_env, tcl_env) $ 
	case group_tail of {
	   Nothing -> do { tcg_env <- checkMain ;	-- Check for `main'
			   return (tcg_env, tcl_env) 
		      } ;

#ifndef GHCI
	-- There shouldn't be a splice
	   Just (SpliceDecl {}, _) -> do {
	failWithTc (text "Can't do a top-level splice; need a bootstrapped compiler")
#else
	-- If there's a splice, we must carry on
	   Just (SpliceDecl splice_expr _, rest_ds) -> do {

	-- Rename the splice expression, and get its supporting decls
	(rn_splice_expr, splice_fvs) <- checkNoErrs (rnLExpr splice_expr) ;
		-- checkNoErrs: don't typecheck if renaming failed
	rnDump (ppr rn_splice_expr) ;

	-- Execute the splice
	spliced_decls <- tcSpliceDecls rn_splice_expr ;

	-- Glue them on the front of the remaining decls and loop
	setGblEnv (tcg_env `addTcgDUs` usesOnly splice_fvs) $
	tc_rn_src_decls boot_details (spliced_decls ++ rest_ds)
#endif /* GHCI */
    } } }
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
   = do { (first_group, group_tail) <- findSplice decls

		-- Rename the declarations
        ; (tcg_env, HsGroup { 
		   hs_tyclds = tycl_decls, 
		   hs_instds = inst_decls,
		   hs_derivds = deriv_decls,
		   hs_fords  = for_decls,
		   hs_defds  = def_decls,  
		   hs_ruleds = rule_decls, 
		   hs_annds  = _,
		   hs_valds  = val_binds }) <- rnTopSrcDecls first_group
	; (gbl_env, lie) <- captureConstraints $ setGblEnv tcg_env $ do {


		-- Check for illegal declarations
	; case group_tail of
	     Just (SpliceDecl d _, _) -> badBootDecl "splice" d
	     Nothing                  -> return ()
	; mapM_ (badBootDecl "foreign") for_decls
	; mapM_ (badBootDecl "default") def_decls
	; mapM_ (badBootDecl "rule")    rule_decls

		-- Typecheck type/class decls
	; traceTc "Tc2" empty
	; (tcg_env, aux_binds, dm_ids) 
               <- tcTyAndClassDecls emptyModDetails tycl_decls
	; setGblEnv tcg_env    $ 
          tcExtendIdEnv dm_ids $ do {

		-- Typecheck instance decls
		-- Family instance declarations are rejected here
	; traceTc "Tc3" empty
	; (tcg_env, inst_infos, _deriv_binds) 
            <- tcInstDecls1 tycl_decls inst_decls deriv_decls
	; setGblEnv tcg_env	$ do {

		-- Typecheck value declarations
	; traceTc "Tc5" empty 
	; val_ids <- tcHsBootSigs val_binds

		-- Wrap up
		-- No simplification or zonking to do
	; traceTc "Tc7a" empty
	; gbl_env <- getGblEnv 
	
		-- Make the final type-env
		-- Include the dfun_ids so that their type sigs
		-- are written into the interface file. 
		-- And similarly the aux_ids from aux_binds
	; let { type_env0 = tcg_type_env gbl_env
	      ; type_env1 = extendTypeEnvWithIds type_env0 val_ids
	      ; type_env2 = extendTypeEnvWithIds type_env1 dfun_ids 
	      ; type_env3 = extendTypeEnvWithIds type_env2 aux_ids 
	      ; dfun_ids = map iDFunId inst_infos
	      ; aux_ids  = case aux_binds of
	      		     ValBindsOut _ sigs -> [id | L _ (IdSig id) <- sigs]
			     _		   	-> panic "tcRnHsBoodDecls"
	      }

	; setGlobalTypeEnv gbl_env type_env3
   }}}
   ; traceTc "boot" (ppr lie); return gbl_env }

badBootDecl :: String -> Located decl -> TcM ()
badBootDecl what (L loc _) 
  = addErrAt loc (char 'A' <+> text what 
      <+> ptext (sLit "declaration is not (currently) allowed in a hs-boot file"))
\end{code}

Once we've typechecked the body of the module, we want to compare what
we've found (gathered in a TypeEnv) with the hi-boot details (if any).

\begin{code}
checkHiBootIface :: TcGblEnv -> ModDetails -> TcM TcGblEnv
-- Compare the hi-boot file for this module (if there is one)
-- with the type environment we've just come up with
-- In the common case where there is no hi-boot file, the list
-- of boot_names is empty.
--
-- The bindings we return give bindings for the dfuns defined in the
-- hs-boot file, such as 	$fbEqT = $fEqT

checkHiBootIface
	tcg_env@(TcGblEnv { tcg_src = hs_src, tcg_binds = binds,
			    tcg_insts = local_insts, 
			    tcg_type_env = local_type_env, tcg_exports = local_exports })
	(ModDetails { md_insts = boot_insts, md_fam_insts = boot_fam_insts,
		      md_types = boot_type_env, md_exports = boot_exports })
  | isHsBoot hs_src	-- Current module is already a hs-boot file!
  = return tcg_env	

  | otherwise
  = do	{ traceTc "checkHiBootIface" $ vcat
             [ ppr boot_type_env, ppr boot_insts, ppr boot_exports]

		-- Check the exports of the boot module, one by one
	; mapM_ check_export boot_exports

		-- Check for no family instances
	; unless (null boot_fam_insts) $
	    panic ("TcRnDriver.checkHiBootIface: Cannot handle family " ++
		   "instances in boot files yet...")
            -- FIXME: Why?  The actual comparison is not hard, but what would
            --	      be the equivalent to the dfun bindings returned for class
            --	      instances?  We can't easily equate tycons...

		-- Check instance declarations
	; mb_dfun_prs <- mapM check_inst boot_insts
        ; let dfun_prs   = catMaybes mb_dfun_prs
              boot_dfuns = map fst dfun_prs
              dfun_binds = listToBag [ mkVarBind boot_dfun (nlHsVar dfun)
                                     | (boot_dfun, dfun) <- dfun_prs ]
              type_env'  = extendTypeEnvWithIds local_type_env boot_dfuns
              tcg_env'   = tcg_env { tcg_binds = binds `unionBags` dfun_binds }

        ; failIfErrsM
	; setGlobalTypeEnv tcg_env' type_env' }
	     -- Update the global type env *including* the knot-tied one
             -- so that if the source module reads in an interface unfolding
             -- mentioning one of the dfuns from the boot module, then it
             -- can "see" that boot dfun.   See Trac #4003
  where
    check_export boot_avail	-- boot_avail is exported by the boot iface
      | name `elem` dfun_names = return ()	
      | isWiredInName name     = return ()	-- No checking for wired-in names.  In particular,
						-- 'error' is handled by a rather gross hack
						-- (see comments in GHC.Err.hs-boot)

	-- Check that the actual module exports the same thing
      | not (null missing_names)
      = addErrAt (nameSrcSpan (head missing_names)) 
                 (missingBootThing (head missing_names) "exported by")

	-- If the boot module does not *define* the thing, we are done
	-- (it simply re-exports it, and names match, so nothing further to do)
      | isNothing mb_boot_thing = return ()

	-- Check that the actual module also defines the thing, and 
	-- then compare the definitions
      | Just real_thing <- lookupTypeEnv local_type_env name,
        Just boot_thing <- mb_boot_thing
      = when (not (checkBootDecl boot_thing real_thing))
            $ addErrAt (nameSrcSpan (getName boot_thing))
                       (let boot_decl = tyThingToIfaceDecl 
                                               (fromJust mb_boot_thing)
                            real_decl = tyThingToIfaceDecl real_thing
                        in bootMisMatch real_thing boot_decl real_decl)

      | otherwise
      = addErrTc (missingBootThing name "defined in")
      where
	name          = availName boot_avail
	mb_boot_thing = lookupTypeEnv boot_type_env name
	missing_names = case lookupNameEnv local_export_env name of
			  Nothing    -> [name]
			  Just avail -> availNames boot_avail `minusList` availNames avail
		 
    dfun_names = map getName boot_insts

    local_export_env :: NameEnv AvailInfo
    local_export_env = availsToNameEnv local_exports

    check_inst :: Instance -> TcM (Maybe (Id, Id))
	-- Returns a pair of the boot dfun in terms of the equivalent real dfun
    check_inst boot_inst
	= case [dfun | inst <- local_insts, 
		       let dfun = instanceDFunId inst,
		       idType dfun `tcEqType` boot_inst_ty ] of
	    [] -> do { addErrTc (instMisMatch boot_inst); return Nothing }
	    (dfun:_) -> return (Just (local_boot_dfun, dfun))
	where
	  boot_dfun = instanceDFunId boot_inst
	  boot_inst_ty = idType boot_dfun
	  local_boot_dfun = Id.mkExportedLocalId (idName boot_dfun) boot_inst_ty


-- This has to compare the TyThing from the .hi-boot file to the TyThing
-- in the current source file.  We must be careful to allow alpha-renaming
-- where appropriate, and also the boot declaration is allowed to omit
-- constructors and class methods.
--
-- See rnfail055 for a good test of this stuff.

checkBootDecl :: TyThing -> TyThing -> Bool

checkBootDecl (AnId id1) (AnId id2)
  = ASSERT(id1 == id2) 
    (idType id1 `tcEqType` idType id2)

checkBootDecl (ATyCon tc1) (ATyCon tc2)
  = checkBootTyCon tc1 tc2

checkBootDecl (AClass c1)  (AClass c2)
  = let 
       (clas_tyvars1, clas_fds1, sc_theta1, _, ats1, op_stuff1) 
          = classExtraBigSig c1
       (clas_tyvars2, clas_fds2, sc_theta2, _, ats2, op_stuff2) 
          = classExtraBigSig c2

       env0 = mkRnEnv2 emptyInScopeSet
       env = rnBndrs2 env0 clas_tyvars1 clas_tyvars2

       eqSig (id1, def_meth1) (id2, def_meth2)
         = idName id1 == idName id2 &&
           tcEqTypeX env op_ty1 op_ty2 &&
           def_meth1 == def_meth2
         where
	  (_, rho_ty1) = splitForAllTys (idType id1)
	  op_ty1 = funResultTy rho_ty1
	  (_, rho_ty2) = splitForAllTys (idType id2)
          op_ty2 = funResultTy rho_ty2

       eqFD (as1,bs1) (as2,bs2) = 
         eqListBy (tcEqTypeX env) (mkTyVarTys as1) (mkTyVarTys as2) &&
         eqListBy (tcEqTypeX env) (mkTyVarTys bs1) (mkTyVarTys bs2)

       same_kind tv1 tv2 = eqKind (tyVarKind tv1) (tyVarKind tv2)
    in
       eqListBy same_kind clas_tyvars1 clas_tyvars2 &&
       	     -- Checks kind of class
       eqListBy eqFD clas_fds1 clas_fds2 &&
       (null sc_theta1 && null op_stuff1 && null ats1
        ||   -- Above tests for an "abstract" class
        eqListBy (tcEqPredX env) sc_theta1 sc_theta2 &&
        eqListBy eqSig op_stuff1 op_stuff2 &&
        eqListBy checkBootTyCon ats1 ats2)

checkBootDecl (ADataCon dc1) (ADataCon _)
  = pprPanic "checkBootDecl" (ppr dc1)

checkBootDecl _ _ = False -- probably shouldn't happen

----------------
checkBootTyCon :: TyCon -> TyCon -> Bool
checkBootTyCon tc1 tc2
  | not (eqKind (tyConKind tc1) (tyConKind tc2))
  = False	-- First off, check the kind

  | isSynTyCon tc1 && isSynTyCon tc2
  = ASSERT(tc1 == tc2)
    let tvs1 = tyConTyVars tc1; tvs2 = tyConTyVars tc2
        env = rnBndrs2 env0 tvs1 tvs2

        eqSynRhs SynFamilyTyCon SynFamilyTyCon
            = True
        eqSynRhs (SynonymTyCon t1) (SynonymTyCon t2)
            = tcEqTypeX env t1 t2
        eqSynRhs _ _ = False
    in
    equalLength tvs1 tvs2 &&
    eqSynRhs (synTyConRhs tc1) (synTyConRhs tc2)

  | isAlgTyCon tc1 && isAlgTyCon tc2
  = ASSERT(tc1 == tc2)
    eqKind (tyConKind tc1) (tyConKind tc2) &&
    eqListBy tcEqPred (tyConStupidTheta tc1) (tyConStupidTheta tc2) &&
    eqAlgRhs (algTyConRhs tc1) (algTyConRhs tc2)

  | isForeignTyCon tc1 && isForeignTyCon tc2
  = eqKind (tyConKind tc1) (tyConKind tc2) &&
    tyConExtName tc1 == tyConExtName tc2

  | otherwise = False
  where 
        env0 = mkRnEnv2 emptyInScopeSet

        eqAlgRhs AbstractTyCon _ = True
        eqAlgRhs DataFamilyTyCon{} DataFamilyTyCon{} = True
        eqAlgRhs tc1@DataTyCon{} tc2@DataTyCon{} =
            eqListBy eqCon (data_cons tc1) (data_cons tc2)
        eqAlgRhs tc1@NewTyCon{} tc2@NewTyCon{} =
            eqCon (data_con tc1) (data_con tc2)
        eqAlgRhs _ _ = False

        eqCon c1 c2
          =  dataConName c1 == dataConName c2
          && dataConIsInfix c1 == dataConIsInfix c2
          && dataConStrictMarks c1 == dataConStrictMarks c2
          && dataConFieldLabels c1 == dataConFieldLabels c2
          && let tvs1 = dataConUnivTyVars c1 ++ dataConExTyVars c1
                 tvs2 = dataConUnivTyVars c2 ++ dataConExTyVars c2
                 env = rnBndrs2 env0 tvs1 tvs2
             in
              equalLength tvs1 tvs2 &&              
              eqListBy (tcEqPredX env)
                        (dataConEqTheta c1 ++ dataConDictTheta c1)
                        (dataConEqTheta c2 ++ dataConDictTheta c2) &&
              eqListBy (tcEqTypeX env)
                        (dataConOrigArgTys c1)
                        (dataConOrigArgTys c2)

----------------
missingBootThing :: Name -> String -> SDoc
missingBootThing name what
  = ppr name <+> ptext (sLit "is exported by the hs-boot file, but not") 
	      <+> text what <+> ptext (sLit "the module")

bootMisMatch :: TyThing -> IfaceDecl -> IfaceDecl -> SDoc
bootMisMatch thing boot_decl real_decl
  = vcat [ppr thing <+> ptext (sLit "has conflicting definitions in the module and its hs-boot file"),
	  ptext (sLit "Main module:") <+> ppr real_decl,
	  ptext (sLit "Boot file:  ") <+> ppr boot_decl]

instMisMatch :: Instance -> SDoc
instMisMatch inst
  = hang (ppr inst)
       2 (ptext (sLit "is defined in the hs-boot file, but not in the module itself"))
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
------------------------------------------------
rnTopSrcDecls :: HsGroup RdrName -> TcM (TcGblEnv, HsGroup Name)
-- Fails if there are any errors
rnTopSrcDecls group
 = do { -- Rename the source decls
        traceTc "rn12" empty ;
	(tcg_env, rn_decls) <- checkNoErrs $ rnSrcDecls group ;
        traceTc "rn13" empty ;

        -- save the renamed syntax, if we want it
	let { tcg_env'
	        | Just grp <- tcg_rn_decls tcg_env
	          = tcg_env{ tcg_rn_decls = Just (appendGroups grp rn_decls) }
	        | otherwise
	           = tcg_env };

		-- Dump trace of renaming part
	rnDump (ppr rn_decls) ;

	return (tcg_env', rn_decls)
   }

------------------------------------------------
tcTopSrcDecls :: ModDetails -> HsGroup Name -> TcM (TcGblEnv, TcLclEnv)
tcTopSrcDecls boot_details
	(HsGroup { hs_tyclds = tycl_decls, 
		   hs_instds = inst_decls,
                   hs_derivds = deriv_decls,
		   hs_fords  = foreign_decls,
		   hs_defds  = default_decls,
		   hs_annds  = annotation_decls,
		   hs_ruleds = rule_decls,
		   hs_valds  = val_binds })
 = do {		-- Type-check the type and class decls, and all imported decls
		-- The latter come in via tycl_decls
        traceTc "Tc2" empty ;

	(tcg_env, aux_binds, dm_ids) <- tcTyAndClassDecls boot_details tycl_decls ;
		-- If there are any errors, tcTyAndClassDecls fails here
	
	setGblEnv tcg_env	$
        tcExtendIdEnv dm_ids    $ do {

		-- Source-language instances, including derivings,
		-- and import the supporting declarations
        traceTc "Tc3" empty ;
	(tcg_env, inst_infos, deriv_binds) 
            <- tcInstDecls1 tycl_decls inst_decls deriv_decls;
	setGblEnv tcg_env	$ do {

	        -- Foreign import declarations next. 
        traceTc "Tc4" empty ;
	(fi_ids, fi_decls) <- tcForeignImports foreign_decls ;
	tcExtendGlobalValEnv fi_ids	$ do {

		-- Default declarations
        traceTc "Tc4a" empty ;
	default_tys <- tcDefaults default_decls ;
	updGblEnv (\gbl -> gbl { tcg_default = default_tys }) $ do {
	
		-- Now GHC-generated derived bindings, generics, and selectors
		-- Do not generate warnings from compiler-generated code;
		-- hence the use of discardWarnings
	(tc_aux_binds,   specs1, tcl_env) <- discardWarnings (tcTopBinds aux_binds) ;
	(tc_deriv_binds, specs2, tcl_env) <- setLclTypeEnv tcl_env $ 
			 	             discardWarnings (tcTopBinds deriv_binds) ;

		-- Value declarations next
        traceTc "Tc5" empty ;
	(tc_val_binds, specs3, tcl_env) <- setLclTypeEnv tcl_env $
			 	           tcTopBinds val_binds;

        setLclTypeEnv tcl_env $ do {	-- Environment doesn't change now

	     	-- Second pass over class and instance declarations, 
        traceTc "Tc6" empty ;
	inst_binds <- tcInstDecls2 tycl_decls inst_infos ;

		-- Foreign exports
        traceTc "Tc7" empty ;
	(foe_binds, foe_decls) <- tcForeignExports foreign_decls ;

                -- Annotations
	annotations <- tcAnnotations annotation_decls ;

		-- Rules
	rules <- tcRules rule_decls ;

		-- Wrap up
        traceTc "Tc7a" empty ;
	tcg_env <- getGblEnv ;
	let { all_binds = tc_val_binds	 `unionBags`
			  tc_deriv_binds `unionBags`
			  tc_aux_binds   `unionBags`
			  inst_binds	 `unionBags`
			  foe_binds

            ; sig_names = mkNameSet (collectHsValBinders val_binds) 
                          `minusNameSet` getTypeSigNames val_binds

		-- Extend the GblEnv with the (as yet un-zonked) 
		-- bindings, rules, foreign decls
	    ; tcg_env' = tcg_env { tcg_binds = tcg_binds tcg_env `unionBags` all_binds
	      	       	 	 , tcg_imp_specs = tcg_imp_specs tcg_env ++ specs1 ++ specs2 ++ specs3
                                 , tcg_sigs  = tcg_sigs tcg_env `unionNameSets` sig_names
				 , tcg_rules = tcg_rules tcg_env ++ rules
				 , tcg_anns  = tcg_anns tcg_env ++ annotations
				 , tcg_fords = tcg_fords tcg_env ++ foe_decls ++ fi_decls } } ;
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
  = do { tcg_env   <- getGblEnv ;
	 dflags    <- getDOpts ;
	 check_main dflags tcg_env
    }

check_main :: DynFlags -> TcGblEnv -> TcM TcGblEnv
check_main dflags tcg_env
 | mod /= main_mod
 = traceTc "checkMain not" (ppr main_mod <+> ppr mod) >>
   return tcg_env

 | otherwise
 = do	{ mb_main <- lookupGlobalOccRn_maybe main_fn
		-- Check that 'main' is in scope
		-- It might be imported from another module!
	; case mb_main of {
	     Nothing -> do { traceTc "checkMain fail" (ppr main_mod <+> ppr main_fn)
			   ; complain_no_main	
			   ; return tcg_env } ;
	     Just main_name -> do

	{ traceTc "checkMain found" (ppr main_mod <+> ppr main_fn)
	; let loc = srcLocSpan (getSrcLoc main_name)
	; ioTyCon <- tcLookupTyCon ioTyConName
        ; res_ty <- newFlexiTyVarTy liftedTypeKind
	; main_expr
		<- addErrCtxt mainCtxt	  $
		   tcMonoExpr (L loc (HsVar main_name)) (mkTyConApp ioTyCon [res_ty])

		-- See Note [Root-main Id]
	   	-- Construct the binding
		-- 	:Main.main :: IO res_ty = runMainIO res_ty main 
	; run_main_id <- tcLookupId runMainIOName
	; let { root_main_name =  mkExternalName rootMainKey rOOT_MAIN 
				   (mkVarOccFS (fsLit "main")) 
				   (getSrcSpan main_name)
	      ; root_main_id = Id.mkExportedLocalId root_main_name 
						    (mkTyConApp ioTyCon [res_ty])
	      ; co  = mkWpTyApps [res_ty]
	      ; rhs = nlHsApp (mkLHsWrap co (nlHsVar run_main_id)) main_expr
	      ; main_bind = mkVarBind root_main_id rhs }

	; return (tcg_env { tcg_main  = Just main_name,
                            tcg_binds = tcg_binds tcg_env
					`snocBag` main_bind,
			    tcg_dus   = tcg_dus tcg_env
				        `plusDU` usesOnly (unitFV main_name)
			-- Record the use of 'main', so that we don't 
			-- complain about it being defined but not used
		 })
    }}}
  where
    mod 	 = tcg_mod tcg_env
    main_mod     = mainModIs dflags
    main_fn      = getMainFun dflags

    complain_no_main | ghcLink dflags == LinkInMemory = return ()
		     | otherwise = failWithTc noMainMsg
	-- In interactive mode, don't worry about the absence of 'main'
	-- In other modes, fail altogether, so that we don't go on
	-- and complain a second time when processing the export list.

    mainCtxt  = ptext (sLit "When checking the type of the") <+> pp_main_fn
    noMainMsg = ptext (sLit "The") <+> pp_main_fn
		<+> ptext (sLit "is not defined in module") <+> quotes (ppr main_mod)
    pp_main_fn = ppMainFn main_fn

ppMainFn :: RdrName -> SDoc
ppMainFn main_fn
  | main_fn == main_RDR_Unqual
  = ptext (sLit "function") <+> quotes (ppr main_fn)
  | otherwise
  = ptext (sLit "main function") <+> quotes (ppr main_fn)
	       
-- | Get the unqualified name of the function to use as the \"main\" for the main module.
-- Either returns the default name or the one configured on the command line with -main-is
getMainFun :: DynFlags -> RdrName
getMainFun dflags = case (mainFunIs dflags) of
    Just fn -> mkRdrUnqual (mkVarOccFS (mkFastString fn))
    Nothing -> main_RDR_Unqual

checkMainExported :: TcGblEnv -> TcM ()
checkMainExported tcg_env = do
  dflags    <- getDOpts
  case tcg_main tcg_env of
    Nothing -> return () -- not the main module
    Just main_name -> do
      let main_mod = mainModIs dflags
      checkTc (main_name `elem` concatMap availNames (tcg_exports tcg_env)) $
              ptext (sLit "The") <+> ppMainFn (nameRdrName main_name) <+>
              ptext (sLit "is not exported by module") <+> quotes (ppr main_mod)
\end{code}

Note [Root-main Id]
~~~~~~~~~~~~~~~~~~~
The function that the RTS invokes is always :Main.main, which we call
root_main_id.  (Because GHC allows the user to have a module not
called Main as the main module, we can't rely on the main function
being called "Main.main".  That's why root_main_id has a fixed module
":Main".)  

This is unusual: it's a LocalId whose Name has a Module from another
module.  Tiresomely, we must filter it out again in MkIface, les we
get two defns for 'main' in the interface file!


%*********************************************************
%*						 	 *
		GHCi stuff
%*							 *
%*********************************************************

\begin{code}
setInteractiveContext :: HscEnv -> InteractiveContext -> TcRn a -> TcRn a
setInteractiveContext hsc_env icxt thing_inside 
  = let -- Initialise the tcg_inst_env with instances from all home modules.  
        -- This mimics the more selective call to hptInstances in tcRnModule.
	(home_insts, home_fam_insts) = hptInstances hsc_env (\_ -> True)
    in
    updGblEnv (\env -> env { 
	tcg_rdr_env      = ic_rn_gbl_env icxt,
	tcg_inst_env     = extendInstEnvList    (tcg_inst_env env) home_insts,
	tcg_fam_inst_env = extendFamInstEnvList (tcg_fam_inst_env env) 
                                                home_fam_insts 
      }) $

    tcExtendGhciEnv (ic_tmp_ids icxt) $
        -- tcExtendGhciEnv does lots: 
        --   - it extends the local type env (tcl_env) with the given Ids,
        --   - it extends the local rdr env (tcl_rdr) with the Names from 
        --     the given Ids
        --   - it adds the free tyvars of the Ids to the tcl_tyvars
        --     set.
        --
        -- later ids in ic_tmp_ids must shadow earlier ones with the same
        -- OccName, and tcExtendIdEnv implements this behaviour.

    do	{ traceTc "setIC" (ppr (ic_tmp_ids icxt))
 	; thing_inside }
\end{code}


\begin{code}
#ifdef GHCI
tcRnStmt :: HscEnv
	 -> InteractiveContext
	 -> LStmt RdrName
	 -> IO (Messages, Maybe ([Id], LHsExpr Id))
		-- The returned [Id] is the list of new Ids bound by
                -- this statement.  It can be used to extend the
                -- InteractiveContext via extendInteractiveContext.
		--
		-- The returned TypecheckedHsExpr is of type IO [ () ],
		-- a list of the bound values, coerced to ().

tcRnStmt hsc_env ictxt rdr_stmt
  = initTcPrintErrors hsc_env iNTERACTIVE $ 
    setInteractiveContext hsc_env ictxt $ do {

    -- Rename; use CmdLineMode because tcRnStmt is only used interactively
    (([rn_stmt], _), fvs) <- rnStmts GhciStmt [rdr_stmt] $ \_ ->
                             return ((), emptyFVs) ;
    traceRn (text "tcRnStmt" <+> vcat [ppr rdr_stmt, ppr rn_stmt, ppr fvs]) ;
    failIfErrsM ;
    rnDump (ppr rn_stmt) ;
    
    -- The real work is done here
    (bound_ids, tc_expr) <- mkPlan rn_stmt ;
    zonked_expr <- zonkTopLExpr tc_expr ;
    zonked_ids  <- zonkTopBndrs bound_ids ;
    
	-- None of the Ids should be of unboxed type, because we
	-- cast them all to HValues in the end!
    mapM_ bad_unboxed (filter (isUnLiftedType . idType) zonked_ids) ;

    traceTc "tcs 1" empty ;
    let { global_ids = map globaliseAndTidyId zonked_ids } ;
        -- Note [Interactively-bound Ids in GHCi]

{- ---------------------------------------------
   At one stage I removed any shadowed bindings from the type_env;
   they are inaccessible but might, I suppose, cause a space leak if we leave them there.
   However, with Template Haskell they aren't necessarily inaccessible.  Consider this
   GHCi session
	 Prelude> let f n = n * 2 :: Int
	 Prelude> fName <- runQ [| f |]
	 Prelude> $(return $ AppE fName (LitE (IntegerL 7)))
	 14
	 Prelude> let f n = n * 3 :: Int
	 Prelude> $(return $ AppE fName (LitE (IntegerL 7)))
   In the last line we use 'fName', which resolves to the *first* 'f'
   in scope. If we delete it from the type env, GHCi crashes because
   it doesn't expect that.
 
   Hence this code is commented out

-------------------------------------------------- -}

    dumpOptTcRn Opt_D_dump_tc 
    	(vcat [text "Bound Ids" <+> pprWithCommas ppr global_ids,
    	       text "Typechecked expr" <+> ppr zonked_expr]) ;

    return (global_ids, zonked_expr)
    }
  where
    bad_unboxed id = addErr (sep [ptext (sLit "GHCi can't bind a variable of unlifted type:"),
				  nest 2 (ppr id <+> dcolon <+> ppr (idType id))])
\end{code}

Note [Interactively-bound Ids in GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Ids bound by previous Stmts in Template Haskell are currently
	a) GlobalIds
	b) with an Internal Name (not External)
	c) and a tidied type

 (a) They must be GlobalIds (not LocalIds) otherwise when we come to
     compile an expression using these ids later, the byte code
     generator will consider the occurrences to be free rather than
     global.

 (b) They retain their Internal names becuase we don't have a suitable
     Module to name them with.  We could revisit this choice.

 (c) Their types are tidied.  This is important, because :info may ask
     to look at them, and :info expects the things it looks up to have
     tidy types
	

--------------------------------------------------------------------------
		Typechecking Stmts in GHCi

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
		       ; when (isUnitTy it_ty) failM
		       ; return stuff },

			-- Plan B; a naked bind statment
		    tcGhciStmts [bind_stmt],	

			-- Plan C; check that the let-binding is typeable all by itself.
			-- If not, fail; if so, try to print it.
			-- The two-step process avoids getting two errors: one from
			-- the expression itself, and one from the 'print it' part
			-- This two-step story is very clunky, alas
		    do { _ <- checkNoErrs (tcGhciStmts [let_stmt]) 
				--- checkNoErrs defeats the error recovery of let-bindings
		       ; tcGhciStmts [let_stmt, print_it] }
	  ]}

mkPlan stmt@(L loc (BindStmt {}))
  | [v] <- collectLStmtBinders stmt		-- One binder, for a bind stmt 
  = do	{ let print_v  = L loc $ ExprStmt (nlHsApp (nlHsVar printName) (nlHsVar v))
			          	   (HsVar thenIOName) placeHolderType

	; print_bind_result <- doptM Opt_PrintBindResult
	; let print_plan = do
		  { stuff@([v_id], _) <- tcGhciStmts [stmt, print_v]
		  ; v_ty <- zonkTcType (idType v_id)
		  ; when (isUnitTy v_ty || not (isTauTy v_ty)) failM
		  ; return stuff }

	-- The plans are:
	--	[stmt; print v]		but not if v::()
	--	[stmt]
	; runPlans ((if print_bind_result then [print_plan] else []) ++
		    [tcGhciStmts [stmt]])
	}

mkPlan stmt
  = tcGhciStmts [stmt]

---------------------------
tcGhciStmts :: [LStmt Name] -> TcM PlanResult
tcGhciStmts stmts
 = do { ioTyCon <- tcLookupTyCon ioTyConName ;
	ret_id  <- tcLookupId returnIOName ;		-- return @ IO
	let {
	    ret_ty    = mkListTy unitTy ;
	    io_ret_ty = mkTyConApp ioTyCon [ret_ty] ;
	    tc_io_stmts stmts = tcStmts GhciStmt tcDoStmt stmts io_ret_ty ;

	    names = collectLStmtsBinders stmts ;

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
	    mk_return ids = nlHsApp (nlHsTyApp ret_id [ret_ty]) 
			 	    (noLoc $ ExplicitList unitTy (map mk_item ids)) ;
	    mk_item id = nlHsApp (nlHsTyApp unsafeCoerceId [idType id, unitTy])
		    	         (nlHsVar id) 
	 } ;

	-- OK, we're ready to typecheck the stmts
	traceTc "TcRnDriver.tcGhciStmts: tc stmts" empty ;
	((tc_stmts, ids), lie) <- captureConstraints $ tc_io_stmts stmts $ \ _ ->
					   mapM tcLookupId names ;
					-- Look up the names right in the middle,
					-- where they will all be in scope

	-- Simplify the context
	traceTc "TcRnDriver.tcGhciStmts: simplify ctxt" empty ;
	const_binds <- checkNoErrs (simplifyInteractive lie) ;
		-- checkNoErrs ensures that the plan fails if context redn fails

	traceTc "TcRnDriver.tcGhciStmts: done" empty ;
	return (ids, mkHsDictLet (EvBinds const_binds) $
		     noLoc (HsDo GhciStmt tc_stmts (mk_return ids) io_ret_ty))
    }
\end{code}


tcRnExpr just finds the type of an expression

\begin{code}
tcRnExpr :: HscEnv
	 -> InteractiveContext
	 -> LHsExpr RdrName
	 -> IO (Messages, Maybe Type)
tcRnExpr hsc_env ictxt rdr_expr
  = initTcPrintErrors hsc_env iNTERACTIVE $ 
    setInteractiveContext hsc_env ictxt $ do {

    (rn_expr, _fvs) <- rnLExpr rdr_expr ;
    failIfErrsM ;

	-- Now typecheck the expression; 
	-- it might have a rank-2 type (e.g. :t runST)
    ((_tc_expr, res_ty), lie)	<- captureConstraints (tcInferRho rn_expr) ;
    ((qtvs, dicts, _), lie_top) <- captureConstraints (simplifyInfer False {- No MR for now -}
                                                      (tyVarsOfType res_ty) lie)  ;
    _ <- simplifyInteractive lie_top ;       -- Ignore the dicionary bindings

    let { all_expr_ty = mkForAllTys qtvs (mkPiTypes dicts res_ty) } ;
    zonkTcType all_expr_ty
    }
\end{code}

tcRnType just finds the kind of a type

\begin{code}
tcRnType :: HscEnv
	 -> InteractiveContext
	 -> LHsType RdrName
	 -> IO (Messages, Maybe Kind)
tcRnType hsc_env ictxt rdr_type
  = initTcPrintErrors hsc_env iNTERACTIVE $ 
    setInteractiveContext hsc_env ictxt $ do {

    rn_type <- rnLHsType doc rdr_type ;
    failIfErrsM ;

	-- Now kind-check the type
    (_ty', kind) <- kcLHsType rn_type ;
    return kind
    }
  where
    doc = ptext (sLit "In GHCi input")

#endif /* GHCi */
\end{code}


%************************************************************************
%*									*
	More GHCi stuff, to do with browsing and getting info
%*									*
%************************************************************************

\begin{code}
#ifdef GHCI
-- | ASSUMES that the module is either in the 'HomePackageTable' or is
-- a package module with an interface on disk.  If neither of these is
-- true, then the result will be an error indicating the interface
-- could not be found.
getModuleExports :: HscEnv -> Module -> IO (Messages, Maybe [AvailInfo])
getModuleExports hsc_env mod
  = let
      ic        = hsc_IC hsc_env
      checkMods = ic_toplev_scope ic ++ map fst (ic_exports ic)
    in
    initTc hsc_env HsSrcFile False iNTERACTIVE (tcGetModuleExports mod checkMods)

-- Get the export avail info and also load all orphan and family-instance
-- modules.  Finally, check that the family instances of all modules in the
-- interactive context are consistent (these modules are in the second
-- argument).
tcGetModuleExports :: Module -> [Module] -> TcM [AvailInfo]
tcGetModuleExports mod directlyImpMods
  = do { let doc = ptext (sLit "context for compiling statements")
       ; iface <- initIfaceTcRn $ loadSysInterface doc mod

  		-- Load any orphan-module and family instance-module
  		-- interfaces, so their instances are visible.
       ; loadOrphanModules (dep_orphs (mi_deps iface)) False 
       ; loadOrphanModules (dep_finsts (mi_deps iface)) True

                -- Check that the family instances of all directly loaded
                -- modules are consistent.
       ; checkFamInstConsistency (dep_finsts (mi_deps iface)) directlyImpMods

       ; ifaceExportNames (mi_exports iface)
       }

tcRnLookupRdrName :: HscEnv -> RdrName -> IO (Messages, Maybe [Name])
tcRnLookupRdrName hsc_env rdr_name 
  = initTcPrintErrors hsc_env iNTERACTIVE $ 
    setInteractiveContext hsc_env (hsc_IC hsc_env) $ 
    lookup_rdr_name rdr_name

lookup_rdr_name :: RdrName -> TcM [Name]
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
#endif

tcRnLookupName :: HscEnv -> Name -> IO (Messages, Maybe TyThing)
tcRnLookupName hsc_env name
  = initTcPrintErrors hsc_env iNTERACTIVE $ 
    setInteractiveContext hsc_env (hsc_IC hsc_env) $
    tcRnLookupName' name

-- To look up a name we have to look in the local environment (tcl_lcl)
-- as well as the global environment, which is what tcLookup does. 
-- But we also want a TyThing, so we have to convert:

tcRnLookupName' :: Name -> TcRn TyThing
tcRnLookupName' name = do
   tcthing <- tcLookup name
   case tcthing of
     AGlobal thing    -> return thing
     ATcId{tct_id=id} -> return (AnId id)
     _ -> panic "tcRnLookupName'"

tcRnGetInfo :: HscEnv
            -> Name
            -> IO (Messages, Maybe (TyThing, Fixity, [Instance]))

-- Used to implement :info in GHCi
--
-- Look up a RdrName and return all the TyThings it might be
-- A capitalised RdrName is given to us in the DataName namespace,
-- but we want to treat it as *both* a data constructor 
--  *and* as a type or class constructor; 
-- hence the call to dataTcOccs, and we return up to two results
tcRnGetInfo hsc_env name
  = initTcPrintErrors hsc_env iNTERACTIVE $
    tcRnGetInfo' hsc_env name

tcRnGetInfo' :: HscEnv
             -> Name
             -> TcRn (TyThing, Fixity, [Instance])
tcRnGetInfo' hsc_env name
  = let ictxt = hsc_IC hsc_env in
    setInteractiveContext hsc_env ictxt $ do

	-- Load the interface for all unqualified types and classes
	-- That way we will find all the instance declarations
	-- (Packages have not orphan modules, and we assume that
	--  in the home package all relevant modules are loaded.)
    loadUnqualIfaces ictxt

    thing  <- tcRnLookupName' name
    fixity <- lookupFixityRn name
    ispecs <- lookupInsts thing
    return (thing, fixity, ispecs)

lookupInsts :: TyThing -> TcM [Instance]
lookupInsts (AClass cls)
  = do	{ inst_envs <- tcGetInstEnvs
	; return (classInstances inst_envs cls) }

lookupInsts (ATyCon tc)
  = do 	{ (pkg_ie, home_ie) <- tcGetInstEnvs
	   	-- Load all instances for all classes that are
		-- in the type environment (which are all the ones
		-- we've seen in any interface file so far)
	; return [ ispec 	-- Search all
		 | ispec <- instEnvElts home_ie ++ instEnvElts pkg_ie
		 , let dfun = instanceDFunId ispec
		 , relevant dfun ] } 
  where
    relevant df = tc_name `elemNameSet` tyClsNamesOfDFunHead (idType df)
    tc_name     = tyConName tc		  

lookupInsts _ = return []

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
                    not (isInternalName name),
		    isTcOcc (nameOccName name),  -- Types and classes only
		    unQualOK gre ]		 -- In scope unqualified
    doc = ptext (sLit "Need interface for module whose export(s) are in scope unqualified")
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
	when (dopt Opt_D_dump_types dflags || dopt Opt_D_dump_tc dflags)
	     (dumpTcRn short_dump) ;

	-- Dump bindings if -ddump-tc
	dumpOptTcRn Opt_D_dump_tc (mkDumpDoc "Typechecker" full_dump)
   }
  where
    short_dump = pprTcGblEnv env
    full_dump  = pprLHsBinds (tcg_binds env)
	-- NB: foreign x-d's have undefined's in their types; 
	--     hence can't show the tc_fords

tcCoreDump :: ModGuts -> TcM ()
tcCoreDump mod_guts
 = do { dflags <- getDOpts ;
	when (dopt Opt_D_dump_types dflags || dopt Opt_D_dump_tc dflags)
 	     (dumpTcRn (pprModGuts mod_guts)) ;

	-- Dump bindings if -ddump-tc
	dumpOptTcRn Opt_D_dump_tc (mkDumpDoc "Typechecker" full_dump) }
  where
    full_dump = pprCoreBindings (mg_binds mod_guts)

-- It's unpleasant having both pprModGuts and pprModDetails here
pprTcGblEnv :: TcGblEnv -> SDoc
pprTcGblEnv (TcGblEnv { tcg_type_env  = type_env, 
		        tcg_insts     = insts, 
		        tcg_fam_insts = fam_insts, 
		        tcg_rules     = rules,
			tcg_imports   = imports })
  = vcat [ ppr_types insts type_env
	 , ppr_tycons fam_insts type_env
	 , ppr_insts insts
	 , ppr_fam_insts fam_insts
	 , vcat (map ppr rules)
	 , ppr_gen_tycons (typeEnvTyCons type_env)
	 , ptext (sLit "Dependent modules:") <+> 
		ppr (sortBy cmp_mp $ eltsUFM (imp_dep_mods imports))
	 , ptext (sLit "Dependent packages:") <+> 
		ppr (sortBy stablePackageIdCmp $ imp_dep_pkgs imports)]
  where		-- The two uses of sortBy are just to reduce unnecessary
		-- wobbling in testsuite output
    cmp_mp (mod_name1, is_boot1) (mod_name2, is_boot2)
	= (mod_name1 `stableModuleNameCmp` mod_name2)
		  `thenCmp`	
	  (is_boot1 `compare` is_boot2)

pprModGuts :: ModGuts -> SDoc
pprModGuts (ModGuts { mg_types = type_env,
		      mg_rules = rules })
  = vcat [ ppr_types [] type_env,
	   ppr_rules rules ]

ppr_types :: [Instance] -> TypeEnv -> SDoc
ppr_types insts type_env
  = text "TYPE SIGNATURES" $$ nest 4 (ppr_sigs ids)
  where
    dfun_ids = map instanceDFunId insts
    ids = [id | id <- typeEnvIds type_env, want_sig id]
    want_sig id | opt_PprStyle_Debug = True
	        | otherwise	     = isLocalId id && 
				       isExternalName (idName id) && 
				       not (id `elem` dfun_ids)
	-- isLocalId ignores data constructors, records selectors etc.
	-- The isExternalName ignores local dictionary and method bindings
	-- that the type checker has invented.  Top-level user-defined things 
	-- have External names.

ppr_tycons :: [FamInst] -> TypeEnv -> SDoc
ppr_tycons fam_insts type_env
  = text "TYPE CONSTRUCTORS" $$ nest 4 (ppr_tydecls tycons)
  where
    fi_tycons = map famInstTyCon fam_insts
    tycons = [tycon | tycon <- typeEnvTyCons type_env, want_tycon tycon]
    want_tycon tycon | opt_PprStyle_Debug = True
	             | otherwise	  = not (isImplicitTyCon tycon) &&
					    isExternalName (tyConName tycon) &&
				            not (tycon `elem` fi_tycons)

ppr_insts :: [Instance] -> SDoc
ppr_insts []     = empty
ppr_insts ispecs = text "INSTANCES" $$ nest 2 (pprInstances ispecs)

ppr_fam_insts :: [FamInst] -> SDoc
ppr_fam_insts []        = empty
ppr_fam_insts fam_insts = 
  text "FAMILY INSTANCES" $$ nest 2 (pprFamInsts fam_insts)

ppr_sigs :: [Var] -> SDoc
ppr_sigs ids
	-- Print type signatures; sort by OccName 
  = vcat (map ppr_sig (sortLe le_sig ids))
  where
    le_sig id1 id2 = getOccName id1 <= getOccName id2
    ppr_sig id = ppr id <+> dcolon <+> ppr (tidyTopType (idType id))

ppr_tydecls :: [TyCon] -> SDoc
ppr_tydecls tycons
	-- Print type constructor info; sort by OccName 
  = vcat (map ppr_tycon (sortLe le_sig tycons))
  where
    le_sig tycon1 tycon2 = getOccName tycon1 <= getOccName tycon2
    ppr_tycon tycon 
      | isCoercionTyCon tycon 
      = sep [ptext (sLit "coercion") <+> ppr tycon <+> ppr tvs
            , nest 2 (dcolon <+> pprEqPred (coercionKind (mkTyConApp tycon (mkTyVarTys tvs))))]
      | otherwise             = ppr (tyThingToIfaceDecl (ATyCon tycon))
      where
        tvs = take (tyConArity tycon) alphaTyVars

ppr_rules :: [CoreRule] -> SDoc
ppr_rules [] = empty
ppr_rules rs = vcat [ptext (sLit "{-# RULES"),
		      nest 2 (pprRules rs),
		      ptext (sLit "#-}")]

ppr_gen_tycons :: [TyCon] -> SDoc
ppr_gen_tycons []  = empty
ppr_gen_tycons tcs = vcat [ptext (sLit "Tycons with generics:"),
			   nest 2 (fsep (map ppr (filter tyConHasGenerics tcs)))]
\end{code}
