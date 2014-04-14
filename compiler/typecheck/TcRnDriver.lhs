%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcMovectle]{Typechecking a whole module}

\begin{code}
module TcRnDriver (
#ifdef GHCI
        tcRnStmt, tcRnExpr, tcRnType,
        tcRnImportDecls,
        tcRnLookupRdrName,
        getModuleInterface,
        tcRnDeclsi,
        isGHCiMonad,
        runTcInteractive,    -- Used by GHC API clients (Trac #8878)
#endif
        tcRnLookupName,
        tcRnGetInfo,
        tcRnModule, tcRnModuleTcRnM,
        tcTopSrcDecls,
        tcRnExtCore
    ) where

#ifdef GHCI
import {-# SOURCE #-} TcSplice ( runQuasi )
import RnSplice ( rnTopSpliceDecls )
#endif

import DynFlags
import StaticFlags
import HsSyn
import PrelNames
import RdrName
import TcHsSyn
import TcExpr
import TcRnMonad
import TcEvidence
import PprTyThing( pprTyThing )
import Coercion( pprCoAxiom )
import FamInst
import InstEnv
import FamInstEnv
import TcAnnotations
import TcBinds
import HeaderInfo       ( mkPrelImports )
import TcDefaults
import TcEnv
import TcRules
import TcForeign
import TcInstDcls
import TcIface
import TcMType
import MkIface
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
import Module
import UniqFM
import Name
import NameEnv
import NameSet
import Avail
import TyCon
import SrcLoc
import HscTypes
import ListSetOps
import Outputable
import ConLike
import DataCon
import Type
import Class
import CoAxiom
import Inst     ( tcGetInstEnvs, tcGetInsts )
import Annotations
import Data.List ( sortBy )
import Data.IORef ( readIORef )
import Data.Ord
#ifndef GHCI
import BasicTypes ( Origin(..) )
#else
import BasicTypes hiding( SuccessFlag(..) )
import TcType   ( isUnitTy, isTauTy )
import TcHsType
import TcMatches
import RnTypes
import RnExpr
import MkId
import TidyPgm    ( globaliseAndTidyId )
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
%*                                                                      *
        Typecheck and rename a module
%*                                                                      *
%************************************************************************


\begin{code}
-- | Top level entry point for typechecker and renamer
tcRnModule :: HscEnv
           -> HscSource
           -> Bool              -- True <=> save renamed syntax
           -> HsParsedModule
           -> IO (Messages, Maybe TcGblEnv)

tcRnModule hsc_env hsc_src save_rn_syntax
   parsedModule@HsParsedModule {hpm_module=L loc this_module}
 = do { showPass (hsc_dflags hsc_env) "Renamer/typechecker" ;

      ; let { this_pkg = thisPackage (hsc_dflags hsc_env)
            ; pair@(this_mod,_)
                = case hsmodName this_module of
                    Nothing -- 'module M where' is omitted
                        ->  (mAIN, srcLocSpan (srcSpanStart loc))

                    Just (L mod_loc mod)  -- The normal case
                        -> (mkModule this_pkg mod, mod_loc) } ;

      ; initTc hsc_env hsc_src save_rn_syntax this_mod $
        tcRnModuleTcRnM hsc_env hsc_src parsedModule pair }

tcRnModuleTcRnM :: HscEnv
                -> HscSource
                -> HsParsedModule
                -> (Module, SrcSpan)
                -> TcRn TcGblEnv
-- Factored out separately so that a Core plugin can
-- call the type checker directly
tcRnModuleTcRnM hsc_env hsc_src
                (HsParsedModule {
                   hpm_module =
                      (L loc (HsModule maybe_mod export_ies
                                       import_decls local_decls mod_deprec
                                       maybe_doc_hdr)),
                   hpm_src_files = src_files
                })
                (this_mod, prel_imp_loc)
 = setSrcSpan loc $
   do {         -- Deal with imports; first add implicit prelude
        implicit_prelude <- xoptM Opt_ImplicitPrelude;
        let { prel_imports = mkPrelImports (moduleName this_mod) prel_imp_loc
                                         implicit_prelude import_decls } ;

        whenWOptM Opt_WarnImplicitPrelude $
             when (notNull prel_imports) $ addWarn (implicitPreludeWarn) ;

        tcg_env <- {-# SCC "tcRnImports" #-}
                   tcRnImports hsc_env (prel_imports ++ import_decls) ;

          -- If the whole module is warned about or deprecated 
          -- (via mod_deprec) record that in tcg_warns. If we do thereby add
          -- a WarnAll, it will override any subseqent depracations added to tcg_warns
        let { tcg_env1 = case mod_deprec of 
                         Just txt -> tcg_env { tcg_warns = WarnAll txt } 
                         Nothing  -> tcg_env 
            } ;
 
        setGblEnv tcg_env1 $ do {

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
                        {-# SCC "tcRnSrcDecls" #-}
                        tcRnSrcDecls boot_iface local_decls ;
        setGblEnv tcg_env               $ do {

                -- Process the export list
        traceRn (text "rn4a: before exports");
        tcg_env <- rnExports (isJust maybe_mod) export_ies tcg_env ;
        traceRn (text "rn4b: after exports") ;

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

                -- add extra source files to tcg_dependent_files
        addDependentFiles src_files ;

                -- Dump output and return
        tcDump tcg_env ;
        return tcg_env
    }}}


implicitPreludeWarn :: SDoc
implicitPreludeWarn
  = ptext (sLit "Module `Prelude' implicitly imported")
\end{code}


%************************************************************************
%*                                                                      *
                Import declarations
%*                                                                      *
%************************************************************************

\begin{code}
tcRnImports :: HscEnv -> [LImportDecl RdrName] -> TcM TcGblEnv
tcRnImports hsc_env import_decls
  = do  { (rn_imports, rdr_env, imports, hpc_info) <- rnImports import_decls ;

        ; this_mod <- getModule
        ; let { dep_mods :: ModuleNameEnv (ModuleName, IsBootInterface)
              ; dep_mods = imp_dep_mods imports

                -- We want instance declarations from all home-package
                -- modules below this one, including boot modules, except
                -- ourselves.  The 'except ourselves' is so that we don't
                -- get the instances from this module's hs-boot file.  This
                -- filtering also ensures that we don't see instances from
                -- modules batch (@--make@) compiled before this one, but
                -- which are not below this one.
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
              tcg_rdr_env      = tcg_rdr_env gbl `plusGlobalRdrEnv` rdr_env,
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
--      ; traceIf (text "rdr_env: " <+> ppr rdr_env)
        ; failIfErrsM

                -- Load any orphan-module and family instance-module
                -- interfaces, so that their rules and instance decls will be
                -- found.
        ; loadModuleInterfaces (ptext (sLit "Loading orphan modules"))
                               (imp_orphs imports)

                -- Check type-family consistency
        ; traceRn (text "rn1: checking family instance consistency")
        ; let { dir_imp_mods = moduleEnvKeys
                             . imp_mods
                             $ imports }
        ; checkFamInstConsistency (imp_finsts imports) dir_imp_mods ;

        ; getGblEnv } }
\end{code}


%************************************************************************
%*                                                                      *
        Type-checking external-core modules
%*                                                                      *
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

       -- Bring the type and class decls into scope
       -- ToDo: check that this doesn't need to extract the val binds.
       --       It seems that only the type and class decls need to be in scope below because
       --          (a) tcTyAndClassDecls doesn't need the val binds, and
       --          (b) tcExtCoreBindings doesn't need anything
       --              (in fact, it might not even need to be in the scope of
       --               this tcg_env at all)
   (tc_envs, _bndrs) <- getLocalNonValBinders emptyFsEnv {- no fixity decls -}
                                              (mkFakeGroup ldecls) ;
   setEnvs tc_envs $ do {

   (rn_decls, _fvs) <- checkNoErrs $ rnTyClDecls [] [mkTyClGroup ldecls] ;
   -- The empty list is for extra dependencies coming from .hs-boot files
   -- See Note [Extra dependencies from .hs-boot files] in RnSource

        -- Dump trace of renaming part
   rnDump (ppr rn_decls) ;

        -- Typecheck them all together so that
        -- any mutually recursive types are done right
        -- Just discard the auxiliary bindings; they are generated
        -- only for Haskell source code, and should already be in Core
   tcg_env   <- tcTyAndClassDecls emptyModDetails rn_decls ;
   safe_mode <- liftIO $ finalSafeMode (hsc_dflags hsc_env) tcg_env ;
   dep_files <- liftIO $ readIORef (tcg_dependent_files tcg_env) ;

   setGblEnv tcg_env $ do {
        -- Make the new type env available to stuff slurped from interface files

        -- Now the core bindings
   core_binds <- initIfaceExtCore (tcExtCoreBindings src_binds) ;


        -- Wrap up
   let {
        bndrs      = bindersOfBinds core_binds ;
        my_exports = map (Avail . idName) bndrs ;
                -- ToDo: export the data types also?

        mod_guts = ModGuts {    mg_module    = this_mod,
                                mg_boot      = False,
                                mg_used_names = emptyNameSet, -- ToDo: compute usage
                                mg_used_th   = False,
                                mg_dir_imps  = emptyModuleEnv, -- ??
                                mg_deps      = noDependencies,  -- ??
                                mg_exports   = my_exports,
                                mg_tcs       = tcg_tcs tcg_env,
                                mg_insts     = tcg_insts tcg_env,
                                mg_fam_insts = tcg_fam_insts tcg_env,
                                mg_inst_env  = tcg_inst_env tcg_env,
                                mg_fam_inst_env = tcg_fam_inst_env tcg_env,
                                mg_patsyns      = [], -- TODO
                                mg_rules        = [],
                                mg_vect_decls   = [],
                                mg_anns         = [],
                                mg_binds        = core_binds,

                                -- Stubs
                                mg_rdr_env      = emptyGlobalRdrEnv,
                                mg_fix_env      = emptyFixityEnv,
                                mg_warns        = NoWarnings,
                                mg_foreign      = NoStubs,
                                mg_hpc_info     = emptyHpcInfo False,
                                mg_modBreaks    = emptyModBreaks,
                                mg_vect_info    = noVectInfo,
                                mg_safe_haskell = safe_mode,
                                mg_trust_pkg    = False,
                                mg_dependent_files = dep_files
                            } } ;

   tcCoreDump mod_guts ;

   return mod_guts
   }}}}

mkFakeGroup :: [LTyClDecl a] -> HsGroup a
mkFakeGroup decls -- Rather clumsy; lots of unused fields
  = emptyRdrGroup { hs_tyclds = [mkTyClGroup decls] }
\end{code}


%************************************************************************
%*                                                                      *
        Type-checking the top level of a module
%*                                                                      *
%************************************************************************

\begin{code}
tcRnSrcDecls :: ModDetails -> [LHsDecl RdrName] -> TcM TcGblEnv
        -- Returns the variables free in the decls
        -- Reason: solely to report unused imports and bindings
tcRnSrcDecls boot_iface decls
 = do {         -- Do all the declarations
        ((tcg_env, tcl_env), lie) <- captureConstraints $ tc_rn_src_decls boot_iface decls ;
      ; traceTc "Tc8" empty ;
      ; setEnvs (tcg_env, tcl_env) $
   do {

             --         Finish simplifying class constraints
             --
             -- simplifyTop deals with constant or ambiguous InstIds.
             -- How could there be ambiguous ones?  They can only arise if a
             -- top-level decl falls under the monomorphism restriction
             -- and no subsequent decl instantiates its type.
             --
             -- We do this after checkMain, so that we use the type info
             -- that checkMain adds
             --
             -- We do it with both global and local env in scope:
             --  * the global env exposes the instances to simplifyTop
             --  * the local env exposes the local Ids to simplifyTop,
             --    so that we get better error messages (monomorphism restriction)
        new_ev_binds <- {-# SCC "simplifyTop" #-}
                        simplifyTop lie ;
        traceTc "Tc9" empty ;

        failIfErrsM ;   -- Don't zonk if there have been errors
                        -- It's a waste of time; and we may get debug warnings
                        -- about strangely-typed TyCons!

        -- Zonk the final code.  This must be done last.
        -- Even simplifyTop may do some unification.
        -- This pass also warns about missing type signatures
        let { TcGblEnv { tcg_type_env  = type_env,
                         tcg_binds     = binds,
                         tcg_sigs      = sig_ns,
                         tcg_ev_binds  = cur_ev_binds,
                         tcg_imp_specs = imp_specs,
                         tcg_rules     = rules,
                         tcg_vects     = vects,
                         tcg_fords     = fords } = tcg_env
            ; all_ev_binds = cur_ev_binds `unionBags` new_ev_binds } ;

        (bind_ids, ev_binds', binds', fords', imp_specs', rules', vects')
            <- {-# SCC "zonkTopDecls" #-}
               zonkTopDecls all_ev_binds binds sig_ns rules vects imp_specs fords ;

        let { final_type_env = extendTypeEnvWithIds type_env bind_ids
            ; tcg_env' = tcg_env { tcg_binds    = binds',
                                   tcg_ev_binds = ev_binds',
                                   tcg_imp_specs = imp_specs',
                                   tcg_rules    = rules',
                                   tcg_vects    = vects',
                                   tcg_fords    = fords' } } ;

        setGlobalTypeEnv tcg_env' final_type_env
       
   } }

tc_rn_src_decls :: ModDetails
                -> [LHsDecl RdrName]
                -> TcM (TcGblEnv, TcLclEnv)
-- Loops around dealing with each top level inter-splice group
-- in turn, until it's dealt with the entire module
tc_rn_src_decls boot_details ds
 = {-# SCC "tc_rn_src_decls" #-}
   do { (first_group, group_tail) <- findSplice ds
                -- If ds is [] we get ([], Nothing)

        -- The extra_deps are needed while renaming type and class declarations
        -- See Note [Extra dependencies from .hs-boot files] in RnSource
      ; let { extra_deps = map tyConName (typeEnvTyCons (md_types boot_details)) }
        -- Deal with decls up to, but not including, the first splice
      ; (tcg_env, rn_decls) <- rnTopSrcDecls extra_deps first_group
                -- rnTopSrcDecls fails if there are any errors

#ifdef GHCI
        -- Get TH-generated top-level declarations and make sure they don't
        -- contain any splices since we don't handle that at the moment
      ; th_topdecls_var <- fmap tcg_th_topdecls getGblEnv
      ; th_ds <- readTcRef th_topdecls_var
      ; writeTcRef th_topdecls_var []

      ; (tcg_env, rn_decls) <-
            if null th_ds
            then return (tcg_env, rn_decls)
            else do { (th_group, th_group_tail) <- findSplice th_ds
                    ; case th_group_tail of
                        { Nothing -> return () ;
                        ; Just (SpliceDecl (L loc _) _, _)
                            -> setSrcSpan loc $
                               addErr (ptext (sLit "Declaration splices are not permitted inside top-level declarations added with addTopDecls"))
                        } ;
                                         
                    -- Rename TH-generated top-level declarations
                    ; (tcg_env, th_rn_decls) <- setGblEnv tcg_env $
                      rnTopSrcDecls extra_deps th_group

                    -- Dump generated top-level declarations
                    ; loc <- getSrcSpanM
                    ; traceSplice (vcat [ppr loc <> colon <+> text "Splicing top-level declarations added with addTopDecls ",
                                   nest 2 (nest 2 (ppr th_rn_decls))])

                    ; return (tcg_env, appendGroups rn_decls th_rn_decls)
                    }
#endif /* GHCI */

      -- Type check all declarations
      ; (tcg_env, tcl_env) <- setGblEnv tcg_env $
                              tcTopSrcDecls boot_details rn_decls

        -- If there is no splice, we're nearly done
      ; setEnvs (tcg_env, tcl_env) $
        case group_tail of
          { Nothing -> do { tcg_env <- checkMain       -- Check for `main'
#ifdef GHCI
                            -- Run all module finalizers
                          ; th_modfinalizers_var <- fmap tcg_th_modfinalizers getGblEnv
                          ; modfinalizers <- readTcRef th_modfinalizers_var
                          ; writeTcRef th_modfinalizers_var []
                          ; mapM_ runQuasi modfinalizers
#endif /* GHCI */
                          ; return (tcg_env, tcl_env)
                          }

#ifndef GHCI
            -- There shouldn't be a splice
          ; Just (SpliceDecl {}, _) ->
            failWithTc (text "Can't do a top-level splice; need a bootstrapped compiler")
          }
#else
            -- If there's a splice, we must carry on
          ; Just (SpliceDecl (L _ splice) _, rest_ds) ->
            do { -- Rename the splice expression, and get its supporting decls
                 (spliced_decls, splice_fvs) <- checkNoErrs (rnTopSpliceDecls splice)

                 -- Glue them on the front of the remaining decls and loop
               ; setGblEnv (tcg_env `addTcgDUs` usesOnly splice_fvs) $
                 tc_rn_src_decls boot_details (spliced_decls ++ rest_ds)
               }
          }
#endif /* GHCI */
      }
\end{code}

%************************************************************************
%*                                                                      *
        Compiling hs-boot source files, and
        comparing the hi-boot interface with the real thing
%*                                                                      *
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
                   hs_vects  = vect_decls,
                   hs_annds  = _,
                   hs_valds  = val_binds }) <- rnTopSrcDecls [] first_group
        -- The empty list is for extra dependencies coming from .hs-boot files
        -- See Note [Extra dependencies from .hs-boot files] in RnSource
        ; (gbl_env, lie) <- captureConstraints $ setGblEnv tcg_env $ do {


                -- Check for illegal declarations
        ; case group_tail of
             Just (SpliceDecl d _, _) -> badBootDecl "splice" d
             Nothing                  -> return ()
        ; mapM_ (badBootDecl "foreign") for_decls
        ; mapM_ (badBootDecl "default") def_decls
        ; mapM_ (badBootDecl "rule")    rule_decls
        ; mapM_ (badBootDecl "vect")    vect_decls

                -- Typecheck type/class/isntance decls
        ; traceTc "Tc2 (boot)" empty
        ; (tcg_env, inst_infos, _deriv_binds)
             <- tcTyClsInstDecls emptyModDetails tycl_decls inst_decls deriv_decls
        ; setGblEnv tcg_env     $ do {

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
        ; let { type_env0 = tcg_type_env gbl_env
              ; type_env1 = extendTypeEnvWithIds type_env0 val_ids
              ; type_env2 = extendTypeEnvWithIds type_env1 dfun_ids
              ; dfun_ids = map iDFunId inst_infos
              }

        ; setGlobalTypeEnv gbl_env type_env2
   }}
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
-- hs-boot file, such as        $fbEqT = $fEqT

checkHiBootIface
        tcg_env@(TcGblEnv { tcg_src = hs_src, tcg_binds = binds,
                            tcg_insts = local_insts,
                            tcg_type_env = local_type_env, tcg_exports = local_exports })
        (ModDetails { md_insts = boot_insts, md_fam_insts = boot_fam_insts,
                      md_types = boot_type_env, md_exports = boot_exports })
  | isHsBoot hs_src     -- Current module is already a hs-boot file!
  = return tcg_env

  | otherwise
  = do  { traceTc "checkHiBootIface" $ vcat
             [ ppr boot_type_env, ppr boot_insts, ppr boot_exports]

                -- Check the exports of the boot module, one by one
        ; mapM_ check_export boot_exports

                -- Check for no family instances
        ; unless (null boot_fam_insts) $
            panic ("TcRnDriver.checkHiBootIface: Cannot handle family " ++
                   "instances in boot files yet...")
            -- FIXME: Why?  The actual comparison is not hard, but what would
            --        be the equivalent to the dfun bindings returned for class
            --        instances?  We can't easily equate tycons...

                -- Check instance declarations
        ; mb_dfun_prs <- mapM check_inst boot_insts
        ; let dfun_prs   = catMaybes mb_dfun_prs
              boot_dfuns = map fst dfun_prs
              dfun_binds = listToBag [ (Generated, mkVarBind boot_dfun (nlHsVar dfun))
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
    check_export boot_avail     -- boot_avail is exported by the boot iface
      | name `elem` dfun_names = return ()
      | isWiredInName name     = return ()      -- No checking for wired-in names.  In particular,
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
                       (bootMisMatch real_thing boot_thing)

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

    check_inst :: ClsInst -> TcM (Maybe (Id, Id))
        -- Returns a pair of the boot dfun in terms of the equivalent real dfun
    check_inst boot_inst
        = case [dfun | inst <- local_insts,
                       let dfun = instanceDFunId inst,
                       idType dfun `eqType` boot_inst_ty ] of
            [] -> do { traceTc "check_inst" (vcat [ text "local_insts" <+> vcat (map (ppr . idType . instanceDFunId) local_insts)
                                                  , text "boot_inst"   <+> ppr boot_inst
                                                  , text "boot_inst_ty" <+> ppr boot_inst_ty
                                                  ])
                     ; addErrTc (instMisMatch boot_inst); return Nothing }
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
    (idType id1 `eqType` idType id2)

checkBootDecl (ATyCon tc1) (ATyCon tc2)
  = checkBootTyCon tc1 tc2

checkBootDecl (AConLike (RealDataCon dc1)) (AConLike (RealDataCon _))
  = pprPanic "checkBootDecl" (ppr dc1)

checkBootDecl _ _ = False -- probably shouldn't happen

----------------
checkBootTyCon :: TyCon -> TyCon -> Bool
checkBootTyCon tc1 tc2
  | not (eqKind (tyConKind tc1) (tyConKind tc2))
  = False       -- First off, check the kind

  | Just c1 <- tyConClass_maybe tc1
  , Just c2 <- tyConClass_maybe tc2
  , let (clas_tvs1, clas_fds1, sc_theta1, _, ats1, op_stuff1)
          = classExtraBigSig c1
        (clas_tvs2, clas_fds2, sc_theta2, _, ats2, op_stuff2)
          = classExtraBigSig c2
  , Just env <- eqTyVarBndrs emptyRnEnv2 clas_tvs1 clas_tvs2
  = let
       eqSig (id1, def_meth1) (id2, def_meth2)
         = idName id1 == idName id2 &&
           eqTypeX env op_ty1 op_ty2 &&
           def_meth1 == def_meth2
         where
          (_, rho_ty1) = splitForAllTys (idType id1)
          op_ty1 = funResultTy rho_ty1
          (_, rho_ty2) = splitForAllTys (idType id2)
          op_ty2 = funResultTy rho_ty2

       eqAT (tc1, def_ats1) (tc2, def_ats2)
         = checkBootTyCon tc1 tc2 &&
           eqListBy eqATDef def_ats1 def_ats2

       -- Ignore the location of the defaults
       eqATDef (CoAxBranch { cab_tvs = tvs1, cab_lhs =  ty_pats1, cab_rhs = ty1 })
               (CoAxBranch { cab_tvs = tvs2, cab_lhs =  ty_pats2, cab_rhs = ty2 })
         | Just env <- eqTyVarBndrs emptyRnEnv2 tvs1 tvs2
         = eqListBy (eqTypeX env) ty_pats1 ty_pats2 &&
           eqTypeX env ty1 ty2
         | otherwise = False

       eqFD (as1,bs1) (as2,bs2) =
         eqListBy (eqTypeX env) (mkTyVarTys as1) (mkTyVarTys as2) &&
         eqListBy (eqTypeX env) (mkTyVarTys bs1) (mkTyVarTys bs2)
    in
       roles1 == roles2 &&
             -- Checks kind of class
       eqListBy eqFD clas_fds1 clas_fds2 &&
       (null sc_theta1 && null op_stuff1 && null ats1
        ||   -- Above tests for an "abstract" class
        eqListBy (eqPredX env) sc_theta1 sc_theta2 &&
        eqListBy eqSig op_stuff1 op_stuff2 &&
        eqListBy eqAT ats1 ats2)

  | Just syn_rhs1 <- synTyConRhs_maybe tc1
  , Just syn_rhs2 <- synTyConRhs_maybe tc2
  , Just env <- eqTyVarBndrs emptyRnEnv2 (tyConTyVars tc1) (tyConTyVars tc2)
  = ASSERT(tc1 == tc2)
    let eqSynRhs OpenSynFamilyTyCon OpenSynFamilyTyCon = True
        eqSynRhs AbstractClosedSynFamilyTyCon (ClosedSynFamilyTyCon {}) = True
        eqSynRhs (ClosedSynFamilyTyCon {}) AbstractClosedSynFamilyTyCon = True
        eqSynRhs (ClosedSynFamilyTyCon ax1) (ClosedSynFamilyTyCon ax2)
            = eqClosedFamilyAx ax1 ax2
        eqSynRhs (SynonymTyCon t1) (SynonymTyCon t2)
            = eqTypeX env t1 t2
        eqSynRhs (BuiltInSynFamTyCon _) (BuiltInSynFamTyCon _) = tc1 == tc2
        eqSynRhs _ _ = False
    in
    roles1 == roles2 &&
    eqSynRhs syn_rhs1 syn_rhs2

  | isAlgTyCon tc1 && isAlgTyCon tc2
  , Just env <- eqTyVarBndrs emptyRnEnv2 (tyConTyVars tc1) (tyConTyVars tc2)
  = ASSERT(tc1 == tc2)
    roles1 == roles2 &&
    eqListBy (eqPredX env) (tyConStupidTheta tc1) (tyConStupidTheta tc2) &&
    eqAlgRhs (algTyConRhs tc1) (algTyConRhs tc2)

  | isForeignTyCon tc1 && isForeignTyCon tc2
  = eqKind (tyConKind tc1) (tyConKind tc2) &&
    tyConExtName tc1 == tyConExtName tc2

  | otherwise = False
  where
    roles1 = tyConRoles tc1
    roles2 = tyConRoles tc2

    eqAlgRhs (AbstractTyCon dis1) rhs2
      | dis1      = isDistinctAlgRhs rhs2   --Check compatibility
      | otherwise = True
    eqAlgRhs DataFamilyTyCon{} DataFamilyTyCon{} = True
    eqAlgRhs tc1@DataTyCon{} tc2@DataTyCon{} =
        eqListBy eqCon (data_cons tc1) (data_cons tc2)
    eqAlgRhs tc1@NewTyCon{} tc2@NewTyCon{} =
        eqCon (data_con tc1) (data_con tc2)
    eqAlgRhs _ _ = False

    eqCon c1 c2
      =  dataConName c1 == dataConName c2
      && dataConIsInfix c1 == dataConIsInfix c2
      && eqListBy eqHsBang (dataConStrictMarks c1) (dataConStrictMarks c2)
      && dataConFieldLabels c1 == dataConFieldLabels c2
      && eqType (dataConUserType c1) (dataConUserType c2)

    eqClosedFamilyAx (CoAxiom { co_ax_branches = branches1 })
                     (CoAxiom { co_ax_branches = branches2 })
      =  brListLength branches1 == brListLength branches2
      && (and $ brListZipWith eqClosedFamilyBranch branches1 branches2)

    eqClosedFamilyBranch (CoAxBranch { cab_tvs = tvs1, cab_lhs = lhs1, cab_rhs = rhs1 })
                         (CoAxBranch { cab_tvs = tvs2, cab_lhs = lhs2, cab_rhs = rhs2 })
      | Just env <- eqTyVarBndrs emptyRnEnv2 tvs1 tvs2
      = eqListBy (eqTypeX env) lhs1 lhs2 &&
        eqTypeX env rhs1 rhs2

      | otherwise = False

emptyRnEnv2 :: RnEnv2
emptyRnEnv2 = mkRnEnv2 emptyInScopeSet

----------------
missingBootThing :: Name -> String -> SDoc
missingBootThing name what
  = ppr name <+> ptext (sLit "is exported by the hs-boot file, but not")
              <+> text what <+> ptext (sLit "the module")

bootMisMatch :: TyThing -> TyThing -> SDoc
bootMisMatch real_thing boot_thing
  = vcat [ppr real_thing <+>
          ptext (sLit "has conflicting definitions in the module"),
          ptext (sLit "and its hs-boot file"),
          ptext (sLit "Main module:") <+> PprTyThing.pprTyThing real_thing,
          ptext (sLit "Boot file:  ") <+> PprTyThing.pprTyThing boot_thing]

instMisMatch :: ClsInst -> SDoc
instMisMatch inst
  = hang (ppr inst)
       2 (ptext (sLit "is defined in the hs-boot file, but not in the module itself"))
\end{code}


%************************************************************************
%*                                                                      *
        Type-checking the top level of a module
%*                                                                      *
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
rnTopSrcDecls :: [Name] -> HsGroup RdrName -> TcM (TcGblEnv, HsGroup Name)
-- Fails if there are any errors
rnTopSrcDecls extra_deps group
 = do { -- Rename the source decls
        traceTc "rn12" empty ;
        (tcg_env, rn_decls) <- checkNoErrs $ rnSrcDecls extra_deps group ;
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
\end{code}


%************************************************************************
%*                                                                      *
                AMP warnings
     The functions defined here issue warnings according to
     the 2013 Applicative-Monad proposal. (Trac #8004)
%*                                                                      *
%************************************************************************

Note [No AMP warning with NoImplicitPrelude]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If you have -XNoImplicitPrelude, then we suppress the AMP warnings.
The AMP warnings need access to Monad, Applicative, etc, and they
are defined in 'base'. If, when compiling package 'ghc-prim' (say),
you try to load Monad (from 'base'), chaos results because 'base'
depends on 'ghc-prim'.  See Note [Home module load error] in LoadIface,
and Trac #8320.

Using -XNoImplicitPrelude is a proxy for ensuring that all the
'base' modules are below the home module in the dependency tree.

\begin{code}
-- | Main entry point for generating AMP warnings
tcAmpWarn :: TcM ()
tcAmpWarn =
    do { implicit_prel <- xoptM Opt_ImplicitPrelude
       ; warnFlag <- woptM Opt_WarnAMP
       ; when (warnFlag && implicit_prel) $ do {
              -- See Note [No AMP warning with NoImplicitPrelude]

         -- Monad without Applicative
       ; tcAmpMissingParentClassWarn monadClassName
                                     applicativeClassName

         -- MonadPlus without Alternative
       ; tcAmpMissingParentClassWarn monadPlusClassName
                                     alternativeClassName

         -- Custom local definitions of join/pure/<*>
       ; mapM_ tcAmpFunctionWarn [joinMName, apAName, pureAName]
    }}



-- | Warn on local definitions of names that would clash with Prelude versions,
--   i.e. join/pure/<*>
--
--   A name clashes if the following criteria are met:
--       1. It would is imported (unqualified) from Prelude
--       2. It is locally defined in the current module
--       3. It has the same literal name as the reference function
--       4. It is not identical to the reference function
tcAmpFunctionWarn :: Name -- ^ Name to check, e.g. joinMName for join
                  -> TcM ()
tcAmpFunctionWarn name = do
    { traceTc "tcAmpFunctionWarn/wouldBeImported" empty
    -- Is the name imported (unqualified) from Prelude? (Point 4 above)
    ; rnImports <- fmap (map unLoc . tcg_rn_imports) getGblEnv
    -- (Note that this automatically handles -XNoImplicitPrelude, as Prelude
    -- will not appear in rnImports automatically if it is set.)

    -- Continue only the name is imported from Prelude
    ; when (tcAmpImportViaPrelude name rnImports) $ do
      -- Handle 2.-4.
    { rdrElts <- fmap (concat . occEnvElts . tcg_rdr_env) getGblEnv

    ; let clashes :: GlobalRdrElt -> Bool
          clashes x = and [ gre_prov x == LocalDef
                          , nameOccName (gre_name x) == nameOccName name
                          , gre_name x /= name
                          ]

          -- List of all offending definitions
          clashingElts :: [GlobalRdrElt]
          clashingElts = filter clashes rdrElts

    ; traceTc "tcAmpFunctionWarn/amp_prelude_functions"
                (hang (ppr name) 4 (sep [ppr clashingElts]))

    ; let warn_msg x = addWarnAt (nameSrcSpan $ gre_name x) . hsep $
              [ ptext (sLit "Local definition of")
              , quotes . ppr . nameOccName $ gre_name x
              , ptext (sLit "clashes with a future Prelude name")
              , ptext (sLit "- this will become an error in GHC 7.10,")
              , ptext (sLit "under the Applicative-Monad Proposal.")
              ]
    ; mapM_ warn_msg clashingElts
    }}

-- | Is the given name imported via Prelude?
--
--   This function makes sure that e.g. "import Prelude (map)" should silence
--   AMP warnings about "join" even when they are locally defined.
--
--   Possible scenarios:
--     a) Prelude is imported implicitly, issue warnings.
--     b) Prelude is imported explicitly, but without mentioning the name in
--        question. Issue no warnings.
--     c) Prelude is imported hiding the name in question. Issue no warnings.
--     d) Qualified import of Prelude, no warnings.
tcAmpImportViaPrelude :: Name
                      -> [ImportDecl Name]
                      -> Bool
tcAmpImportViaPrelude name = any importViaPrelude
  where
    isPrelude :: ImportDecl Name -> Bool
    isPrelude imp = unLoc (ideclName imp) == pRELUDE_NAME

    -- Implicit (Prelude) import?
    isImplicit :: ImportDecl Name -> Bool
    isImplicit = ideclImplicit

    -- Unqualified import?
    isUnqualified :: ImportDecl Name -> Bool
    isUnqualified = not . ideclQualified

    second :: (a -> b) -> (x, a) -> (x, b)
    second f (x, y) = (x, f y)

    -- List of explicitly imported (or hidden) Names from a single import.
    --   Nothing -> No explicit imports
    --   Just (False, <names>) -> Explicit import list of <names>
    --   Just (True , <names>) -> Explicit hiding of <names>
    importList :: ImportDecl Name -> Maybe (Bool, [Name])
    importList = fmap (second (map (ieName . unLoc))) . ideclHiding

    -- Check whether the given name would be imported (unqualified) from
    -- an import declaration.
    importViaPrelude :: ImportDecl Name -> Bool
    importViaPrelude x = isPrelude x && isUnqualified x && or [
        -- Whole Prelude imported -> potential clash
          isImplicit x
        -- Explicit import/hiding list, if applicable
        , case importList x of
            Just (False, explicit) -> nameOccName name `elem`    map nameOccName explicit
            Just (True , hidden  ) -> nameOccName name `notElem` map nameOccName hidden
            Nothing                -> False
        ]

-- | Issue a warning for instance definitions lacking a should-be parent class.
--   Used for Monad without Applicative and MonadPlus without Alternative.
tcAmpMissingParentClassWarn :: Name -- ^ Class instance is defined for
                            -> Name -- ^ Class it should also be instance of
                            -> TcM ()

-- Notation: is* is for classes the type is an instance of, should* for those
--           that it should also be an instance of based on the corresponding
--           is*.
--           Example: in case of Applicative/Monad: is = Monad,
--                                                  should = Applicative
tcAmpMissingParentClassWarn isName shouldName
  = do { isClass'     <- tcLookupClass_maybe isName
       ; shouldClass' <- tcLookupClass_maybe shouldName
       ; case (isClass', shouldClass') of
              (Just isClass, Just shouldClass) -> do
                  { localInstances <- tcGetInsts
                  ; let isInstance m = is_cls m == isClass
                        isInsts = filter isInstance localInstances
                  ; traceTc "tcAmpMissingParentClassWarn/isInsts" (ppr isInsts)
                  ; forM_ isInsts $ checkShouldInst isClass shouldClass
                  }
              _ -> return ()
       }
  where
    -- Checks whether the desired superclass exists in a given environment.
    checkShouldInst :: Class   -- ^ Class of existing instance
                    -> Class   -- ^ Class there should be an instance of
                    -> ClsInst -- ^ Existing instance
                    -> TcM ()
    checkShouldInst isClass shouldClass isInst
      = do { instEnv <- tcGetInstEnvs
           ; let (instanceMatches, shouldInsts, _)
                    = lookupInstEnv instEnv shouldClass (is_tys isInst)

           ; traceTc "tcAmpMissingParentClassWarn/checkShouldInst"
                     (hang (ppr isInst) 4
                         (sep [ppr instanceMatches, ppr shouldInsts]))

           -- "<location>: Warning: <type> is an instance of <is> but not <should>"
           -- e.g. "Foo is an instance of Monad but not Applicative"
           ; let instLoc = srcLocSpan . nameSrcLoc $ getName isInst
                 warnMsg (Just name:_) =
                      addWarnAt instLoc . hsep $
                           [ quotes (ppr $ nameOccName name)
                           , ptext (sLit "is an instance of")
                           , ppr . nameOccName $ className isClass
                           , ptext (sLit "but not")
                           , ppr . nameOccName $ className shouldClass
                           , ptext (sLit "- this will become an error in GHC 7.10,")
                           , ptext (sLit "under the Applicative-Monad Proposal.")
                           ]
                 warnMsg _ = return ()
           ; when (null shouldInsts && null instanceMatches) $
                  warnMsg (is_tcs isInst)
           }


-- | Looks up a class, returning Nothing on failure. Similar to
--   TcEnv.tcLookupClass, but does not issue any error messages.
--
-- In particular, it may be called by the AMP check on, say, 
-- Control.Applicative.Applicative, well before Control.Applicative 
-- has been compiled.  In this case we just return Nothing, and the
-- AMP test is silently dropped.
tcLookupClass_maybe :: Name -> TcM (Maybe Class)
tcLookupClass_maybe name
  = do { mb_thing <- tcLookupImported_maybe name
       ; case mb_thing of
            Succeeded (ATyCon tc) | Just cls <- tyConClass_maybe tc -> return (Just cls)
            _ -> return Nothing }
\end{code}


%************************************************************************
%*                                                                      *
                tcTopSrcDecls
%*                                                                      *
%************************************************************************


\begin{code}
tcTopSrcDecls :: ModDetails -> HsGroup Name -> TcM (TcGblEnv, TcLclEnv)
tcTopSrcDecls boot_details
        (HsGroup { hs_tyclds = tycl_decls,
                   hs_instds = inst_decls,
                   hs_derivds = deriv_decls,
                   hs_fords  = foreign_decls,
                   hs_defds  = default_decls,
                   hs_annds  = annotation_decls,
                   hs_ruleds = rule_decls,
                   hs_vects  = vect_decls,
                   hs_valds  = val_binds })
 = do {         -- Type-check the type and class decls, and all imported decls
                -- The latter come in via tycl_decls
        traceTc "Tc2 (src)" empty ;

                -- Source-language instances, including derivings,
                -- and import the supporting declarations
        traceTc "Tc3" empty ;
        (tcg_env, inst_infos, deriv_binds)
            <- tcTyClsInstDecls boot_details tycl_decls inst_decls deriv_decls ;
        setGblEnv tcg_env       $ do {


                -- Generate Applicative/Monad proposal (AMP) warnings
        traceTc "Tc3b" empty ;
        tcAmpWarn ;

                -- Foreign import declarations next.
        traceTc "Tc4" empty ;
        (fi_ids, fi_decls, fi_gres) <- tcForeignImports foreign_decls ;
        tcExtendGlobalValEnv fi_ids     $ do {

                -- Default declarations
        traceTc "Tc4a" empty ;
        default_tys <- tcDefaults default_decls ;
        updGblEnv (\gbl -> gbl { tcg_default = default_tys }) $ do {

                -- Now GHC-generated derived bindings, generics, and selectors
                -- Do not generate warnings from compiler-generated code;
                -- hence the use of discardWarnings
        tc_envs <- discardWarnings (tcTopBinds deriv_binds) ;
        setEnvs tc_envs $ do {

                -- Value declarations next
        traceTc "Tc5" empty ;
        tc_envs@(tcg_env, tcl_env) <- tcTopBinds val_binds;
        setEnvs tc_envs $ do {  -- Environment doesn't change now

                -- Second pass over class and instance declarations,
                -- now using the kind-checked decls
        traceTc "Tc6" empty ;
        inst_binds <- tcInstDecls2 (tyClGroupConcat tycl_decls) inst_infos ;

                -- Foreign exports
        traceTc "Tc7" empty ;
        (foe_binds, foe_decls, foe_gres) <- tcForeignExports foreign_decls ;

                -- Annotations
        annotations <- tcAnnotations annotation_decls ;

                -- Rules
        rules <- tcRules rule_decls ;

                -- Vectorisation declarations
        vects <- tcVectDecls vect_decls ;

                -- Wrap up
        traceTc "Tc7a" empty ;
        let { all_binds = inst_binds     `unionBags`
                          foe_binds

            ; fo_gres = fi_gres `unionBags` foe_gres
            ; fo_fvs = foldrBag (\gre fvs -> fvs `addOneFV` gre_name gre) 
                                emptyFVs fo_gres
            ; fo_rdr_names :: [RdrName]
            ; fo_rdr_names = foldrBag gre_to_rdr_name [] fo_gres

            ; sig_names = mkNameSet (collectHsValBinders val_binds)
                          `minusNameSet` getTypeSigNames val_binds

                -- Extend the GblEnv with the (as yet un-zonked)
                -- bindings, rules, foreign decls
            ; tcg_env' = tcg_env { tcg_binds   = tcg_binds tcg_env `unionBags` all_binds
                                 , tcg_sigs    = tcg_sigs tcg_env `unionNameSets` sig_names
                                 , tcg_rules   = tcg_rules tcg_env ++ rules
                                 , tcg_vects   = tcg_vects tcg_env ++ vects
                                 , tcg_anns    = tcg_anns tcg_env ++ annotations
                                 , tcg_ann_env = extendAnnEnvList (tcg_ann_env tcg_env) annotations
                                 , tcg_fords   = tcg_fords tcg_env ++ foe_decls ++ fi_decls
                                 , tcg_dus     = tcg_dus tcg_env `plusDU` usesOnly fo_fvs } } ;
                                 -- tcg_dus: see Note [Newtype constructor usage in foreign declarations]

        addUsedRdrNames fo_rdr_names ;
        return (tcg_env', tcl_env)
    }}}}}}
  where
    gre_to_rdr_name :: GlobalRdrElt -> [RdrName] -> [RdrName]
        -- For *imported* newtype data constructors, we want to
        -- make sure that at least one of the imports for them is used
        -- See Note [Newtype constructor usage in foreign declarations]
    gre_to_rdr_name gre rdrs
      = case gre_prov gre of
           LocalDef          -> rdrs
           Imported []       -> panic "gre_to_rdr_name: Imported []"
           Imported (is : _) -> mkRdrQual modName occName : rdrs
              where
                modName = is_as (is_decl is)
                occName = nameOccName (gre_name gre)

---------------------------
tcTyClsInstDecls :: ModDetails 
                 -> [TyClGroup Name] 
                 -> [LInstDecl Name]
                 -> [LDerivDecl Name]
                 -> TcM (TcGblEnv,            -- The full inst env
                         [InstInfo Name],     -- Source-code instance decls to process;
                                              -- contains all dfuns for this module
                          HsValBinds Name)    -- Supporting bindings for derived instances

tcTyClsInstDecls boot_details tycl_decls inst_decls deriv_decls
 = tcExtendKindEnv2 [ (con, APromotionErr FamDataConPE) 
                    | lid <- inst_decls, con <- get_cons lid ] $
      -- Note [AFamDataCon: not promoting data family constructors]
   do { tcg_env <- tcTyAndClassDecls boot_details tycl_decls ;
      ; setGblEnv tcg_env $
        tcInstDecls1 (tyClGroupConcat tycl_decls) inst_decls deriv_decls }
  where
    -- get_cons extracts the *constructor* bindings of the declaration
    get_cons :: LInstDecl Name -> [Name]
    get_cons (L _ (TyFamInstD {}))                     = []
    get_cons (L _ (DataFamInstD { dfid_inst = fid }))  = get_fi_cons fid
    get_cons (L _ (ClsInstD { cid_inst = ClsInstDecl { cid_datafam_insts = fids } }))
      = concatMap (get_fi_cons . unLoc) fids

    get_fi_cons :: DataFamInstDecl Name -> [Name]
    get_fi_cons (DataFamInstDecl { dfid_defn = HsDataDefn { dd_cons = cons } }) 
      = map (unLoc . con_name . unLoc) cons
\end{code}

Note [AFamDataCon: not promoting data family constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data family T a
  data instance T Int = MkT
  data Proxy (a :: k)
  data S = MkS (Proxy 'MkT)

Is it ok to use the promoted data family instance constructor 'MkT' in
the data declaration for S?  No, we don't allow this. It *might* make
sense, but at least it would mean that we'd have to interleave
typechecking instances and data types, whereas at present we do data
types *then* instances.

So to check for this we put in the TcLclEnv a binding for all the family
constructors, bound to AFamDataCon, so that if we trip over 'MkT' when
type checking 'S' we'll produce a decent error message.


%************************************************************************
%*                                                                      *
        Checking for 'main'
%*                                                                      *
%************************************************************************

\begin{code}
checkMain :: TcM TcGblEnv
-- If we are in module Main, check that 'main' is defined.
checkMain
  = do { tcg_env   <- getGblEnv ;
         dflags    <- getDynFlags ;
         check_main dflags tcg_env
    }

check_main :: DynFlags -> TcGblEnv -> TcM TcGblEnv
check_main dflags tcg_env
 | mod /= main_mod
 = traceTc "checkMain not" (ppr main_mod <+> ppr mod) >>
   return tcg_env

 | otherwise
 = do   { mb_main <- lookupGlobalOccRn_maybe main_fn
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
                <- addErrCtxt mainCtxt    $
                   tcMonoExpr (L loc (HsVar main_name)) (mkTyConApp ioTyCon [res_ty])

                -- See Note [Root-main Id]
                -- Construct the binding
                --      :Main.main :: IO res_ty = runMainIO res_ty main
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
                                        `snocBag` (Generated, main_bind),
                            tcg_dus   = tcg_dus tcg_env
                                        `plusDU` usesOnly (unitFV main_name)
                        -- Record the use of 'main', so that we don't
                        -- complain about it being defined but not used
                 })
    }}}
  where
    mod          = tcg_mod tcg_env
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

-- | Get the unqualified name of the function to use as the \"main\" for the main module.
-- Either returns the default name or the one configured on the command line with -main-is
getMainFun :: DynFlags -> RdrName
getMainFun dflags = case mainFunIs dflags of
                      Just fn -> mkRdrUnqual (mkVarOccFS (mkFastString fn))
                      Nothing -> main_RDR_Unqual

checkMainExported :: TcGblEnv -> TcM ()
checkMainExported tcg_env
  = case tcg_main tcg_env of
      Nothing -> return () -- not the main module
      Just main_name -> 
         do { dflags <- getDynFlags
            ; let main_mod = mainModIs dflags
            ; checkTc (main_name `elem` concatMap availNames (tcg_exports tcg_env)) $
                ptext (sLit "The") <+> ppMainFn (nameRdrName main_name) <+>
                ptext (sLit "is not exported by module") <+> quotes (ppr main_mod) }

ppMainFn :: RdrName -> SDoc
ppMainFn main_fn
  | rdrNameOcc main_fn == mainOcc
  = ptext (sLit "IO action") <+> quotes (ppr main_fn)
  | otherwise
  = ptext (sLit "main IO action") <+> quotes (ppr main_fn)

mainOcc :: OccName
mainOcc = mkVarOccFS (fsLit "main")
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
%*                                                       *
                GHCi stuff
%*                                                       *
%*********************************************************

\begin{code}
runTcInteractive :: HscEnv -> TcRn a -> IO (Messages, Maybe a)
-- Initialise the tcg_inst_env with instances from all home modules.
-- This mimics the more selective call to hptInstances in tcRnImports
runTcInteractive hsc_env thing_inside
  = initTcInteractive hsc_env $
    do { traceTc "setInteractiveContext" $
            vcat [ text "ic_tythings:" <+> vcat (map ppr (ic_tythings icxt))
                 , text "ic_insts:" <+> vcat (map (pprBndr LetBind . instanceDFunId) ic_insts)
                 , text "ic_rn_gbl_env (LocalDef)" <+>
                      vcat (map ppr [ local_gres | gres <- occEnvElts (ic_rn_gbl_env icxt)
                                                 , let local_gres = filter isLocalGRE gres
                                                 , not (null local_gres) ]) ]
       ; gbl_env <- getGblEnv
       ; let gbl_env' = gbl_env {
                           tcg_rdr_env      = ic_rn_gbl_env icxt
                         , tcg_type_env     = type_env
                         , tcg_insts        = ic_insts
                         , tcg_fam_insts    = ic_finsts
                         , tcg_inst_env     = extendInstEnvList
                                               (extendInstEnvList (tcg_inst_env gbl_env) ic_insts)
                                               home_insts
                         , tcg_fam_inst_env = extendFamInstEnvList
                                               (extendFamInstEnvList (tcg_fam_inst_env gbl_env)
                                                                     ic_finsts)
                                               home_fam_insts
                         , tcg_field_env    = RecFields (mkNameEnv con_fields)
                                                        (mkNameSet (concatMap snd con_fields))
                              -- setting tcg_field_env is necessary
                              -- to make RecordWildCards work (test: ghci049)
                         , tcg_fix_env      = ic_fix_env icxt
                         , tcg_default      = ic_default icxt }

       ; setGblEnv gbl_env' $
         tcExtendGhciIdEnv ty_things $   -- See Note [Initialising the type environment for GHCi]
         thing_inside }                  -- in TcEnv
  where
    (home_insts, home_fam_insts) = hptInstances hsc_env (\_ -> True)

    icxt                  = hsc_IC hsc_env
    (ic_insts, ic_finsts) = ic_instances icxt
    ty_things             = ic_tythings icxt

    type_env1 = mkTypeEnvWithImplicits ty_things
    type_env  = extendTypeEnvWithIds type_env1 (map instanceDFunId ic_insts)
                -- Putting the dfuns in the type_env
                -- is just to keep Core Lint happy

    con_fields = [ (dataConName c, dataConFieldLabels c)
                 | ATyCon t <- ty_things
                 , c <- tyConDataCons t ]


#ifdef GHCI
-- | The returned [Id] is the list of new Ids bound by this statement. It can
-- be used to extend the InteractiveContext via extendInteractiveContext.
--
-- The returned TypecheckedHsExpr is of type IO [ () ], a list of the bound
-- values, coerced to ().
tcRnStmt :: HscEnv -> GhciLStmt RdrName
         -> IO (Messages, Maybe ([Id], LHsExpr Id, FixityEnv))
tcRnStmt hsc_env rdr_stmt
  = runTcInteractive hsc_env $ do {

    -- The real work is done here
    ((bound_ids, tc_expr), fix_env) <- tcUserStmt rdr_stmt ;
    zonked_expr <- zonkTopLExpr tc_expr ;
    zonked_ids  <- zonkTopBndrs bound_ids ;

        -- None of the Ids should be of unboxed type, because we
        -- cast them all to HValues in the end!
    mapM_ bad_unboxed (filter (isUnLiftedType . idType) zonked_ids) ;

    traceTc "tcs 1" empty ;
    let { global_ids = map globaliseAndTidyId zonked_ids } ;
        -- Note [Interactively-bound Ids in GHCi] in HscTypes

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

    return (global_ids, zonked_expr, fix_env)
    }
  where
    bad_unboxed id = addErr (sep [ptext (sLit "GHCi can't bind a variable of unlifted type:"),
                                  nest 2 (ppr id <+> dcolon <+> ppr (idType id))])
\end{code}


--------------------------------------------------------------------------
                Typechecking Stmts in GHCi

Here is the grand plan, implemented in tcUserStmt

        What you type                   The IO [HValue] that hscStmt returns
        -------------                   ------------------------------------
        let pat = expr          ==>     let pat = expr in return [coerce HVal x, coerce HVal y, ...]
                                        bindings: [x,y,...]

        pat <- expr             ==>     expr >>= \ pat -> return [coerce HVal x, coerce HVal y, ...]
                                        bindings: [x,y,...]

        expr (of IO type)       ==>     expr >>= \ it -> return [coerce HVal it]
          [NB: result not printed]      bindings: [it]

        expr (of non-IO type,   ==>     let it = expr in print it >> return [coerce HVal it]
          result showable)              bindings: [it]

        expr (of non-IO type,
          result not showable)  ==>     error

\begin{code}

-- | A plan is an attempt to lift some code into the IO monad.
type PlanResult = ([Id], LHsExpr Id)
type Plan = TcM PlanResult

-- | Try the plans in order. If one fails (by raising an exn), try the next.
-- If one succeeds, take it.
runPlans :: [Plan] -> TcM PlanResult
runPlans []     = panic "runPlans"
runPlans [p]    = p
runPlans (p:ps) = tryTcLIE_ (runPlans ps) p

-- | Typecheck (and 'lift') a stmt entered by the user in GHCi into the
-- GHCi 'environemnt'.
--
-- By 'lift' and 'environment we mean that the code is changed to
-- execute properly in an IO monad. See Note [Interactively-bound Ids
-- in GHCi] in HscTypes for more details. We do this lifting by trying
-- different ways ('plans') of lifting the code into the IO monad and
-- type checking each plan until one succeeds.
tcUserStmt :: GhciLStmt RdrName -> TcM (PlanResult, FixityEnv)

-- An expression typed at the prompt is treated very specially
tcUserStmt (L loc (BodyStmt expr _ _ _))
  = do  { (rn_expr, fvs) <- checkNoErrs (rnLExpr expr)
               -- Don't try to typecheck if the renamer fails!
        ; ghciStep <- getGhciStepIO
        ; uniq <- newUnique
        ; interPrintName <- getInteractivePrintName
        ; let fresh_it  = itName uniq loc
              matches   = [mkMatch [] rn_expr emptyLocalBinds]
              -- [it = expr]
              the_bind  = L loc $ (mkTopFunBind (L loc fresh_it) matches) { bind_fvs = fvs }
                          -- Care here!  In GHCi the expression might have
                          -- free variables, and they in turn may have free type variables
                          -- (if we are at a breakpoint, say).  We must put those free vars

              -- [let it = expr]
              let_stmt  = L loc $ LetStmt $ HsValBinds $
                          ValBindsOut [(NonRecursive,unitBag (FromSource, the_bind))] []

              -- [it <- e]
              bind_stmt = L loc $ BindStmt (L loc (VarPat fresh_it))
                                           (nlHsApp ghciStep rn_expr)
                                           (HsVar bindIOName) noSyntaxExpr

              -- [; print it]
              print_it  = L loc $ BodyStmt (nlHsApp (nlHsVar interPrintName) (nlHsVar fresh_it))
                                           (HsVar thenIOName) noSyntaxExpr placeHolderType

        -- The plans are:
        --   A. [it <- e; print it]     but not if it::()
        --   B. [it <- e]
        --   C. [let it = e; print it]
        --
        -- Ensure that type errors don't get deferred when type checking the
        -- naked expression. Deferring type errors here is unhelpful because the
        -- expression gets evaluated right away anyway. It also would potentially
        -- emit two redundant type-error warnings, one from each plan.
        ; plan <- unsetGOptM Opt_DeferTypeErrors $ runPlans [
                    -- Plan A
                    do { stuff@([it_id], _) <- tcGhciStmts [bind_stmt, print_it]
                       ; it_ty <- zonkTcType (idType it_id)
                       ; when (isUnitTy $ it_ty) failM
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
                       ; tcGhciStmts [let_stmt, print_it] } ]

        ; fix_env <- getFixityEnv
        ; return (plan, fix_env) }

tcUserStmt rdr_stmt@(L loc _)
  = do { (([rn_stmt], fix_env), fvs) <- checkNoErrs $
           rnStmts GhciStmtCtxt rnLExpr [rdr_stmt] $ \_ -> do
             fix_env <- getFixityEnv
             return (fix_env, emptyFVs)
            -- Don't try to typecheck if the renamer fails!
       ; traceRn (text "tcRnStmt" <+> vcat [ppr rdr_stmt, ppr rn_stmt, ppr fvs])
       ; rnDump (ppr rn_stmt) ;

       ; ghciStep <- getGhciStepIO
       ; let gi_stmt
               | (L loc (BindStmt pat expr op1 op2)) <- rn_stmt
                           = L loc $ BindStmt pat (nlHsApp ghciStep expr) op1 op2
               | otherwise = rn_stmt

       ; opt_pr_flag <- goptM Opt_PrintBindResult
       ; let print_result_plan
               | opt_pr_flag                         -- The flag says "print result"   
               , [v] <- collectLStmtBinders gi_stmt  -- One binder
                           =  [mk_print_result_plan gi_stmt v]
               | otherwise = []

        -- The plans are:
        --      [stmt; print v]         if one binder and not v::()
        --      [stmt]                  otherwise
       ; plan <- runPlans (print_result_plan ++ [tcGhciStmts [gi_stmt]])
       ; return (plan, fix_env) }
  where
    mk_print_result_plan stmt v
      = do { stuff@([v_id], _) <- tcGhciStmts [stmt, print_v]
           ; v_ty <- zonkTcType (idType v_id)
           ; when (isUnitTy v_ty || not (isTauTy v_ty)) failM
           ; return stuff }
      where
        print_v  = L loc $ BodyStmt (nlHsApp (nlHsVar printName) (nlHsVar v))
                                    (HsVar thenIOName) noSyntaxExpr placeHolderType

-- | Typecheck the statements given and then return the results of the
-- statement in the form 'IO [()]'.
tcGhciStmts :: [GhciLStmt Name] -> TcM PlanResult
tcGhciStmts stmts
 = do { ioTyCon <- tcLookupTyCon ioTyConName ;
        ret_id  <- tcLookupId returnIOName ;            -- return @ IO
        let {
            ret_ty      = mkListTy unitTy ;
            io_ret_ty   = mkTyConApp ioTyCon [ret_ty] ;
            tc_io_stmts = tcStmtsAndThen GhciStmtCtxt tcDoStmt stmts io_ret_ty ;
            names = collectLStmtsBinders stmts ;
         } ;

        -- OK, we're ready to typecheck the stmts
        traceTc "TcRnDriver.tcGhciStmts: tc stmts" empty ;
        ((tc_stmts, ids), lie) <- captureConstraints $
                                  tc_io_stmts $ \ _ ->
                                  mapM tcLookupId names  ;
                        -- Look up the names right in the middle,
                        -- where they will all be in scope

        -- Simplify the context
        traceTc "TcRnDriver.tcGhciStmts: simplify ctxt" empty ;
        const_binds <- checkNoErrs (simplifyInteractive lie) ;
                -- checkNoErrs ensures that the plan fails if context redn fails

        traceTc "TcRnDriver.tcGhciStmts: done" empty ;
        let {   -- mk_return builds the expression
                --      returnIO @ [()] [coerce () x, ..,  coerce () z]
                --
                -- Despite the inconvenience of building the type applications etc,
                -- this *has* to be done in type-annotated post-typecheck form
                -- because we are going to return a list of *polymorphic* values
                -- coerced to type (). If we built a *source* stmt
                --      return [coerce x, ..., coerce z]
                -- then the type checker would instantiate x..z, and we wouldn't
                -- get their *polymorphic* values.  (And we'd get ambiguity errs
                -- if they were overloaded, since they aren't applied to anything.)
            ret_expr = nlHsApp (nlHsTyApp ret_id [ret_ty])
                       (noLoc $ ExplicitList unitTy Nothing (map mk_item ids)) ;
            mk_item id = nlHsApp (nlHsTyApp unsafeCoerceId [idType id, unitTy])
                                 (nlHsVar id) ;
            stmts = tc_stmts ++ [noLoc (mkLastStmt ret_expr)]
        } ;
        return (ids, mkHsDictLet (EvBinds const_binds) $
                     noLoc (HsDo GhciStmtCtxt stmts io_ret_ty))
    }

-- | Generate a typed ghciStepIO expression (ghciStep :: Ty a -> IO a)
getGhciStepIO :: TcM (LHsExpr Name)
getGhciStepIO = do
    ghciTy <- getGHCiMonad
    fresh_a <- newUnique
    let a_tv   = mkTcTyVarName fresh_a (fsLit "a")
        ghciM  = nlHsAppTy (nlHsTyVar ghciTy) (nlHsTyVar a_tv)
        ioM    = nlHsAppTy (nlHsTyVar ioTyConName) (nlHsTyVar a_tv)

        stepTy :: LHsType Name    -- Renamed, so needs all binders in place
        stepTy = noLoc $ HsForAllTy Implicit
                            (HsQTvs { hsq_tvs = [noLoc (UserTyVar a_tv)]
                                    , hsq_kvs = [] })
                            (noLoc [])
                            (nlHsFunTy ghciM ioM)
        step   = noLoc $ ExprWithTySig (nlHsVar ghciStepIoMName) stepTy
    return step

isGHCiMonad :: HscEnv -> String -> IO (Messages, Maybe Name)
isGHCiMonad hsc_env ty
  = runTcInteractive hsc_env $ do
        rdrEnv <- getGlobalRdrEnv
        let occIO = lookupOccEnv rdrEnv (mkOccName tcName ty)
        case occIO of
            Just [n] -> do
                let name = gre_name n
                ghciClass <- tcLookupClass ghciIoClassName 
                userTyCon <- tcLookupTyCon name
                let userTy = mkTyConApp userTyCon []
                _ <- tcLookupInstance ghciClass [userTy]
                return name

            Just _  -> failWithTc $ text "Ambigous type!"
            Nothing -> failWithTc $ text ("Can't find type:" ++ ty)

\end{code}

tcRnExpr just finds the type of an expression

\begin{code}
tcRnExpr :: HscEnv
         -> LHsExpr RdrName
         -> IO (Messages, Maybe Type)
-- Type checks the expression and returns its most general type
tcRnExpr hsc_env rdr_expr
  = runTcInteractive hsc_env $ do {

    (rn_expr, _fvs) <- rnLExpr rdr_expr ;
    failIfErrsM ;

        -- Now typecheck the expression;
        -- it might have a rank-2 type (e.g. :t runST)
    uniq <- newUnique ;
    let { fresh_it  = itName uniq (getLoc rdr_expr) } ;
    (((_tc_expr, res_ty), _), lie) <- captureConstraints $ 
                                      captureUntouchables $
                                      tcInferRho rn_expr ;
    ((qtvs, dicts, _, _), lie_top) <- captureConstraints $
                                      {-# SCC "simplifyInfer" #-}
                                      simplifyInfer True {- Free vars are closed -}
                                                    False {- No MR for now -}
                                                    [(fresh_it, res_ty)]
                                                    lie ;
    _ <- simplifyInteractive lie_top ;       -- Ignore the dicionary bindings

    let { all_expr_ty = mkForAllTys qtvs (mkPiTypes dicts res_ty) } ;
    zonkTcType all_expr_ty
    }

--------------------------
tcRnImportDecls :: HscEnv
                -> [LImportDecl RdrName]
                -> IO (Messages, Maybe GlobalRdrEnv)
-- Find the new chunk of GlobalRdrEnv created by this list of import
-- decls.  In contract tcRnImports *extends* the TcGblEnv.
tcRnImportDecls hsc_env import_decls
 =  runTcInteractive hsc_env $
    do { gbl_env <- updGblEnv zap_rdr_env $
                    tcRnImports hsc_env import_decls
       ; return (tcg_rdr_env gbl_env) }
  where
    zap_rdr_env gbl_env = gbl_env { tcg_rdr_env = emptyGlobalRdrEnv }
\end{code}

tcRnType just finds the kind of a type

\begin{code}
tcRnType :: HscEnv
         -> Bool        -- Normalise the returned type
         -> LHsType RdrName
         -> IO (Messages, Maybe (Type, Kind))
tcRnType hsc_env normalise rdr_type
  = runTcInteractive hsc_env $
    setXOptM Opt_PolyKinds $   -- See Note [Kind-generalise in tcRnType]
    do { (rn_type, _fvs) <- rnLHsType GHCiCtx rdr_type
       ; failIfErrsM

        -- Now kind-check the type
        -- It can have any rank or kind
       ; ty <- tcHsSigType GhciCtxt rn_type ;

       ; ty' <- if normalise
                then do { fam_envs <- tcGetFamInstEnvs
                        ; return (snd (normaliseType fam_envs Nominal ty)) }
                        -- normaliseType returns a coercion
                        -- which we discard, so the Role is irrelevant
                else return ty ;

       ; return (ty', typeKind ty) }
\end{code}

Note [Kind-generalise in tcRnType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We switch on PolyKinds when kind-checking a user type, so that we will
kind-generalise the type.  This gives the right default behaviour at
the GHCi prompt, where if you say ":k T", and T has a polymorphic
kind, you'd like to see that polymorphism. Of course.  If T isn't
kind-polymorphic you won't get anything unexpected, but the apparent
*loss* of polymorphism, for types that you know are polymorphic, is
quite surprising.  See Trac #7688 for a discussion.


%************************************************************************
%*                                                                      *
                 tcRnDeclsi
%*                                                                      *
%************************************************************************

tcRnDeclsi exists to allow class, data, and other declarations in GHCi.

\begin{code}
tcRnDeclsi :: HscEnv
           -> [LHsDecl RdrName]
           -> IO (Messages, Maybe TcGblEnv)

tcRnDeclsi hsc_env local_decls =
  runTcInteractive hsc_env $ do

    ((tcg_env, tclcl_env), lie) <-
        captureConstraints $ tc_rn_src_decls emptyModDetails local_decls
    setEnvs (tcg_env, tclcl_env) $ do

    new_ev_binds <- simplifyTop lie
    failIfErrsM
    let TcGblEnv { tcg_type_env  = type_env,
                   tcg_binds     = binds,
                   tcg_sigs      = sig_ns,
                   tcg_ev_binds  = cur_ev_binds,
                   tcg_imp_specs = imp_specs,
                   tcg_rules     = rules,
                   tcg_vects     = vects,
                   tcg_fords     = fords } = tcg_env
        all_ev_binds = cur_ev_binds `unionBags` new_ev_binds

    (bind_ids, ev_binds', binds', fords', imp_specs', rules', vects')
        <- zonkTopDecls all_ev_binds binds sig_ns rules vects imp_specs fords

    let --global_ids = map globaliseAndTidyId bind_ids
        final_type_env = extendTypeEnvWithIds type_env bind_ids --global_ids
        tcg_env' = tcg_env { tcg_binds     = binds',
                             tcg_ev_binds  = ev_binds',
                             tcg_imp_specs = imp_specs',
                             tcg_rules     = rules',
                             tcg_vects     = vects',
                             tcg_fords     = fords' }

    setGlobalTypeEnv tcg_env' final_type_env
    
#endif /* GHCi */
\end{code}


%************************************************************************
%*                                                                      *
        More GHCi stuff, to do with browsing and getting info
%*                                                                      *
%************************************************************************

\begin{code}
#ifdef GHCI
-- | ASSUMES that the module is either in the 'HomePackageTable' or is
-- a package module with an interface on disk.  If neither of these is
-- true, then the result will be an error indicating the interface
-- could not be found.
getModuleInterface :: HscEnv -> Module -> IO (Messages, Maybe ModIface)
getModuleInterface hsc_env mod
  = runTcInteractive hsc_env $
    loadModuleInterface (ptext (sLit "getModuleInterface")) mod

tcRnLookupRdrName :: HscEnv -> RdrName -> IO (Messages, Maybe [Name])
tcRnLookupRdrName hsc_env rdr_name
  = runTcInteractive hsc_env $
    lookup_rdr_name rdr_name

lookup_rdr_name :: RdrName -> TcM [Name]
lookup_rdr_name rdr_name = do
        -- If the identifier is a constructor (begins with an
        -- upper-case letter), then we need to consider both
        -- constructor and type class identifiers.
    let rdr_names = dataTcOccs rdr_name

        -- results :: [Either Messages Name]
    results <- mapM (tryTcErrs . lookupOccRn) rdr_names

    traceRn (text "xx" <+> vcat [ppr rdr_names, ppr (map snd results)])
        -- The successful lookups will be (Just name)
    let (warns_s, good_names) = unzip [ (msgs, name)
                                      | (msgs, Just name) <- results]
        errs_s = [msgs | (msgs, Nothing) <- results]

        -- Fail if nothing good happened, else add warnings
    if null good_names
      then  addMessages (head errs_s) >> failM
                -- No lookup succeeded, so
                -- pick the first error message and report it
                -- ToDo: If one of the errors is "could be Foo.X or Baz.X",
                --       while the other is "X is not in scope",
                --       we definitely want the former; but we might pick the latter
      else      mapM_ addMessages warns_s
                -- Add deprecation warnings
    return good_names

#endif

tcRnLookupName :: HscEnv -> Name -> IO (Messages, Maybe TyThing)
tcRnLookupName hsc_env name
  = runTcInteractive hsc_env $
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
            -> IO (Messages, Maybe (TyThing, Fixity, [ClsInst], [FamInst]))

-- Used to implement :info in GHCi
--
-- Look up a RdrName and return all the TyThings it might be
-- A capitalised RdrName is given to us in the DataName namespace,
-- but we want to treat it as *both* a data constructor
--  *and* as a type or class constructor;
-- hence the call to dataTcOccs, and we return up to two results
tcRnGetInfo hsc_env name
  = runTcInteractive hsc_env $
    do { loadUnqualIfaces hsc_env (hsc_IC hsc_env)
           -- Load the interface for all unqualified types and classes
           -- That way we will find all the instance declarations
           -- (Packages have not orphan modules, and we assume that
           --  in the home package all relevant modules are loaded.)

       ; thing  <- tcRnLookupName' name
       ; fixity <- lookupFixityRn name
       ; (cls_insts, fam_insts) <- lookupInsts thing
       ; return (thing, fixity, cls_insts, fam_insts) }

lookupInsts :: TyThing -> TcM ([ClsInst],[FamInst])
lookupInsts (ATyCon tc)
  = do  { (pkg_ie, home_ie) <- tcGetInstEnvs
        ; (pkg_fie, home_fie) <- tcGetFamInstEnvs
                -- Load all instances for all classes that are
                -- in the type environment (which are all the ones
                -- we've seen in any interface file so far)

          -- Return only the instances relevant to the given thing, i.e.
          -- the instances whose head contains the thing's name.
        ; let cls_insts =
                 [ ispec        -- Search all
                 | ispec <- instEnvElts home_ie ++ instEnvElts pkg_ie
                 , tc_name `elemNameSet` orphNamesOfClsInst ispec ]
        ; let fam_insts =
                 [ fispec
                 | fispec <- famInstEnvElts home_fie ++ famInstEnvElts pkg_fie
                 , tc_name `elemNameSet` orphNamesOfFamInst fispec ]
        ; return (cls_insts, fam_insts) }
  where
    tc_name     = tyConName tc

lookupInsts _ = return ([],[])

loadUnqualIfaces :: HscEnv -> InteractiveContext -> TcM ()
-- Load the interface for everything that is in scope unqualified
-- This is so that we can accurately report the instances for
-- something
loadUnqualIfaces hsc_env ictxt
  = initIfaceTcRn $ do
    mapM_ (loadSysInterface doc) (moduleSetElts (mkModuleSet unqual_mods))
  where
    this_pkg = thisPackage (hsc_dflags hsc_env)

    unqual_mods = [ mod
                  | gre <- globalRdrEnvElts (ic_rn_gbl_env ictxt)
                  , let name = gre_name gre
                  , not (isInternalName name)
                  , let mod = nameModule name
                  , not (modulePackageId mod == this_pkg || isInteractiveModule mod)
                      -- Don't attempt to load an interface for stuff
                      -- from the command line, or from the home package
                  , isTcOcc (nameOccName name)   -- Types and classes only
                  , unQualOK gre ]               -- In scope unqualified
    doc = ptext (sLit "Need interface for module whose export(s) are in scope unqualified")
\end{code}

%************************************************************************
%*                                                                      *
                Degugging output
%*                                                                      *
%************************************************************************

\begin{code}
rnDump :: SDoc -> TcRn ()
-- Dump, with a banner, if -ddump-rn
rnDump doc = do { dumpOptTcRn Opt_D_dump_rn (mkDumpDoc "Renamer" doc) }

tcDump :: TcGblEnv -> TcRn ()
tcDump env
 = do { dflags <- getDynFlags ;

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
 = do { dflags <- getDynFlags ;
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
                        tcg_vects     = vects,
                        tcg_imports   = imports })
  = vcat [ ppr_types insts type_env
         , ppr_tycons fam_insts type_env
         , ppr_insts insts
         , ppr_fam_insts fam_insts
         , vcat (map ppr rules)
         , vcat (map ppr vects)
         , ptext (sLit "Dependent modules:") <+>
                ppr (sortBy cmp_mp $ eltsUFM (imp_dep_mods imports))
         , ptext (sLit "Dependent packages:") <+>
                ppr (sortBy stablePackageIdCmp $ imp_dep_pkgs imports)]
  where         -- The two uses of sortBy are just to reduce unnecessary
                -- wobbling in testsuite output
    cmp_mp (mod_name1, is_boot1) (mod_name2, is_boot2)
        = (mod_name1 `stableModuleNameCmp` mod_name2)
                  `thenCmp`
          (is_boot1 `compare` is_boot2)

pprModGuts :: ModGuts -> SDoc
pprModGuts (ModGuts { mg_tcs = tcs
                    , mg_rules = rules })
  = vcat [ ppr_types [] (mkTypeEnv (map ATyCon tcs)),
           ppr_rules rules ]

ppr_types :: [ClsInst] -> TypeEnv -> SDoc
ppr_types insts type_env
  = text "TYPE SIGNATURES" $$ nest 2 (ppr_sigs ids)
  where
    dfun_ids = map instanceDFunId insts
    ids = [id | id <- typeEnvIds type_env, want_sig id]
    want_sig id | opt_PprStyle_Debug = True
                | otherwise          = isLocalId id &&
                                       isExternalName (idName id) &&
                                       not (id `elem` dfun_ids)
        -- isLocalId ignores data constructors, records selectors etc.
        -- The isExternalName ignores local dictionary and method bindings
        -- that the type checker has invented.  Top-level user-defined things
        -- have External names.

ppr_tycons :: [FamInst] -> TypeEnv -> SDoc
ppr_tycons fam_insts type_env
  = vcat [ text "TYPE CONSTRUCTORS"
         ,   nest 2 (ppr_tydecls tycons)
         , text "COERCION AXIOMS"
         ,   nest 2 (vcat (map pprCoAxiom (typeEnvCoAxioms type_env))) ]
  where
    fi_tycons = famInstsRepTyCons fam_insts
    tycons = [tycon | tycon <- typeEnvTyCons type_env, want_tycon tycon]
    want_tycon tycon | opt_PprStyle_Debug = True
                     | otherwise          = not (isImplicitTyCon tycon) &&
                                            isExternalName (tyConName tycon) &&
                                            not (tycon `elem` fi_tycons)

ppr_insts :: [ClsInst] -> SDoc
ppr_insts []     = empty
ppr_insts ispecs = text "INSTANCES" $$ nest 2 (pprInstances ispecs)

ppr_fam_insts :: [FamInst] -> SDoc
ppr_fam_insts []        = empty
ppr_fam_insts fam_insts =
  text "FAMILY INSTANCES" $$ nest 2 (pprFamInsts fam_insts)

ppr_sigs :: [Var] -> SDoc
ppr_sigs ids
        -- Print type signatures; sort by OccName
  = vcat (map ppr_sig (sortBy (comparing getOccName) ids))
  where
    ppr_sig id = hang (ppr id <+> dcolon) 2 (ppr (tidyTopType (idType id)))

ppr_tydecls :: [TyCon] -> SDoc
ppr_tydecls tycons
        -- Print type constructor info; sort by OccName
  = vcat (map ppr_tycon (sortBy (comparing getOccName) tycons))
  where
    ppr_tycon tycon = vcat [ ppr (tyConName tycon) <+> dcolon <+> ppr (tyConKind tycon)
                              -- Temporarily print the kind signature too
                           , ppr (tyThingToIfaceDecl (ATyCon tycon)) ]

ppr_rules :: [CoreRule] -> SDoc
ppr_rules [] = empty
ppr_rules rs = vcat [ptext (sLit "{-# RULES"),
                      nest 2 (pprRules rs),
                      ptext (sLit "#-}")]
\end{code}
