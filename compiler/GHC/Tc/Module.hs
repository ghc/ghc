{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

-- | Typechecking a whole module
--
-- https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/type-checker
module GHC.Tc.Module (
        tcRnStmt, tcRnExpr, TcRnExprMode(..), tcRnType,
        tcRnImportDecls,
        tcRnLookupRdrName,
        getModuleInterface,
        tcRnDeclsi,
        isGHCiMonad,
        runTcInteractive,    -- Used by GHC API clients (#8878)
        tcRnLookupName,
        tcRnGetInfo,
        tcRnModule, tcRnModuleTcRnM,
        tcTopSrcDecls,
        rnTopSrcDecls,
        checkBootDecl, checkHiBootIface',
        findExtraSigImports,
        implicitRequirements,
        checkUnit,
        mergeSignatures,
        tcRnMergeSignatures,
        instantiateSignature,
        tcRnInstantiateSignature,
        loadUnqualIfaces,
        -- More private...
        badReexportedBootThing,
        checkBootDeclM,
        missingBootThing,
        getRenamedStuff, RenamedStuff
    ) where

import GHC.Prelude

import GHC.Driver.Env
import GHC.Driver.Plugins
import GHC.Driver.Session

import GHC.Tc.Errors.Hole.FitTypes ( HoleFitPluginR (..) )
import {-# SOURCE #-} GHC.Tc.Gen.Splice ( finishTH, runRemoteModFinalizers )
import GHC.Tc.Gen.HsType
import GHC.Tc.Validity( checkValidType )
import GHC.Tc.Gen.Match
import GHC.Tc.Utils.Unify( checkConstraints )
import GHC.Tc.Utils.Zonk
import GHC.Tc.Gen.Expr
import GHC.Tc.Errors( reportAllUnsolved )
import GHC.Tc.Gen.App( tcInferSigma )
import GHC.Tc.Utils.Monad
import GHC.Tc.Gen.Export
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Origin
import GHC.Tc.Instance.Family
import GHC.Tc.Gen.Annotation
import GHC.Tc.Gen.Bind
import GHC.Tc.Gen.Default
import GHC.Tc.Utils.Env
import GHC.Tc.Gen.Rule
import GHC.Tc.Gen.Foreign
import GHC.Tc.TyCl.Instance
import GHC.Tc.Utils.TcMType
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Instantiate (tcGetInsts)
import GHC.Tc.Solver
import GHC.Tc.TyCl
import GHC.Tc.Instance.Typeable ( mkTypeableBinds )
import GHC.Tc.Utils.Backpack

import GHC.Rename.Splice ( rnTopSpliceDecls, traceSplice, SpliceInfo(..) )
import GHC.Rename.HsType
import GHC.Rename.Expr
import GHC.Rename.Utils  ( HsDocContext(..) )
import GHC.Rename.Fixity ( lookupFixityRn )
import GHC.Rename.Names
import GHC.Rename.Env
import GHC.Rename.Module

import GHC.Iface.Syntax   ( ShowSub(..), showToHeader )
import GHC.Iface.Type     ( ShowForAllFlag(..) )
import GHC.Iface.Env     ( externaliseName )
import GHC.Iface.Make   ( coAxiomToIfaceDecl )
import GHC.Iface.Load

import GHC.Builtin.Types ( unitTy, mkListTy )
import GHC.Builtin.Names
import GHC.Builtin.Utils

import GHC.Hs
import GHC.Hs.Dump

import GHC.Core.PatSyn    ( pprPatSynType )
import GHC.Core.Predicate ( classMethodTy )
import GHC.Core.FVs         ( orphNamesOfFamInst )
import GHC.Core.InstEnv
import GHC.Core.TyCon
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.Type
import GHC.Core.Class
import GHC.Core.Coercion.Axiom
import GHC.Core.FamInstEnv
   ( FamInst, pprFamInst, famInstsRepTyCons
   , famInstEnvElts, extendFamInstEnvList, normaliseType )

import GHC.Parser.Header       ( mkPrelImports )

import GHC.IfaceToCore

import GHC.Runtime.Context

import GHC.Utils.Error
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc

import GHC.Types.Name.Reader
import GHC.Types.Fixity.Env
import GHC.Types.Id as Id
import GHC.Types.Id.Info( IdDetails(..) )
import GHC.Types.Var.Env
import GHC.Types.TypeEnv
import GHC.Types.Unique.FM
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.Avail
import GHC.Types.Basic hiding( SuccessFlag(..) )
import GHC.Types.Annotations
import GHC.Types.SrcLoc
import GHC.Types.SourceText
import GHC.Types.SourceFile
import GHC.Types.TyThing.Ppr ( pprTyThingInContext )
import qualified GHC.LanguageExtensions as LangExt

import GHC.Unit.External
import GHC.Unit.Types
import GHC.Unit.State
import GHC.Unit.Home
import GHC.Unit.Module
import GHC.Unit.Module.Warnings
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.Deps

import GHC.Data.FastString
import GHC.Data.Maybe
import GHC.Data.List.SetOps
import GHC.Data.Bag
import qualified GHC.Data.BooleanFormula as BF

import Data.List ( find, sortBy, sort )
import Data.Ord
import Data.Data ( Data )
import qualified Data.Set as S
import Control.DeepSeq
import Control.Monad

#include "HsVersions.h"

{-
************************************************************************
*                                                                      *
        Typecheck and rename a module
*                                                                      *
************************************************************************
-}

-- | Top level entry point for typechecker and renamer
tcRnModule :: HscEnv
           -> ModSummary
           -> Bool              -- True <=> save renamed syntax
           -> HsParsedModule
           -> IO (Messages, Maybe TcGblEnv)

tcRnModule hsc_env mod_sum save_rn_syntax
   parsedModule@HsParsedModule {hpm_module= L loc this_module}
 | RealSrcSpan real_loc _ <- loc
 = withTiming dflags
              (text "Renamer/typechecker"<+>brackets (ppr this_mod))
              (const ()) $
   initTc hsc_env hsc_src save_rn_syntax this_mod real_loc $
          withTcPlugins hsc_env $ withHoleFitPlugins hsc_env $

          tcRnModuleTcRnM hsc_env mod_sum parsedModule pair

  | otherwise
  = return ((emptyBag, unitBag err_msg), Nothing)

  where
    hsc_src = ms_hsc_src mod_sum
    dflags = hsc_dflags hsc_env
    home_unit = hsc_home_unit hsc_env
    err_msg = mkPlainErrMsg dflags loc $
              text "Module does not have a RealSrcSpan:" <+> ppr this_mod

    pair :: (Module, SrcSpan)
    pair@(this_mod,_)
      | Just (L mod_loc mod) <- hsmodName this_module
      = (mkHomeModule home_unit mod, mod_loc)

      | otherwise   -- 'module M where' is omitted
      = (mkHomeModule home_unit mAIN_NAME, srcLocSpan (srcSpanStart loc))




tcRnModuleTcRnM :: HscEnv
                -> ModSummary
                -> HsParsedModule
                -> (Module, SrcSpan)
                -> TcRn TcGblEnv
-- Factored out separately from tcRnModule so that a Core plugin can
-- call the type checker directly
tcRnModuleTcRnM hsc_env mod_sum
                (HsParsedModule {
                   hpm_module =
                      (L loc (HsModule _ maybe_mod export_ies
                                       import_decls local_decls mod_deprec
                                       maybe_doc_hdr)),
                   hpm_src_files = src_files
                })
                (this_mod, prel_imp_loc)
 = setSrcSpan loc $
   do { let { explicit_mod_hdr = isJust maybe_mod
            ; hsc_src          = ms_hsc_src mod_sum }
      ; -- Load the hi-boot interface for this module, if any
        -- We do this now so that the boot_names can be passed
        -- to tcTyAndClassDecls, because the boot_names are
        -- automatically considered to be loop breakers
        tcg_env <- getGblEnv
      ; boot_info <- tcHiBootIface hsc_src this_mod
      ; setGblEnv (tcg_env { tcg_self_boot = boot_info })
        $ do
        { -- Deal with imports; first add implicit prelude
          implicit_prelude <- xoptM LangExt.ImplicitPrelude
        ; let { prel_imports = mkPrelImports (moduleName this_mod) prel_imp_loc
                               implicit_prelude import_decls }

        ; whenWOptM Opt_WarnImplicitPrelude $
             when (notNull prel_imports) $
                addWarn (Reason Opt_WarnImplicitPrelude) (implicitPreludeWarn)

        ; -- TODO This is a little skeevy; maybe handle a bit more directly
          let { simplifyImport (L _ idecl) =
                  ( fmap sl_fs (ideclPkgQual idecl) , ideclName idecl)
              }
        ; raw_sig_imports <- liftIO
                             $ findExtraSigImports hsc_env hsc_src
                                 (moduleName this_mod)
        ; raw_req_imports <- liftIO
                             $ implicitRequirements hsc_env
                                (map simplifyImport (prel_imports
                                                     ++ import_decls))
        ; let { mkImport (Nothing, L _ mod_name) = noLoc
                $ (simpleImportDecl mod_name)
                  { ideclHiding = Just (False, noLoc [])}
              ; mkImport _ = panic "mkImport" }
        ; let { all_imports = prel_imports ++ import_decls
                       ++ map mkImport (raw_sig_imports ++ raw_req_imports) }
        ; -- OK now finally rename the imports
          tcg_env <- {-# SCC "tcRnImports" #-}
                     tcRnImports hsc_env all_imports

        ; -- If the whole module is warned about or deprecated
          -- (via mod_deprec) record that in tcg_warns. If we do thereby add
          -- a WarnAll, it will override any subsequent deprecations added to tcg_warns
          let { tcg_env1 = case mod_deprec of
                             Just (L _ txt) ->
                               tcg_env {tcg_warns = WarnAll txt}
                             Nothing            -> tcg_env
              }
        ; setGblEnv tcg_env1
          $ do { -- Rename and type check the declarations
                 traceRn "rn1a" empty
               ; tcg_env <- if isHsBootOrSig hsc_src
                            then tcRnHsBootDecls hsc_src local_decls
                            else {-# SCC "tcRnSrcDecls" #-}
                                 tcRnSrcDecls explicit_mod_hdr local_decls export_ies

               ; whenM (goptM Opt_DoCoreLinting) $
                 lintGblEnv (hsc_dflags hsc_env) tcg_env

               ; setGblEnv tcg_env
                 $ do { -- Process the export list
                        traceRn "rn4a: before exports" empty
                      ; tcg_env <- tcRnExports explicit_mod_hdr export_ies
                                     tcg_env
                      ; traceRn "rn4b: after exports" empty
                      ; -- Compare hi-boot iface (if any) with the real thing
                        -- Must be done after processing the exports
                        tcg_env <- checkHiBootIface tcg_env boot_info
                      ; -- The new type env is already available to stuff
                        -- slurped from interface files, via
                        -- GHC.Tc.Utils.Env.setGlobalTypeEnv. It's important that this
                        -- includes the stuff in checkHiBootIface,
                        -- because the latter might add new bindings for
                        -- boot_dfuns, which may be mentioned in imported
                        -- unfoldings.

                        -- Don't need to rename the Haddock documentation,
                        -- it's not parsed by GHC anymore.
                        tcg_env <- return (tcg_env
                                           { tcg_doc_hdr = maybe_doc_hdr })
                      ; -- Report unused names
                        -- Do this /after/ type inference, so that when reporting
                        -- a function with no type signature we can give the
                        -- inferred type
                        reportUnusedNames tcg_env hsc_src
                      ; -- add extra source files to tcg_dependent_files
                        addDependentFiles src_files
                      ; tcg_env <- runTypecheckerPlugin mod_sum tcg_env
                      ; -- Dump output and return
                        tcDump tcg_env
                      ; return tcg_env }
               }
        }
      }

implicitPreludeWarn :: SDoc
implicitPreludeWarn
  = text "Module `Prelude' implicitly imported"

{-
************************************************************************
*                                                                      *
                Import declarations
*                                                                      *
************************************************************************
-}

tcRnImports :: HscEnv -> [LImportDecl GhcPs] -> TcM TcGblEnv
tcRnImports hsc_env import_decls
  = do  { (rn_imports, rdr_env, imports, hpc_info) <- rnImports import_decls ;

        ; this_mod <- getModule
        ; let { dep_mods :: ModuleNameEnv ModuleNameWithIsBoot
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

        ; traceRn "rn1" (ppr (imp_dep_mods imports))
                -- Fail if there are any errors so far
                -- The error printing (if needed) takes advantage
                -- of the tcg_env we have now set
--      ; traceIf (text "rdr_env: " <+> ppr rdr_env)
        ; failIfErrsM

                -- Load any orphan-module (including orphan family
                -- instance-module) interfaces, so that their rules and
                -- instance decls will be found.  But filter out a
                -- self hs-boot: these instances will be checked when
                -- we define them locally.
                -- (We don't need to load non-orphan family instance
                -- modules until we either try to use the instances they
                -- define, or define our own family instances, at which
                -- point we need to check them for consistency.)
        ; loadModuleInterfaces (text "Loading orphan modules")
                               (filter (/= this_mod) (imp_orphs imports))

                -- Check type-family consistency between imports.
                -- See Note [The type family instance consistency story]
        ; traceRn "rn1: checking family instance consistency {" empty
        ; let { dir_imp_mods = moduleEnvKeys
                             . imp_mods
                             $ imports }
        ; checkFamInstConsistency dir_imp_mods
        ; traceRn "rn1: } checking family instance consistency" empty

        ; getGblEnv } }

{-
************************************************************************
*                                                                      *
        Type-checking the top level of a module
*                                                                      *
************************************************************************
-}

tcRnSrcDecls :: Bool  -- False => no 'module M(..) where' header at all
             -> [LHsDecl GhcPs]               -- Declarations
             -> Maybe (Located [LIE GhcPs])
             -> TcM TcGblEnv
tcRnSrcDecls explicit_mod_hdr decls export_ies
 = do { -- Do all the declarations
      ; (tcg_env, tcl_env, lie) <- tc_rn_src_decls decls

        -- Check for the 'main' declaration
        -- Must do this inside the captureTopConstraints
        -- NB: always set envs *before* captureTopConstraints
      ; (tcg_env, lie_main) <- setEnvs (tcg_env, tcl_env) $
                               captureTopConstraints $
                               checkMain explicit_mod_hdr export_ies

      ; setEnvs (tcg_env, tcl_env) $ do {

             --         Simplify constraints
             --
             -- We do this after checkMain, so that we use the type info
             -- that checkMain adds
             --
             -- We do it with both global and local env in scope:
             --  * the global env exposes the instances to simplifyTop
             --  * the local env exposes the local Ids to simplifyTop,
             --    so that we get better error messages (monomorphism restriction)
      ; new_ev_binds <- {-# SCC "simplifyTop" #-}
                        simplifyTop (lie `andWC` lie_main)

        -- Emit Typeable bindings
      ; tcg_env <- mkTypeableBinds


      ; traceTc "Tc9" empty

      ; failIfErrsM     -- Don't zonk if there have been errors
                        -- It's a waste of time; and we may get debug warnings
                        -- about strangely-typed TyCons!
      ; traceTc "Tc10" empty

        -- Zonk the final code.  This must be done last.
        -- Even simplifyTop may do some unification.
        -- This pass also warns about missing type signatures
      ; (bind_env, ev_binds', binds', fords', imp_specs', rules')
            <- zonkTcGblEnv new_ev_binds tcg_env

        -- Finalizers must run after constraints are simplified, or some types
        -- might not be complete when using reify (see #12777).
        -- and also after we zonk the first time because we run typed splices
        -- in the zonker which gives rise to the finalisers.
      ; (tcg_env_mf, _) <- setGblEnv (clearTcGblEnv tcg_env)
                                     run_th_modfinalizers
      ; finishTH
      ; traceTc "Tc11" empty

      ; -- zonk the new bindings arising from running the finalisers.
        -- This won't give rise to any more finalisers as you can't nest
        -- finalisers inside finalisers.
      ; (bind_env_mf, ev_binds_mf, binds_mf, fords_mf, imp_specs_mf, rules_mf)
            <- zonkTcGblEnv emptyBag tcg_env_mf


      ; let { final_type_env = plusTypeEnv (tcg_type_env tcg_env)
                                (plusTypeEnv bind_env_mf bind_env)
            ; tcg_env' = tcg_env_mf
                          { tcg_binds    = binds' `unionBags` binds_mf,
                            tcg_ev_binds = ev_binds' `unionBags` ev_binds_mf ,
                            tcg_imp_specs = imp_specs' ++ imp_specs_mf ,
                            tcg_rules    = rules' ++ rules_mf ,
                            tcg_fords    = fords' ++ fords_mf } } ;

      ; setGlobalTypeEnv tcg_env' final_type_env

   } }

zonkTcGblEnv :: Bag EvBind -> TcGblEnv
             -> TcM (TypeEnv, Bag EvBind, LHsBinds GhcTc,
                       [LForeignDecl GhcTc], [LTcSpecPrag], [LRuleDecl GhcTc])
zonkTcGblEnv new_ev_binds tcg_env =
  let TcGblEnv {   tcg_binds     = binds,
                   tcg_ev_binds  = cur_ev_binds,
                   tcg_imp_specs = imp_specs,
                   tcg_rules     = rules,
                   tcg_fords     = fords } = tcg_env

      all_ev_binds = cur_ev_binds `unionBags` new_ev_binds

  in {-# SCC "zonkTopDecls" #-}
      zonkTopDecls all_ev_binds binds rules imp_specs fords


-- | Remove accumulated bindings, rules and so on from TcGblEnv
clearTcGblEnv :: TcGblEnv -> TcGblEnv
clearTcGblEnv tcg_env
  = tcg_env { tcg_binds    = emptyBag,
              tcg_ev_binds = emptyBag ,
              tcg_imp_specs = [],
              tcg_rules    = [],
              tcg_fords    = [] }

-- | Runs TH finalizers and renames and typechecks the top-level declarations
-- that they could introduce.
run_th_modfinalizers :: TcM (TcGblEnv, TcLclEnv)
run_th_modfinalizers = do
  th_modfinalizers_var <- fmap tcg_th_modfinalizers getGblEnv
  th_modfinalizers <- readTcRef th_modfinalizers_var
  if null th_modfinalizers
  then getEnvs
  else do
    writeTcRef th_modfinalizers_var []
    let run_finalizer (lcl_env, f) =
            setLclEnv lcl_env (runRemoteModFinalizers f)

    (_, lie_th) <- captureTopConstraints $
                   mapM_ run_finalizer th_modfinalizers

      -- Finalizers can add top-level declarations with addTopDecls, so
      -- we have to run tc_rn_src_decls to get them
    (tcg_env, tcl_env, lie_top_decls) <- tc_rn_src_decls []

    setEnvs (tcg_env, tcl_env) $ do
      -- Subsequent rounds of finalizers run after any new constraints are
      -- simplified, or some types might not be complete when using reify
      -- (see #12777).
      new_ev_binds <- {-# SCC "simplifyTop2" #-}
                      simplifyTop (lie_th `andWC` lie_top_decls)
      addTopEvBinds new_ev_binds run_th_modfinalizers
        -- addTopDecls can add declarations which add new finalizers.

tc_rn_src_decls :: [LHsDecl GhcPs]
                -> TcM (TcGblEnv, TcLclEnv, WantedConstraints)
-- Loops around dealing with each top level inter-splice group
-- in turn, until it's dealt with the entire module
-- Never emits constraints; calls captureTopConstraints internally
tc_rn_src_decls ds
 = {-# SCC "tc_rn_src_decls" #-}
   do { (first_group, group_tail) <- findSplice ds
                -- If ds is [] we get ([], Nothing)

        -- Deal with decls up to, but not including, the first splice
      ; (tcg_env, rn_decls) <- rnTopSrcDecls first_group
                -- rnTopSrcDecls fails if there are any errors

        -- Get TH-generated top-level declarations and make sure they don't
        -- contain any splices since we don't handle that at the moment
        --
        -- The plumbing here is a bit odd: see #10853
      ; th_topdecls_var <- fmap tcg_th_topdecls getGblEnv
      ; th_ds <- readTcRef th_topdecls_var
      ; writeTcRef th_topdecls_var []

      ; (tcg_env, rn_decls) <-
            if null th_ds
            then return (tcg_env, rn_decls)
            else do { (th_group, th_group_tail) <- findSplice th_ds
                    ; case th_group_tail of
                        { Nothing -> return ()
                        ; Just (SpliceDecl _ (L loc _) _, _) ->
                            setSrcSpan loc
                            $ addErr (text
                                ("Declaration splices are not "
                                  ++ "permitted inside top-level "
                                  ++ "declarations added with addTopDecls"))
                        }
                      -- Rename TH-generated top-level declarations
                    ; (tcg_env, th_rn_decls) <- setGblEnv tcg_env
                        $ rnTopSrcDecls th_group

                      -- Dump generated top-level declarations
                    ; let msg = "top-level declarations added with addTopDecls"
                    ; traceSplice
                        $ SpliceInfo { spliceDescription = msg
                                     , spliceIsDecl    = True
                                     , spliceSource    = Nothing
                                     , spliceGenerated = ppr th_rn_decls }
                    ; return (tcg_env, appendGroups rn_decls th_rn_decls)
                    }

      -- Type check all declarations
      -- NB: set the env **before** captureTopConstraints so that error messages
      -- get reported w.r.t. the right GlobalRdrEnv. It is for this reason that
      -- the captureTopConstraints must go here, not in tcRnSrcDecls.
      ; ((tcg_env, tcl_env), lie1) <- setGblEnv tcg_env $
                                      captureTopConstraints $
                                      tcTopSrcDecls rn_decls

        -- If there is no splice, we're nearly done
      ; setEnvs (tcg_env, tcl_env) $
        case group_tail of
          { Nothing -> return (tcg_env, tcl_env, lie1)

            -- If there's a splice, we must carry on
          ; Just (SpliceDecl _ (L _ splice) _, rest_ds) ->
            do {
                 -- We need to simplify any constraints from the previous declaration
                 -- group, or else we might reify metavariables, as in #16980.
               ; ev_binds1 <- simplifyTop lie1

                 -- Rename the splice expression, and get its supporting decls
               ; (spliced_decls, splice_fvs) <- rnTopSpliceDecls splice

                 -- Glue them on the front of the remaining decls and loop
               ; (tcg_env, tcl_env, lie2) <-
                   setGblEnv (tcg_env `addTcgDUs` usesOnly splice_fvs) $
                   addTopEvBinds ev_binds1 $
                   tc_rn_src_decls (spliced_decls ++ rest_ds)

               ; return (tcg_env, tcl_env, lie2)
               }
          }
      }

{-
************************************************************************
*                                                                      *
        Compiling hs-boot source files, and
        comparing the hi-boot interface with the real thing
*                                                                      *
************************************************************************
-}

tcRnHsBootDecls :: HscSource -> [LHsDecl GhcPs] -> TcM TcGblEnv
tcRnHsBootDecls hsc_src decls
   = do { (first_group, group_tail) <- findSplice decls

                -- Rename the declarations
        ; (tcg_env, HsGroup { hs_tyclds = tycl_decls
                            , hs_derivds = deriv_decls
                            , hs_fords  = for_decls
                            , hs_defds  = def_decls
                            , hs_ruleds = rule_decls
                            , hs_annds  = _
                            , hs_valds  = XValBindsLR (NValBinds val_binds val_sigs) })
              <- rnTopSrcDecls first_group

        -- The empty list is for extra dependencies coming from .hs-boot files
        -- See Note [Extra dependencies from .hs-boot files] in GHC.Rename.Module

        ; (gbl_env, lie) <- setGblEnv tcg_env $ captureTopConstraints $ do {
              -- NB: setGblEnv **before** captureTopConstraints so that
              -- if the latter reports errors, it knows what's in scope

                -- Check for illegal declarations
        ; case group_tail of
             Just (SpliceDecl _ d _, _) -> badBootDecl hsc_src "splice" d
             Nothing                    -> return ()
        ; mapM_ (badBootDecl hsc_src "foreign") for_decls
        ; mapM_ (badBootDecl hsc_src "default") def_decls
        ; mapM_ (badBootDecl hsc_src "rule")    rule_decls

                -- Typecheck type/class/instance decls
        ; traceTc "Tc2 (boot)" empty
        ; (tcg_env, inst_infos, _deriv_binds)
             <- tcTyClsInstDecls tycl_decls deriv_decls val_binds
        ; setGblEnv tcg_env     $ do {

        -- Emit Typeable bindings
        ; tcg_env <- mkTypeableBinds
        ; setGblEnv tcg_env $ do {

                -- Typecheck value declarations
        ; traceTc "Tc5" empty
        ; val_ids <- tcHsBootSigs val_binds val_sigs

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
   }}}
   ; traceTc "boot" (ppr lie); return gbl_env }

badBootDecl :: HscSource -> String -> Located decl -> TcM ()
badBootDecl hsc_src what (L loc _)
  = addErrAt loc (char 'A' <+> text what
      <+> text "declaration is not (currently) allowed in a"
      <+> (case hsc_src of
            HsBootFile -> text "hs-boot"
            HsigFile -> text "hsig"
            _ -> panic "badBootDecl: should be an hsig or hs-boot file")
      <+> text "file")

{-
Once we've typechecked the body of the module, we want to compare what
we've found (gathered in a TypeEnv) with the hi-boot details (if any).
-}

checkHiBootIface :: TcGblEnv -> SelfBootInfo -> TcM TcGblEnv
-- Compare the hi-boot file for this module (if there is one)
-- with the type environment we've just come up with
-- In the common case where there is no hi-boot file, the list
-- of boot_names is empty.

checkHiBootIface tcg_env boot_info
  | NoSelfBoot <- boot_info  -- Common case
  = return tcg_env

  | HsBootFile <- tcg_src tcg_env   -- Current module is already a hs-boot file!
  = return tcg_env

  | SelfBoot { sb_mds = boot_details } <- boot_info
  , TcGblEnv { tcg_binds    = binds
             , tcg_insts    = local_insts
             , tcg_type_env = local_type_env
             , tcg_exports  = local_exports } <- tcg_env
  = do  { -- This code is tricky, see Note [DFun knot-tying]
        ; dfun_prs <- checkHiBootIface' local_insts local_type_env
                                        local_exports boot_details

        -- Now add the boot-dfun bindings  $fxblah = $fblah
        -- to (a) the type envt, and (b) the top-level bindings
        ; let boot_dfuns = map fst dfun_prs
              type_env'  = extendTypeEnvWithIds local_type_env boot_dfuns
              dfun_binds = listToBag [ mkVarBind boot_dfun (nlHsVar dfun)
                                     | (boot_dfun, dfun) <- dfun_prs ]
              tcg_env_w_binds
                = tcg_env { tcg_binds = binds `unionBags` dfun_binds }

        ; type_env' `seq`
             -- Why the seq?  Without, we will put a TypeEnv thunk in
             -- tcg_type_env_var.  That thunk will eventually get
             -- forced if we are typechecking interfaces, but that
             -- is no good if we are trying to typecheck the very
             -- DFun we were going to put in.
             -- TODO: Maybe setGlobalTypeEnv should be strict.
          setGlobalTypeEnv tcg_env_w_binds type_env' }

#if __GLASGOW_HASKELL__ <= 810
  | otherwise = panic "checkHiBootIface: unreachable code"
#endif

{- Note [DFun impedance matching]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We return a list of "impedance-matching" bindings for the dfuns
defined in the hs-boot file, such as
          $fxEqT = $fEqT
We need these because the module and hi-boot file might differ in
the name it chose for the dfun: the name of a dfun is not
uniquely determined by its type; there might be multiple dfuns
which, individually, would map to the same name (in which case
we have to disambiguate them.)  There's no way for the hi file
to know exactly what disambiguation to use... without looking
at the hi-boot file itself.

In fact, the names will always differ because we always pick names
prefixed with "$fx" for boot dfuns, and "$f" for real dfuns
(so that this impedance matching is always possible).

Note [DFun knot-tying]
~~~~~~~~~~~~~~~~~~~~~~
The 'SelfBootInfo' that is fed into 'checkHiBootIface' comes from
typechecking the hi-boot file that we are presently implementing.
Suppose we are typechecking the module A: when we typecheck the
hi-boot file, whenever we see an identifier A.T, we knot-tie this
identifier to the *local* type environment (via if_rec_types.)  The
contract then is that we don't *look* at 'SelfBootInfo' until we've
finished typechecking the module and updated the type environment with
the new tycons and ids.

This most works well, but there is one problem: DFuns!  We do not want
to look at the mb_insts of the ModDetails in SelfBootInfo, because a
dfun in one of those ClsInsts is gotten (in GHC.IfaceToCore.tcIfaceInst) by a
(lazily evaluated) lookup in the if_rec_types.  We could extend the
type env, do a setGloblaTypeEnv etc; but that all seems very indirect.
It is much more directly simply to extract the DFunIds from the
md_types of the SelfBootInfo.

See #4003, #16038 for why we need to take care here.
-}

checkHiBootIface' :: [ClsInst] -> TypeEnv -> [AvailInfo]
                  -> ModDetails -> TcM [(Id, Id)]
-- Variant which doesn't require a full TcGblEnv; you could get the
-- local components from another ModDetails.
checkHiBootIface'
        local_insts local_type_env local_exports
        (ModDetails { md_types = boot_type_env
                    , md_fam_insts = boot_fam_insts
                    , md_exports = boot_exports })
  = do  { traceTc "checkHiBootIface" $ vcat
             [ ppr boot_type_env, ppr boot_exports]

                -- Check the exports of the boot module, one by one
        ; mapM_ check_export boot_exports

                -- Check for no family instances
        ; unless (null boot_fam_insts) $
            panic ("GHC.Tc.Module.checkHiBootIface: Cannot handle family " ++
                   "instances in boot files yet...")
            -- FIXME: Why?  The actual comparison is not hard, but what would
            --        be the equivalent to the dfun bindings returned for class
            --        instances?  We can't easily equate tycons...

                -- Check instance declarations
                -- and generate an impedance-matching binding
        ; mb_dfun_prs <- mapM check_cls_inst boot_dfuns

        ; failIfErrsM

        ; return (catMaybes mb_dfun_prs) }

  where
    boot_dfun_names = map idName boot_dfuns
    boot_dfuns      = filter isDFunId $ typeEnvIds boot_type_env
       -- NB: boot_dfuns is /not/ defined thus: map instanceDFunId md_insts
       --     We don't want to look at md_insts!
       --     Why not?  See Note [DFun knot-tying]

    check_export boot_avail     -- boot_avail is exported by the boot iface
      | name `elem` boot_dfun_names = return ()
      | isWiredInName name          = return () -- No checking for wired-in names.  In particular,
                                                -- 'error' is handled by a rather gross hack
                                                -- (see comments in GHC.Err.hs-boot)

        -- Check that the actual module exports the same thing
      | not (null missing_names)
      = addErrAt (nameSrcSpan (head missing_names))
                 (missingBootThing True (head missing_names) "exported by")

        -- If the boot module does not *define* the thing, we are done
        -- (it simply re-exports it, and names match, so nothing further to do)
      | isNothing mb_boot_thing = return ()

        -- Check that the actual module also defines the thing, and
        -- then compare the definitions
      | Just real_thing <- lookupTypeEnv local_type_env name,
        Just boot_thing <- mb_boot_thing
      = checkBootDeclM True boot_thing real_thing

      | otherwise
      = addErrTc (missingBootThing True name "defined in")
      where
        name          = availName boot_avail
        mb_boot_thing = lookupTypeEnv boot_type_env name
        missing_names = case lookupNameEnv local_export_env name of
                          Nothing    -> [name]
                          Just avail -> availNames boot_avail `minusList` availNames avail

    local_export_env :: NameEnv AvailInfo
    local_export_env = availsToNameEnv local_exports

    check_cls_inst :: DFunId -> TcM (Maybe (Id, Id))
        -- Returns a pair of the boot dfun in terms of the equivalent
        -- real dfun. Delicate (like checkBootDecl) because it depends
        -- on the types lining up precisely even to the ordering of
        -- the type variables in the foralls.
    check_cls_inst boot_dfun
      | (real_dfun : _) <- find_real_dfun boot_dfun
      , let local_boot_dfun = Id.mkExportedVanillaId
                                  (idName boot_dfun) (idType real_dfun)
      = return (Just (local_boot_dfun, real_dfun))
          -- Two tricky points here:
          --
          --  * The local_boot_fun should have a Name from the /boot-file/,
          --    but type from the dfun defined in /this module/.
          --    That ensures that the TyCon etc inside the type are
          --    the ones defined in this module, not the ones gotten
          --    from the hi-boot file, which may have a lot less info
          --    (#8743, comment:10).
          --
          --  * The DFunIds from boot_details are /GlobalIds/, because
          --    they come from typechecking M.hi-boot.
          --    But all bindings in this module should be for /LocalIds/,
          --    otherwise dependency analysis fails (#16038). This
          --    is another reason for using mkExportedVanillaId, rather
          --    that modifying boot_dfun, to make local_boot_fun.

      | otherwise
      = setSrcSpan (nameSrcSpan (getName boot_dfun)) $
        do { traceTc "check_cls_inst" $ vcat
                [ text "local_insts"  <+>
                     vcat (map (ppr . idType . instanceDFunId) local_insts)
                , text "boot_dfun_ty" <+> ppr (idType boot_dfun) ]

           ; addErrTc (instMisMatch boot_dfun)
           ; return Nothing }

    find_real_dfun :: DFunId -> [DFunId]
    find_real_dfun boot_dfun
       = [dfun | inst <- local_insts
               , let dfun = instanceDFunId inst
               , idType dfun `eqType` boot_dfun_ty ]
       where
          boot_dfun_ty   = idType boot_dfun


-- In general, to perform these checks we have to
-- compare the TyThing from the .hi-boot file to the TyThing
-- in the current source file.  We must be careful to allow alpha-renaming
-- where appropriate, and also the boot declaration is allowed to omit
-- constructors and class methods.
--
-- See rnfail055 for a good test of this stuff.

-- | Compares two things for equivalence between boot-file and normal code,
-- reporting an error if they don't match up.
checkBootDeclM :: Bool  -- ^ True <=> an hs-boot file (could also be a sig)
               -> TyThing -> TyThing -> TcM ()
checkBootDeclM is_boot boot_thing real_thing
  = whenIsJust (checkBootDecl is_boot boot_thing real_thing) $ \ err ->
       addErrAt span
                (bootMisMatch is_boot err real_thing boot_thing)
  where
    -- Here we use the span of the boot thing or, if it doesn't have a sensible
    -- span, that of the real thing,
    span
      | let span = nameSrcSpan (getName boot_thing)
      , isGoodSrcSpan span
      = span
      | otherwise
      = nameSrcSpan (getName real_thing)

-- | Compares the two things for equivalence between boot-file and normal
-- code. Returns @Nothing@ on success or @Just "some helpful info for user"@
-- failure. If the difference will be apparent to the user, @Just empty@ is
-- perfectly suitable.
checkBootDecl :: Bool -> TyThing -> TyThing -> Maybe SDoc

checkBootDecl _ (AnId id1) (AnId id2)
  = ASSERT(id1 == id2)
    check (idType id1 `eqType` idType id2)
          (text "The two types are different")

checkBootDecl is_boot (ATyCon tc1) (ATyCon tc2)
  = checkBootTyCon is_boot tc1 tc2

checkBootDecl _ (AConLike (RealDataCon dc1)) (AConLike (RealDataCon _))
  = pprPanic "checkBootDecl" (ppr dc1)

checkBootDecl _ _ _ = Just empty -- probably shouldn't happen

-- | Combines two potential error messages
andThenCheck :: Maybe SDoc -> Maybe SDoc -> Maybe SDoc
Nothing `andThenCheck` msg     = msg
msg     `andThenCheck` Nothing = msg
Just d1 `andThenCheck` Just d2 = Just (d1 $$ d2)
infixr 0 `andThenCheck`

-- | If the test in the first parameter is True, succeed with @Nothing@;
-- otherwise, return the provided check
checkUnless :: Bool -> Maybe SDoc -> Maybe SDoc
checkUnless True  _ = Nothing
checkUnless False k = k

-- | Run the check provided for every pair of elements in the lists.
-- The provided SDoc should name the element type, in the plural.
checkListBy :: (a -> a -> Maybe SDoc) -> [a] -> [a] -> SDoc
            -> Maybe SDoc
checkListBy check_fun as bs whats = go [] as bs
  where
    herald = text "The" <+> whats <+> text "do not match"

    go []   [] [] = Nothing
    go docs [] [] = Just (hang (herald <> colon) 2 (vcat $ reverse docs))
    go docs (x:xs) (y:ys) = case check_fun x y of
      Just doc -> go (doc:docs) xs ys
      Nothing  -> go docs       xs ys
    go _    _  _ = Just (hang (herald <> colon)
                            2 (text "There are different numbers of" <+> whats))

-- | If the test in the first parameter is True, succeed with @Nothing@;
-- otherwise, fail with the given SDoc.
check :: Bool -> SDoc -> Maybe SDoc
check True  _   = Nothing
check False doc = Just doc

-- | A more perspicuous name for @Nothing@, for @checkBootDecl@ and friends.
checkSuccess :: Maybe SDoc
checkSuccess = Nothing

----------------
checkBootTyCon :: Bool -> TyCon -> TyCon -> Maybe SDoc
checkBootTyCon is_boot tc1 tc2
  | not (eqType (tyConKind tc1) (tyConKind tc2))
  = Just $ text "The types have different kinds"    -- First off, check the kind

  | Just c1 <- tyConClass_maybe tc1
  , Just c2 <- tyConClass_maybe tc2
  , let (clas_tvs1, clas_fds1, sc_theta1, _, ats1, op_stuff1)
          = classExtraBigSig c1
        (clas_tvs2, clas_fds2, sc_theta2, _, ats2, op_stuff2)
          = classExtraBigSig c2
  , Just env <- eqVarBndrs emptyRnEnv2 clas_tvs1 clas_tvs2
  = let
       eqSig (id1, def_meth1) (id2, def_meth2)
         = check (name1 == name2)
                 (text "The names" <+> pname1 <+> text "and" <+> pname2 <+>
                  text "are different") `andThenCheck`
           check (eqTypeX env op_ty1 op_ty2)
                 (text "The types of" <+> pname1 <+>
                  text "are different") `andThenCheck`
           if is_boot
               then check (eqMaybeBy eqDM def_meth1 def_meth2)
                          (text "The default methods associated with" <+> pname1 <+>
                           text "are different")
               else check (subDM op_ty1 def_meth1 def_meth2)
                          (text "The default methods associated with" <+> pname1 <+>
                           text "are not compatible")
         where
          name1 = idName id1
          name2 = idName id2
          pname1 = quotes (ppr name1)
          pname2 = quotes (ppr name2)
          op_ty1 = classMethodTy id1
          op_ty2 = classMethodTy id2

       eqAT (ATI tc1 def_ats1) (ATI tc2 def_ats2)
         = checkBootTyCon is_boot tc1 tc2 `andThenCheck`
           check (eqATDef def_ats1 def_ats2)
                 (text "The associated type defaults differ")

       eqDM (_, VanillaDM)    (_, VanillaDM)    = True
       eqDM (_, GenericDM t1) (_, GenericDM t2) = eqTypeX env t1 t2
       eqDM _ _ = False

       -- NB: first argument is from hsig, second is from real impl.
       -- Order of pattern matching matters.
       subDM _ Nothing _ = True
       subDM _ _ Nothing = False
       -- If the hsig wrote:
       --
       --   f :: a -> a
       --   default f :: a -> a
       --
       -- this should be validly implementable using an old-fashioned
       -- vanilla default method.
       subDM t1 (Just (_, GenericDM t2)) (Just (_, VanillaDM))
        = eqTypeX env t1 t2
       -- This case can occur when merging signatures
       subDM t1 (Just (_, VanillaDM)) (Just (_, GenericDM t2))
        = eqTypeX env t1 t2
       subDM _ (Just (_, VanillaDM)) (Just (_, VanillaDM)) = True
       subDM _ (Just (_, GenericDM t1)) (Just (_, GenericDM t2))
        = eqTypeX env t1 t2

       -- Ignore the location of the defaults
       eqATDef Nothing             Nothing             = True
       eqATDef (Just (ty1, _loc1)) (Just (ty2, _loc2)) = eqTypeX env ty1 ty2
       eqATDef _ _ = False

       eqFD (as1,bs1) (as2,bs2) =
         eqListBy (eqTypeX env) (mkTyVarTys as1) (mkTyVarTys as2) &&
         eqListBy (eqTypeX env) (mkTyVarTys bs1) (mkTyVarTys bs2)
    in
    checkRoles roles1 roles2 `andThenCheck`
          -- Checks kind of class
    check (eqListBy eqFD clas_fds1 clas_fds2)
          (text "The functional dependencies do not match") `andThenCheck`
    checkUnless (isAbstractTyCon tc1) $
    check (eqListBy (eqTypeX env) sc_theta1 sc_theta2)
          (text "The class constraints do not match") `andThenCheck`
    checkListBy eqSig op_stuff1 op_stuff2 (text "methods") `andThenCheck`
    checkListBy eqAT ats1 ats2 (text "associated types") `andThenCheck`
    check (classMinimalDef c1 `BF.implies` classMinimalDef c2)
        (text "The MINIMAL pragmas are not compatible")

  | Just syn_rhs1 <- synTyConRhs_maybe tc1
  , Just syn_rhs2 <- synTyConRhs_maybe tc2
  , Just env <- eqVarBndrs emptyRnEnv2 (tyConTyVars tc1) (tyConTyVars tc2)
  = ASSERT(tc1 == tc2)
    checkRoles roles1 roles2 `andThenCheck`
    check (eqTypeX env syn_rhs1 syn_rhs2) empty   -- nothing interesting to say
  -- This allows abstract 'data T a' to be implemented using 'type T = ...'
  -- and abstract 'class K a' to be implement using 'type K = ...'
  -- See Note [Synonyms implement abstract data]
  | not is_boot -- don't support for hs-boot yet
  , isAbstractTyCon tc1
  , Just (tvs, ty) <- synTyConDefn_maybe tc2
  , Just (tc2', args) <- tcSplitTyConApp_maybe ty
  = checkSynAbsData tvs ty tc2' args
    -- TODO: When it's a synonym implementing a class, we really
    -- should check if the fundeps are satisfied, but
    -- there is not an obvious way to do this for a constraint synonym.
    -- So for now, let it all through (it won't cause segfaults, anyway).
    -- Tracked at #12704.

  -- This allows abstract 'data T :: Nat' to be implemented using
  -- 'type T = 42' Since the kinds already match (we have checked this
  -- upfront) all we need to check is that the implementation 'type T
  -- = ...' defined an actual literal.  See #15138 for the case this
  -- handles.
  | not is_boot
  , isAbstractTyCon tc1
  , Just (_,ty2) <- synTyConDefn_maybe tc2
  , isJust (isLitTy ty2)
  = Nothing

  | Just fam_flav1 <- famTyConFlav_maybe tc1
  , Just fam_flav2 <- famTyConFlav_maybe tc2
  = ASSERT(tc1 == tc2)
    let eqFamFlav OpenSynFamilyTyCon   OpenSynFamilyTyCon = True
        eqFamFlav (DataFamilyTyCon {}) (DataFamilyTyCon {}) = True
        -- This case only happens for hsig merging:
        eqFamFlav AbstractClosedSynFamilyTyCon AbstractClosedSynFamilyTyCon = True
        eqFamFlav AbstractClosedSynFamilyTyCon (ClosedSynFamilyTyCon {}) = True
        eqFamFlav (ClosedSynFamilyTyCon {}) AbstractClosedSynFamilyTyCon = True
        eqFamFlav (ClosedSynFamilyTyCon ax1) (ClosedSynFamilyTyCon ax2)
            = eqClosedFamilyAx ax1 ax2
        eqFamFlav (BuiltInSynFamTyCon {}) (BuiltInSynFamTyCon {}) = tc1 == tc2
        eqFamFlav _ _ = False
        injInfo1 = tyConInjectivityInfo tc1
        injInfo2 = tyConInjectivityInfo tc2
    in
    -- check equality of roles, family flavours and injectivity annotations
    -- (NB: Type family roles are always nominal. But the check is
    -- harmless enough.)
    checkRoles roles1 roles2 `andThenCheck`
    check (eqFamFlav fam_flav1 fam_flav2)
        (whenPprDebug $
            text "Family flavours" <+> ppr fam_flav1 <+> text "and" <+> ppr fam_flav2 <+>
            text "do not match") `andThenCheck`
    check (injInfo1 == injInfo2) (text "Injectivities do not match")

  | isAlgTyCon tc1 && isAlgTyCon tc2
  , Just env <- eqVarBndrs emptyRnEnv2 (tyConTyVars tc1) (tyConTyVars tc2)
  = ASSERT(tc1 == tc2)
    checkRoles roles1 roles2 `andThenCheck`
    check (eqListBy (eqTypeX env)
                     (tyConStupidTheta tc1) (tyConStupidTheta tc2))
          (text "The datatype contexts do not match") `andThenCheck`
    eqAlgRhs tc1 (algTyConRhs tc1) (algTyConRhs tc2)

  | otherwise = Just empty   -- two very different types -- should be obvious
  where
    roles1 = tyConRoles tc1 -- the abstract one
    roles2 = tyConRoles tc2
    roles_msg = text "The roles do not match." $$
                (text "Roles on abstract types default to" <+>
                 quotes (text "representational") <+> text "in boot files.")

    roles_subtype_msg = text "The roles are not compatible:" $$
                        text "Main module:" <+> ppr roles2 $$
                        text "Hsig file:" <+> ppr roles1

    checkRoles r1 r2
      | is_boot || isInjectiveTyCon tc1 Representational -- See Note [Role subtyping]
      = check (r1 == r2) roles_msg
      | otherwise = check (r2 `rolesSubtypeOf` r1) roles_subtype_msg

    -- Note [Role subtyping]
    -- ~~~~~~~~~~~~~~~~~~~~~
    -- In the current formulation of roles, role subtyping is only OK if the
    -- "abstract" TyCon was not representationally injective.  Among the most
    -- notable examples of non representationally injective TyCons are abstract
    -- data, which can be implemented via newtypes (which are not
    -- representationally injective).  The key example is
    -- in this example from #13140:
    --
    --      -- In an hsig file
    --      data T a -- abstract!
    --      type role T nominal
    --
    --      -- Elsewhere
    --      foo :: Coercible (T a) (T b) => a -> b
    --      foo x = x
    --
    -- We must NOT allow foo to typecheck, because if we instantiate
    -- T with a concrete data type with a phantom role would cause
    -- Coercible (T a) (T b) to be provable.  Fortunately, if T is not
    -- representationally injective, we cannot make the inference that a ~N b if
    -- T a ~R T b.
    --
    -- Unconditional role subtyping would be possible if we setup
    -- an extra set of roles saying when we can project out coercions
    -- (we call these proj-roles); then it would NOT be valid to instantiate T
    -- with a data type at phantom since the proj-role subtyping check
    -- would fail.  See #13140 for more details.
    --
    -- One consequence of this is we get no role subtyping for non-abstract
    -- data types in signatures. Suppose you have:
    --
    --      signature A where
    --          type role T nominal
    --          data T a = MkT
    --
    -- If you write this, we'll treat T as injective, and make inferences
    -- like T a ~R T b ==> a ~N b (mkNthCo).  But if we can
    -- subsequently replace T with one at phantom role, we would then be able to
    -- infer things like T Int ~R T Bool which is bad news.
    --
    -- We could allow role subtyping here if we didn't treat *any* data types
    -- defined in signatures as injective.  But this would be a bit surprising,
    -- replacing a data type in a module with one in a signature could cause
    -- your code to stop typechecking (whereas if you made the type abstract,
    -- it is more understandable that the type checker knows less).
    --
    -- It would have been best if this was purely a question of defaults
    -- (i.e., a user could explicitly ask for one behavior or another) but
    -- the current role system isn't expressive enough to do this.
    -- Having explicit proj-roles would solve this problem.

    rolesSubtypeOf [] [] = True
    -- NB: this relation is the OPPOSITE of the subroling relation
    rolesSubtypeOf (x:xs) (y:ys) = x >= y && rolesSubtypeOf xs ys
    rolesSubtypeOf _ _ = False

    -- Note [Synonyms implement abstract data]
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- An abstract data type or class can be implemented using a type synonym,
    -- but ONLY if the type synonym is nullary and has no type family
    -- applications.  This arises from two properties of skolem abstract data:
    --
    --    For any T (with some number of paramaters),
    --
    --    1. T is a valid type (it is "curryable"), and
    --
    --    2. T is valid in an instance head (no type families).
    --
    -- See also 'HowAbstract' and Note [Skolem abstract data].

    -- | Given @type T tvs = ty@, where @ty@ decomposes into @tc2' args@,
    -- check that this synonym is an acceptable implementation of @tc1@.
    -- See Note [Synonyms implement abstract data]
    checkSynAbsData :: [TyVar] -> Type -> TyCon -> [Type] -> Maybe SDoc
    checkSynAbsData tvs ty tc2' args =
        check (null (tcTyFamInsts ty))
              (text "Illegal type family application in implementation of abstract data.")
                `andThenCheck`
        check (null tvs)
              (text "Illegal parameterized type synonym in implementation of abstract data." $$
               text "(Try eta reducing your type synonym so that it is nullary.)")
                `andThenCheck`
        -- Don't report roles errors unless the type synonym is nullary
        checkUnless (not (null tvs)) $
            ASSERT( null roles2 )
            -- If we have something like:
            --
            --  signature H where
            --      data T a
            --  module H where
            --      data K a b = ...
            --      type T = K Int
            --
            -- we need to drop the first role of K when comparing!
            checkRoles roles1 (drop (length args) (tyConRoles tc2'))
{-
        -- Hypothetically, if we were allow to non-nullary type synonyms, here
        -- is how you would check the roles
        if length tvs == length roles1
            then checkRoles roles1 roles2
            else case tcSplitTyConApp_maybe ty of
                    Just (tc2', args) ->
                        checkRoles roles1 (drop (length args) (tyConRoles tc2') ++ roles2)
                    Nothing -> Just roles_msg
-}

    eqAlgRhs _ AbstractTyCon _rhs2
      = checkSuccess -- rhs2 is guaranteed to be injective, since it's an AlgTyCon
    eqAlgRhs _  tc1@DataTyCon{} tc2@DataTyCon{} =
        checkListBy eqCon (data_cons tc1) (data_cons tc2) (text "constructors")
    eqAlgRhs _  tc1@NewTyCon{} tc2@NewTyCon{} =
        eqCon (data_con tc1) (data_con tc2)
    eqAlgRhs _ _ _ = Just (text "Cannot match a" <+> quotes (text "data") <+>
                           text "definition with a" <+> quotes (text "newtype") <+>
                           text "definition")

    eqCon c1 c2
      =  check (name1 == name2)
               (text "The names" <+> pname1 <+> text "and" <+> pname2 <+>
                text "differ") `andThenCheck`
         check (dataConIsInfix c1 == dataConIsInfix c2)
               (text "The fixities of" <+> pname1 <+>
                text "differ") `andThenCheck`
         check (eqListBy eqHsBang (dataConImplBangs c1) (dataConImplBangs c2))
               (text "The strictness annotations for" <+> pname1 <+>
                text "differ") `andThenCheck`
         check (map flSelector (dataConFieldLabels c1) == map flSelector (dataConFieldLabels c2))
               (text "The record label lists for" <+> pname1 <+>
                text "differ") `andThenCheck`
         check (eqType (dataConWrapperType c1) (dataConWrapperType c2))
               (text "The types for" <+> pname1 <+> text "differ")
      where
        name1 = dataConName c1
        name2 = dataConName c2
        pname1 = quotes (ppr name1)
        pname2 = quotes (ppr name2)

    eqClosedFamilyAx Nothing Nothing  = True
    eqClosedFamilyAx Nothing (Just _) = False
    eqClosedFamilyAx (Just _) Nothing = False
    eqClosedFamilyAx (Just (CoAxiom { co_ax_branches = branches1 }))
                     (Just (CoAxiom { co_ax_branches = branches2 }))
      =  numBranches branches1 == numBranches branches2
      && (and $ zipWith eqClosedFamilyBranch branch_list1 branch_list2)
      where
        branch_list1 = fromBranches branches1
        branch_list2 = fromBranches branches2

    eqClosedFamilyBranch (CoAxBranch { cab_tvs = tvs1, cab_cvs = cvs1
                                     , cab_lhs = lhs1, cab_rhs = rhs1 })
                         (CoAxBranch { cab_tvs = tvs2, cab_cvs = cvs2
                                     , cab_lhs = lhs2, cab_rhs = rhs2 })
      | Just env1 <- eqVarBndrs emptyRnEnv2 tvs1 tvs2
      , Just env  <- eqVarBndrs env1        cvs1 cvs2
      = eqListBy (eqTypeX env) lhs1 lhs2 &&
        eqTypeX env rhs1 rhs2

      | otherwise = False

emptyRnEnv2 :: RnEnv2
emptyRnEnv2 = mkRnEnv2 emptyInScopeSet

----------------
missingBootThing :: Bool -> Name -> String -> SDoc
missingBootThing is_boot name what
  = quotes (ppr name) <+> text "is exported by the"
    <+> (if is_boot then text "hs-boot" else text "hsig")
    <+> text "file, but not"
    <+> text what <+> text "the module"

badReexportedBootThing :: Bool -> Name -> Name -> SDoc
badReexportedBootThing is_boot name name'
  = withUserStyle alwaysQualify AllTheWay $ vcat
        [ text "The" <+> (if is_boot then text "hs-boot" else text "hsig")
           <+> text "file (re)exports" <+> quotes (ppr name)
        , text "but the implementing module exports a different identifier" <+> quotes (ppr name')
        ]

bootMisMatch :: Bool -> SDoc -> TyThing -> TyThing -> SDoc
bootMisMatch is_boot extra_info real_thing boot_thing
  = pprBootMisMatch is_boot extra_info real_thing real_doc boot_doc
  where
    to_doc
      = pprTyThingInContext $ showToHeader { ss_forall =
                                              if is_boot
                                                then ShowForAllMust
                                                else ShowForAllWhen }

    real_doc = to_doc real_thing
    boot_doc = to_doc boot_thing

    pprBootMisMatch :: Bool -> SDoc -> TyThing -> SDoc -> SDoc -> SDoc
    pprBootMisMatch is_boot extra_info real_thing real_doc boot_doc
      = vcat
          [ ppr real_thing <+>
            text "has conflicting definitions in the module",
            text "and its" <+>
              (if is_boot
                then text "hs-boot file"
                else text "hsig file"),
            text "Main module:" <+> real_doc,
              (if is_boot
                then text "Boot file:  "
                else text "Hsig file: ")
                <+> boot_doc,
            extra_info
          ]

instMisMatch :: DFunId -> SDoc
instMisMatch dfun
  = hang (text "instance" <+> ppr (idType dfun))
       2 (text "is defined in the hs-boot file, but not in the module itself")

{-
************************************************************************
*                                                                      *
        Type-checking the top level of a module (continued)
*                                                                      *
************************************************************************
-}

rnTopSrcDecls :: HsGroup GhcPs -> TcM (TcGblEnv, HsGroup GhcRn)
-- Fails if there are any errors
rnTopSrcDecls group
 = do { -- Rename the source decls
        traceRn "rn12" empty ;
        (tcg_env, rn_decls) <- checkNoErrs $ rnSrcDecls group ;
        traceRn "rn13" empty ;
        (tcg_env, rn_decls) <- runRenamerPlugin tcg_env rn_decls ;
        traceRn "rn13-plugin" empty ;

        -- save the renamed syntax, if we want it
        let { tcg_env'
                | Just grp <- tcg_rn_decls tcg_env
                  = tcg_env{ tcg_rn_decls = Just (appendGroups grp rn_decls) }
                | otherwise
                   = tcg_env };

                -- Dump trace of renaming part
        rnDump rn_decls ;
        return (tcg_env', rn_decls)
   }

tcTopSrcDecls :: HsGroup GhcRn -> TcM (TcGblEnv, TcLclEnv)
tcTopSrcDecls (HsGroup { hs_tyclds = tycl_decls,
                         hs_derivds = deriv_decls,
                         hs_fords  = foreign_decls,
                         hs_defds  = default_decls,
                         hs_annds  = annotation_decls,
                         hs_ruleds = rule_decls,
                         hs_valds  = hs_val_binds@(XValBindsLR
                                              (NValBinds val_binds val_sigs)) })
 = do {         -- Type-check the type and class decls, and all imported decls
                -- The latter come in via tycl_decls
        traceTc "Tc2 (src)" empty ;

                -- Source-language instances, including derivings,
                -- and import the supporting declarations
        traceTc "Tc3" empty ;
        (tcg_env, inst_infos, XValBindsLR (NValBinds deriv_binds deriv_sigs))
            <- tcTyClsInstDecls tycl_decls deriv_decls val_binds ;

        setGblEnv tcg_env       $ do {

                -- Generate Applicative/Monad proposal (AMP) warnings
        traceTc "Tc3b" empty ;

                -- Generate Semigroup/Monoid warnings
        traceTc "Tc3c" empty ;
        tcSemigroupWarnings ;

                -- Foreign import declarations next.
        traceTc "Tc4" empty ;
        (fi_ids, fi_decls, fi_gres) <- tcForeignImports foreign_decls ;
        tcExtendGlobalValEnv fi_ids     $ do {

                -- Default declarations
        traceTc "Tc4a" empty ;
        default_tys <- tcDefaults default_decls ;
        updGblEnv (\gbl -> gbl { tcg_default = default_tys }) $ do {

                -- Value declarations next.
                -- It is important that we check the top-level value bindings
                -- before the GHC-generated derived bindings, since the latter
                -- may be defined in terms of the former. (For instance,
                -- the bindings produced in a Data instance.)
        traceTc "Tc5" empty ;
        tc_envs <- tcTopBinds val_binds val_sigs;
        setEnvs tc_envs $ do {

                -- Now GHC-generated derived bindings, generics, and selectors
                -- Do not generate warnings from compiler-generated code;
                -- hence the use of discardWarnings
        tc_envs@(tcg_env, tcl_env)
            <- discardWarnings (tcTopBinds deriv_binds deriv_sigs) ;
        setEnvs tc_envs $ do {  -- Environment doesn't change now

                -- Second pass over class and instance declarations,
                -- now using the kind-checked decls
        traceTc "Tc6" empty ;
        inst_binds <- tcInstDecls2 (tyClGroupTyClDecls tycl_decls) inst_infos ;

                -- Foreign exports
        traceTc "Tc7" empty ;
        (foe_binds, foe_decls, foe_gres) <- tcForeignExports foreign_decls ;

                -- Annotations
        annotations <- tcAnnotations annotation_decls ;

                -- Rules
        rules <- tcRules rule_decls ;

                -- Wrap up
        traceTc "Tc7a" empty ;
        let { all_binds = inst_binds     `unionBags`
                          foe_binds

            ; fo_gres = fi_gres `unionBags` foe_gres
            ; fo_fvs = foldr (\gre fvs -> fvs `addOneFV` greMangledName gre)
                                emptyFVs fo_gres

            ; sig_names = mkNameSet (collectHsValBinders hs_val_binds)
                          `minusNameSet` getTypeSigNames val_sigs

                -- Extend the GblEnv with the (as yet un-zonked)
                -- bindings, rules, foreign decls
            ; tcg_env' = tcg_env { tcg_binds   = tcg_binds tcg_env `unionBags` all_binds
                                 , tcg_sigs    = tcg_sigs tcg_env `unionNameSet` sig_names
                                 , tcg_rules   = tcg_rules tcg_env
                                                      ++ flattenRuleDecls rules
                                 , tcg_anns    = tcg_anns tcg_env ++ annotations
                                 , tcg_ann_env = extendAnnEnvList (tcg_ann_env tcg_env) annotations
                                 , tcg_fords   = tcg_fords tcg_env ++ foe_decls ++ fi_decls
                                 , tcg_dus     = tcg_dus tcg_env `plusDU` usesOnly fo_fvs } } ;
                                 -- tcg_dus: see Note [Newtype constructor usage in foreign declarations]

        -- See Note [Newtype constructor usage in foreign declarations]
        addUsedGREs (bagToList fo_gres) ;

        return (tcg_env', tcl_env)
    }}}}}}

tcTopSrcDecls _ = panic "tcTopSrcDecls: ValBindsIn"


tcSemigroupWarnings :: TcM ()
tcSemigroupWarnings = do
    traceTc "tcSemigroupWarnings" empty
    let warnFlag = Opt_WarnSemigroup
    tcPreludeClashWarn warnFlag sappendName
    tcMissingParentClassWarn warnFlag monoidClassName semigroupClassName


-- | Warn on local definitions of names that would clash with future Prelude
-- elements.
--
--   A name clashes if the following criteria are met:
--       1. It would is imported (unqualified) from Prelude
--       2. It is locally defined in the current module
--       3. It has the same literal name as the reference function
--       4. It is not identical to the reference function
tcPreludeClashWarn :: WarningFlag
                   -> Name
                   -> TcM ()
tcPreludeClashWarn warnFlag name = do
    { warn <- woptM warnFlag
    ; when warn $ do
    { traceTc "tcPreludeClashWarn/wouldBeImported" empty
    -- Is the name imported (unqualified) from Prelude? (Point 4 above)
    ; rnImports <- fmap (map unLoc . tcg_rn_imports) getGblEnv
    -- (Note that this automatically handles -XNoImplicitPrelude, as Prelude
    -- will not appear in rnImports automatically if it is set.)

    -- Continue only the name is imported from Prelude
    ; when (importedViaPrelude name rnImports) $ do
      -- Handle 2.-4.
    { rdrElts <- fmap (concat . occEnvElts . tcg_rdr_env) getGblEnv

    ; let clashes :: GlobalRdrElt -> Bool
          clashes x = isLocalDef && nameClashes && isNotInProperModule
            where
              isLocalDef = gre_lcl x == True
              -- Names are identical ...
              nameClashes = nameOccName (greMangledName x) == nameOccName name
              -- ... but not the actual definitions, because we don't want to
              -- warn about a bad definition of e.g. <> in Data.Semigroup, which
              -- is the (only) proper place where this should be defined
              isNotInProperModule = greMangledName x /= name

          -- List of all offending definitions
          clashingElts :: [GlobalRdrElt]
          clashingElts = filter clashes rdrElts

    ; traceTc "tcPreludeClashWarn/prelude_functions"
                (hang (ppr name) 4 (sep [ppr clashingElts]))

    ; let warn_msg x = addWarnAt (Reason warnFlag) (nameSrcSpan (greMangledName x)) (hsep
              [ text "Local definition of"
              , (quotes . ppr . nameOccName . greMangledName) x
              , text "clashes with a future Prelude name." ]
              $$
              text "This will become an error in a future release." )
    ; mapM_ warn_msg clashingElts
    }}}

  where

    -- Is the given name imported via Prelude?
    --
    -- Possible scenarios:
    --   a) Prelude is imported implicitly, issue warnings.
    --   b) Prelude is imported explicitly, but without mentioning the name in
    --      question. Issue no warnings.
    --   c) Prelude is imported hiding the name in question. Issue no warnings.
    --   d) Qualified import of Prelude, no warnings.
    importedViaPrelude :: Name
                       -> [ImportDecl GhcRn]
                       -> Bool
    importedViaPrelude name = any importViaPrelude
      where
        isPrelude :: ImportDecl GhcRn -> Bool
        isPrelude imp = unLoc (ideclName imp) == pRELUDE_NAME

        -- Implicit (Prelude) import?
        isImplicit :: ImportDecl GhcRn -> Bool
        isImplicit = ideclImplicit

        -- Unqualified import?
        isUnqualified :: ImportDecl GhcRn -> Bool
        isUnqualified = not . isImportDeclQualified . ideclQualified

        -- List of explicitly imported (or hidden) Names from a single import.
        --   Nothing -> No explicit imports
        --   Just (False, <names>) -> Explicit import list of <names>
        --   Just (True , <names>) -> Explicit hiding of <names>
        importListOf :: ImportDecl GhcRn -> Maybe (Bool, [Name])
        importListOf = fmap toImportList . ideclHiding
          where
            toImportList (h, loc) = (h, map (ieName . unLoc) (unLoc loc))

        isExplicit :: ImportDecl GhcRn -> Bool
        isExplicit x = case importListOf x of
            Nothing -> False
            Just (False, explicit)
                -> nameOccName name `elem`    map nameOccName explicit
            Just (True, hidden)
                -> nameOccName name `notElem` map nameOccName hidden

        -- Check whether the given name would be imported (unqualified) from
        -- an import declaration.
        importViaPrelude :: ImportDecl GhcRn -> Bool
        importViaPrelude x = isPrelude x
                          && isUnqualified x
                          && (isImplicit x || isExplicit x)


-- Notation: is* is for classes the type is an instance of, should* for those
--           that it should also be an instance of based on the corresponding
--           is*.
tcMissingParentClassWarn :: WarningFlag
                         -> Name -- ^ Instances of this ...
                         -> Name -- ^ should also be instances of this
                         -> TcM ()
tcMissingParentClassWarn warnFlag isName shouldName
  = do { warn <- woptM warnFlag
       ; when warn $ do
       { traceTc "tcMissingParentClassWarn" empty
       ; isClass'     <- tcLookupClass_maybe isName
       ; shouldClass' <- tcLookupClass_maybe shouldName
       ; case (isClass', shouldClass') of
              (Just isClass, Just shouldClass) -> do
                  { localInstances <- tcGetInsts
                  ; let isInstance m = is_cls m == isClass
                        isInsts = filter isInstance localInstances
                  ; traceTc "tcMissingParentClassWarn/isInsts" (ppr isInsts)
                  ; forM_ isInsts (checkShouldInst isClass shouldClass)
                  }
              (is',should') ->
                  traceTc "tcMissingParentClassWarn/notIsShould"
                          (hang (ppr isName <> text "/" <> ppr shouldName) 2 (
                            (hsep [ quotes (text "Is"), text "lookup for"
                                  , ppr isName
                                  , text "resulted in", ppr is' ])
                            $$
                            (hsep [ quotes (text "Should"), text "lookup for"
                                  , ppr shouldName
                                  , text "resulted in", ppr should' ])))
       }}
  where
    -- Check whether the desired superclass exists in a given environment.
    checkShouldInst :: Class   -- ^ Class of existing instance
                    -> Class   -- ^ Class there should be an instance of
                    -> ClsInst -- ^ Existing instance
                    -> TcM ()
    checkShouldInst isClass shouldClass isInst
      = do { instEnv <- tcGetInstEnvs
           ; let (instanceMatches, shouldInsts, _)
                    = lookupInstEnv False instEnv shouldClass (is_tys isInst)

           ; traceTc "tcMissingParentClassWarn/checkShouldInst"
                     (hang (ppr isInst) 4
                         (sep [ppr instanceMatches, ppr shouldInsts]))

           -- "<location>: Warning: <type> is an instance of <is> but not
           -- <should>" e.g. "Foo is an instance of Monad but not Applicative"
           ; let instLoc = srcLocSpan . nameSrcLoc $ getName isInst
                 warnMsg (Just name:_) =
                      addWarnAt (Reason warnFlag) instLoc $
                           hsep [ (quotes . ppr . nameOccName) name
                                , text "is an instance of"
                                , (ppr . nameOccName . className) isClass
                                , text "but not"
                                , (ppr . nameOccName . className) shouldClass ]
                                <> text "."
                           $$
                           hsep [ text "This will become an error in"
                                , text "a future release." ]
                 warnMsg _ = pure ()
           ; when (null shouldInsts && null instanceMatches) $
                  warnMsg (is_tcs isInst)
           }

    tcLookupClass_maybe :: Name -> TcM (Maybe Class)
    tcLookupClass_maybe name = tcLookupImported_maybe name >>= \case
        Succeeded (ATyCon tc) | cls@(Just _) <- tyConClass_maybe tc -> pure cls
        _else -> pure Nothing


---------------------------
tcTyClsInstDecls :: [TyClGroup GhcRn]
                 -> [LDerivDecl GhcRn]
                 -> [(RecFlag, LHsBinds GhcRn)]
                 -> TcM (TcGblEnv,            -- The full inst env
                         [InstInfo GhcRn],    -- Source-code instance decls to
                                              -- process; contains all dfuns for
                                              -- this module
                          HsValBinds GhcRn)   -- Supporting bindings for derived
                                              -- instances

tcTyClsInstDecls tycl_decls deriv_decls binds
 = tcAddDataFamConPlaceholders (tycl_decls >>= group_instds) $
   tcAddPatSynPlaceholders (getPatSynBinds binds) $
   do { (tcg_env, inst_info, deriv_info)
          <- tcTyAndClassDecls tycl_decls ;
      ; setGblEnv tcg_env $ do {
          -- With the @TyClDecl@s and @InstDecl@s checked we're ready to
          -- process the deriving clauses, including data family deriving
          -- clauses discovered in @tcTyAndClassDecls@.
          --
          -- Careful to quit now in case there were instance errors, so that
          -- the deriving errors don't pile up as well.
          ; failIfErrsM
          ; (tcg_env', inst_info', val_binds)
              <- tcInstDeclsDeriv deriv_info deriv_decls
          ; setGblEnv tcg_env' $ do {
                failIfErrsM
              ; pure (tcg_env', inst_info' ++ inst_info, val_binds)
      }}}

{- *********************************************************************
*                                                                      *
        Checking for 'main'
*                                                                      *
************************************************************************
-}

checkMain :: Bool  -- False => no 'module M(..) where' header at all
          -> Maybe (Located [LIE GhcPs])  -- Export specs of Main module
          -> TcM TcGblEnv
-- If we are in module Main, check that 'main' is defined and exported.
checkMain explicit_mod_hdr export_ies
 = do   { hsc_env  <- getTopEnv
        ; tcg_env <- getGblEnv
        ; check_main hsc_env tcg_env explicit_mod_hdr export_ies }

check_main :: HscEnv -> TcGblEnv -> Bool -> Maybe (Located [LIE GhcPs])
           -> TcM TcGblEnv
check_main hsc_env tcg_env explicit_mod_hdr export_ies
 | mod /= main_mod
 = traceTc "checkMain not" (ppr main_mod <+> ppr mod) >>
   return tcg_env

 | otherwise
   -- Compare the list of main functions in scope with those
   --   specified in the export list.
 = do mains_all <- lookupInfoOccRn main_fn
                    -- get all 'main' functions in scope
                    -- They may also be imported from other modules!
      case exportedMains of -- check the main(s) specified in the export list
        [ ] -> do
          -- The module has no main functions in the export spec, so we must give
          -- some kind of error message. The tricky part is giving an error message
          -- that accurately characterizes what the problem is.
          -- See Note [Main module without a main function in the export spec]
          traceTc "checkMain no main module exported" ppr_mod_mainfn
          complain_no_main
          -- In order to reduce the number of potential error messages, we check
          -- to see if there are any main functions defined (but not exported)...
          case getSomeMain mains_all of
            Nothing -> return tcg_env
              -- ...if there are no such main functions, there is nothing we can do...
            Just some_main -> use_as_main some_main
                -- ...if there is such a main function, then communicate this to the
                -- typechecker. This can prevent a spurious "Ambiguous type variable"
                -- error message in certain cases, as described in
                -- Note [Main module without a main function in the export spec].
        _ -> do    -- The module has one or more main functions in the export spec
          let mains = filterInsMains exportedMains mains_all
          case mains of
            [] -> do  --
              traceTc "checkMain fail" ppr_mod_mainfn
              complain_no_main
              return tcg_env
            [main_name] -> use_as_main main_name
            _ -> do           -- multiple main functions are exported
              addAmbiguousNameErr main_fn          -- issue error msg
              return tcg_env
  where
    dflags      = hsc_dflags hsc_env
    mod         = tcg_mod tcg_env
    main_mod    = mainModIs hsc_env
    main_mod_nm = moduleName main_mod
    main_fn     = getMainFun dflags
    occ_main_fn = occName main_fn
    interactive = ghcLink dflags == LinkInMemory
    exportedMains = selExportMains export_ies
    ppr_mod_mainfn = ppr main_mod <+> ppr main_fn

    -- There is a single exported 'main' function.
    use_as_main :: Name -> TcM TcGblEnv
    use_as_main main_name = do
        { traceTc "checkMain found" (ppr main_mod <+> ppr main_fn)
        ; let loc       = srcLocSpan (getSrcLoc main_name)
        ; ioTyCon <- tcLookupTyCon ioTyConName
        ; res_ty <- newFlexiTyVarTy liftedTypeKind
        ; let io_ty = mkTyConApp ioTyCon [res_ty]
              skol_info = SigSkol (FunSigCtxt main_name False) io_ty []
              main_expr_rn = L loc (HsVar noExtField (L loc main_name))
        ; (ev_binds, main_expr)
               <- checkConstraints skol_info [] [] $
                  addErrCtxt mainCtxt    $
                  tcCheckMonoExpr main_expr_rn io_ty

                -- See Note [Root-main Id]
                -- Construct the binding
                --      :Main.main :: IO res_ty = runMainIO res_ty main
        ; run_main_id <- tcLookupId runMainIOName
        ; let { root_main_name =  mkExternalName rootMainKey rOOT_MAIN
                                   (mkVarOccFS (fsLit "main"))
                                   (getSrcSpan main_name)
              ; root_main_id = Id.mkExportedVanillaId root_main_name
                                                      (mkTyConApp ioTyCon [res_ty])
              ; co  = mkWpTyApps [res_ty]
              -- The ev_binds of the `main` function may contain deferred
              -- type error when type of `main` is not `IO a`. The `ev_binds`
              -- must be put inside `runMainIO` to ensure the deferred type
              -- error can be emitted correctly. See #13838.
              ; rhs = nlHsApp (mkLHsWrap co (nlHsVar run_main_id)) $
                        mkHsDictLet ev_binds main_expr
              ; main_bind = mkVarBind root_main_id rhs }

        ; return (tcg_env { tcg_main  = Just main_name,
                            tcg_binds = tcg_binds tcg_env
                                        `snocBag` main_bind,
                            tcg_dus   = tcg_dus tcg_env
                                        `plusDU` usesOnly (unitFV main_name)
                        -- Record the use of 'main', so that we don't
                        -- complain about it being defined but not used
        })}

    complain_no_main = unless (interactive && not explicit_mod_hdr)
                              (addErrTc noMainMsg)                  -- #12906
        -- Without an explicit module header...
          -- in interactive mode, don't worry about the absence of 'main'.
          -- in other modes, add error message and go on with typechecking.

    mainCtxt  = text "When checking the type of the" <+> pp_main_fn
    noMainMsg = text "The" <+> pp_main_fn
                <+> text "is not" <+> text defOrExp <+> text "module"
                <+> quotes (ppr main_mod)
    defOrExp  = if null exportedMains then "exported by" else "defined in"

    pp_main_fn = ppMainFn main_fn

    -- Select the main functions from the export list.
    -- Only the module name is needed, the function name is fixed.
    selExportMains :: Maybe (Located [LIE GhcPs]) -> [ModuleName]    -- #16453
    selExportMains Nothing = [main_mod_nm]
        -- no main specified, but there is a header.
    selExportMains (Just exps) = fmap fst $
        filter (\(_,n) -> n == occ_main_fn ) texp
      where
        ies = fmap unLoc $ unLoc exps
        texp = mapMaybe transExportIE ies

    -- Filter all main functions in scope that match the export specs
    filterInsMains :: [ModuleName] -> [Name] -> [Name]               -- #16453
    filterInsMains export_mains inscope_mains =
      [mod | mod <- inscope_mains,
          (moduleName . nameModule) mod `elem` export_mains]

    -- Transform an export_ie to a (ModuleName, OccName) pair.
    -- 'IEVar' constructors contain exported values (functions), eg '(Main.main)'
    -- 'IEModuleContents' constructors contain fully exported modules, eg '(Main)'
    -- All other 'IE...' constructors are not used and transformed to Nothing.
    transExportIE :: IE GhcPs -> Maybe (ModuleName, OccName)         -- #16453
    transExportIE (IEVar _  var) = isQual_maybe $
         upqual $ ieWrappedName $ unLoc var
       where
         -- A module name is always needed, so qualify 'UnQual' rdr names.
         upqual (Unqual occ) = Qual main_mod_nm occ
         upqual rdr = rdr
    transExportIE (IEModuleContents _ mod) = Just (unLoc mod, occ_main_fn)
    transExportIE _ = Nothing

    -- Get a main function that is in scope.
    -- See Note [Main module without a main function in the export spec]
    getSomeMain :: [Name] -> Maybe Name                            -- #16453
    getSomeMain all_mains = case all_mains of
        []  -> Nothing                -- No main function in scope
        [m] -> Just m                 -- Just one main function in scope
        _   -> case mbMainOfMain of
          Nothing -> listToMaybe all_mains -- Take the first main function in scope or Nothing
          _       -> mbMainOfMain          -- Take the Main module's main function or Nothing
      where
        mbMainOfMain = find (\n -> (moduleName . nameModule) n == main_mod_nm )
                          all_mains         -- the main function of the Main module

-- | Get the unqualified name of the function to use as the \"main\" for the main module.
-- Either returns the default name or the one configured on the command line with -main-is
getMainFun :: DynFlags -> RdrName
getMainFun dflags = case mainFunIs dflags of
                      Just fn -> mkRdrUnqual (mkVarOccFS (mkFastString fn))
                      Nothing -> main_RDR_Unqual

ppMainFn :: RdrName -> SDoc
ppMainFn main_fn
  | rdrNameOcc main_fn == mainOcc
  = text "IO action" <+> quotes (ppr main_fn)
  | otherwise
  = text "main IO action" <+> quotes (ppr main_fn)

mainOcc :: OccName
mainOcc = mkVarOccFS (fsLit "main")

{-
Note [Root-main Id]
~~~~~~~~~~~~~~~~~~~
The function that the RTS invokes is always :Main.main, which we call
root_main_id.  (Because GHC allows the user to have a module not
called Main as the main module, we can't rely on the main function
being called "Main.main".  That's why root_main_id has a fixed module
":Main".)

This is unusual: it's a LocalId whose Name has a Module from another
module. Tiresomely, we must filter it out again in GHC.Iface.Make, less we
get two defns for 'main' in the interface file!


Note [Main module without a main function in the export spec]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Giving accurate error messages for a Main module that does not export a main
function is surprisingly tricky. To see why, consider a module in a file
`Foo.hs` that has no `main` function in the explicit export specs of the module
header:

    module Main () where
    foo = return ()

This does not export a main function and therefore should be rejected, per
chapter 5 of the Haskell Report 2010:

   A Haskell program is a collection of modules, one of which, by convention,
   must be called Main and must export the value main. The value of the
   program is the value of the identifier main in module Main, which must be
   a computation of type IO τ for some type τ.

In fact, when you compile the program above using `ghc Foo.hs`, you will
actually get *two* errors:

 - The IO action ‘main’ is not defined in module ‘Main’

 - Ambiguous type variable ‘m0’ arising from a use of ‘return’
   prevents the constraint ‘(Monad m0)’ from being solved.

The first error is self-explanatory, while the second error message occurs
due to the monomorphism restriction.

Now consider what would happen if the program above were compiled with
`ghc -main-is foo Foo`. The has the effect of `foo` being designated as the
main function. The program will still be rejected since it does not export
`foo` (and therefore does not export its main function), but there is one
important difference: `foo` will be checked against the type `IO τ`. As a
result, we would *not* expect the monomorphism restriction error message
to occur, since the typechecker should have no trouble figuring out the type
of `foo`. In other words, we should only throw the former error message,
not the latter.

The implementation uses the function `getSomeMain` to find a potential main
function that is defined but not exported. If one is found, it is passed to
`use_as_main` to inform the typechecker that the main function should be of
type `IO τ`. See also the `T414` and `T17171a` test cases for similar examples
of programs whose error messages are influenced by the situation described in
this Note.


*********************************************************
*                                                       *
                GHCi stuff
*                                                       *
*********************************************************
-}

runTcInteractive :: HscEnv -> TcRn a -> IO (Messages, Maybe a)
-- Initialise the tcg_inst_env with instances from all home modules.
-- This mimics the more selective call to hptInstances in tcRnImports
runTcInteractive hsc_env thing_inside
  = initTcInteractive hsc_env $ withTcPlugins hsc_env $ withHoleFitPlugins hsc_env $
    do { traceTc "setInteractiveContext" $
            vcat [ text "ic_tythings:" <+> vcat (map ppr (ic_tythings icxt))
                 , text "ic_insts:" <+> vcat (map (pprBndr LetBind . instanceDFunId) ic_insts)
                 , text "ic_rn_gbl_env (LocalDef)" <+>
                      vcat (map ppr [ local_gres | gres <- occEnvElts (ic_rn_gbl_env icxt)
                                                 , let local_gres = filter isLocalGRE gres
                                                 , not (null local_gres) ]) ]

       ; let getOrphans m mb_pkg = fmap (\iface -> mi_module iface
                                          : dep_orphs (mi_deps iface))
                                 (loadSrcInterface (text "runTcInteractive") m
                                                   NotBoot mb_pkg)

       ; !orphs <- fmap (force . concat) . forM (ic_imports icxt) $ \i ->
            case i of                   -- force above: see #15111
                IIModule n -> getOrphans n Nothing
                IIDecl i ->
                  let mb_pkg = sl_fs <$> ideclPkgQual i in
                  getOrphans (unLoc (ideclName i)) mb_pkg

       ; let imports = emptyImportAvails {
                            imp_orphs = orphs
                        }

       ; (gbl_env, lcl_env) <- getEnvs
       ; let gbl_env' = gbl_env {
                           tcg_rdr_env      = ic_rn_gbl_env icxt
                         , tcg_type_env     = type_env
                         , tcg_inst_env     = extendInstEnvList
                                               (extendInstEnvList (tcg_inst_env gbl_env) ic_insts)
                                               home_insts
                         , tcg_fam_inst_env = extendFamInstEnvList
                                               (extendFamInstEnvList (tcg_fam_inst_env gbl_env)
                                                                     ic_finsts)
                                               home_fam_insts
                         , tcg_field_env    = mkNameEnv con_fields
                              -- setting tcg_field_env is necessary
                              -- to make RecordWildCards work (test: ghci049)
                         , tcg_fix_env      = ic_fix_env icxt
                         , tcg_default      = ic_default icxt
                              -- must calculate imp_orphs of the ImportAvails
                              -- so that instance visibility is done correctly
                         , tcg_imports      = imports
                         }

             lcl_env' = tcExtendLocalTypeEnv lcl_env lcl_ids

       ; setEnvs (gbl_env', lcl_env') thing_inside }
  where
    (home_insts, home_fam_insts) = hptInstances hsc_env (\_ -> True)

    icxt                     = hsc_IC hsc_env
    (ic_insts, ic_finsts)    = ic_instances icxt
    (lcl_ids, top_ty_things) = partitionWith is_closed (ic_tythings icxt)

    is_closed :: TyThing -> Either (Name, TcTyThing) TyThing
    -- Put Ids with free type variables (always RuntimeUnks)
    -- in the *local* type environment
    -- See Note [Initialising the type environment for GHCi]
    is_closed thing
      | AnId id <- thing
      , not (isTypeClosedLetBndr id)
      = Left (idName id, ATcId { tct_id = id
                               , tct_info = NotLetBound })
      | otherwise
      = Right thing

    type_env1 = mkTypeEnvWithImplicits top_ty_things
    type_env  = extendTypeEnvWithIds type_env1 (map instanceDFunId ic_insts)
                -- Putting the dfuns in the type_env
                -- is just to keep Core Lint happy

    con_fields = [ (dataConName c, dataConFieldLabels c)
                 | ATyCon t <- top_ty_things
                 , c <- tyConDataCons t ]


{- Note [Initialising the type environment for GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Most of the Ids in ic_things, defined by the user in 'let' stmts,
have closed types. E.g.
   ghci> let foo x y = x && not y

However the GHCi debugger creates top-level bindings for Ids whose
types have free RuntimeUnk skolem variables, standing for unknown
types.  If we don't register these free TyVars as global TyVars then
the typechecker will try to quantify over them and fall over in
skolemiseQuantifiedTyVar. so we must add any free TyVars to the
typechecker's global TyVar set.  That is done by using
tcExtendLocalTypeEnv.

We do this by splitting out the Ids with open types, using 'is_closed'
to do the partition.  The top-level things go in the global TypeEnv;
the open, NotTopLevel, Ids, with free RuntimeUnk tyvars, go in the
local TypeEnv.

Note that we don't extend the local RdrEnv (tcl_rdr); all the in-scope
things are already in the interactive context's GlobalRdrEnv.
Extending the local RdrEnv isn't terrible, but it means there is an
entry for the same Name in both global and local RdrEnvs, and that
lead to duplicate "perhaps you meant..." suggestions (e.g. T5564).

We don't bother with the tcl_th_bndrs environment either.
-}

-- | The returned [Id] is the list of new Ids bound by this statement. It can
-- be used to extend the InteractiveContext via extendInteractiveContext.
--
-- The returned TypecheckedHsExpr is of type IO [ () ], a list of the bound
-- values, coerced to ().
tcRnStmt :: HscEnv -> GhciLStmt GhcPs
         -> IO (Messages, Maybe ([Id], LHsExpr GhcTc, FixityEnv))
tcRnStmt hsc_env rdr_stmt
  = runTcInteractive hsc_env $ do {

    -- The real work is done here
    ((bound_ids, tc_expr), fix_env) <- tcUserStmt rdr_stmt ;
    zonked_expr <- zonkTopLExpr tc_expr ;
    zonked_ids  <- zonkTopBndrs bound_ids ;

    failIfErrsM ;  -- we can't do the next step if there are levity polymorphism errors
                   -- test case: ghci/scripts/T13202{,a}

        -- None of the Ids should be of unboxed type, because we
        -- cast them all to HValues in the end!
    mapM_ bad_unboxed (filter (isUnliftedType . idType) zonked_ids) ;

    traceTc "tcs 1" empty ;
    this_mod <- getModule ;
    global_ids <- mapM (externaliseAndTidyId this_mod) zonked_ids ;
        -- Note [Interactively-bound Ids in GHCi] in GHC.Driver.Env

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

    traceOptTcRn Opt_D_dump_tc
        (vcat [text "Bound Ids" <+> pprWithCommas ppr global_ids,
               text "Typechecked expr" <+> ppr zonked_expr]) ;

    return (global_ids, zonked_expr, fix_env)
    }
  where
    bad_unboxed id = addErr (sep [text "GHCi can't bind a variable of unlifted type:",
                                  nest 2 (pprPrefixOcc id <+> dcolon <+> ppr (idType id))])

{-
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
-}

-- | A plan is an attempt to lift some code into the IO monad.
type PlanResult = ([Id], LHsExpr GhcTc)
type Plan = TcM PlanResult

-- | Try the plans in order. If one fails (by raising an exn), try the next.
-- If one succeeds, take it.
runPlans :: [Plan] -> TcM PlanResult
runPlans []     = panic "runPlans"
runPlans [p]    = p
runPlans (p:ps) = tryTcDiscardingErrs (runPlans ps) p

-- | Typecheck (and 'lift') a stmt entered by the user in GHCi into the
-- GHCi 'environment'.
--
-- By 'lift' and 'environment we mean that the code is changed to
-- execute properly in an IO monad. See Note [Interactively-bound Ids
-- in GHCi] in GHC.Driver.Env for more details. We do this lifting by trying
-- different ways ('plans') of lifting the code into the IO monad and
-- type checking each plan until one succeeds.
tcUserStmt :: GhciLStmt GhcPs -> TcM (PlanResult, FixityEnv)

-- An expression typed at the prompt is treated very specially
tcUserStmt (L loc (BodyStmt _ expr _ _))
  = do  { (rn_expr, fvs) <- checkNoErrs (rnLExpr expr)
               -- Don't try to typecheck if the renamer fails!
        ; ghciStep <- getGhciStepIO
        ; uniq <- newUnique
        ; interPrintName <- getInteractivePrintName
        ; let fresh_it  = itName uniq loc
              matches   = [mkMatch (mkPrefixFunRhs (L loc fresh_it)) [] rn_expr
                                   (noLoc emptyLocalBinds)]
              -- [it = expr]
              the_bind  = L loc $ (mkTopFunBind FromSource
                                     (L loc fresh_it) matches)
                                         { fun_ext = fvs }
              -- Care here!  In GHCi the expression might have
              -- free variables, and they in turn may have free type variables
              -- (if we are at a breakpoint, say).  We must put those free vars

              -- [let it = expr]
              let_stmt  = L loc $ LetStmt noExtField $ noLoc $ HsValBinds noExtField
                           $ XValBindsLR
                               (NValBinds [(NonRecursive,unitBag the_bind)] [])

              -- [it <- e]
              bind_stmt = L loc $ BindStmt
                                       (XBindStmtRn
                                          { xbsrn_bindOp = mkRnSyntaxExpr bindIOName
                                          , xbsrn_failOp = Nothing
                                          })
                                       (L loc (VarPat noExtField (L loc fresh_it)))
                                       (nlHsApp ghciStep rn_expr)

              -- [; print it]
              print_it  = L loc $ BodyStmt noExtField
                                           (nlHsApp (nlHsVar interPrintName)
                                           (nlHsVar fresh_it))
                                           (mkRnSyntaxExpr thenIOName)
                                                  noSyntaxExpr

              -- NewA
              no_it_a = L loc $ BodyStmt noExtField (nlHsApps bindIOName
                                       [rn_expr , nlHsVar interPrintName])
                                       (mkRnSyntaxExpr thenIOName)
                                       noSyntaxExpr

              no_it_b = L loc $ BodyStmt noExtField (rn_expr)
                                       (mkRnSyntaxExpr thenIOName)
                                       noSyntaxExpr

              no_it_c = L loc $ BodyStmt noExtField
                                      (nlHsApp (nlHsVar interPrintName) rn_expr)
                                      (mkRnSyntaxExpr thenIOName)
                                      noSyntaxExpr

              -- See Note [GHCi Plans]

              it_plans = [
                    -- Plan A
                    do { stuff@([it_id], _) <- tcGhciStmts [bind_stmt, print_it]
                       ; it_ty <- zonkTcType (idType it_id)
                       ; when (isUnitTy $ it_ty) failM
                       ; return stuff },

                        -- Plan B; a naked bind statement
                    tcGhciStmts [bind_stmt],

                        -- Plan C; check that the let-binding is typeable all by itself.
                        -- If not, fail; if so, try to print it.
                        -- The two-step process avoids getting two errors: one from
                        -- the expression itself, and one from the 'print it' part
                        -- This two-step story is very clunky, alas
                    do { _ <- checkNoErrs (tcGhciStmts [let_stmt])
                                --- checkNoErrs defeats the error recovery of let-bindings
                       ; tcGhciStmts [let_stmt, print_it] } ]

              -- Plans where we don't bind "it"
              no_it_plans = [
                    tcGhciStmts [no_it_a] ,
                    tcGhciStmts [no_it_b] ,
                    tcGhciStmts [no_it_c] ]

        ; generate_it <- goptM Opt_NoIt

        -- We disable `-fdefer-type-errors` in GHCi for naked expressions.
        -- See Note [Deferred type errors in GHCi]

        -- NB: The flag `-fdefer-type-errors` implies `-fdefer-type-holes`
        -- and `-fdefer-out-of-scope-variables`. However the flag
        -- `-fno-defer-type-errors` doesn't imply `-fdefer-type-holes` and
        -- `-fno-defer-out-of-scope-variables`. Thus the later two flags
        -- also need to be unset here.
        ; plan <- unsetGOptM Opt_DeferTypeErrors $
                  unsetGOptM Opt_DeferTypedHoles $
                  unsetGOptM Opt_DeferOutOfScopeVariables $
                    runPlans $ if generate_it
                                 then no_it_plans
                                 else it_plans

        ; fix_env <- getFixityEnv
        ; return (plan, fix_env) }

{- Note [Deferred type errors in GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In GHCi, we ensure that type errors don't get deferred when type checking the
naked expressions. Deferring type errors here is unhelpful because the
expression gets evaluated right away anyway. It also would potentially emit
two redundant type-error warnings, one from each plan.

#14963 reveals another bug that when deferred type errors is enabled
in GHCi, any reference of imported/loaded variables (directly or indirectly)
in interactively issued naked expressions will cause ghc panic. See more
detailed discussion in #14963.

The interactively issued declarations, statements, as well as the modules
loaded into GHCi, are not affected. That means, for declaration, you could
have

    Prelude> :set -fdefer-type-errors
    Prelude> x :: IO (); x = putStrLn True
    <interactive>:14:26: warning: [-Wdeferred-type-errors]
        ? Couldn't match type ‘Bool’ with ‘[Char]’
          Expected type: String
            Actual type: Bool
        ? In the first argument of ‘putStrLn’, namely ‘True’
          In the expression: putStrLn True
          In an equation for ‘x’: x = putStrLn True

But for naked expressions, you will have

    Prelude> :set -fdefer-type-errors
    Prelude> putStrLn True
    <interactive>:2:10: error:
        ? Couldn't match type ‘Bool’ with ‘[Char]’
          Expected type: String
            Actual type: Bool
        ? In the first argument of ‘putStrLn’, namely ‘True’
          In the expression: putStrLn True
          In an equation for ‘it’: it = putStrLn True

    Prelude> let x = putStrLn True
    <interactive>:2:18: warning: [-Wdeferred-type-errors]
        ? Couldn't match type ‘Bool’ with ‘[Char]’
          Expected type: String
            Actual type: Bool
        ? In the first argument of ‘putStrLn’, namely ‘True’
          In the expression: putStrLn True
          In an equation for ‘x’: x = putStrLn True
-}

tcUserStmt rdr_stmt@(L loc _)
  = do { (([rn_stmt], fix_env), fvs) <- checkNoErrs $
           rnStmts GhciStmtCtxt rnLExpr [rdr_stmt] $ \_ -> do
             fix_env <- getFixityEnv
             return (fix_env, emptyFVs)
            -- Don't try to typecheck if the renamer fails!
       ; traceRn "tcRnStmt" (vcat [ppr rdr_stmt, ppr rn_stmt, ppr fvs])
       ; rnDump rn_stmt ;

       ; ghciStep <- getGhciStepIO
       ; let gi_stmt
               | (L loc (BindStmt x pat expr)) <- rn_stmt
                     = L loc $ BindStmt x pat (nlHsApp ghciStep expr)
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
        print_v  = L loc $ BodyStmt noExtField (nlHsApp (nlHsVar printName)
                                    (nlHsVar v))
                                    (mkRnSyntaxExpr thenIOName) noSyntaxExpr

{-
Note [GHCi Plans]
~~~~~~~~~~~~~~~~~
When a user types an expression in the repl we try to print it in three different
ways. Also, depending on whether -fno-it is set, we bind a variable called `it`
which can be used to refer to the result of the expression subsequently in the repl.

The normal plans are :
  A. [it <- e; print e]     but not if it::()
  B. [it <- e]
  C. [let it = e; print it]

When -fno-it is set, the plans are:
  A. [e >>= print]
  B. [e]
  C. [let it = e in print it]

The reason for -fno-it is explained in #14336. `it` can lead to the repl
leaking memory as it is repeatedly queried.
-}

-- | Typecheck the statements given and then return the results of the
-- statement in the form 'IO [()]'.
tcGhciStmts :: [GhciLStmt GhcRn] -> TcM PlanResult
tcGhciStmts stmts
 = do { ioTyCon <- tcLookupTyCon ioTyConName
      ; ret_id  <- tcLookupId returnIOName             -- return @ IO
      ; let ret_ty      = mkListTy unitTy
            io_ret_ty   = mkTyConApp ioTyCon [ret_ty]
            tc_io_stmts = tcStmtsAndThen GhciStmtCtxt tcDoStmt stmts
                                         (mkCheckExpType io_ret_ty)
            names = collectLStmtsBinders stmts

        -- OK, we're ready to typecheck the stmts
      ; traceTc "GHC.Tc.Module.tcGhciStmts: tc stmts" empty
      ; ((tc_stmts, ids), lie) <- captureTopConstraints $
                                  tc_io_stmts $ \ _ ->
                                  mapM tcLookupId names
                        -- Look up the names right in the middle,
                        -- where they will all be in scope

        -- Simplify the context
      ; traceTc "GHC.Tc.Module.tcGhciStmts: simplify ctxt" empty
      ; const_binds <- checkNoErrs (simplifyInteractive lie)
                -- checkNoErrs ensures that the plan fails if context redn fails


      ; traceTc "GHC.Tc.Module.tcGhciStmts: done" empty

      -- rec_expr is the expression
      --      returnIO @ [()] [unsafeCoerce# () x, ..,  unsafeCorece# () z]
      --
      -- Despite the inconvenience of building the type applications etc,
      -- this *has* to be done in type-annotated post-typecheck form
      -- because we are going to return a list of *polymorphic* values
      -- coerced to type (). If we built a *source* stmt
      --      return [coerce x, ..., coerce z]
      -- then the type checker would instantiate x..z, and we wouldn't
      -- get their *polymorphic* values.  (And we'd get ambiguity errs
      -- if they were overloaded, since they aren't applied to anything.)

      ; AnId unsafe_coerce_id <- tcLookupGlobal unsafeCoercePrimName
           -- We use unsafeCoerce# here because of (U11) in
           -- Note [Implementing unsafeCoerce] in base:Unsafe.Coerce

      ; let ret_expr = nlHsApp (nlHsTyApp ret_id [ret_ty]) $
                       noLoc $ ExplicitList unitTy Nothing $
                       map mk_item ids

            mk_item id = unsafe_coerce_id `nlHsTyApp` [ getRuntimeRep (idType id)
                                                      , getRuntimeRep unitTy
                                                      , idType id, unitTy]
                                          `nlHsApp` nlHsVar id
            stmts = tc_stmts ++ [noLoc (mkLastStmt ret_expr)]

      ; return (ids, mkHsDictLet (EvBinds const_binds) $
                     noLoc (HsDo io_ret_ty GhciStmtCtxt (noLoc stmts)))
    }

-- | Generate a typed ghciStepIO expression (ghciStep :: Ty a -> IO a)
getGhciStepIO :: TcM (LHsExpr GhcRn)
getGhciStepIO = do
    ghciTy <- getGHCiMonad
    a_tv <- newName (mkTyVarOccFS (fsLit "a"))
    let ghciM   = nlHsAppTy (nlHsTyVar ghciTy) (nlHsTyVar a_tv)
        ioM     = nlHsAppTy (nlHsTyVar ioTyConName) (nlHsTyVar a_tv)

        step_ty :: LHsSigType GhcRn
        step_ty = noLoc $ HsSig
                     { sig_bndrs = HsOuterImplicit{hso_ximplicit = [a_tv]}
                     , sig_ext = noExtField
                     , sig_body = nlHsFunTy ghciM ioM }

        stepTy :: LHsSigWcType GhcRn
        stepTy = mkEmptyWildCardBndrs step_ty

    return (noLoc $ ExprWithTySig noExtField (nlHsVar ghciStepIoMName) stepTy)

isGHCiMonad :: HscEnv -> String -> IO (Messages, Maybe Name)
isGHCiMonad hsc_env ty
  = runTcInteractive hsc_env $ do
        rdrEnv <- getGlobalRdrEnv
        let occIO = lookupOccEnv rdrEnv (mkOccName tcName ty)
        case occIO of
            Just [n] -> do
                let name = greMangledName n
                ghciClass <- tcLookupClass ghciIoClassName
                userTyCon <- tcLookupTyCon name
                let userTy = mkTyConApp userTyCon []
                _ <- tcLookupInstance ghciClass [userTy]
                return name

            Just _  -> failWithTc $ text "Ambiguous type!"
            Nothing -> failWithTc $ text ("Can't find type:" ++ ty)

-- | How should we infer a type? See Note [TcRnExprMode]
data TcRnExprMode = TM_Inst     -- ^ Instantiate inferred quantifiers only (:type)
                  | TM_Default  -- ^ Instantiate all quantifiers,
                                --   and do eager defaulting (:type +d)

-- | tcRnExpr just finds the type of an expression
--   for :type
tcRnExpr :: HscEnv
         -> TcRnExprMode
         -> LHsExpr GhcPs
         -> IO (Messages, Maybe Type)
tcRnExpr hsc_env mode rdr_expr
  = runTcInteractive hsc_env $
    do {

    (rn_expr, _fvs) <- rnLExpr rdr_expr ;
    failIfErrsM ;

    -- Typecheck the expression
    ((tclvl, res_ty), lie)
          <- captureTopConstraints $
             pushTcLevelM          $
             tcInferSigma inst rn_expr ;

    -- Generalise
    uniq <- newUnique ;
    let { fresh_it = itName uniq (getLoc rdr_expr) } ;
    (qtvs, dicts, _, residual, _)
         <- simplifyInfer tclvl infer_mode
                          []    {- No sig vars -}
                          [(fresh_it, res_ty)]
                          lie ;

    -- Ignore the dictionary bindings
    _ <- perhaps_disable_default_warnings $
         simplifyInteractive residual ;

    let { all_expr_ty = mkInfForAllTys qtvs $
                        mkPhiTy (map idType dicts) res_ty } ;
    ty <- zonkTcType all_expr_ty ;

    -- We normalise type families, so that the type of an expression is the
    -- same as of a bound expression (GHC.Tc.Gen.Bind.mkInferredPolyId). See Trac
    -- #10321 for further discussion.
    fam_envs <- tcGetFamInstEnvs ;
    -- normaliseType returns a coercion which we discard, so the Role is
    -- irrelevant
    return (snd (normaliseType fam_envs Nominal ty))
    }
  where
    -- Optionally instantiate the type of the expression
    -- See Note [TcRnExprMode]
    (inst, infer_mode, perhaps_disable_default_warnings) = case mode of
      TM_Inst    -> (False, NoRestrictions,  id)
      TM_Default -> (True,  EagerDefaulting, unsetWOptM Opt_WarnTypeDefaults)

{- Note [Implementing :type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider   :type const

We want    forall a b. a -> b -> a
and not    forall {a}{b}. a -> b -> a

The latter is what we'd get if we eagerly instantiated and then
re-generalised with Inferred binders.  It makes a difference, because
it tells us we where we can use Visible Type Application (VTA).

And also for   :type const @Int
we want        forall b. Int -> b -> Int
and not        forall {b}. Int -> b -> Int

Solution: use tcInferSigma, which in turn uses tcInferApp, which
has a special case for application chains.
-}

--------------------------
tcRnImportDecls :: HscEnv
                -> [LImportDecl GhcPs]
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

-- tcRnType just finds the kind of a type
tcRnType :: HscEnv
         -> ZonkFlexi
         -> Bool        -- Normalise the returned type
         -> LHsType GhcPs
         -> IO (Messages, Maybe (Type, Kind))
tcRnType hsc_env flexi normalise rdr_type
  = runTcInteractive hsc_env $
    setXOptM LangExt.PolyKinds $   -- See Note [Kind-generalise in tcRnType]
    do { (HsWC { hswc_ext = wcs, hswc_body = rn_type }, _fvs)
               <- rnHsWcType GHCiCtx (mkHsWildCardBndrs rdr_type)
                  -- The type can have wild cards, but no implicit
                  -- generalisation; e.g.   :kind (T _)
       ; failIfErrsM

        -- We follow Note [Recipe for checking a signature] in GHC.Tc.Gen.HsType here

        -- Now kind-check the type
        -- It can have any rank or kind
        -- First bring into scope any wildcards
       ; traceTc "tcRnType" (vcat [ppr wcs, ppr rn_type])
       ; (_tclvl, wanted, (ty, kind))
               <- pushLevelAndSolveEqualitiesX "tcRnType"  $
                  bindNamedWildCardBinders wcs $ \ wcs' ->
                  do { mapM_ emitNamedTypeHole wcs'
                     ; tcInferLHsTypeUnsaturated rn_type }

       ; checkNoErrs (reportAllUnsolved wanted)

       -- Do kind generalisation; see Note [Kind-generalise in tcRnType]
       ; kvs <- kindGeneralizeAll kind

       ; e <- mkEmptyZonkEnv flexi
       ; ty  <- zonkTcTypeToTypeX e ty

       -- Do validity checking on type
       ; checkValidType (GhciCtxt True) ty

       -- Optionally (:k vs :k!) normalise the type. Does two things:
       --   normaliseType: expand type-family applications
       --   expandTypeSynonyms: expand type synonyms (#18828)
       ; fam_envs <- tcGetFamInstEnvs
       ; let ty' | normalise = expandTypeSynonyms $ snd $
                               normaliseType fam_envs Nominal ty
                 | otherwise = ty

       ; return (ty', mkInfForAllTys kvs (tcTypeKind ty')) }


{- Note [TcRnExprMode]
~~~~~~~~~~~~~~~~~~~~~~
How should we infer a type when a user asks for the type of an expression e
at the GHCi prompt? We offer 2 different possibilities, described below. Each
considers this example, with -fprint-explicit-foralls enabled.  See also
https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0179-printing-foralls.rst

:type / TM_Inst

  In this mode, we report the type obained by instantiating only the
  /inferred/ quantifiers of e's type, solving constraints, and
  re-generalising, as discussed in #11376.

  > :type reverse
  reverse :: forall a. [a] -> [a]

  -- foo :: forall a f b. (Show a, Num b, Foldable f) => a -> f b -> String
  > :type +v foo @Int
  forall f b. (Show Int, Num b, Foldable f) => Int -> f b -> String

  Note that Show Int is still reported, because the solver never got a chance
  to see it.

:type +d / TM_Default

  This mode is for the benefit of users who wish to see instantiations
  of generalized types, and in particular to instantiate Foldable and
  Traversable.  In this mode, all type variables (inferred or
  specified) are instantiated.  Because GHCi uses
  -XExtendedDefaultRules, this means that Foldable and Traversable are
  defaulted.

  > :type +d reverse
  reverse :: forall {a}. [a] -> [a]

  -- foo :: forall a f b. (Show a, Num b, Foldable f) => a -> f b -> String
  > :type +d foo @Int
  Int -> [Integer] -> String

  Note that this mode can sometimes lead to a type error, if a type variable is
  used with a defaultable class but cannot actually be defaulted:

  bar :: (Num a, Monoid a) => a -> a
  > :type +d bar
  ** error **

  The error arises because GHC tries to default a but cannot find a concrete
  type in the defaulting list that is both Num and Monoid. (If this list is
  modified to include an element that is both Num and Monoid, the defaulting
  would succeed, of course.)

  Note that the variables and constraints are reordered here, because this
  is possible during regeneralization. Also note that the variables are
  reported as Inferred instead of Specified.

Note [Kind-generalise in tcRnType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We switch on PolyKinds when kind-checking a user type, so that we will
kind-generalise the type, even when PolyKinds is not otherwise on.
This gives the right default behaviour at the GHCi prompt, where if
you say ":k T", and T has a polymorphic kind, you'd like to see that
polymorphism. Of course.  If T isn't kind-polymorphic you won't get
anything unexpected, but the apparent *loss* of polymorphism, for
types that you know are polymorphic, is quite surprising.  See Trac
#7688 for a discussion.

Note that the goal is to generalise the *kind of the type*, not
the type itself! Example:
  ghci> data SameKind :: k -> k -> Type
  ghci> :k SameKind _

We want to get `k -> Type`, not `Any -> Type`, which is what we would
get without kind-generalisation. Note that `:k SameKind` is OK, as
GHC will not instantiate SameKind here, and so we see its full kind
of `forall k. k -> k -> Type`.

************************************************************************
*                                                                      *
                 tcRnDeclsi
*                                                                      *
************************************************************************

tcRnDeclsi exists to allow class, data, and other declarations in GHCi.
-}

tcRnDeclsi :: HscEnv
           -> [LHsDecl GhcPs]
           -> IO (Messages, Maybe TcGblEnv)
tcRnDeclsi hsc_env local_decls
  = runTcInteractive hsc_env $
    tcRnSrcDecls False local_decls Nothing

externaliseAndTidyId :: Module -> Id -> TcM Id
externaliseAndTidyId this_mod id
  = do { name' <- externaliseName this_mod (idName id)
       ; return $ globaliseId id
                     `setIdName` name'
                     `setIdType` tidyTopType (idType id) }


{-
************************************************************************
*                                                                      *
        More GHCi stuff, to do with browsing and getting info
*                                                                      *
************************************************************************
-}

-- | ASSUMES that the module is either in the 'HomePackageTable' or is
-- a package module with an interface on disk.  If neither of these is
-- true, then the result will be an error indicating the interface
-- could not be found.
getModuleInterface :: HscEnv -> Module -> IO (Messages, Maybe ModIface)
getModuleInterface hsc_env mod
  = runTcInteractive hsc_env $
    loadModuleInterface (text "getModuleInterface") mod

tcRnLookupRdrName :: HscEnv -> Located RdrName
                  -> IO (Messages, Maybe [Name])
-- ^ Find all the Names that this RdrName could mean, in GHCi
tcRnLookupRdrName hsc_env (L loc rdr_name)
  = runTcInteractive hsc_env $
    setSrcSpan loc           $
    do {   -- If the identifier is a constructor (begins with an
           -- upper-case letter), then we need to consider both
           -- constructor and type class identifiers.
         let rdr_names = dataTcOccs rdr_name
       ; names_s <- mapM lookupInfoOccRn rdr_names
       ; let names = concat names_s
       ; when (null names) (addErrTc (text "Not in scope:" <+> quotes (ppr rdr_name)))
       ; return names }

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
            -> IO ( Messages
                  , Maybe (TyThing, Fixity, [ClsInst], [FamInst], SDoc))

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
       ; let info = lookupKnownNameInfo name
       ; return (thing, fixity, cls_insts, fam_insts, info) }


-- Lookup all class and family instances for a type constructor.
--
-- This function filters all instances in the type environment, so there
-- is a lot of duplicated work if it is called many times in the same
-- type environment. If this becomes a problem, the NameEnv computed
-- in GHC.getNameToInstancesIndex could be cached in TcM and both functions
-- could be changed to consult that index.
lookupInsts :: TyThing -> TcM ([ClsInst],[FamInst])
lookupInsts (ATyCon tc)
  = do  { InstEnvs { ie_global = pkg_ie, ie_local = home_ie, ie_visible = vis_mods } <- tcGetInstEnvs
        ; (pkg_fie, home_fie) <- tcGetFamInstEnvs
                -- Load all instances for all classes that are
                -- in the type environment (which are all the ones
                -- we've seen in any interface file so far)

          -- Return only the instances relevant to the given thing, i.e.
          -- the instances whose head contains the thing's name.
        ; let cls_insts =
                 [ ispec        -- Search all
                 | ispec <- instEnvElts home_ie ++ instEnvElts pkg_ie
                 , instIsVisible vis_mods ispec
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
  = initIfaceTcRn $
    mapM_ (loadSysInterface doc) (moduleSetElts (mkModuleSet unqual_mods))
  where
    home_unit = hsc_home_unit hsc_env

    unqual_mods = [ nameModule name
                  | gre <- globalRdrEnvElts (ic_rn_gbl_env ictxt)
                  , let name = greMangledName gre
                  , nameIsFromExternalPackage home_unit name
                  , isTcOcc (nameOccName name)   -- Types and classes only
                  , unQualOK gre ]               -- In scope unqualified
    doc = text "Need interface for module whose export(s) are in scope unqualified"



{-
************************************************************************
*                                                                      *
                Debugging output
      This is what happens when you do -ddump-types
*                                                                      *
************************************************************************
-}

-- | Dump, with a banner, if -ddump-rn
rnDump :: (Outputable a, Data a) => a -> TcRn ()
rnDump rn = dumpOptTcRn Opt_D_dump_rn "Renamer" FormatHaskell (ppr rn)

tcDump :: TcGblEnv -> TcRn ()
tcDump env
 = do { dflags <- getDynFlags ;
        unit_state <- hsc_units <$> getTopEnv ;

        -- Dump short output if -ddump-types or -ddump-tc
        when (dopt Opt_D_dump_types dflags || dopt Opt_D_dump_tc dflags)
          (dumpTcRn True (dumpOptionsFromFlag Opt_D_dump_types)
            "" FormatText (pprWithUnitState unit_state short_dump)) ;

        -- Dump bindings if -ddump-tc
        dumpOptTcRn Opt_D_dump_tc "Typechecker" FormatHaskell full_dump;

        -- Dump bindings as an hsSyn AST if -ddump-tc-ast
        dumpOptTcRn Opt_D_dump_tc_ast "Typechecker AST" FormatHaskell ast_dump
   }
  where
    short_dump = pprTcGblEnv env
    full_dump  = pprLHsBinds (tcg_binds env)
        -- NB: foreign x-d's have undefined's in their types;
        --     hence can't show the tc_fords
    ast_dump = showAstData NoBlankSrcSpan (tcg_binds env)

-- It's unpleasant having both pprModGuts and pprModDetails here
pprTcGblEnv :: TcGblEnv -> SDoc
pprTcGblEnv (TcGblEnv { tcg_type_env  = type_env,
                        tcg_insts     = insts,
                        tcg_fam_insts = fam_insts,
                        tcg_rules     = rules,
                        tcg_imports   = imports })
  = getPprDebug $ \debug ->
    vcat [ ppr_types debug type_env
         , ppr_tycons debug fam_insts type_env
         , ppr_datacons debug type_env
         , ppr_patsyns type_env
         , ppr_insts insts
         , ppr_fam_insts fam_insts
         , ppr_rules rules
         , text "Dependent modules:" <+>
                pprUFM (imp_dep_mods imports) (ppr . sort)
         , text "Dependent packages:" <+>
                ppr (S.toList $ imp_dep_pkgs imports)]
                -- The use of sort is just to reduce unnecessary
                -- wobbling in testsuite output

ppr_rules :: [LRuleDecl GhcTc] -> SDoc
ppr_rules rules
  = ppUnless (null rules) $
    hang (text "RULES")
       2 (vcat (map ppr rules))

ppr_types :: Bool -> TypeEnv -> SDoc
ppr_types debug type_env
  = ppr_things "TYPE SIGNATURES" ppr_sig
             (sortBy (comparing getOccName) ids)
  where
    ids = [id | id <- typeEnvIds type_env, want_sig id]
    want_sig id
      | debug     = True
      | otherwise = hasTopUserName id
                    && case idDetails id of
                         VanillaId    -> True
                         RecSelId {}  -> True
                         ClassOpId {} -> True
                         FCallId {}   -> True
                         _            -> False
             -- Data cons (workers and wrappers), pattern synonyms,
             -- etc are suppressed (unless -dppr-debug),
             -- because they appear elsewhere

    ppr_sig id = hang (pprPrefixOcc id <+> dcolon) 2 (ppr (tidyTopType (idType id)))

ppr_tycons :: Bool -> [FamInst] -> TypeEnv -> SDoc
ppr_tycons debug fam_insts type_env
  = vcat [ ppr_things "TYPE CONSTRUCTORS" ppr_tc tycons
         , ppr_things "COERCION AXIOMS" ppr_ax
                      (typeEnvCoAxioms type_env) ]
  where
    fi_tycons = famInstsRepTyCons fam_insts

    tycons = sortBy (comparing getOccName) $
             [tycon | tycon <- typeEnvTyCons type_env
                    , want_tycon tycon]
             -- Sort by OccName to reduce unnecessary changes
    want_tycon tycon | debug      = True
                     | otherwise  = isExternalName (tyConName tycon) &&
                                    not (tycon `elem` fi_tycons)
    ppr_tc tc
       = vcat [ hang (ppr (tyConFlavour tc) <+> pprPrefixOcc (tyConName tc)
                      <> braces (ppr (tyConArity tc)) <+> dcolon)
                   2 (ppr (tidyTopType (tyConKind tc)))
              , nest 2 $
                ppWhen show_roles $
                text "roles" <+> (sep (map ppr roles)) ]
       where
         show_roles = debug || not (all (== boring_role) roles)
         roles = tyConRoles tc
         boring_role | isClassTyCon tc = Nominal
                     | otherwise       = Representational
            -- Matches the choice in GHC.Iface.Syntax, calls to pprRoles

    ppr_ax ax = ppr (coAxiomToIfaceDecl ax)
      -- We go via IfaceDecl rather than using pprCoAxiom
      -- This way we get the full axiom (both LHS and RHS) with
      -- wildcard binders tidied to _1, _2, etc.

ppr_datacons :: Bool -> TypeEnv -> SDoc
ppr_datacons debug type_env
  = ppr_things "DATA CONSTRUCTORS" ppr_dc wanted_dcs
      -- The filter gets rid of class data constructors
  where
    ppr_dc dc = sdocOption sdocLinearTypes (\show_linear_types ->
                ppr dc <+> dcolon <+> ppr (dataConDisplayType show_linear_types dc))
    all_dcs    = typeEnvDataCons type_env
    wanted_dcs | debug     = all_dcs
               | otherwise = filterOut is_cls_dc all_dcs
    is_cls_dc dc = isClassTyCon (dataConTyCon dc)

ppr_patsyns :: TypeEnv -> SDoc
ppr_patsyns type_env
  = ppr_things "PATTERN SYNONYMS" ppr_ps
               (typeEnvPatSyns type_env)
  where
    ppr_ps ps = pprPrefixOcc ps <+> dcolon <+> pprPatSynType ps

ppr_insts :: [ClsInst] -> SDoc
ppr_insts ispecs
  = ppr_things "CLASS INSTANCES" pprInstance ispecs

ppr_fam_insts :: [FamInst] -> SDoc
ppr_fam_insts fam_insts
  = ppr_things "FAMILY INSTANCES" pprFamInst fam_insts

ppr_things :: String -> (a -> SDoc) -> [a] -> SDoc
ppr_things herald ppr_one things
  | null things = empty
  | otherwise   = text herald $$ nest 2 (vcat (map ppr_one things))

hasTopUserName :: NamedThing x => x -> Bool
-- A top-level thing whose name is not "derived"
-- Thus excluding things like $tcX, from Typeable boilerplate
-- and C:Coll from class-dictionary data constructors
hasTopUserName x
  = isExternalName name && not (isDerivedOccName (nameOccName name))
  where
    name = getName x

{-
********************************************************************************

Type Checker Plugins

********************************************************************************
-}

withTcPlugins :: HscEnv -> TcM a -> TcM a
withTcPlugins hsc_env m =
    case getTcPlugins hsc_env of
       []      -> m  -- Common fast case
       plugins -> do
                ev_binds_var <- newTcEvBinds
                (solvers,stops) <- unzip `fmap` mapM (startPlugin ev_binds_var) plugins
                -- This ensures that tcPluginStop is called even if a type
                -- error occurs during compilation (Fix of #10078)
                eitherRes <- tryM $
                  updGblEnv (\e -> e { tcg_tc_plugins = solvers }) m
                mapM_ (flip runTcPluginM ev_binds_var) stops
                case eitherRes of
                  Left _ -> failM
                  Right res -> return res
  where
  startPlugin ev_binds_var (TcPlugin start solve stop) =
    do s <- runTcPluginM start ev_binds_var
       return (solve s, stop s)

getTcPlugins :: HscEnv -> [GHC.Tc.Utils.Monad.TcPlugin]
getTcPlugins hsc_env = catMaybes $ mapPlugins hsc_env (\p args -> tcPlugin p args)


withHoleFitPlugins :: HscEnv -> TcM a -> TcM a
withHoleFitPlugins hsc_env m =
  case getHfPlugins hsc_env of
    [] -> m  -- Common fast case
    plugins -> do (plugins,stops) <- unzip `fmap` mapM startPlugin plugins
                  -- This ensures that hfPluginStop is called even if a type
                  -- error occurs during compilation.
                  eitherRes <- tryM $
                    updGblEnv (\e -> e { tcg_hf_plugins = plugins }) m
                  sequence_ stops
                  case eitherRes of
                    Left _ -> failM
                    Right res -> return res
  where
    startPlugin (HoleFitPluginR init plugin stop) =
      do ref <- init
         return (plugin ref, stop ref)

getHfPlugins :: HscEnv -> [HoleFitPluginR]
getHfPlugins hsc_env =
  catMaybes $ mapPlugins hsc_env (\p args -> holeFitPlugin p args)


runRenamerPlugin :: TcGblEnv
                 -> HsGroup GhcRn
                 -> TcM (TcGblEnv, HsGroup GhcRn)
runRenamerPlugin gbl_env hs_group = do
    hsc_env <- getTopEnv
    withPlugins hsc_env
      (\p opts (e, g) -> ( mark_plugin_unsafe (hsc_dflags hsc_env)
                            >> renamedResultAction p opts e g))
      (gbl_env, hs_group)


-- XXX: should this really be a Maybe X?  Check under which circumstances this
-- can become a Nothing and decide whether this should instead throw an
-- exception/signal an error.
type RenamedStuff =
        (Maybe (HsGroup GhcRn, [LImportDecl GhcRn], Maybe [(LIE GhcRn, Avails)],
                Maybe LHsDocString))

-- | Extract the renamed information from TcGblEnv.
getRenamedStuff :: TcGblEnv -> RenamedStuff
getRenamedStuff tc_result
  = fmap (\decls -> ( decls, tcg_rn_imports tc_result
                    , tcg_rn_exports tc_result, tcg_doc_hdr tc_result ) )
         (tcg_rn_decls tc_result)

runTypecheckerPlugin :: ModSummary -> TcGblEnv -> TcM TcGblEnv
runTypecheckerPlugin sum gbl_env = do
    hsc_env <- getTopEnv
    withPlugins hsc_env
      (\p opts env -> mark_plugin_unsafe (hsc_dflags hsc_env)
                        >> typeCheckResultAction p opts sum env)
      gbl_env

mark_plugin_unsafe :: DynFlags -> TcM ()
mark_plugin_unsafe dflags = unless (gopt Opt_PluginTrustworthy dflags) $
  recordUnsafeInfer pluginUnsafe
  where
    unsafeText = "Use of plugins makes the module unsafe"
    pluginUnsafe = unitBag ( mkPlainWarnMsg dflags noSrcSpan
                                   (Outputable.text unsafeText) )
