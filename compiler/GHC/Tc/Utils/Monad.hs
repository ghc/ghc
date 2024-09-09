{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
(c) The University of Glasgow 2006

-}

-- | Functions for working with the typechecker environment (setters,
-- getters...).
module GHC.Tc.Utils.Monad(
  -- * Initialisation
  initTc, initTcWithGbl, initTcInteractive, initTcRnIf,

  -- * Simple accessors
  discardResult,
  getTopEnv, updTopEnv, updTopEnvIO, getGblEnv, updGblEnv,
  setGblEnv, getLclEnv, updLclEnv, updLclCtxt, setLclEnv, restoreLclEnv,
  updTopFlags,
  getEnvs, setEnvs, updEnvs, restoreEnvs,
  xoptM, doptM, goptM, woptM,
  setXOptM, setWOptM,
  unsetXOptM, unsetGOptM, unsetWOptM,
  whenDOptM, whenGOptM, whenWOptM,
  whenXOptM, unlessXOptM,
  getGhcMode,
  withoutDynamicNow,
  getEpsVar,
  getEps,
  updateEps, updateEps_,
  getHpt, getEpsAndHug,

  -- * Arrow scopes
  newArrowScope, escapeArrowScope,

  -- * Unique supply
  newUnique, newUniqueSupply, newName, newNameAt, cloneLocalName,
  newSysName, newSysLocalId, newSysLocalIds,

  -- * Accessing input/output
  newTcRef, readTcRef, writeTcRef, updTcRef, updTcRefM,

  -- * Debugging
  traceTc, traceRn, traceOptTcRn, dumpOptTcRn,
  dumpTcRn,
  getNamePprCtx,
  printForUserTcRn,
  traceIf, traceOptIf,
  debugTc,

  -- * Typechecker global environment
  getIsGHCi, getGHCiMonad, getInteractivePrintName,
  tcHscSource, tcIsHsBootOrSig, tcIsHsig, tcSelfBootInfo, getGlobalRdrEnv,
  getRdrEnvs, getImports,
  getFixityEnv, extendFixityEnv,
  getDeclaredDefaultTys,
  addDependentFiles,

  -- * Error management
  getSrcSpanM, setSrcSpan, setSrcSpanA, addLocM,
  inGeneratedCode, setInGeneratedCode,
  wrapLocM, wrapLocFstM, wrapLocFstMA, wrapLocSndM, wrapLocSndMA, wrapLocM_,
  wrapLocMA_,wrapLocMA,
  getErrsVar, setErrsVar,
  addErr,
  failWith, failAt,
  addErrAt, addErrs,
  checkErr, checkErrAt,
  addMessages,
  discardWarnings, mkDetailedMessage,

  -- * Usage environment
  tcCollectingUsage, tcScalingUsage, tcEmitBindingUsage,

  -- * Shared error message stuff: renamer and typechecker
  recoverM, mapAndRecoverM, mapAndReportM, foldAndRecoverM,
  attemptM, tryTc,
  askNoErrs, discardErrs,
  tryTcDiscardingErrs,
  tryTcDiscardingErrs',
  checkNoErrs, whenNoErrs,
  ifErrsM, failIfErrsM,

  -- * Context management for the type checker
  getErrCtxt, setErrCtxt, addErrCtxt, addErrCtxtM, addLandmarkErrCtxt,
  addLandmarkErrCtxtM, popErrCtxt, getCtLocM, setCtLocM, mkCtLocEnv,

  -- * Diagnostic message generation (type checker)
  addErrTc,
  addErrTcM,
  failWithTc, failWithTcM,
  checkTc, checkTcM,
  checkJustTc, checkJustTcM,
  failIfTc, failIfTcM,
  mkErrCtxt,
  addTcRnDiagnostic, addDetailedDiagnostic,
  mkTcRnMessage, reportDiagnostic, reportDiagnostics,
  warnIf, diagnosticTc, diagnosticTcM,
  addDiagnosticTc, addDiagnosticTcM, addDiagnostic, addDiagnosticAt,

  -- * Type constraints
  newTcEvBinds, newNoTcEvBinds, cloneEvBindsVar,
  addTcEvBind, addTcEvBinds, addTopEvBinds,
  getTcEvTyCoVars, getTcEvBindsMap, setTcEvBindsMap,
  chooseUniqueOccTc,
  getConstraintVar, setConstraintVar,
  emitConstraints, emitStaticConstraints, emitSimple, emitSimples,
  emitImplication, emitImplications, ensureReflMultiplicityCo,
  emitDelayedErrors, emitHole, emitHoles, emitNotConcreteError,
  discardConstraints, captureConstraints, tryCaptureConstraints,
  pushLevelAndCaptureConstraints,
  pushTcLevelM_, pushTcLevelM,
  getTcLevel, setTcLevel, isTouchableTcM,
  getLclTypeEnv, setLclTypeEnv,
  traceTcConstraints,
  emitNamedTypeHole, IsExtraConstraint(..), emitAnonTypeHole,

  -- * Template Haskell context
  recordThUse, recordThNeededRuntimeDeps,
  keepAlive, getThLevel, getCurrentAndBindLevel, setThLevel,
  addModFinalizersWithLclEnv,

  -- * Safe Haskell context
  recordUnsafeInfer, finalSafeMode, fixSafeInstances,

  -- * Stuff for the renamer's local env
  getLocalRdrEnv, setLocalRdrEnv,

  -- * Stuff for interface decls
  mkIfLclEnv,
  initIfaceTcRn,
  initIfaceCheck,
  initIfaceLcl,
  initIfaceLclWithSubst,
  initIfaceLoad,
  initIfaceLoadModule,
  getIfModule,
  failIfM,
  forkM,
  setImplicitEnvM,

  withException, withIfaceErr,

  -- * Stuff for cost centres.
  getCCIndexM, getCCIndexTcM,

  -- * Zonking
  liftZonkM, newZonkAnyType,

  -- * Complete matches
  localAndImportedCompleteMatches, getCompleteMatchesTcM,

  -- * Types etc.
  module GHC.Tc.Types,
  module GHC.Data.IOEnv
  ) where

import GHC.Prelude


import GHC.Builtin.Names
import GHC.Builtin.Types( zonkAnyTyCon )

import GHC.Tc.Errors.Types
import GHC.Tc.Types     -- Re-export all
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.CtLoc
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.LclEnv
import GHC.Tc.Types.Origin
import GHC.Tc.Types.TcRef
import GHC.Tc.Types.TH
import GHC.Tc.Utils.TcType
import GHC.Tc.Zonk.TcType

import GHC.Hs hiding (LIE)

import GHC.Unit
import GHC.Unit.Env
import GHC.Unit.External
import GHC.Unit.Module.Warnings
import GHC.Unit.Home.PackageTable

import GHC.Core.UsageEnv
import GHC.Core.Multiplicity
import GHC.Core.InstEnv
import GHC.Core.FamInstEnv
import GHC.Core.Type( mkNumLitTy )

import GHC.Driver.Env
import GHC.Driver.Env.KnotVars
import GHC.Driver.Session
import GHC.Driver.Config.Diagnostic

import GHC.Iface.Errors.Types
import GHC.Iface.Errors.Ppr

import GHC.Linker.Types

import GHC.Runtime.Context

import GHC.Data.IOEnv -- Re-export all
import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Data.Maybe

import GHC.Utils.Outputable as Outputable
import GHC.Utils.Error
import GHC.Utils.Panic
import GHC.Utils.Constants (debugIsOn)
import GHC.Utils.Logger
import qualified GHC.Data.Strict as Strict
import qualified Data.Set as Set

import GHC.Types.Error
import GHC.Types.DefaultEnv ( DefaultEnv, emptyDefaultEnv )
import GHC.Types.Fixity.Env
import GHC.Types.Name.Reader
import GHC.Types.Name
import GHC.Types.SafeHaskell
import GHC.Types.Id
import GHC.Types.TypeEnv
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.SrcLoc
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.Name.Ppr
import GHC.Types.Unique.FM ( emptyUFM )
import GHC.Types.Unique.DFM
import GHC.Types.Unique.Supply
import GHC.Types.Annotations
import GHC.Types.Basic( TopLevelFlag(..), TypeOrKind(..) )
import GHC.Types.CostCentre.State
import GHC.Types.SourceFile

import qualified GHC.LanguageExtensions as LangExt

import Data.IORef
import Control.Monad

import qualified Data.Map as Map
import GHC.Core.Coercion (isReflCo)


{-
************************************************************************
*                                                                      *
                        initTc
*                                                                      *
************************************************************************
-}

-- | Setup the initial typechecking environment
initTc :: HscEnv
       -> HscSource
       -> Bool          -- True <=> retain renamed syntax trees
       -> Module
       -> RealSrcSpan
       -> TcM r
       -> IO (Messages TcRnMessage, Maybe r)
                -- Nothing => error thrown by the thing inside
                -- (error messages should have been printed already)

initTc hsc_env hsc_src keep_rn_syntax mod loc do_this
 = do { keep_var     <- newIORef emptyNameSet ;
        used_gre_var <- newIORef [] ;
        th_var       <- newIORef False ;
        infer_var    <- newIORef True ;
        infer_reasons_var <- newIORef emptyMessages ;
        dfun_n_var   <- newIORef emptyOccSet ;
        zany_n_var   <- newIORef 0 ;
        let { type_env_var = hsc_type_env_vars hsc_env };

        dependent_files_var <- newIORef [] ;
        static_wc_var       <- newIORef emptyWC ;
        cc_st_var           <- newIORef newCostCentreState ;
        th_topdecls_var      <- newIORef [] ;
        th_foreign_files_var <- newIORef [] ;
        th_topnames_var      <- newIORef emptyNameSet ;
        th_modfinalizers_var <- newIORef [] ;
        th_coreplugins_var <- newIORef [] ;
        th_state_var         <- newIORef Map.empty ;
        th_remote_state_var  <- newIORef Nothing ;
        th_docs_var          <- newIORef Map.empty ;
        th_needed_deps_var   <- newIORef ([], emptyUDFM) ;
        next_wrapper_num     <- newIORef emptyModuleEnv ;
        let {
             -- bangs to avoid leaking the env (#19356)
             !dflags = hsc_dflags hsc_env ;
             !mhome_unit = hsc_home_unit_maybe hsc_env;
             !logger = hsc_logger hsc_env ;

             maybe_rn_syntax :: forall a. a -> Maybe a ;
             maybe_rn_syntax empty_val
                | logHasDumpFlag logger Opt_D_dump_rn_ast = Just empty_val

                | gopt Opt_WriteHie dflags       = Just empty_val

                  -- We want to serialize the documentation in the .hi-files,
                  -- and need to extract it from the renamed syntax first.
                  -- See 'GHC.HsToCore.Docs.extractDocs'.
                | gopt Opt_Haddock dflags       = Just empty_val

                | keep_rn_syntax                = Just empty_val
                | otherwise                     = Nothing ;

             gbl_env = TcGblEnv {
                tcg_th_topdecls      = th_topdecls_var,
                tcg_th_foreign_files = th_foreign_files_var,
                tcg_th_topnames      = th_topnames_var,
                tcg_th_modfinalizers = th_modfinalizers_var,
                tcg_th_coreplugins = th_coreplugins_var,
                tcg_th_state         = th_state_var,
                tcg_th_remote_state  = th_remote_state_var,
                tcg_th_docs          = th_docs_var,

                tcg_mod            = mod,
                tcg_semantic_mod   = homeModuleInstantiation mhome_unit mod,
                tcg_src            = hsc_src,
                tcg_rdr_env        = emptyGlobalRdrEnv,
                tcg_fix_env        = emptyNameEnv,
                tcg_default        = emptyDefaultEnv,
                tcg_default_exports = emptyDefaultEnv,
                tcg_type_env       = emptyNameEnv,
                tcg_type_env_var   = type_env_var,
                tcg_inst_env       = emptyInstEnv,
                tcg_fam_inst_env   = emptyFamInstEnv,
                tcg_ann_env        = emptyAnnEnv,
                tcg_complete_match_env = [],
                tcg_th_used        = th_var,
                tcg_th_needed_deps = th_needed_deps_var,
                tcg_exports        = [],
                tcg_imports        = emptyImportAvails,
                tcg_import_decls   = [],
                tcg_used_gres     = used_gre_var,
                tcg_dus            = emptyDUs,

                tcg_rn_imports     = [],
                tcg_rn_exports     =
                    if hsc_src == HsigFile
                        -- Always retain renamed syntax, so that we can give
                        -- better errors.  (TODO: how?)
                        then Just []
                        else maybe_rn_syntax [],
                tcg_rn_decls       = maybe_rn_syntax emptyRnGroup,
                tcg_tr_module      = Nothing,
                tcg_binds          = emptyLHsBinds,
                tcg_imp_specs      = [],
                tcg_sigs           = emptyNameSet,
                tcg_ksigs          = emptyNameSet,
                tcg_ev_binds       = emptyBag,
                tcg_warns          = emptyWarn,
                tcg_anns           = [],
                tcg_tcs            = [],
                tcg_insts          = [],
                tcg_fam_insts      = [],
                tcg_rules          = [],
                tcg_fords          = [],
                tcg_patsyns        = [],
                tcg_merged         = [],
                tcg_dfun_n         = dfun_n_var,
                tcg_zany_n         = zany_n_var,
                tcg_keep           = keep_var,
                tcg_hdr_info        = (Nothing,Nothing),
                tcg_main           = Nothing,
                tcg_self_boot      = NoSelfBoot,
                tcg_safe_infer     = infer_var,
                tcg_safe_infer_reasons = infer_reasons_var,
                tcg_dependent_files = dependent_files_var,
                tcg_tc_plugin_solvers   = [],
                tcg_tc_plugin_rewriters = emptyUFM,
                tcg_defaulting_plugins  = [],
                tcg_hf_plugins     = [],
                tcg_top_loc        = loc,
                tcg_static_wc      = static_wc_var,
                tcg_complete_matches = [],
                tcg_cc_st          = cc_st_var,
                tcg_next_wrapper_num = next_wrapper_num
             } ;
        } ;

        -- OK, here's the business end!
        initTcWithGbl hsc_env gbl_env loc do_this
    }

-- | Run a 'TcM' action in the context of an existing 'GblEnv'.
initTcWithGbl :: HscEnv
              -> TcGblEnv
              -> RealSrcSpan
              -> TcM r
              -> IO (Messages TcRnMessage, Maybe r)
initTcWithGbl hsc_env gbl_env loc do_this
 = do { lie_var      <- newIORef emptyWC
      ; errs_var     <- newIORef emptyMessages
      ; usage_var    <- newIORef zeroUE
      ; let lcl_env = TcLclEnv {
                tcl_lcl_ctxt   = TcLclCtxt {
                tcl_loc        = loc,
                -- tcl_loc should be over-ridden very soon!
                tcl_in_gen_code = False,
                tcl_ctxt       = [],
                tcl_rdr        = emptyLocalRdrEnv,
                tcl_th_ctxt    = topLevel,
                tcl_th_bndrs   = emptyNameEnv,
                tcl_arrow_ctxt = NoArrowCtxt,
                tcl_env        = emptyNameEnv,
                tcl_bndrs      = [],
                tcl_tclvl      = topTcLevel
                },
                tcl_usage      = usage_var,
                tcl_lie        = lie_var,
                tcl_errs       = errs_var
                }

      ; maybe_res <- initTcRnIf 'a' hsc_env gbl_env lcl_env $
                     do { r <- tryM do_this
                        ; case r of
                          Right res -> return (Just res)
                          Left _    -> return Nothing }

      -- Check for unsolved constraints
      -- If we succeed (maybe_res = Just r), there should be
      -- no unsolved constraints.  But if we exit via an
      -- exception (maybe_res = Nothing), we may have skipped
      -- solving, so don't panic then (#13466)
      ; lie <- readIORef (tcl_lie lcl_env)
      ; when (isJust maybe_res && not (isEmptyWC lie)) $
        pprPanic "initTc: unsolved constraints" (ppr lie)

        -- Collect any error messages
      ; msgs <- readIORef (tcl_errs lcl_env)

      ; let { final_res | errorsFound msgs = Nothing
                        | otherwise        = maybe_res }

      ; return (msgs, final_res)
      }

initTcInteractive :: HscEnv -> TcM a -> IO (Messages TcRnMessage, Maybe a)
-- Initialise the type checker monad for use in GHCi
initTcInteractive hsc_env thing_inside
  = initTc hsc_env HsSrcFile False
           (icInteractiveModule (hsc_IC hsc_env))
           (realSrcLocSpan interactive_src_loc)
           thing_inside
  where
    interactive_src_loc = mkRealSrcLoc (fsLit "<interactive>") 1 1

initTcRnIf :: Char              -- ^ Tag for unique supply
           -> HscEnv
           -> gbl -> lcl
           -> TcRnIf gbl lcl a
           -> IO a
initTcRnIf uniq_tag hsc_env gbl_env lcl_env thing_inside
   = do { let { env = Env { env_top = hsc_env,
                            env_ut  = uniq_tag,
                            env_gbl = gbl_env,
                            env_lcl = lcl_env} }

        ; runIOEnv env thing_inside
        }

{-
************************************************************************
*                                                                      *
                Simple accessors
*                                                                      *
************************************************************************
-}

discardResult :: TcM a -> TcM ()
discardResult a = a >> return ()

getTopEnv :: TcRnIf gbl lcl HscEnv
getTopEnv = do { env <- getEnv; return (env_top env) }

updTopEnv :: (HscEnv -> HscEnv) -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
updTopEnv upd = updEnv (\ env@(Env { env_top = top }) ->
                          env { env_top = upd top })

updTopEnvIO :: (HscEnv -> IO HscEnv) -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
updTopEnvIO upd = updEnvIO (\ env@(Env { env_top = top }) ->
                                upd top >>= \t' ->
                                pure env{ env_top = t' })

getGblEnv :: TcRnIf gbl lcl gbl
getGblEnv = do { Env{..} <- getEnv; return env_gbl }

updGblEnv :: (gbl -> gbl) -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
updGblEnv upd = updEnv (\ env@(Env { env_gbl = gbl }) ->
                          env { env_gbl = upd gbl })

setGblEnv :: gbl' -> TcRnIf gbl' lcl a -> TcRnIf gbl lcl a
setGblEnv gbl_env = updEnv (\ env -> env { env_gbl = gbl_env })

getLclEnv :: TcRnIf gbl lcl lcl
getLclEnv = do { Env{..} <- getEnv; return env_lcl }

updLclEnv :: (lcl -> lcl) -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
updLclEnv upd = updEnv (\ env@(Env { env_lcl = lcl }) ->
                          env { env_lcl = upd lcl })

updLclCtxt :: (TcLclCtxt -> TcLclCtxt) -> TcRnIf gbl TcLclEnv a -> TcRnIf gbl TcLclEnv a
updLclCtxt upd = updLclEnv (modifyLclCtxt upd)

setLclEnv :: lcl' -> TcRnIf gbl lcl' a -> TcRnIf gbl lcl a
setLclEnv lcl_env = updEnv (\ env -> env { env_lcl = lcl_env })

restoreLclEnv :: TcLclEnv -> TcRnIf gbl TcLclEnv a -> TcRnIf gbl TcLclEnv a
-- See Note [restoreLclEnv vs setLclEnv]
restoreLclEnv new_lcl_env = updLclEnv upd
  where
    upd old_lcl_env =  new_lcl_env { tcl_errs  = tcl_errs  old_lcl_env
                                   , tcl_lie   = tcl_lie   old_lcl_env
                                   , tcl_usage = tcl_usage old_lcl_env }

getEnvs :: TcRnIf gbl lcl (gbl, lcl)
getEnvs = do { env <- getEnv; return (env_gbl env, env_lcl env) }

setEnvs :: (gbl', lcl') -> TcRnIf gbl' lcl' a -> TcRnIf gbl lcl a
setEnvs (gbl_env, lcl_env) = setGblEnv gbl_env . setLclEnv lcl_env

updEnvs :: ((gbl,lcl) -> (gbl, lcl)) -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
updEnvs upd_envs = updEnv upd
  where
    upd env@(Env { env_gbl = gbl, env_lcl = lcl })
      = env { env_gbl = gbl', env_lcl = lcl' }
      where
        !(gbl', lcl') = upd_envs (gbl, lcl)

restoreEnvs :: (TcGblEnv, TcLclEnv) -> TcRn a -> TcRn a
-- See Note [restoreLclEnv vs setLclEnv]
restoreEnvs (gbl, lcl) = setGblEnv gbl . restoreLclEnv lcl

{- Note [restoreLclEnv vs setLclEnv]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the typechecker we use this idiom quite a lot
   do { (gbl_env, lcl_env) <- tcRnSrcDecls ...
      ; setGblEnv gbl_env $ setLclEnv lcl_env $
        more_stuff }

The `tcRnSrcDecls` extends the environments in `gbl_env` and `lcl_env`
which we then want to be in scope in `more stuff`.

The problem is that `lcl_env :: TcLclEnv` has an IORef for error
messages `tcl_errs`, and another for constraints (`tcl_lie`), and
another for Linear Haskell usage information (`tcl_usage`).  Now
suppose we change it a tiny bit
   do { (gbl_env, lcl_env) <- checkNoErrs $
                              tcRnSrcDecls ...
      ; setGblEnv gbl_env $ setLclEnv lcl_env $
        more_stuff }

That should be innocuous.  But *alas*, `checkNoErrs` gathers errors in
a fresh IORef *which is then captured in the returned `lcl_env`.  When
we do the `setLclEnv` we'll make that captured IORef into the place
where we gather error messages -- but no one is going to look at that!!!
This led to #19470 and #20981.

Solution: instead of setLclEnv use restoreLclEnv, which preserves from
the /parent/ context these mutable collection IORefs:
      tcl_errs, tcl_lie, tcl_usage
-}

-- Command-line flags

xoptM :: LangExt.Extension -> TcRnIf gbl lcl Bool
xoptM flag = xopt flag <$> getDynFlags

doptM :: DumpFlag -> TcRnIf gbl lcl Bool
doptM flag = do
  logger <- getLogger
  return (logHasDumpFlag logger flag)

goptM :: GeneralFlag -> TcRnIf gbl lcl Bool
goptM flag = gopt flag <$> getDynFlags

woptM :: WarningFlag -> TcRnIf gbl lcl Bool
woptM flag = wopt flag <$> getDynFlags

setXOptM :: LangExt.Extension -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
setXOptM flag = updTopFlags (\dflags -> xopt_set dflags flag)

setWOptM :: WarningFlag -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
setWOptM flag = updTopFlags (\dflags -> wopt_set dflags flag)

unsetXOptM :: LangExt.Extension -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
unsetXOptM flag = updTopFlags (\dflags -> xopt_unset dflags flag)

unsetGOptM :: GeneralFlag -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
unsetGOptM flag = updTopFlags (\dflags -> gopt_unset dflags flag)

unsetWOptM :: WarningFlag -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
unsetWOptM flag = updTopFlags (\dflags -> wopt_unset dflags flag)

-- | Do it flag is true
whenDOptM :: DumpFlag -> TcRnIf gbl lcl () -> TcRnIf gbl lcl ()
whenDOptM flag thing_inside = do b <- doptM flag
                                 when b thing_inside
{-# INLINE whenDOptM #-} -- see Note [INLINE conditional tracing utilities]


whenGOptM :: GeneralFlag -> TcRnIf gbl lcl () -> TcRnIf gbl lcl ()
whenGOptM flag thing_inside = do b <- goptM flag
                                 when b thing_inside
{-# INLINE whenGOptM #-} -- see Note [INLINE conditional tracing utilities]

whenWOptM :: WarningFlag -> TcRnIf gbl lcl () -> TcRnIf gbl lcl ()
whenWOptM flag thing_inside = do b <- woptM flag
                                 when b thing_inside
{-# INLINE whenWOptM #-} -- see Note [INLINE conditional tracing utilities]

whenXOptM :: LangExt.Extension -> TcRnIf gbl lcl () -> TcRnIf gbl lcl ()
whenXOptM flag thing_inside = do b <- xoptM flag
                                 when b thing_inside
{-# INLINE whenXOptM #-} -- see Note [INLINE conditional tracing utilities]

unlessXOptM :: LangExt.Extension -> TcRnIf gbl lcl () -> TcRnIf gbl lcl ()
unlessXOptM flag thing_inside = do b <- xoptM flag
                                   unless b thing_inside
{-# INLINE unlessXOptM #-} -- see Note [INLINE conditional tracing utilities]

getGhcMode :: TcRnIf gbl lcl GhcMode
getGhcMode = ghcMode <$> getDynFlags

withoutDynamicNow :: TcRnIf gbl lcl a -> TcRnIf gbl lcl a
withoutDynamicNow = updTopFlags (\dflags -> dflags { dynamicNow = False})

updTopFlags :: (DynFlags -> DynFlags) -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
updTopFlags f = updTopEnv (hscUpdateFlags f)

getEpsVar :: TcRnIf gbl lcl (TcRef ExternalPackageState)
getEpsVar = do
  env <- getTopEnv
  return (euc_eps (ue_eps (hsc_unit_env env)))

getEps :: TcRnIf gbl lcl ExternalPackageState
getEps = do { env <- getTopEnv; liftIO $ hscEPS env }

-- | Update the external package state.  Returns the second result of the
-- modifier function.
--
-- This is an atomic operation and forces evaluation of the modified EPS in
-- order to avoid space leaks.
updateEps :: (ExternalPackageState -> (ExternalPackageState, a))
          -> TcRnIf gbl lcl a
updateEps upd_fn = do
  traceIf (text "updating EPS")
  eps_var <- getEpsVar
  atomicUpdMutVar' eps_var upd_fn

-- | Update the external package state.
--
-- This is an atomic operation and forces evaluation of the modified EPS in
-- order to avoid space leaks.
updateEps_ :: (ExternalPackageState -> ExternalPackageState)
           -> TcRnIf gbl lcl ()
updateEps_ upd_fn = updateEps (\eps -> (upd_fn eps, ()))

getHpt :: TcRnIf gbl lcl HomePackageTable
getHpt = do { env <- getTopEnv; return (hsc_HPT env) }

getEpsAndHug :: TcRnIf gbl lcl (ExternalPackageState, HomeUnitGraph)
getEpsAndHug = do { env <- getTopEnv; eps <- liftIO $ hscEPS env
                  ; return (eps, hsc_HUG env) }

-- | A convenient wrapper for taking a @MaybeErr SDoc a@ and throwing
-- an exception if it is an error.
withException :: MonadIO m => SDocContext -> m (MaybeErr SDoc a) -> m a
withException ctx do_this = do
    r <- do_this
    case r of
        Failed err -> liftIO $ throwGhcExceptionIO (ProgramError (renderWithContext ctx err))
        Succeeded result -> return result

withIfaceErr :: MonadIO m => SDocContext -> m (MaybeErr MissingInterfaceError a) -> m a
withIfaceErr ctx do_this = do
    r <- do_this
    case r of
        Failed err -> do
          let opts = defaultDiagnosticOpts @IfaceMessage
              msg   = missingInterfaceErrorDiagnostic opts err
          liftIO $ throwGhcExceptionIO (ProgramError (renderWithContext ctx msg))
        Succeeded result -> return result

{-
************************************************************************
*                                                                      *
                Arrow scopes
*                                                                      *
************************************************************************
-}

newArrowScope :: TcM a -> TcM a
newArrowScope
  = updLclEnv $ \env ->
      modifyLclCtxt (\ctx -> ctx { tcl_arrow_ctxt = ArrowCtxt (getLclEnvRdrEnv env) (tcl_lie env) } ) env

-- Return to the stored environment (from the enclosing proc)
escapeArrowScope :: TcM a -> TcM a
escapeArrowScope
  = updLclEnv $ \ env ->
    case getLclEnvArrowCtxt env of
      NoArrowCtxt       -> env
      ArrowCtxt rdr_env lie -> env { tcl_lcl_ctxt = (tcl_lcl_ctxt env) { tcl_arrow_ctxt = NoArrowCtxt
                                                                       , tcl_rdr = rdr_env }
                                   , tcl_lie = lie }

{-
************************************************************************
*                                                                      *
                Unique supply
*                                                                      *
************************************************************************
-}

newUnique :: TcRnIf gbl lcl Unique
newUnique
 = do { env <- getEnv
      ; let tag = env_ut env
      ; liftIO $! uniqFromTag tag }

newUniqueSupply :: TcRnIf gbl lcl UniqSupply
newUniqueSupply
 = do { env <- getEnv
      ; let tag = env_ut env
      ; liftIO $! mkSplitUniqSupply tag }

cloneLocalName :: Name -> TcM Name
-- Make a fresh Internal name with the same OccName and SrcSpan
cloneLocalName name = newNameAt (nameOccName name) (nameSrcSpan name)

newName :: OccName -> TcM Name
newName occ = do { loc  <- getSrcSpanM
                 ; newNameAt occ loc }

newNameAt :: OccName -> SrcSpan -> TcM Name
newNameAt occ span
  = do { uniq <- newUnique
       ; return (mkInternalName uniq occ span) }

newSysName :: OccName -> TcRnIf gbl lcl Name
newSysName occ
  = do { uniq <- newUnique
       ; return (mkSystemName uniq occ) }

newSysLocalId :: FastString -> Mult -> TcType -> TcRnIf gbl lcl TcId
newSysLocalId fs w ty
  = do  { u <- newUnique
        ; return (mkSysLocal fs u w ty) }

newSysLocalIds :: FastString -> [Scaled TcType] -> TcRnIf gbl lcl [TcId]
newSysLocalIds fs tys
  = do  { us <- getUniquesM
        ; let mkId' n (Scaled w t) = mkSysLocal fs n w t
        ; return (zipWith mkId' us tys) }

instance MonadUnique (IOEnv (Env gbl lcl)) where
        getUniqueM = newUnique
        getUniqueSupplyM = newUniqueSupply

{-
************************************************************************
*                                                                      *
                Debugging
*                                                                      *
************************************************************************
-}

{- Note [INLINE conditional tracing utilities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general we want to optimise for the case where tracing is not enabled.
To ensure this happens, we ensure that traceTc and friends are inlined; this
ensures that the allocation of the document can be pushed into the tracing
path, keeping the non-traced path free of this extraneous work. For
instance, if we don't inline traceTc, we'll get

    let stuff_to_print = ...
    in traceTc "wombat" stuff_to_print

and the stuff_to_print thunk will be allocated in the "hot path", regardless
of tracing.  But if we INLINE traceTc we get

    let stuff_to_print = ...
    in if doTracing
         then emitTraceMsg "wombat" stuff_to_print
         else return ()

and then we float in:

    if doTracing
      then let stuff_to_print = ...
           in emitTraceMsg "wombat" stuff_to_print
      else return ()

Now stuff_to_print is allocated only in the "cold path".

Moreover, on the "cold" path, after the conditional, we want to inline
as /little/ as possible.  Performance doesn't matter here, and we'd like
to bloat the caller's code as little as possible.  So we put a NOINLINE
on 'emitTraceMsg'

See #18168.
-}

-- Typechecker trace
traceTc :: String -> SDoc -> TcRn ()
traceTc herald doc =
    labelledTraceOptTcRn Opt_D_dump_tc_trace herald doc
{-# INLINE traceTc #-} -- see Note [INLINE conditional tracing utilities]

-- Renamer Trace
traceRn :: String -> SDoc -> TcRn ()
traceRn herald doc =
    labelledTraceOptTcRn Opt_D_dump_rn_trace herald doc
{-# INLINE traceRn #-} -- see Note [INLINE conditional tracing utilities]

-- | Trace when a certain flag is enabled. This is like `traceOptTcRn`
-- but accepts a string as a label and formats the trace message uniformly.
labelledTraceOptTcRn :: DumpFlag -> String -> SDoc -> TcRn ()
labelledTraceOptTcRn flag herald doc =
  traceOptTcRn flag (formatTraceMsg herald doc)
{-# INLINE labelledTraceOptTcRn #-} -- see Note [INLINE conditional tracing utilities]

formatTraceMsg :: String -> SDoc -> SDoc
formatTraceMsg herald doc = hang (text herald) 2 doc

traceOptTcRn :: DumpFlag -> SDoc -> TcRn ()
traceOptTcRn flag doc =
  whenDOptM flag $
    dumpTcRn False flag "" FormatText doc
{-# INLINE traceOptTcRn #-} -- see Note [INLINE conditional tracing utilities]

-- | Dump if the given 'DumpFlag' is set.
dumpOptTcRn :: DumpFlag -> String -> DumpFormat -> SDoc -> TcRn ()
dumpOptTcRn flag title fmt doc =
  whenDOptM flag $
    dumpTcRn False flag title fmt doc
{-# INLINE dumpOptTcRn #-} -- see Note [INLINE conditional tracing utilities]

-- | Unconditionally dump some trace output
--
-- Certain tests (T3017, Roles3, T12763 etc.) expect part of the
-- output generated by `-ddump-types` to be in 'PprUser' style. However,
-- generally we want all other debugging output to use 'PprDump'
-- style. We 'PprUser' style if 'useUserStyle' is True.
--
dumpTcRn :: Bool -> DumpFlag -> String -> DumpFormat -> SDoc -> TcRn ()
dumpTcRn useUserStyle flag title fmt doc = do
  logger <- getLogger
  name_ppr_ctx <- getNamePprCtx
  real_doc <- wrapDocLoc doc
  let sty = if useUserStyle
              then mkUserStyle name_ppr_ctx AllTheWay
              else mkDumpStyle name_ppr_ctx
  liftIO $ logDumpFile logger sty flag title fmt real_doc

-- | Add current location if -dppr-debug
-- (otherwise the full location is usually way too much)
wrapDocLoc :: SDoc -> TcRn SDoc
wrapDocLoc doc = do
  logger <- getLogger
  if logHasDumpFlag logger Opt_D_ppr_debug
    then do
      loc <- getSrcSpanM
      return (mkLocMessage MCOutput loc doc)
    else
      return doc

getNamePprCtx :: TcRn NamePprCtx
getNamePprCtx
  = do { ptc <- initPromotionTickContext <$> getDynFlags
       ; rdr_env <- getGlobalRdrEnv
       ; hsc_env <- getTopEnv
       ; return $ mkNamePprCtx ptc (hsc_unit_env hsc_env) rdr_env }

-- | Like logInfoTcRn, but for user consumption
printForUserTcRn :: SDoc -> TcRn ()
printForUserTcRn doc = do
    logger <- getLogger
    name_ppr_ctx <- getNamePprCtx
    liftIO (printOutputForUser logger name_ppr_ctx doc)

{-
traceIf works in the TcRnIf monad, where no RdrEnv is
available.  Alas, they behave inconsistently with the other stuff;
e.g. are unaffected by -dump-to-file.
-}

traceIf :: SDoc -> TcRnIf m n ()
traceIf = traceOptIf Opt_D_dump_if_trace
{-# INLINE traceIf #-}
  -- see Note [INLINE conditional tracing utilities]

traceOptIf :: DumpFlag -> SDoc -> TcRnIf m n ()
traceOptIf flag doc
  = whenDOptM flag $ do   -- No RdrEnv available, so qualify everything
        logger <- getLogger
        liftIO (putMsg logger doc)
{-# INLINE traceOptIf #-}  -- see Note [INLINE conditional tracing utilities]

{-
************************************************************************
*                                                                      *
                Typechecker global environment
*                                                                      *
************************************************************************
-}

getIsGHCi :: TcRn Bool
getIsGHCi = do { mod <- getModule
               ; return (isInteractiveModule mod) }

getGHCiMonad :: TcRn Name
getGHCiMonad = do { hsc <- getTopEnv; return (ic_monad $ hsc_IC hsc) }

getInteractivePrintName :: TcRn Name
getInteractivePrintName = do { hsc <- getTopEnv; return (ic_int_print $ hsc_IC hsc) }

tcIsHsBootOrSig :: TcRn Bool
tcIsHsBootOrSig = isHsBootOrSig <$> tcHscSource

tcHscSource :: TcRn HscSource
tcHscSource = do { env <- getGblEnv; return (tcg_src env)}

tcIsHsig :: TcRn Bool
tcIsHsig = do { env <- getGblEnv; return (isHsigFile (tcg_src env)) }

tcSelfBootInfo :: TcRn SelfBootInfo
tcSelfBootInfo = do { env <- getGblEnv; return (tcg_self_boot env) }

getGlobalRdrEnv :: TcRn GlobalRdrEnv
getGlobalRdrEnv = do { env <- getGblEnv; return (tcg_rdr_env env) }

getRdrEnvs :: TcRn (GlobalRdrEnv, LocalRdrEnv)
getRdrEnvs = do { (gbl,lcl) <- getEnvs; return (tcg_rdr_env gbl, getLclEnvRdrEnv lcl) }

getImports :: TcRn ImportAvails
getImports = do { env <- getGblEnv; return (tcg_imports env) }

getFixityEnv :: TcRn FixityEnv
getFixityEnv = do { env <- getGblEnv; return (tcg_fix_env env) }

extendFixityEnv :: [(Name,FixItem)] -> RnM a -> RnM a
extendFixityEnv new_bit
  = updGblEnv (\env@(TcGblEnv { tcg_fix_env = old_fix_env }) ->
                env {tcg_fix_env = extendNameEnvList old_fix_env new_bit})

getDeclaredDefaultTys :: TcRn DefaultEnv
getDeclaredDefaultTys = do { env <- getGblEnv; return (tcg_default env) }

addDependentFiles :: [FilePath] -> TcRn ()
addDependentFiles fs = do
  ref <- fmap tcg_dependent_files getGblEnv
  dep_files <- readTcRef ref
  writeTcRef ref (fs ++ dep_files)

{-
************************************************************************
*                                                                      *
                Error management
*                                                                      *
************************************************************************
-}

getSrcSpanM :: TcRn SrcSpan
        -- Avoid clash with Name.getSrcLoc
getSrcSpanM = do { env <- getLclEnv; return (RealSrcSpan (getLclEnvLoc env) Strict.Nothing) }

-- See Note [Error contexts in generated code]
inGeneratedCode :: TcRn Bool
inGeneratedCode = lclEnvInGeneratedCode <$> getLclEnv

setSrcSpan :: SrcSpan -> TcRn a -> TcRn a
-- See Note [Error contexts in generated code]
-- for the tcl_in_gen_code manipulation
setSrcSpan (RealSrcSpan loc _) thing_inside
  = updLclCtxt (\env -> env { tcl_loc = loc, tcl_in_gen_code = False })
              thing_inside

setSrcSpan loc@(UnhelpfulSpan _) thing_inside
  | isGeneratedSrcSpan loc
  = setInGeneratedCode thing_inside

  | otherwise
  = thing_inside

-- | Mark the inner computation as being done inside generated code.
--
-- See Note [Error contexts in generated code]
setInGeneratedCode :: TcRn a -> TcRn a
setInGeneratedCode thing_inside =
  updLclCtxt (\env -> env { tcl_in_gen_code = True }) thing_inside

setSrcSpanA :: EpAnn ann -> TcRn a -> TcRn a
setSrcSpanA l = setSrcSpan (locA l)

addLocM :: (HasLoc t) => (a -> TcM b) -> GenLocated t a -> TcM b
addLocM fn (L loc a) = setSrcSpan (getHasLoc loc) $ fn a

wrapLocM :: (HasLoc t) =>  (a -> TcM b) -> GenLocated t a -> TcM (Located b)
wrapLocM fn (L loc a) =
  let
    loc' = getHasLoc loc
  in setSrcSpan loc' $ do { b <- fn a
                          ; return (L loc' b) }

wrapLocMA :: (a -> TcM b) -> GenLocated (EpAnn ann) a -> TcRn (GenLocated (EpAnn ann) b)
wrapLocMA fn (L loc a) = setSrcSpanA loc $ do { b <- fn a
                                              ; return (L loc b) }

wrapLocFstM :: (a -> TcM (b,c)) -> Located a -> TcM (Located b, c)
wrapLocFstM fn (L loc a) =
  setSrcSpan loc $ do
    (b,c) <- fn a
    return (L loc b, c)

-- Possible instantiations:
--    wrapLocFstMA :: (a -> TcM (b,c)) -> LocatedA    a -> TcM (LocatedA    b, c)
--    wrapLocFstMA :: (a -> TcM (b,c)) -> LocatedN    a -> TcM (LocatedN    b, c)
--    wrapLocFstMA :: (a -> TcM (b,c)) -> LocatedAn t a -> TcM (LocatedAn t b, c)
-- and so on.
wrapLocFstMA :: (a -> TcM (b,c)) -> GenLocated (EpAnn ann) a -> TcM (GenLocated (EpAnn ann) b, c)
wrapLocFstMA fn (L loc a) =
  setSrcSpanA loc $ do
    (b,c) <- fn a
    return (L loc b, c)

wrapLocSndM :: (a -> TcM (b, c)) -> Located a -> TcM (b, Located c)
wrapLocSndM fn (L loc a) =
  setSrcSpan loc $ do
    (b,c) <- fn a
    return (b, L loc c)

-- Possible instantiations:
--    wrapLocSndMA :: (a -> TcM (b, c)) -> LocatedA    a -> TcM (b, LocatedA    c)
--    wrapLocSndMA :: (a -> TcM (b, c)) -> LocatedN    a -> TcM (b, LocatedN    c)
--    wrapLocSndMA :: (a -> TcM (b, c)) -> LocatedAn t a -> TcM (b, LocatedAn t c)
-- and so on.
wrapLocSndMA :: (a -> TcM (b, c)) -> GenLocated (EpAnn ann) a -> TcM (b, GenLocated (EpAnn ann) c)
wrapLocSndMA fn (L loc a) =
  setSrcSpanA loc $ do
    (b,c) <- fn a
    return (b, L loc c)

wrapLocM_ :: (a -> TcM ()) -> Located a -> TcM ()
wrapLocM_ fn (L loc a) = setSrcSpan loc (fn a)

wrapLocMA_ :: (a -> TcM ()) -> LocatedA a -> TcM ()
wrapLocMA_ fn (L loc a) = setSrcSpan (locA loc) (fn a)

-- Reporting errors

getErrsVar :: TcRn (TcRef (Messages TcRnMessage))
getErrsVar = do { env <- getLclEnv; return (tcl_errs env) }

setErrsVar :: TcRef (Messages TcRnMessage) -> TcRn a -> TcRn a
setErrsVar v = updLclEnv (\ env -> env { tcl_errs =  v })

addErr :: TcRnMessage -> TcRn ()
addErr msg = do { loc <- getSrcSpanM; addErrAt loc msg }

failWith :: TcRnMessage -> TcRn a
failWith msg = addErr msg >> failM

failAt :: SrcSpan -> TcRnMessage -> TcRn a
failAt loc msg = addErrAt loc msg >> failM

addErrAt :: SrcSpan -> TcRnMessage -> TcRn ()
-- addErrAt is mainly (exclusively?) used by the renamer, where
-- tidying is not an issue, but it's all lazy so the extra
-- work doesn't matter
addErrAt loc msg = do { ctxt <- getErrCtxt
                      ; tidy_env <- liftZonkM $ tcInitTidyEnv
                      ; err_ctxt <- mkErrCtxt tidy_env ctxt
                      ; let detailed_msg = mkDetailedMessage (ErrInfo err_ctxt Nothing noHints) msg
                      ; add_long_err_at loc detailed_msg }

mkDetailedMessage :: ErrInfo-> TcRnMessage -> TcRnMessageDetailed
mkDetailedMessage err_info msg =
  TcRnMessageDetailed err_info msg

addErrs :: [(SrcSpan,TcRnMessage)] -> TcRn ()
addErrs msgs = mapM_ add msgs
             where
               add (loc,msg) = addErrAt loc msg

checkErr :: Bool -> TcRnMessage -> TcRn ()
-- Add the error if the bool is False
checkErr ok msg = unless ok (addErr msg)

checkErrAt :: SrcSpan -> Bool -> TcRnMessage -> TcRn ()
checkErrAt loc ok msg = unless ok (addErrAt loc msg)

addMessages :: Messages TcRnMessage -> TcRn ()
addMessages msgs1
  = do { errs_var <- getErrsVar
       ; msgs0    <- readTcRef errs_var
       ; writeTcRef errs_var (msgs0 `unionMessages` msgs1) }

discardWarnings :: TcRn a -> TcRn a
-- Ignore warnings inside the thing inside;
-- used to ignore-unused-variable warnings inside derived code
discardWarnings thing_inside
  = do  { errs_var <- getErrsVar
        ; old_warns <- getWarningMessages <$> readTcRef errs_var

        ; result <- thing_inside

        -- Revert warnings to old_warns
        ; new_errs <- getErrorMessages <$> readTcRef errs_var
        ; writeTcRef errs_var $ mkMessages (old_warns `unionBags` new_errs)

        ; return result }

{-
************************************************************************
*                                                                      *
        Shared error message stuff: renamer and typechecker
*                                                                      *
************************************************************************
-}

add_long_err_at :: SrcSpan -> TcRnMessageDetailed -> TcRn ()
add_long_err_at loc msg = mk_long_err_at loc msg >>= reportDiagnostic
  where
    mk_long_err_at :: SrcSpan -> TcRnMessageDetailed -> TcRn (MsgEnvelope TcRnMessage)
    mk_long_err_at loc msg
      = do { name_ppr_ctx <- getNamePprCtx ;
             unit_state <- hsc_units <$> getTopEnv ;
             return $ mkErrorMsgEnvelope loc name_ppr_ctx
                    $ TcRnMessageWithInfo unit_state msg
                    }

mkTcRnMessage :: SrcSpan
              -> TcRnMessage
              -> TcRn (MsgEnvelope TcRnMessage)
mkTcRnMessage loc msg
  = do { name_ppr_ctx <- getNamePprCtx ;
         diag_opts <- initDiagOpts <$> getDynFlags ;
         return $ mkMsgEnvelope diag_opts loc name_ppr_ctx msg }

reportDiagnostics :: [MsgEnvelope TcRnMessage] -> TcM ()
reportDiagnostics = mapM_ reportDiagnostic

reportDiagnostic :: MsgEnvelope TcRnMessage -> TcRn ()
reportDiagnostic msg
  = do { traceTc "Adding diagnostic:" (pprLocMsgEnvelopeDefault msg) ;
         errs_var <- getErrsVar ;
         msgs     <- readTcRef errs_var ;
         writeTcRef errs_var (msg `addMessage` msgs) }

-----------------------
checkNoErrs :: TcM r -> TcM r
-- (checkNoErrs m) succeeds iff m succeeds and generates no errors
-- If m fails then (checkNoErrs m) fails.
-- If m succeeds, it checks whether m generated any errors messages
--      (it might have recovered internally)
--      If so, it fails too.
-- Regardless, any errors generated by m are propagated to the enclosing context.
checkNoErrs main
  = do  { (res, no_errs) <- askNoErrs main
        ; unless no_errs failM
        ; return res }

-----------------------
whenNoErrs :: TcM () -> TcM ()
whenNoErrs thing = ifErrsM (return ()) thing

ifErrsM :: TcRn r -> TcRn r -> TcRn r
--      ifErrsM bale_out normal
-- does 'bale_out' if there are errors in errors collection
-- otherwise does 'normal'
ifErrsM bale_out normal
 = do { errs_var <- getErrsVar ;
        msgs <- readTcRef errs_var ;
        if errorsFound msgs then
           bale_out
        else
           normal }

failIfErrsM :: TcRn ()
-- Useful to avoid error cascades
failIfErrsM = ifErrsM failM (return ())

{- *********************************************************************
*                                                                      *
        Context management for the type checker
*                                                                      *
************************************************************************
-}

{- Note [Inlining addErrCtxt]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
You will notice a bunch of INLINE pragamas on addErrCtxt and friends.
The reason is to promote better eta-expansion in client modules.
Consider
    \e s. addErrCtxt c (tc_foo x) e s
It looks as if tc_foo is applied to only two arguments, but if we
inline addErrCtxt it'll turn into something more like
    \e s. tc_foo x (munge c e) s
This is much better because Called Arity analysis can see that tc_foo
is applied to four arguments.  See #18379 for a concrete example.

This reliance on delicate inlining and Called Arity is not good.
See #18202 for a more general approach.  But meanwhile, these
inlinings seem unobjectional, and they solve the immediate
problem.

Note [Error contexts in generated code]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* setSrcSpan sets tcl_in_gen_code to True if the SrcSpan is GeneratedSrcSpan,
  and back to False when we get a useful SrcSpan

* When tcl_in_gen_code is True, addErrCtxt becomes a no-op.

So typically it's better to do setSrcSpan /before/ addErrCtxt.

See Note [Rebindable syntax and XXExprGhcRn] in GHC.Hs.Expr for
more discussion of this fancy footwork, as well as
Note [Generated code and pattern-match checking] in GHC.Types.Basic for the
relation with pattern-match checks.
-}

getErrCtxt :: TcM [ErrCtxt]
getErrCtxt = do { env <- getLclEnv; return (getLclEnvErrCtxt env) }

setErrCtxt :: [ErrCtxt] -> TcM a -> TcM a
{-# INLINE setErrCtxt #-}   -- Note [Inlining addErrCtxt]
setErrCtxt ctxt = updLclEnv (setLclEnvErrCtxt ctxt)

-- | Add a fixed message to the error context. This message should not
-- do any tidying.
-- no op in generated code
-- See Note [Rebindable syntax and XXExprGhcRn] in GHC.Hs.Expr
addErrCtxt :: ErrCtxtMsg -> TcM a -> TcM a
{-# INLINE addErrCtxt #-}   -- Note [Inlining addErrCtxt]
addErrCtxt msg = addErrCtxtM (\env -> return (env, msg))

-- | Add a message to the error context. This message may do tidying.
--   no op in generated code
--   See Note [Rebindable syntax and XXExprGhcRn] in GHC.Hs.Expr
addErrCtxtM :: (TidyEnv -> ZonkM (TidyEnv, ErrCtxtMsg)) -> TcM a -> TcM a
{-# INLINE addErrCtxtM #-}  -- Note [Inlining addErrCtxt]
addErrCtxtM ctxt = pushCtxt (False, ctxt)

-- | Add a fixed landmark message to the error context. A landmark
-- message is always sure to be reported, even if there is a lot of
-- context. It also doesn't count toward the maximum number of contexts
-- reported.
addLandmarkErrCtxt :: ErrCtxtMsg -> TcM a -> TcM a
{-# INLINE addLandmarkErrCtxt #-}  -- Note [Inlining addErrCtxt]
addLandmarkErrCtxt msg = addLandmarkErrCtxtM (\env -> return (env, msg))

-- | Variant of 'addLandmarkErrCtxt' that allows for monadic operations
-- and tidying.
addLandmarkErrCtxtM :: (TidyEnv -> ZonkM (TidyEnv, ErrCtxtMsg)) -> TcM a -> TcM a
{-# INLINE addLandmarkErrCtxtM #-}  -- Note [Inlining addErrCtxt]
addLandmarkErrCtxtM ctxt = pushCtxt (True, ctxt)

-- | NB. no op in generated code
-- See Note [Rebindable syntax and XXExprGhcRn] in GHC.Hs.Expr
pushCtxt :: ErrCtxt -> TcM a -> TcM a
{-# INLINE pushCtxt #-} -- Note [Inlining addErrCtxt]
pushCtxt ctxt = updLclEnv (updCtxt ctxt)

updCtxt :: ErrCtxt -> TcLclEnv -> TcLclEnv
-- Do not update the context if we are in generated code
-- See Note [Rebindable syntax and XXExprGhcRn] in GHC.Hs.Expr
updCtxt ctxt env
  | lclEnvInGeneratedCode env = env
  | otherwise = addLclEnvErrCtxt ctxt env

popErrCtxt :: TcM a -> TcM a
popErrCtxt thing_inside = updLclEnv (\env -> setLclEnvErrCtxt (pop $ getLclEnvErrCtxt env) env) $
                          thing_inside
           where
             pop []       = []
             pop (_:msgs) = msgs

getCtLocM :: CtOrigin -> Maybe TypeOrKind -> TcM CtLoc
getCtLocM origin t_or_k
  = do { env <- getLclEnv
       ; return (CtLoc { ctl_origin   = origin
                       , ctl_env      = mkCtLocEnv env
                       , ctl_t_or_k   = t_or_k
                       , ctl_depth    = initialSubGoalDepth }) }

mkCtLocEnv :: TcLclEnv -> CtLocEnv
mkCtLocEnv lcl_env =
  CtLocEnv { ctl_bndrs = getLclEnvBinderStack lcl_env
           , ctl_ctxt  = getLclEnvErrCtxt lcl_env
           , ctl_loc = getLclEnvLoc lcl_env
           , ctl_tclvl = getLclEnvTcLevel lcl_env
           , ctl_in_gen_code = lclEnvInGeneratedCode lcl_env
           , ctl_rdr = getLclEnvRdrEnv lcl_env
           }

setCtLocM :: CtLoc -> TcM a -> TcM a
-- Set the SrcSpan and error context from the CtLoc
setCtLocM (CtLoc { ctl_env = lcl }) thing_inside
  = updLclEnv (\env -> setLclEnvLoc (ctl_loc lcl)
                     $ setLclEnvErrCtxt (ctl_ctxt lcl)
                     $ setLclEnvBinderStack (ctl_bndrs lcl)
                     $ env) thing_inside

{- *********************************************************************
*                                                                      *
             Error recovery and exceptions
*                                                                      *
********************************************************************* -}

tcTryM :: TcRn r -> TcRn (Maybe r)
-- The most basic function: catch the exception
--   Nothing => an exception happened
--   Just r  => no exception, result R
-- Errors and constraints are propagated in both cases
-- Never throws an exception
tcTryM thing_inside
  = do { either_res <- tryM thing_inside
       ; return (case either_res of
                    Left _  -> Nothing
                    Right r -> Just r) }
         -- In the Left case the exception is always the IOEnv
         -- built-in in exception; see IOEnv.failM

-----------------------
capture_constraints :: TcM r -> TcM (r, WantedConstraints)
-- capture_constraints simply captures and returns the
--                     constraints generated by thing_inside
-- Precondition: thing_inside must not throw an exception!
-- Reason for precondition: an exception would blow past the place
-- where we read the lie_var, and we'd lose the constraints altogether
capture_constraints thing_inside
  = do { lie_var <- newTcRef emptyWC
       ; res <- updLclEnv (\ env -> env { tcl_lie = lie_var }) $
                thing_inside
       ; lie <- readTcRef lie_var
       ; return (res, lie) }

capture_messages :: TcM r -> TcM (r, Messages TcRnMessage)
-- capture_messages simply captures and returns the
--                  errors and warnings generated by thing_inside
-- Precondition: thing_inside must not throw an exception!
-- Reason for precondition: an exception would blow past the place
-- where we read the msg_var, and we'd lose the constraints altogether
capture_messages thing_inside
  = do { msg_var <- newTcRef emptyMessages
       ; res     <- setErrsVar msg_var thing_inside
       ; msgs    <- readTcRef msg_var
       ; return (res, msgs) }

-----------------------
-- (askNoErrs m) runs m
-- If m fails,
--    then (askNoErrs m) fails, propagating only
--         insoluble constraints
--
-- If m succeeds with result r,
--    then (askNoErrs m) succeeds with result (r, b),
--         where b is True iff m generated no errors
--
-- Regardless of success or failure,
--   propagate any errors/warnings generated by m
askNoErrs :: TcRn a -> TcRn (a, Bool)
askNoErrs thing_inside
  = do { ((mb_res, lie), msgs) <- capture_messages    $
                                  capture_constraints $
                                  tcTryM thing_inside
       ; addMessages msgs

       ; case mb_res of
           Nothing  -> do { emitConstraints (dropMisleading lie)
                          ; failM }

           Just res -> do { emitConstraints lie
                          ; let errs_found = errorsFound msgs
                                          || insolubleWC lie
                          ; return (res, not errs_found) } }

-----------------------
tryCaptureConstraints :: TcM a -> TcM (Maybe a, WantedConstraints)
-- (tryCaptureConstraints_maybe m) runs m,
--   and returns the type constraints it generates
-- It never throws an exception; instead if thing_inside fails,
--   it returns Nothing and the /insoluble/ constraints
-- Error messages are propagated
tryCaptureConstraints thing_inside
  = do { (mb_res, lie) <- capture_constraints $
                          tcTryM thing_inside

       -- See Note [Constraints and errors]
       ; case mb_res of
            Just {} -> return (mb_res, lie)
            Nothing -> do { let pruned_lie = dropMisleading lie
                          ; traceTc "tryCaptureConstraints" $
                            vcat [ text "lie:" <+> ppr lie
                                 , text "dropMisleading lie:" <+> ppr pruned_lie ]
                          ; return (Nothing, pruned_lie) } }

captureConstraints :: TcM a -> TcM (a, WantedConstraints)
-- (captureConstraints m) runs m, and returns the type constraints it generates
-- If thing_inside fails (throwing an exception),
--   then (captureConstraints thing_inside) fails too
--   propagating the insoluble constraints only
-- Error messages are propagated in either case
captureConstraints thing_inside
  = do { (mb_res, lie) <- tryCaptureConstraints thing_inside

            -- See Note [Constraints and errors]
            -- If the thing_inside threw an exception, emit the insoluble
            -- constraints only (returned by tryCaptureConstraints)
            -- so that they are not lost
       ; case mb_res of
           Nothing  -> do { emitConstraints lie; failM }
           Just res -> return (res, lie) }

-----------------------
-- | @tcCollectingUsage thing_inside@ runs @thing_inside@ and returns the usage
-- information which was collected as part of the execution of
-- @thing_inside@. Careful: @tcCollectingUsage thing_inside@ itself does not
-- report any usage information, it's up to the caller to incorporate the
-- returned usage information into the larger context appropriately.
tcCollectingUsage :: TcM a -> TcM (UsageEnv,a)
tcCollectingUsage thing_inside
  = do { local_usage_ref <- newTcRef zeroUE
       ; result <- updLclEnv (\env -> env { tcl_usage = local_usage_ref }) thing_inside
       ; local_usage <- readTcRef local_usage_ref
       ; return (local_usage,result) }

-- | @tcScalingUsage mult thing_inside@ runs @thing_inside@ and scales all the
-- usage information by @mult@.
tcScalingUsage :: Mult -> TcM a -> TcM a
tcScalingUsage mult thing_inside
  = do { (usage, result) <- tcCollectingUsage thing_inside
       ; traceTc "tcScalingUsage" $ vcat [ppr mult, ppr usage]
       ; tcEmitBindingUsage $ scaleUE mult usage
       ; return result }

tcEmitBindingUsage :: UsageEnv -> TcM ()
tcEmitBindingUsage ue
  = do { lcl_env <- getLclEnv
       ; let usage = tcl_usage lcl_env
       ; updTcRef usage (addUE ue) }

-----------------------
attemptM :: TcRn r -> TcRn (Maybe r)
-- (attemptM thing_inside) runs thing_inside
-- If thing_inside succeeds, returning r,
--   we return (Just r), and propagate all constraints and errors
-- If thing_inside fail, throwing an exception,
--   we return Nothing, propagating insoluble constraints,
--                      and all errors
-- attemptM never throws an exception
attemptM thing_inside
  = do { (mb_r, lie) <- tryCaptureConstraints thing_inside
       ; emitConstraints lie

       -- Debug trace
       ; when (isNothing mb_r) $
         traceTc "attemptM recovering with insoluble constraints" $
                 (ppr lie)

       ; return mb_r }

-----------------------
recoverM :: TcRn r      -- Recovery action; do this if the main one fails
         -> TcRn r      -- Main action: do this first;
                        --  if it generates errors, propagate them all
         -> TcRn r
-- (recoverM recover thing_inside) runs thing_inside
-- If thing_inside fails, propagate its errors and insoluble constraints
--                        and run 'recover'
-- If thing_inside succeeds, propagate all its errors and constraints
--
-- Can fail, if 'recover' fails
recoverM recover thing
  = do { mb_res <- attemptM thing ;
         case mb_res of
           Nothing  -> recover
           Just res -> return res }

-----------------------

-- | Drop elements of the input that fail, so the result
-- list can be shorter than the argument list
mapAndRecoverM :: (a -> TcRn b) -> [a] -> TcRn [b]
mapAndRecoverM f xs
  = do { mb_rs <- mapM (attemptM . f) xs
       ; return [r | Just r <- mb_rs] }

-- | Apply the function to all elements on the input list
-- If all succeed, return the list of results
-- Otherwise fail, propagating all errors
mapAndReportM :: (a -> TcRn b) -> [a] -> TcRn [b]
mapAndReportM f xs
  = do { mb_rs <- mapM (attemptM . f) xs
       ; when (any isNothing mb_rs) failM
       ; return [r | Just r <- mb_rs] }

-- | The accumulator is not updated if the action fails
foldAndRecoverM :: (b -> a -> TcRn b) -> b -> [a] -> TcRn b
foldAndRecoverM _ acc []     = return acc
foldAndRecoverM f acc (x:xs) =
                          do { mb_r <- attemptM (f acc x)
                             ; case mb_r of
                                Nothing   -> foldAndRecoverM f acc xs
                                Just acc' -> foldAndRecoverM f acc' xs  }

-----------------------
tryTc :: TcRn a -> TcRn (Maybe a, Messages TcRnMessage)
-- (tryTc m) executes m, and returns
--      Just r,  if m succeeds (returning r)
--      Nothing, if m fails
-- It also returns all the errors and warnings accumulated by m
-- It always succeeds (never raises an exception)
tryTc thing_inside
 = capture_messages (attemptM thing_inside)

-----------------------
discardErrs :: TcRn a -> TcRn a
-- (discardErrs m) runs m,
--   discarding all error messages and warnings generated by m
-- If m fails, discardErrs fails, and vice versa
discardErrs m
 = do { errs_var <- newTcRef emptyMessages
      ; setErrsVar errs_var m }

-----------------------
tryTcDiscardingErrs :: TcM r -> TcM r -> TcM r
-- (tryTcDiscardingErrs recover thing_inside) tries 'thing_inside';
--      if 'main' succeeds with no error messages, it's the answer
--      otherwise discard everything from 'main', including errors,
--          and try 'recover' instead.
tryTcDiscardingErrs recover =
  tryTcDiscardingErrs'
    (\_ _ _ -> True)     -- No validation
    recover recover      -- Discard all errors and warnings
                         -- and unsolved constraints entirely

tryTcDiscardingErrs' :: (WantedConstraints -> Messages TcRnMessage -> r -> Bool)  -- Validation
                     -> TcM r  -- Recover from validation error
                     -> TcM r  -- Recover from failure
                     -> TcM r  -- Action to try
                     -> TcM r
-- (tryTcDiscardingErrs' validate recover_invalid recover_error thing_inside) tries 'thing_inside';
--      if 'thing_inside' succeeds and validation produces no errors, it's the answer
--      otherwise discard everything from 'thing_inside', including errors,
--          and try 'recover' instead.
tryTcDiscardingErrs' validate recover_invalid recover_error thing_inside
  = do { ((mb_res, lie), msgs) <- capture_messages    $
                                  capture_constraints $
                                  tcTryM thing_inside
        ; case mb_res of
            Just res | not (errorsFound msgs)
                     , not (insolubleWC lie)
                    -- 'thing_inside' succeeded with no errors
              -> if validate lie msgs res
                 then do { addMessages msgs  -- msgs might still have warnings
                         ; emitConstraints lie
                         ; return res }
                 else recover_invalid

            _ -> -- 'thing_inside' failed, or produced an error message
                 recover_error
        }

{-
************************************************************************
*                                                                      *
             Error message generation (type checker)
*                                                                      *
************************************************************************

    The addErrTc functions add an error message, but do not cause failure.
    The 'M' variants pass a TidyEnv that has already been used to
    tidy up the message; we then use it to tidy the context messages
-}

{-

Note [Reporting warning diagnostics]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We use functions below to report warnings.  For the most part,
we do /not/ need to check any warning flags before doing so.
See https://gitlab.haskell.org/ghc/ghc/-/wikis/Errors-as-(structured)-values
for the design.

-}

addErrTc :: TcRnMessage -> TcM ()
addErrTc err_msg = do { env0 <- liftZonkM tcInitTidyEnv
                      ; addErrTcM (env0, err_msg) }

addErrTcM :: (TidyEnv, TcRnMessage) -> TcM ()
addErrTcM (tidy_env, err_msg)
  = do { ctxt <- getErrCtxt ;
         loc  <- getSrcSpanM ;
         add_err_tcm tidy_env err_msg loc ctxt }

-- The failWith functions add an error message and cause failure

failWithTc :: TcRnMessage -> TcM a               -- Add an error message and fail
failWithTc err_msg
  = addErrTc err_msg >> failM

failWithTcM :: (TidyEnv, TcRnMessage) -> TcM a   -- Add an error message and fail
failWithTcM local_and_msg
  = addErrTcM local_and_msg >> failM

checkTc :: Bool -> TcRnMessage -> TcM ()         -- Check that the boolean is true
checkTc True  _   = return ()
checkTc False err = failWithTc err

checkTcM :: Bool -> (TidyEnv, TcRnMessage) -> TcM ()
checkTcM True  _   = return ()
checkTcM False err = failWithTcM err

checkJustTc :: TcRnMessage -> Maybe a -> TcM a
checkJustTc err = maybe (failWithTc err) pure

checkJustTcM :: (TidyEnv, TcRnMessage) -> Maybe a -> TcM a
checkJustTcM err = maybe (failWithTcM err) pure

failIfTc :: Bool -> TcRnMessage -> TcM ()         -- Check that the boolean is false
failIfTc False _   = return ()
failIfTc True  err = failWithTc err

failIfTcM :: Bool -> (TidyEnv, TcRnMessage) -> TcM ()
   -- Check that the boolean is false
failIfTcM False _   = return ()
failIfTcM True  err = failWithTcM err


--         Warnings have no 'M' variant, nor failure

-- | Display a warning if a condition is met.
warnIf :: Bool -> TcRnMessage -> TcRn ()
warnIf is_bad msg -- No need to check any flag here, it will be done in 'diagReasonSeverity'.
  = when is_bad (addDiagnostic msg)

-- | Display a warning if a condition is met.
diagnosticTc :: Bool -> TcRnMessage -> TcM ()
diagnosticTc should_report warn_msg
  | should_report = addDiagnosticTc warn_msg
  | otherwise     = return ()

-- | Display a diagnostic if a condition is met.
diagnosticTcM :: Bool -> (TidyEnv, TcRnMessage) -> TcM ()
diagnosticTcM should_report warn_msg
  | should_report = addDiagnosticTcM warn_msg
  | otherwise     = return ()

-- | Display a diagnostic in the current context.
addDiagnosticTc :: TcRnMessage -> TcM ()
addDiagnosticTc msg
 = do { env0 <- liftZonkM tcInitTidyEnv
      ; addDiagnosticTcM (env0, msg) }

-- | Display a diagnostic in a given context.
addDiagnosticTcM :: (TidyEnv, TcRnMessage) -> TcM ()
addDiagnosticTcM (env0, msg)
 = do { ctxt <- getErrCtxt
      ; extra <- mkErrCtxt env0 ctxt
      ; let detailed_msg = mkDetailedMessage (ErrInfo extra Nothing noHints) msg
      ; add_diagnostic detailed_msg }

-- | A variation of 'addDiagnostic' that takes a function to produce a 'TcRnDsMessage'
-- given some additional context about the diagnostic.
addDetailedDiagnostic :: ([ErrCtxtMsg] -> TcRnMessage) -> TcM ()
addDetailedDiagnostic mkMsg = do
  loc <- getSrcSpanM
  name_ppr_ctx <- getNamePprCtx
  !diag_opts  <- initDiagOpts <$> getDynFlags
  env0 <- liftZonkM tcInitTidyEnv
  ctxt <- getErrCtxt
  err_info <- mkErrCtxt env0 ctxt
  reportDiagnostic $
    mkMsgEnvelope diag_opts loc name_ppr_ctx $
      mkMsg err_info

addTcRnDiagnostic :: TcRnMessage -> TcM ()
addTcRnDiagnostic msg = do
  loc <- getSrcSpanM
  mkTcRnMessage loc msg >>= reportDiagnostic

-- | Display a diagnostic for the current source location, taken from
-- the 'TcRn' monad.
addDiagnostic :: TcRnMessage -> TcRn ()
addDiagnostic msg = add_diagnostic (mkDetailedMessage (ErrInfo [] Nothing noHints) msg)

-- | Display a diagnostic for a given source location.
addDiagnosticAt :: SrcSpan -> TcRnMessage -> TcRn ()
addDiagnosticAt loc msg = do
  unit_state <- hsc_units <$> getTopEnv
  let detailed_msg = mkDetailedMessage (ErrInfo [] Nothing noHints) msg
  mkTcRnMessage loc (TcRnMessageWithInfo unit_state detailed_msg) >>= reportDiagnostic

-- | Display a diagnostic, with an optional flag, for the current source
-- location.
add_diagnostic :: TcRnMessageDetailed -> TcRn ()
add_diagnostic msg
  = do { loc <- getSrcSpanM
       ; unit_state <- hsc_units <$> getTopEnv
       ; mkTcRnMessage loc (TcRnMessageWithInfo unit_state msg) >>= reportDiagnostic
       }

{-
-----------------------------------
        Other helper functions
-}

add_err_tcm :: TidyEnv -> TcRnMessage -> SrcSpan
            -> [ErrCtxt]
            -> TcM ()
add_err_tcm tidy_env msg loc ctxt
 = do { err_ctxt <- mkErrCtxt tidy_env ctxt
      ; add_long_err_at loc $
          mkDetailedMessage (ErrInfo err_ctxt Nothing noHints) msg }

mkErrCtxt :: TidyEnv -> [ErrCtxt] -> TcM [ErrCtxtMsg]
-- Tidy the error info, trimming excessive contexts
mkErrCtxt env ctxts
--  = do
--       dbg <- hasPprDebug <$> getDynFlags
--       if dbg                -- In -dppr-debug style the output
--          then return empty  -- just becomes too voluminous
--          else go dbg 0 env ctxts
 = go False 0 env ctxts
 where
   go :: Bool -> Int -> TidyEnv -> [ErrCtxt] -> TcM [ErrCtxtMsg]
   go _ _ _   [] = return []
   go dbg n env ((is_landmark, ctxt) : ctxts)
     | is_landmark || n < mAX_CONTEXTS -- Too verbose || dbg
     = do { (env', msg) <- liftZonkM $ ctxt env
          ; let n' = if is_landmark then n else n+1
          ; rest <- go dbg n' env' ctxts
          ; return (msg : rest) }
     | otherwise
     = go dbg n env ctxts

mAX_CONTEXTS :: Int     -- No more than this number of non-landmark contexts
mAX_CONTEXTS = 3

-- debugTc is useful for monadic debugging code

debugTc :: TcM () -> TcM ()
debugTc thing
 | debugIsOn = thing
 | otherwise = return ()

{-
************************************************************************
*                                                                      *
             Type constraints
*                                                                      *
************************************************************************
-}

addTopEvBinds :: Bag EvBind -> TcM a -> TcM a
addTopEvBinds new_ev_binds thing_inside
  =updGblEnv upd_env thing_inside
  where
    upd_env tcg_env = tcg_env { tcg_ev_binds = tcg_ev_binds tcg_env
                                               `unionBags` new_ev_binds }

newTcEvBinds :: TcM EvBindsVar
newTcEvBinds = do { binds_ref <- newTcRef emptyEvBindMap
                  ; tcvs_ref  <- newTcRef emptyVarSet
                  ; uniq <- newUnique
                  ; traceTc "newTcEvBinds" (text "unique =" <+> ppr uniq)
                  ; return (EvBindsVar { ebv_binds = binds_ref
                                       , ebv_tcvs = tcvs_ref
                                       , ebv_uniq = uniq }) }

-- | Creates an EvBindsVar incapable of holding any bindings. It still
-- tracks covar usages (see comments on ebv_tcvs in "GHC.Tc.Types.Evidence"), thus
-- must be made monadically
newNoTcEvBinds :: TcM EvBindsVar
newNoTcEvBinds
  = do { tcvs_ref  <- newTcRef emptyVarSet
       ; uniq <- newUnique
       ; traceTc "newNoTcEvBinds" (text "unique =" <+> ppr uniq)
       ; return (CoEvBindsVar { ebv_tcvs = tcvs_ref
                              , ebv_uniq = uniq }) }

cloneEvBindsVar :: EvBindsVar -> TcM EvBindsVar
-- Clone the refs, so that any binding created when
-- solving don't pollute the original
cloneEvBindsVar ebv@(EvBindsVar {})
  = do { binds_ref <- newTcRef emptyEvBindMap
       ; tcvs_ref  <- newTcRef emptyVarSet
       ; return (ebv { ebv_binds = binds_ref
                     , ebv_tcvs = tcvs_ref }) }
cloneEvBindsVar ebv@(CoEvBindsVar {})
  = do { tcvs_ref  <- newTcRef emptyVarSet
       ; return (ebv { ebv_tcvs = tcvs_ref }) }

getTcEvTyCoVars :: EvBindsVar -> TcM TyCoVarSet
getTcEvTyCoVars ev_binds_var
  = readTcRef (ebv_tcvs ev_binds_var)

getTcEvBindsMap :: EvBindsVar -> TcM EvBindMap
getTcEvBindsMap (EvBindsVar { ebv_binds = ev_ref })
  = readTcRef ev_ref
getTcEvBindsMap (CoEvBindsVar {})
  = return emptyEvBindMap

setTcEvBindsMap :: EvBindsVar -> EvBindMap -> TcM ()
setTcEvBindsMap (EvBindsVar { ebv_binds = ev_ref }) binds
  = writeTcRef ev_ref binds
setTcEvBindsMap v@(CoEvBindsVar {}) ev_binds
  | isEmptyEvBindMap ev_binds
  = return ()
  | otherwise
  = pprPanic "setTcEvBindsMap" (ppr v $$ ppr ev_binds)

addTcEvBind :: EvBindsVar -> EvBind -> TcM ()
-- Add a binding to the TcEvBinds by side effect
addTcEvBind (EvBindsVar { ebv_binds = ev_ref, ebv_uniq = u }) ev_bind
  = do { traceTc "addTcEvBind" $ ppr u $$
                                 ppr ev_bind
       ; bnds <- readTcRef ev_ref
       ; writeTcRef ev_ref (extendEvBinds bnds ev_bind) }
addTcEvBind (CoEvBindsVar { ebv_uniq = u }) ev_bind
  = pprPanic "addTcEvBind CoEvBindsVar" (ppr ev_bind $$ ppr u)

addTcEvBinds :: EvBindsVar -> EvBindMap -> TcM ()
-- ^ Add a collection of binding to the TcEvBinds by side effect
addTcEvBinds _ new_ev_binds
  | isEmptyEvBindMap new_ev_binds
  = return ()
addTcEvBinds (EvBindsVar { ebv_binds = ev_ref, ebv_uniq = u }) new_ev_binds
  = do { traceTc "addTcEvBinds" $ ppr u $$
                                  ppr new_ev_binds
       ; old_bnds <- readTcRef ev_ref
       ; writeTcRef ev_ref (old_bnds `unionEvBindMap` new_ev_binds) }
addTcEvBinds (CoEvBindsVar { ebv_uniq = u }) new_ev_binds
  = pprPanic "addTcEvBinds CoEvBindsVar" (ppr new_ev_binds $$ ppr u)

chooseUniqueOccTc :: (OccSet -> OccName) -> TcM OccName
chooseUniqueOccTc fn =
  do { env <- getGblEnv
     ; let dfun_n_var = tcg_dfun_n env
     ; set <- readTcRef dfun_n_var
     ; let occ = fn set
     ; writeTcRef dfun_n_var (extendOccSet set occ)
     ; return occ }

newZonkAnyType :: Kind -> TcM Type
-- Return a type (ZonkAny @k n), where n is fresh
-- Recall  ZonkAny :: forall k. Natural -> k
-- See Note [Any types] in GHC.Builtin.Types, wrinkle (Any4)
newZonkAnyType kind
  = do { env <- getGblEnv
       ; let zany_n_var = tcg_zany_n env
       ; i <- readTcRef zany_n_var
       ; let !i2 = i+1
       ; writeTcRef zany_n_var i2
       ; return (mkTyConApp zonkAnyTyCon [kind, mkNumLitTy i]) }

getConstraintVar :: TcM (TcRef WantedConstraints)
getConstraintVar = do { env <- getLclEnv; return (tcl_lie env) }

setConstraintVar :: TcRef WantedConstraints -> TcM a -> TcM a
setConstraintVar lie_var = updLclEnv (\ env -> env { tcl_lie = lie_var })

emitStaticConstraints :: WantedConstraints -> TcM ()
emitStaticConstraints static_lie
  = do { gbl_env <- getGblEnv
       ; updTcRef (tcg_static_wc gbl_env) (`andWC` static_lie) }

emitConstraints :: WantedConstraints -> TcM ()
emitConstraints ct
  | isEmptyWC ct
  = return ()
  | otherwise
  = do { lie_var <- getConstraintVar ;
         updTcRef lie_var (`andWC` ct) }

emitSimple :: Ct -> TcM ()
emitSimple ct
  = do { lie_var <- getConstraintVar ;
         updTcRef lie_var (`addSimples` unitBag ct) }

emitSimples :: Cts -> TcM ()
emitSimples cts
  = do { lie_var <- getConstraintVar ;
         updTcRef lie_var (`addSimples` cts) }

emitImplication :: Implication -> TcM ()
emitImplication ct
  = do { lie_var <- getConstraintVar ;
         updTcRef lie_var (`addImplics` unitBag ct) }

emitImplications :: Bag Implication -> TcM ()
emitImplications ct
  = unless (isEmptyBag ct) $
    do { lie_var <- getConstraintVar ;
         updTcRef lie_var (`addImplics` ct) }

emitDelayedErrors :: Bag DelayedError -> TcM ()
emitDelayedErrors errs
  = do { traceTc "emitDelayedErrors" (ppr errs)
       ; lie_var <- getConstraintVar
       ; updTcRef lie_var (`addDelayedErrors` errs)}

emitHole :: Hole -> TcM ()
emitHole hole
  = do { traceTc "emitHole" (ppr hole)
       ; lie_var <- getConstraintVar
       ; updTcRef lie_var (`addHoles` unitBag hole) }

emitHoles :: Bag Hole -> TcM ()
emitHoles holes
  = do { traceTc "emitHoles" (ppr holes)
       ; lie_var <- getConstraintVar
       ; updTcRef lie_var (`addHoles` holes) }

emitNotConcreteError :: NotConcreteError -> TcM ()
emitNotConcreteError err
  = do { traceTc "emitNotConcreteError" (ppr err)
       ; lie_var <- getConstraintVar
       ; updTcRef lie_var (`addNotConcreteError` err) }

-- See Note [Coercion errors in tcSubMult] in GHC.Tc.Utils.Unify.
ensureReflMultiplicityCo :: TcCoercion -> CtOrigin -> TcM ()
ensureReflMultiplicityCo mult_co origin
  = do { traceTc "ensureReflMultiplicityCo" (ppr mult_co)
       ; unless (isReflCo mult_co) $ do
           { loc <- getCtLocM origin Nothing
           ; lie_var <- getConstraintVar
           ; updTcRef lie_var (\w -> addMultiplicityCoercionError w mult_co loc) } }

-- | Throw out any constraints emitted by the thing_inside
discardConstraints :: TcM a -> TcM a
discardConstraints thing_inside = fst <$> captureConstraints thing_inside

-- | The name says it all. The returned TcLevel is the *inner* TcLevel.
pushLevelAndCaptureConstraints :: TcM a -> TcM (TcLevel, WantedConstraints, a)
pushLevelAndCaptureConstraints thing_inside
  = do { tclvl <- getTcLevel
       ; let tclvl' = pushTcLevel tclvl
       ; traceTc "pushLevelAndCaptureConstraints {" (ppr tclvl')
       ; (res, lie) <- updLclEnv (setLclEnvTcLevel tclvl') $
                       captureConstraints thing_inside
       ; traceTc "pushLevelAndCaptureConstraints }" (ppr tclvl')
       ; return (tclvl', lie, res) }

pushTcLevelM_ :: TcM a -> TcM a
pushTcLevelM_ = updLclEnv (modifyLclEnvTcLevel pushTcLevel)

pushTcLevelM :: TcM a -> TcM (TcLevel, a)
-- See Note [TcLevel assignment] in GHC.Tc.Utils.TcType
pushTcLevelM thing_inside
  = do { tclvl <- getTcLevel
       ; let tclvl' = pushTcLevel tclvl
       ; res <- updLclEnv (setLclEnvTcLevel tclvl') thing_inside
       ; return (tclvl', res) }

getTcLevel :: TcM TcLevel
getTcLevel = do { env <- getLclEnv
                ; return $! getLclEnvTcLevel env }

setTcLevel :: TcLevel -> TcM a -> TcM a
setTcLevel tclvl thing_inside
  = updLclEnv (setLclEnvTcLevel tclvl) thing_inside

isTouchableTcM :: TcTyVar -> TcM Bool
isTouchableTcM tv
  = do { lvl <- getTcLevel
       ; return (isTouchableMetaTyVar lvl tv) }

getLclTypeEnv :: TcM TcTypeEnv
getLclTypeEnv = do { env <- getLclEnv; return (getLclEnvTypeEnv env) }

setLclTypeEnv :: TcLclEnv -> TcM a -> TcM a
-- Set the local type envt, but do *not* disturb other fields,
-- notably the lie_var
setLclTypeEnv lcl_env thing_inside
  = updLclEnv (setLclEnvTypeEnv (getLclEnvTypeEnv lcl_env)) thing_inside

traceTcConstraints :: String -> TcM ()
traceTcConstraints msg
  = do { lie_var <- getConstraintVar
       ; lie     <- readTcRef lie_var
       ; traceOptTcRn Opt_D_dump_tc_trace $
         hang (text (msg ++ ": LIE:")) 2 (ppr lie)
       }

data IsExtraConstraint = YesExtraConstraint
                       | NoExtraConstraint

instance Outputable IsExtraConstraint where
  ppr YesExtraConstraint = text "YesExtraConstraint"
  ppr NoExtraConstraint  = text "NoExtraConstraint"

emitAnonTypeHole :: IsExtraConstraint
                 -> TcTyVar -> TcM ()
emitAnonTypeHole extra_constraints tv
  = do { ct_loc <- getCtLocM (TypeHoleOrigin occ) Nothing
       ; let hole = Hole { hole_sort = sort
                         , hole_occ  = mkRdrUnqual occ
                         , hole_ty   = mkTyVarTy tv
                         , hole_loc  = ct_loc }
       ; emitHole hole }
  where
    occ = mkTyVarOccFS (fsLit "_")
    sort | YesExtraConstraint <- extra_constraints = ConstraintHole
         | otherwise                               = TypeHole

emitNamedTypeHole :: (Name, TcTyVar) -> TcM ()
emitNamedTypeHole (name, tv)
  = do { ct_loc <- setSrcSpan (nameSrcSpan name) $
                   getCtLocM (TypeHoleOrigin occ) Nothing
       ; let hole = Hole { hole_sort = TypeHole
                         , hole_occ  = nameRdrName name
                         , hole_ty   = mkTyVarTy tv
                         , hole_loc  = ct_loc }
       ; emitHole hole }
  where
    occ       = nameOccName name

{- Note [Constraints and errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (#12124):

  foo :: Maybe Int
  foo = return (case Left 3 of
                  Left -> 1  -- Hard error here!
                  _    -> 0)

The call to 'return' will generate a (Monad m) wanted constraint; but
then there'll be "hard error" (i.e. an exception in the TcM monad), from
the unsaturated Left constructor pattern.

We'll recover in tcPolyBinds, using recoverM.  But then the final
tcSimplifyTop will see that (Monad m) constraint, with 'm' utterly
un-filled-in, and will emit a misleading error message.

The underlying problem is that an exception interrupts the constraint
gathering process. Bottom line: if we have an exception, it's best
simply to discard any gathered constraints.  Hence in 'attemptM' we
capture the constraints in a fresh variable, and only emit them into
the surrounding context if we exit normally.  If an exception is
raised, simply discard the collected constraints... we have a hard
error to report.  So this capture-the-emit dance isn't as stupid as it
looks :-).

However suppose we throw an exception inside an invocation of
captureConstraints, and discard all the constraints. Some of those
constraints might be "variable out of scope" Hole constraints, and that
might have been the actual original cause of the exception!  For
example (#12529):
   f = p @ Int
Here 'p' is out of scope, so we get an insoluble Hole constraint. But
the visible type application fails in the monad (throws an exception).
We must not discard the out-of-scope error.

It's distressingly delicate though:

* If we discard too /many/ constraints we may fail to report the error
  that led us to interrupt the constraint gathering process.

  One particular example "variable out of scope" Hole constraints. For
  example (#12529):
   f = p @ Int
  Here 'p' is out of scope, so we get an insoluble Hole constraint. But
  the visible type application fails in the monad (throws an exception).
  We must not discard the out-of-scope error.

  Also GHC.Tc.Solver.simplifyAndEmitFlatConstraints may fail having
  emitted some constraints with skolem-escape problems.

* If we discard too /few/ constraints, we may get the misleading
  class constraints mentioned above.

  We may /also/ end up taking constraints built at some inner level, and
  emitting them (via the exception catching in `tryCaptureConstraints` at some
  outer level, and then breaking the TcLevel invariants See Note [TcLevel
  invariants] in GHC.Tc.Utils.TcType

So `dropMisleading` has a horridly ad-hoc structure:

* It keeps only /insoluble/ flat constraints (which are unlikely to very visibly
  trip up on the TcLevel invariant

* But it keeps all /implication/ constraints (except the class constraints
  inside them).  The implication constraints are OK because they set the ambient
  level before attempting to solve any inner constraints.

Ugh! I hate this. But it seems to work.

Other wrinkles

(CERR1) Note that freshly-generated constraints like (Int ~ Bool), or
    ((a -> b) ~ Int) are all CNonCanonical, and hence won't be flagged as
    insoluble.  The constraint solver does that.  So they'll be discarded.
    That's probably ok; but see th/5358 as a not-so-good example:
       t1 :: Int
       t1 x = x   -- Manifestly wrong

       foo = $(...raises exception...)
    We report the exception, but not the bug in t1.  Oh well.  Possible
    solution: make GHC.Tc.Utils.Unify.uType spot manifestly-insoluble constraints.

(CERR2) In #26015 I found that from the constraints
           [W] alpha ~ Int      -- A class constraint
           [W] F alpha ~# Bool  -- An equality constraint
  we were dropping the first (becuase it's a class constraint) but not the
  second, and then getting a misleading error message from the second.  As
  #25607 shows, we can get not just one but a zillion bogus messages, which
  conceal the one genuine error.  Boo.

  For now I have added an even more ad-hoc "drop class constraints except
  equality classes (~) and (~~)"; see `dropMisleading`.  That just kicks the can
  down the road; but this problem seems somewhat rare anyway.  The code in
  `dropMisleading` hasn't changed for years.

It would be great to have a more systematic solution to this entire mess.


************************************************************************
*                                                                      *
             Template Haskell context
*                                                                      *
************************************************************************
-}

recordThUse :: TcM ()
recordThUse = do { env <- getGblEnv; writeTcRef (tcg_th_used env) True }

recordThNeededRuntimeDeps :: [Linkable] -> PkgsLoaded -> TcM ()
recordThNeededRuntimeDeps new_links new_pkgs
  = do { env <- getGblEnv
       ; updTcRef (tcg_th_needed_deps env) $ \(needed_links, needed_pkgs) ->
           let links = new_links ++ needed_links
               !pkgs = plusUDFM needed_pkgs new_pkgs
               in (links, pkgs)
       }

keepAlive :: Name -> TcRn ()     -- Record the name in the keep-alive set
keepAlive name
  = do { env <- getGblEnv
       ; traceRn "keep alive" (ppr name)
       ; updTcRef (tcg_keep env) (`extendNameSet` name) }

getThLevel :: TcM ThLevel
getThLevel = do { env <- getLclEnv; return (getLclEnvThLevel env) }

getCurrentAndBindLevel :: Name -> TcRn (Maybe (TopLevelFlag, Set.Set ThLevelIndex, ThLevel))
getCurrentAndBindLevel name
  = do { env <- getLclEnv;
       ; case lookupNameEnv (getLclEnvThBndrs env) name of
           Nothing                  -> do
              lvls <- getExternalBindLvl name
              if Set.empty == lvls
                -- This case happens when code is generated for identifiers which are not
                -- in scope.
                --
                -- TODO: What happens if someone generates [|| GHC.Magic.dataToTag# ||]
                then do
                  return Nothing
                else return (Just (TopLevel, lvls, getLclEnvThLevel env))
           Just (top_lvl, bind_lvl) -> return (Just (top_lvl, Set.singleton bind_lvl, getLclEnvThLevel env)) }

getExternalBindLvl :: Name -> TcRn (Set.Set ThLevelIndex)
getExternalBindLvl name = do
  env <- getGlobalRdrEnv
  mod <- getModule
  case lookupGRE_Name env name of
    Just gre -> return $ (Set.map thLevelIndexFromImportLevel (greLevels gre))
    Nothing ->
      if nameIsLocalOrFrom mod name
        then return $ Set.singleton topLevelIndex
        else return Set.empty

setThLevel :: ThLevel -> TcM a -> TcRn a
setThLevel l = updLclEnv (setLclEnvThLevel l)

-- | Adds the given modFinalizers to the global environment and set them to use
-- the current local environment.
addModFinalizersWithLclEnv :: ThModFinalizers -> TcM ()
addModFinalizersWithLclEnv mod_finalizers
  = do lcl_env <- getLclEnv
       th_modfinalizers_var <- fmap tcg_th_modfinalizers getGblEnv
       updTcRef th_modfinalizers_var $ \fins ->
         (lcl_env, mod_finalizers) : fins

{-
************************************************************************
*                                                                      *
             Safe Haskell context
*                                                                      *
************************************************************************
-}

-- | Mark that safe inference has failed
-- See Note [Safe Haskell Overlapping Instances Implementation]
-- although this is used for more than just that failure case.
recordUnsafeInfer :: Messages TcRnMessage -> TcM ()
recordUnsafeInfer msgs =
    getGblEnv >>= \env -> do writeTcRef (tcg_safe_infer env) False
                             writeTcRef (tcg_safe_infer_reasons env) msgs

-- | Figure out the final correct safe haskell mode
finalSafeMode :: DynFlags -> TcGblEnv -> IO SafeHaskellMode
finalSafeMode dflags tcg_env = do
    safeInf <- readIORef (tcg_safe_infer tcg_env)
    return $ case safeHaskell dflags of
        Sf_None | safeInferOn dflags && safeInf -> Sf_SafeInferred
                | otherwise                     -> Sf_None
        s -> s

-- | Switch instances to safe instances if we're in Safe mode.
fixSafeInstances :: SafeHaskellMode -> [ClsInst] -> [ClsInst]
fixSafeInstances sfMode | sfMode /= Sf_Safe && sfMode /= Sf_SafeInferred = id
fixSafeInstances _ = map fixSafe
  where fixSafe inst = let new_flag = (is_flag inst) { isSafeOverlap = True }
                       in inst { is_flag = new_flag }

{-
************************************************************************
*                                                                      *
             Stuff for the renamer's local env
*                                                                      *
************************************************************************
-}

getLocalRdrEnv :: RnM LocalRdrEnv
getLocalRdrEnv = do { env <- getLclEnv; return (getLclEnvRdrEnv env) }

setLocalRdrEnv :: LocalRdrEnv -> RnM a -> RnM a
setLocalRdrEnv rdr_env thing_inside
  = updLclEnv (setLclEnvRdrEnv rdr_env) thing_inside

{-
************************************************************************
*                                                                      *
             Stuff for interface decls
*                                                                      *
************************************************************************
-}

mkIfLclEnv :: Module -> SDoc -> IsBootInterface -> IfLclEnv
mkIfLclEnv mod loc boot
                   = IfLclEnv { if_mod     = mod,
                                if_loc     = loc,
                                if_boot    = boot,
                                if_nsubst  = Nothing,
                                if_implicits_env = Nothing,
                                if_tv_env  = emptyFsEnv,
                                if_id_env  = emptyFsEnv }

-- | Run an 'IfG' (top-level interface monad) computation inside an existing
-- 'TcRn' (typecheck-renaming monad) computation by initializing an 'IfGblEnv'
-- based on 'TcGblEnv'.
initIfaceTcRn :: IfG a -> TcRn a
initIfaceTcRn thing_inside
  = do  { tcg_env <- getGblEnv
        ; hsc_env <- getTopEnv
          -- bangs to avoid leaking the envs (#19356)
        ; let !mhome_unit = hsc_home_unit_maybe hsc_env
              !knot_vars = tcg_type_env_var tcg_env
              -- When we are instantiating a signature, we DEFINITELY
              -- do not want to knot tie.
              is_instantiate = fromMaybe False (isHomeUnitInstantiating <$> mhome_unit)
        ; let { if_env = IfGblEnv {
                            if_doc = text "initIfaceTcRn",
                            if_rec_types =
                                if is_instantiate
                                    then emptyKnotVars
                                    else readTcRef <$> knot_vars
                            }
                         }
        ; setEnvs (if_env, ()) thing_inside }

-- | 'initIfaceLoad' can be used when there's no chance that the action will
-- call 'typecheckIface' when inside a module loop and hence 'tcIfaceGlobal'.
initIfaceLoad :: HscEnv -> IfG a -> IO a
initIfaceLoad hsc_env do_this
 = do let gbl_env = IfGblEnv {
                        if_doc = text "initIfaceLoad",
                        if_rec_types = emptyKnotVars
                    }
      initTcRnIf 'i' (hsc_env { hsc_type_env_vars = emptyKnotVars }) gbl_env () do_this

-- | This is used when we are doing to call 'typecheckModule' on an 'ModIface',
-- if it's part of a loop with some other modules then we need to use their
-- IORef TypeEnv vars when typechecking but crucially not our own.
initIfaceLoadModule :: HscEnv -> Module -> IfG a -> IO a
initIfaceLoadModule hsc_env this_mod do_this
 = do let gbl_env = IfGblEnv {
                        if_doc = text "initIfaceLoadModule",
                        if_rec_types = readTcRef <$> knotVarsWithout this_mod (hsc_type_env_vars hsc_env)
                    }
      initTcRnIf 'i' hsc_env gbl_env () do_this

initIfaceCheck :: SDoc -> HscEnv -> IfG a -> IO a
-- Used when checking the up-to-date-ness of the old Iface
-- Initialise the environment with no useful info at all
initIfaceCheck doc hsc_env do_this
 = do let gbl_env = IfGblEnv {
                        if_doc = text "initIfaceCheck" <+> doc,
                        if_rec_types = readTcRef <$> hsc_type_env_vars hsc_env
                    }
      initTcRnIf 'i' hsc_env gbl_env () do_this

initIfaceLcl :: Module -> SDoc -> IsBootInterface -> IfL a -> IfM lcl a
initIfaceLcl mod loc_doc hi_boot_file thing_inside
  = setLclEnv (mkIfLclEnv mod loc_doc hi_boot_file) thing_inside

-- | Initialize interface typechecking, but with a 'NameShape'
-- to apply when typechecking top-level 'OccName's (see
-- 'lookupIfaceTop')
initIfaceLclWithSubst :: Module -> SDoc -> IsBootInterface -> NameShape -> IfL a -> IfM lcl a
initIfaceLclWithSubst mod loc_doc hi_boot_file nsubst thing_inside
  = setLclEnv ((mkIfLclEnv mod loc_doc hi_boot_file) { if_nsubst = Just nsubst }) thing_inside

getIfModule :: IfL Module
getIfModule = do { env <- getLclEnv; return (if_mod env) }

--------------------
failIfM :: SDoc -> IfL a
-- The Iface monad doesn't have a place to accumulate errors, so we
-- just fall over fast if one happens; it "shouldn't happen".
-- We use IfL here so that we can get context info out of the local env
failIfM msg = do
    env <- getLclEnv
    let full_msg = (if_loc env <> colon) $$ nest 2 msg
    logger <- getLogger
    liftIO (logMsg logger MCFatal
             noSrcSpan $ withPprStyle defaultErrStyle full_msg)
    failM

--------------------

-- | Run thing_inside in an interleaved thread.
-- It shares everything with the parent thread, so this is DANGEROUS.
--
-- It throws an error if the computation fails
--
-- It's used for lazily type-checking interface
-- signatures, which is pretty benign.
--
-- See Note [Masking exceptions in forkM]
forkM :: SDoc -> IfL a -> IfL a
forkM doc thing_inside
 = unsafeInterleaveM $ uninterruptibleMaskM_ $
    do { traceIf (text "Starting fork {" <+> doc)
       ; mb_res <- tryM $
                   updLclEnv (\env -> env { if_loc = if_loc env $$ doc }) $
                   thing_inside
       ; case mb_res of
            Right r  -> do  { traceIf (text "} ending fork" <+> doc)
                            ; return r }
            Left exn -> do {
                -- Bleat about errors in the forked thread, if -ddump-if-trace is on
                -- Otherwise we silently discard errors. Errors can legitimately
                -- happen when compiling interface signatures.
                  whenDOptM Opt_D_dump_if_trace $ do
                      logger <- getLogger
                      let msg = hang (text "forkM failed:" <+> doc)
                                   2 (text (show exn))
                      liftIO $ logMsg logger
                                         MCFatal
                                         noSrcSpan
                                         $ withPprStyle defaultErrStyle msg
                ; traceIf (text "} ending fork (badly)" <+> doc)
                ; pgmError "Cannot continue after interface file error" }
    }

setImplicitEnvM :: TypeEnv -> IfL a -> IfL a
setImplicitEnvM tenv m = updLclEnv (\lcl -> lcl
                                     { if_implicits_env = Just tenv }) m

{-
Note [Masking exceptions in forkM]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When using GHC-as-API it must be possible to interrupt snippets of code
executed using runStmt (#1381). Since commit 02c4ab04 this is almost possible
by throwing an asynchronous interrupt to the GHC thread. However, there is a
subtle problem: runStmt first typechecks the code before running it, and the
exception might interrupt the type checker rather than the code. Moreover, the
typechecker might be inside an unsafeInterleaveIO (through forkM), and
more importantly might be inside an exception handler inside that
unsafeInterleaveIO. If that is the case, the exception handler will rethrow the
asynchronous exception as a synchronous exception, and the exception will end
up as the value of the unsafeInterleaveIO thunk (see #8006 for a detailed
discussion).  We don't currently know a general solution to this problem, but
we can use uninterruptibleMask_ to avoid the situation.
-}

-- | Get the next cost centre index associated with a given name.
getCCIndexM :: (gbl -> TcRef CostCentreState) -> FastString -> TcRnIf gbl lcl CostCentreIndex
getCCIndexM get_ccs nm = do
  env <- getGblEnv
  let cc_st_ref = get_ccs env
  cc_st <- readTcRef cc_st_ref
  let (idx, cc_st') = getCCIndex nm cc_st
  writeTcRef cc_st_ref cc_st'
  return idx

-- | See 'getCCIndexM'.
getCCIndexTcM :: FastString -> TcM CostCentreIndex
getCCIndexTcM = getCCIndexM tcg_cc_st

--------------------------------------------------------------------------------

-- | Lift a computation from the dedicated zonking monad 'ZonkM' to the
-- full-fledged 'TcM' monad.
liftZonkM :: ZonkM a -> TcM a
liftZonkM (ZonkM f) =
  do { logger       <- getLogger
     ; name_ppr_ctx <- getNamePprCtx
     ; lvl          <- getTcLevel
     ; src_span     <- getSrcSpanM
     ; bndrs        <- getLclEnvBinderStack <$> getLclEnv
     ; let zge = ZonkGblEnv { zge_logger = logger
                            , zge_name_ppr_ctx = name_ppr_ctx
                            , zge_src_span = src_span
                            , zge_tc_level = lvl
                            , zge_binder_stack = bndrs }
     ; liftIO $ f zge }
{-# INLINE liftZonkM #-}

--------------------------------------------------------------------------------

getCompleteMatchesTcM :: TcM CompleteMatches
getCompleteMatchesTcM
  = do { hsc_env <- getTopEnv
       ; eps <- liftIO $ hscEPS hsc_env
       ; tcg_env <- getGblEnv
       ; let tcg_comps = tcg_complete_match_env tcg_env
       ; liftIO $ localAndImportedCompleteMatches tcg_comps eps
       }

localAndImportedCompleteMatches :: CompleteMatches -> ExternalPackageState -> IO CompleteMatches
localAndImportedCompleteMatches tcg_comps eps = do
  return $
       tcg_comps                -- from the current modulea and from the home package
    ++ eps_complete_matches eps -- from external packages
