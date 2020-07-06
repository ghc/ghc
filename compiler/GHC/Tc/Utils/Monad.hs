{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

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
  getTopEnv, updTopEnv, getGblEnv, updGblEnv,
  setGblEnv, getLclEnv, updLclEnv, setLclEnv,
  getEnvs, setEnvs,
  xoptM, doptM, goptM, woptM,
  setXOptM, unsetXOptM, unsetGOptM, unsetWOptM,
  whenDOptM, whenGOptM, whenWOptM,
  whenXOptM, unlessXOptM,
  getGhcMode,
  withDynamicNow, withoutDynamicNow,
  getEpsVar,
  getEps,
  updateEps, updateEps_,
  getHpt, getEpsAndHpt,

  -- * Arrow scopes
  newArrowScope, escapeArrowScope,

  -- * Unique supply
  newUnique, newUniqueSupply, newName, newNameAt, cloneLocalName,
  newSysName, newSysLocalId, newSysLocalIds,

  -- * Accessing input/output
  newTcRef, readTcRef, writeTcRef, updTcRef,

  -- * Debugging
  traceTc, traceRn, traceOptTcRn, dumpOptTcRn,
  dumpTcRn,
  getPrintUnqualified,
  printForUserTcRn,
  traceIf, traceHiDiffs, traceOptIf,
  debugTc,

  -- * Typechecker global environment
  getIsGHCi, getGHCiMonad, getInteractivePrintName,
  tcIsHsBootOrSig, tcIsHsig, tcSelfBootInfo, getGlobalRdrEnv,
  getRdrEnvs, getImports,
  getFixityEnv, extendFixityEnv, getRecFieldEnv,
  getDeclaredDefaultTys,
  addDependentFiles,

  -- * Error management
  getSrcSpanM, setSrcSpan, setSrcSpanA, addLocM, addLocMA, inGeneratedCode,
  wrapLocM, wrapLocFstM, wrapLocFstMA, wrapLocSndM, wrapLocSndMA, wrapLocM_,
  wrapLocMA_,wrapLocMA,
  getErrsVar, setErrsVar,
  addErr,
  failWith, failAt,
  addErrAt, addErrs,
  checkErr,
  addMessages,
  discardWarnings,

  -- * Usage environment
  tcCollectingUsage, tcScalingUsage, tcEmitBindingUsage,

  -- * Shared error message stuff: renamer and typechecker
  mkLongErrAt, mkErrDocAt, addLongErrAt, reportErrors, reportError,
  reportWarning, recoverM, mapAndRecoverM, mapAndReportM, foldAndRecoverM,
  attemptM, tryTc,
  askNoErrs, discardErrs, tryTcDiscardingErrs,
  checkNoErrs, whenNoErrs,
  ifErrsM, failIfErrsM,

  -- * Context management for the type checker
  getErrCtxt, setErrCtxt, addErrCtxt, addErrCtxtM, addLandmarkErrCtxt,
  addLandmarkErrCtxtM, popErrCtxt, getCtLocM, setCtLocM,

  -- * Error message generation (type checker)
  addErrTc,
  addErrTcM,
  failWithTc, failWithTcM,
  checkTc, checkTcM,
  failIfTc, failIfTcM,
  warnIfFlag, warnIf, warnTc, warnTcM,
  addWarnTc, addWarnTcM, addWarn, addWarnAt, add_warn,
  mkErrInfo,

  -- * Type constraints
  newTcEvBinds, newNoTcEvBinds, cloneEvBindsVar,
  addTcEvBind, addTopEvBinds,
  getTcEvTyCoVars, getTcEvBindsMap, setTcEvBindsMap,
  chooseUniqueOccTc,
  getConstraintVar, setConstraintVar,
  emitConstraints, emitStaticConstraints, emitSimple, emitSimples,
  emitImplication, emitImplications, emitInsoluble,
  emitHole, emitHoles,
  discardConstraints, captureConstraints, tryCaptureConstraints,
  pushLevelAndCaptureConstraints,
  pushTcLevelM_, pushTcLevelM, pushTcLevelsM,
  getTcLevel, setTcLevel, isTouchableTcM,
  getLclTypeEnv, setLclTypeEnv,
  traceTcConstraints,
  emitNamedTypeHole, IsExtraConstraint(..), emitAnonTypeHole,

  -- * Template Haskell context
  recordThUse, recordThSpliceUse,
  keepAlive, getStage, getStageAndBindLevel, setStage,
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
  getIfModule,
  failIfM,
  forkM_maybe,
  forkM,
  setImplicitEnvM,

  withException,

  -- * Stuff for cost centres.
  getCCIndexM, getCCIndexTcM,

  -- * Types etc.
  module GHC.Tc.Types,
  module GHC.Data.IOEnv
  ) where

#include "HsVersions.h"

import GHC.Prelude


import GHC.Builtin.Names

import GHC.Tc.Types     -- Re-export all
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType

import GHC.Hs hiding (LIE)

import GHC.Unit
import GHC.Unit.External
import GHC.Unit.Module.Warnings
import GHC.Unit.Home.ModInfo

import GHC.Core.UsageEnv
import GHC.Core.Multiplicity
import GHC.Core.InstEnv
import GHC.Core.FamInstEnv

import GHC.Driver.Env
import GHC.Driver.Ppr
import GHC.Driver.Session

import GHC.Runtime.Context

import GHC.Data.IOEnv -- Re-export all
import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Data.Maybe

import GHC.Utils.Outputable as Outputable
import GHC.Utils.Error
import GHC.Utils.Panic
import GHC.Utils.Misc

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
import GHC.Types.Unique (uniqFromMask)
import GHC.Types.Unique.Supply
import GHC.Types.Annotations
import GHC.Types.Basic( TopLevelFlag, TypeOrKind(..) )
import GHC.Types.CostCentre.State
import GHC.Types.SourceFile

import qualified GHC.LanguageExtensions as LangExt

import Data.IORef
import Control.Monad

import {-# SOURCE #-} GHC.Tc.Utils.Env    ( tcInitTidyEnv )

import qualified Data.Map as Map

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
       -> IO (Messages, Maybe r)
                -- Nothing => error thrown by the thing inside
                -- (error messages should have been printed already)

initTc hsc_env hsc_src keep_rn_syntax mod loc do_this
 = do { keep_var     <- newIORef emptyNameSet ;
        used_gre_var <- newIORef [] ;
        th_var       <- newIORef False ;
        th_splice_var<- newIORef False ;
        infer_var    <- newIORef (True, emptyBag) ;
        dfun_n_var   <- newIORef emptyOccSet ;
        type_env_var <- case hsc_type_env_var hsc_env of {
                           Just (_mod, te_var) -> return te_var ;
                           Nothing             -> newIORef emptyNameEnv } ;

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
        let {
             dflags = hsc_dflags hsc_env ;
             home_unit = hsc_home_unit hsc_env ;

             maybe_rn_syntax :: forall a. a -> Maybe a ;
             maybe_rn_syntax empty_val
                | dopt Opt_D_dump_rn_ast dflags = Just empty_val

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

                tcg_mod            = mod,
                tcg_semantic_mod   = homeModuleInstantiation home_unit mod,
                tcg_src            = hsc_src,
                tcg_rdr_env        = emptyGlobalRdrEnv,
                tcg_fix_env        = emptyNameEnv,
                tcg_field_env      = emptyNameEnv,
                tcg_default        = if moduleUnit mod == primUnit
                                     || moduleUnit mod == bignumUnit
                                     then Just []  -- See Note [Default types]
                                     else Nothing,
                tcg_type_env       = emptyNameEnv,
                tcg_type_env_var   = type_env_var,
                tcg_inst_env       = emptyInstEnv,
                tcg_fam_inst_env   = emptyFamInstEnv,
                tcg_ann_env        = emptyAnnEnv,
                tcg_th_used        = th_var,
                tcg_th_splice_used = th_splice_var,
                tcg_exports        = [],
                tcg_imports        = emptyImportAvails,
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
                tcg_ev_binds       = emptyBag,
                tcg_warns          = NoWarnings,
                tcg_anns           = [],
                tcg_tcs            = [],
                tcg_insts          = [],
                tcg_fam_insts      = [],
                tcg_rules          = [],
                tcg_fords          = [],
                tcg_patsyns        = [],
                tcg_merged         = [],
                tcg_dfun_n         = dfun_n_var,
                tcg_keep           = keep_var,
                tcg_doc_hdr        = Nothing,
                tcg_hpc            = False,
                tcg_main           = Nothing,
                tcg_self_boot      = NoSelfBoot,
                tcg_safeInfer      = infer_var,
                tcg_dependent_files = dependent_files_var,
                tcg_tc_plugins     = [],
                tcg_hf_plugins     = [],
                tcg_top_loc        = loc,
                tcg_static_wc      = static_wc_var,
                tcg_complete_matches = [],
                tcg_cc_st          = cc_st_var
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
              -> IO (Messages, Maybe r)
initTcWithGbl hsc_env gbl_env loc do_this
 = do { lie_var      <- newIORef emptyWC
      ; errs_var     <- newIORef (emptyBag, emptyBag)
      ; usage_var    <- newIORef zeroUE
      ; let lcl_env = TcLclEnv {
                tcl_errs       = errs_var,
                tcl_loc        = loc,
                -- tcl_loc should be over-ridden very soon!
                tcl_in_gen_code = False,
                tcl_ctxt       = [],
                tcl_rdr        = emptyLocalRdrEnv,
                tcl_th_ctxt    = topStage,
                tcl_th_bndrs   = emptyNameEnv,
                tcl_arrow_ctxt = NoArrowCtxt,
                tcl_env        = emptyNameEnv,
                tcl_usage      = usage_var,
                tcl_bndrs      = [],
                tcl_lie        = lie_var,
                tcl_tclvl      = topTcLevel
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

      ; let { final_res | errorsFound dflags msgs = Nothing
                        | otherwise               = maybe_res }

      ; return (msgs, final_res)
      }
  where dflags = hsc_dflags hsc_env

initTcInteractive :: HscEnv -> TcM a -> IO (Messages, Maybe a)
-- Initialise the type checker monad for use in GHCi
initTcInteractive hsc_env thing_inside
  = initTc hsc_env HsSrcFile False
           (icInteractiveModule (hsc_IC hsc_env))
           (realSrcLocSpan interactive_src_loc)
           thing_inside
  where
    interactive_src_loc = mkRealSrcLoc (fsLit "<interactive>") 1 1

{- Note [Default types]
~~~~~~~~~~~~~~~~~~~~~~~
The Integer type is simply not available in ghc-prim and ghc-bignum packages (it
is declared in ghc-bignum). So we set the defaulting types to (Just []), meaning
there are no default types, rather than Nothing, which means "use the default
default types of Integer, Double".

If you don't do this, attempted defaulting in package ghc-prim causes
an actual crash (attempting to look up the Integer type).


************************************************************************
*                                                                      *
                Initialisation
*                                                                      *
************************************************************************
-}

initTcRnIf :: Char              -- ^ Mask for unique supply
           -> HscEnv
           -> gbl -> lcl
           -> TcRnIf gbl lcl a
           -> IO a
initTcRnIf uniq_mask hsc_env gbl_env lcl_env thing_inside
   = do { let { env = Env { env_top = hsc_env,
                            env_um  = uniq_mask,
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

getGblEnv :: TcRnIf gbl lcl gbl
getGblEnv = do { Env{..} <- getEnv; return env_gbl }

updGblEnv :: (gbl -> gbl) -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
updGblEnv upd = updEnv (\ env@(Env { env_gbl = gbl }) ->
                          env { env_gbl = upd gbl })

setGblEnv :: gbl -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
setGblEnv gbl_env = updEnv (\ env -> env { env_gbl = gbl_env })

getLclEnv :: TcRnIf gbl lcl lcl
getLclEnv = do { Env{..} <- getEnv; return env_lcl }

updLclEnv :: (lcl -> lcl) -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
updLclEnv upd = updEnv (\ env@(Env { env_lcl = lcl }) ->
                          env { env_lcl = upd lcl })

setLclEnv :: lcl' -> TcRnIf gbl lcl' a -> TcRnIf gbl lcl a
setLclEnv lcl_env = updEnv (\ env -> env { env_lcl = lcl_env })

getEnvs :: TcRnIf gbl lcl (gbl, lcl)
getEnvs = do { env <- getEnv; return (env_gbl env, env_lcl env) }

setEnvs :: (gbl', lcl') -> TcRnIf gbl' lcl' a -> TcRnIf gbl lcl a
setEnvs (gbl_env, lcl_env) = updEnv (\ env -> env { env_gbl = gbl_env, env_lcl = lcl_env })

-- Command-line flags

xoptM :: LangExt.Extension -> TcRnIf gbl lcl Bool
xoptM flag = do { dflags <- getDynFlags; return (xopt flag dflags) }

doptM :: DumpFlag -> TcRnIf gbl lcl Bool
doptM flag = do { dflags <- getDynFlags; return (dopt flag dflags) }

goptM :: GeneralFlag -> TcRnIf gbl lcl Bool
goptM flag = do { dflags <- getDynFlags; return (gopt flag dflags) }

woptM :: WarningFlag -> TcRnIf gbl lcl Bool
woptM flag = do { dflags <- getDynFlags; return (wopt flag dflags) }

setXOptM :: LangExt.Extension -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
setXOptM flag =
  updTopEnv (\top -> top { hsc_dflags = xopt_set (hsc_dflags top) flag})

unsetXOptM :: LangExt.Extension -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
unsetXOptM flag =
  updTopEnv (\top -> top { hsc_dflags = xopt_unset (hsc_dflags top) flag})

unsetGOptM :: GeneralFlag -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
unsetGOptM flag =
  updTopEnv (\top -> top { hsc_dflags = gopt_unset (hsc_dflags top) flag})

unsetWOptM :: WarningFlag -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
unsetWOptM flag =
  updTopEnv (\top -> top { hsc_dflags = wopt_unset (hsc_dflags top) flag})

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
getGhcMode = do { env <- getTopEnv; return (ghcMode (hsc_dflags env)) }

withDynamicNow :: TcRnIf gbl lcl a -> TcRnIf gbl lcl a
withDynamicNow =
  updTopEnv (\top@(HscEnv { hsc_dflags = dflags }) ->
              top { hsc_dflags = setDynamicNow dflags })

withoutDynamicNow :: TcRnIf gbl lcl a -> TcRnIf gbl lcl a
withoutDynamicNow =
  updTopEnv (\top@(HscEnv { hsc_dflags = dflags }) ->
              top { hsc_dflags = dflags { dynamicNow = False} })

getEpsVar :: TcRnIf gbl lcl (TcRef ExternalPackageState)
getEpsVar = do { env <- getTopEnv; return (hsc_EPS env) }

getEps :: TcRnIf gbl lcl ExternalPackageState
getEps = do { env <- getTopEnv; readMutVar (hsc_EPS env) }

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

getEpsAndHpt :: TcRnIf gbl lcl (ExternalPackageState, HomePackageTable)
getEpsAndHpt = do { env <- getTopEnv; eps <- readMutVar (hsc_EPS env)
                  ; return (eps, hsc_HPT env) }

-- | A convenient wrapper for taking a @MaybeErr MsgDoc a@ and throwing
-- an exception if it is an error.
withException :: TcRnIf gbl lcl (MaybeErr MsgDoc a) -> TcRnIf gbl lcl a
withException do_this = do
    r <- do_this
    dflags <- getDynFlags
    case r of
        Failed err -> liftIO $ throwGhcExceptionIO (ProgramError (showSDoc dflags err))
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
  = updLclEnv $ \env -> env { tcl_arrow_ctxt = ArrowCtxt (tcl_rdr env) (tcl_lie env) }

-- Return to the stored environment (from the enclosing proc)
escapeArrowScope :: TcM a -> TcM a
escapeArrowScope
  = updLclEnv $ \ env ->
    case tcl_arrow_ctxt env of
      NoArrowCtxt       -> env
      ArrowCtxt rdr_env lie -> env { tcl_arrow_ctxt = NoArrowCtxt
                                   , tcl_lie = lie
                                   , tcl_rdr = rdr_env }

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
      ; let mask = env_um env
      ; liftIO $! uniqFromMask mask }

newUniqueSupply :: TcRnIf gbl lcl UniqSupply
newUniqueSupply
 = do { env <- getEnv
      ; let mask = env_um env
      ; liftIO $! mkSplitUniqSupply mask }

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
  = do  { us <- newUniqueSupply
        ; let mkId' n (Scaled w t) = mkSysLocal fs n w t
        ; return (zipWith mkId' (uniqsFromSupply us) tys) }

instance MonadUnique (IOEnv (Env gbl lcl)) where
        getUniqueM = newUnique
        getUniqueSupplyM = newUniqueSupply

{-
************************************************************************
*                                                                      *
                Accessing input/output
*                                                                      *
************************************************************************
-}

newTcRef :: a -> TcRnIf gbl lcl (TcRef a)
newTcRef = newMutVar

readTcRef :: TcRef a -> TcRnIf gbl lcl a
readTcRef = readMutVar

writeTcRef :: TcRef a -> a -> TcRnIf gbl lcl ()
writeTcRef = writeMutVar

updTcRef :: TcRef a -> (a -> a) -> TcRnIf gbl lcl ()
-- Returns ()
updTcRef ref fn = liftIO $ do { old <- readIORef ref
                              ; writeIORef ref (fn old) }

{-
************************************************************************
*                                                                      *
                Debugging
*                                                                      *
************************************************************************
-}

-- Note [INLINE conditional tracing utilities]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- In general we want to optimise for the case where tracing is not enabled.
-- To ensure this happens, we ensure that traceTc and friends are inlined; this
-- ensures that the allocation of the document can be pushed into the tracing
-- path, keeping the non-traced path free of this extraneous work. For
-- instance, instead of
--
--     let thunk = ...
--     in if doTracing
--          then emitTraceMsg thunk
--          else return ()
--
-- where the conditional is buried in a non-inlined utility function (e.g.
-- traceTc), we would rather have:
--
--     if doTracing
--       then let thunk = ...
--            in emitTraceMsg thunk
--       else return ()
--
-- See #18168.
--

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
    dumpTcRn False (dumpOptionsFromFlag flag) "" FormatText doc
{-# INLINE traceOptTcRn #-} -- see Note [INLINE conditional tracing utilities]

-- | Dump if the given 'DumpFlag' is set.
dumpOptTcRn :: DumpFlag -> String -> DumpFormat -> SDoc -> TcRn ()
dumpOptTcRn flag title fmt doc =
  whenDOptM flag $
    dumpTcRn False (dumpOptionsFromFlag flag) title fmt doc
{-# INLINE dumpOptTcRn #-} -- see Note [INLINE conditional tracing utilities]

-- | Unconditionally dump some trace output
--
-- Certain tests (T3017, Roles3, T12763 etc.) expect part of the
-- output generated by `-ddump-types` to be in 'PprUser' style. However,
-- generally we want all other debugging output to use 'PprDump'
-- style. We 'PprUser' style if 'useUserStyle' is True.
--
dumpTcRn :: Bool -> DumpOptions -> String -> DumpFormat -> SDoc -> TcRn ()
dumpTcRn useUserStyle dumpOpt title fmt doc = do
  dflags <- getDynFlags
  printer <- getPrintUnqualified dflags
  real_doc <- wrapDocLoc doc
  let sty = if useUserStyle
              then mkUserStyle printer AllTheWay
              else mkDumpStyle printer
  liftIO $ dumpAction dflags sty dumpOpt title fmt real_doc

-- | Add current location if -dppr-debug
-- (otherwise the full location is usually way too much)
wrapDocLoc :: SDoc -> TcRn SDoc
wrapDocLoc doc = do
  dflags <- getDynFlags
  if hasPprDebug dflags
    then do
      loc <- getSrcSpanM
      return (mkLocMessage SevOutput loc doc)
    else
      return doc

getPrintUnqualified :: DynFlags -> TcRn PrintUnqualified
getPrintUnqualified dflags
  = do { rdr_env <- getGlobalRdrEnv
       ; hsc_env <- getTopEnv
       ; let unit_state = unitState dflags
       ; let home_unit  = hsc_home_unit hsc_env
       ; return $ mkPrintUnqualified unit_state home_unit rdr_env }

-- | Like logInfoTcRn, but for user consumption
printForUserTcRn :: SDoc -> TcRn ()
printForUserTcRn doc
  = do { dflags <- getDynFlags
       ; printer <- getPrintUnqualified dflags
       ; liftIO (printOutputForUser dflags printer doc) }

{-
traceIf and traceHiDiffs work in the TcRnIf monad, where no RdrEnv is
available.  Alas, they behave inconsistently with the other stuff;
e.g. are unaffected by -dump-to-file.
-}

traceIf, traceHiDiffs :: SDoc -> TcRnIf m n ()
traceIf      = traceOptIf Opt_D_dump_if_trace
traceHiDiffs = traceOptIf Opt_D_dump_hi_diffs
{-# INLINE traceIf #-}
{-# INLINE traceHiDiffs #-}
  -- see Note [INLINE conditional tracing utilities]

traceOptIf :: DumpFlag -> SDoc -> TcRnIf m n ()
traceOptIf flag doc
  = whenDOptM flag $    -- No RdrEnv available, so qualify everything
    do { dflags <- getDynFlags
       ; liftIO (putMsg dflags doc) }
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
tcIsHsBootOrSig = do { env <- getGblEnv; return (isHsBootOrSig (tcg_src env)) }

tcIsHsig :: TcRn Bool
tcIsHsig = do { env <- getGblEnv; return (isHsigFile (tcg_src env)) }

tcSelfBootInfo :: TcRn SelfBootInfo
tcSelfBootInfo = do { env <- getGblEnv; return (tcg_self_boot env) }

getGlobalRdrEnv :: TcRn GlobalRdrEnv
getGlobalRdrEnv = do { env <- getGblEnv; return (tcg_rdr_env env) }

getRdrEnvs :: TcRn (GlobalRdrEnv, LocalRdrEnv)
getRdrEnvs = do { (gbl,lcl) <- getEnvs; return (tcg_rdr_env gbl, tcl_rdr lcl) }

getImports :: TcRn ImportAvails
getImports = do { env <- getGblEnv; return (tcg_imports env) }

getFixityEnv :: TcRn FixityEnv
getFixityEnv = do { env <- getGblEnv; return (tcg_fix_env env) }

extendFixityEnv :: [(Name,FixItem)] -> RnM a -> RnM a
extendFixityEnv new_bit
  = updGblEnv (\env@(TcGblEnv { tcg_fix_env = old_fix_env }) ->
                env {tcg_fix_env = extendNameEnvList old_fix_env new_bit})

getRecFieldEnv :: TcRn RecFieldEnv
getRecFieldEnv = do { env <- getGblEnv; return (tcg_field_env env) }

getDeclaredDefaultTys :: TcRn (Maybe [Type])
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
getSrcSpanM = do { env <- getLclEnv; return (RealSrcSpan (tcl_loc env) Nothing) }

-- See Note [Rebindable syntax and HsExpansion].
inGeneratedCode :: TcRn Bool
inGeneratedCode = tcl_in_gen_code <$> getLclEnv

setSrcSpan :: SrcSpan -> TcRn a -> TcRn a
setSrcSpan (RealSrcSpan loc _) thing_inside =
  updLclEnv (\env -> env { tcl_loc = loc, tcl_in_gen_code = False })
            thing_inside
setSrcSpan loc@(UnhelpfulSpan _) thing_inside
  -- See Note [Rebindable syntax and HsExpansion].
  | isGeneratedSrcSpan loc =
      updLclEnv (\env -> env { tcl_in_gen_code = True }) thing_inside
  | otherwise = thing_inside

setSrcSpanA :: SrcSpanAnn' ann -> TcRn a -> TcRn a
setSrcSpanA l = setSrcSpan (locA l)

-- setSrcSpanN :: SrcSpanAnnName -> TcRn a -> TcRn a
-- setSrcSpanN l = setSrcSpan (locA l)

addLocM :: (a -> TcM b) -> Located a -> TcM b
addLocM fn (L loc a) = setSrcSpan loc $ fn a

addLocMA :: (a -> TcM b) -> GenLocated (SrcSpanAnn' ann) a -> TcM b
-- addLocMA :: (t -> TcRn a) -> GenLocated (SrcSpanAnn' ann) t -> TcRn a
addLocMA fn (L loc a) = setSrcSpanA loc $ fn a

-- addLocMN :: (a -> TcM b) -> LocatedN a -> TcM b
-- addLocMN fn (L loc a) = setSrcSpanA loc $ fn a

wrapLocM :: (a -> TcM b) -> Located a -> TcM (Located b)
wrapLocM fn (L loc a) = setSrcSpan loc $ do { b <- fn a
                                            ; return (L loc b) }

-- wrapLocMA :: (a -> TcM b) -> LocatedA a -> TcM (LocatedA b)
wrapLocMA :: (a -> TcM b) -> GenLocated (SrcSpanAnn' ann) a -> TcRn (GenLocated (SrcSpanAnn' ann) b)
wrapLocMA fn (L loc a) = setSrcSpanA loc $ do { b <- fn a
                                              ; return (L loc b) }

-- wrapLocMN :: (a -> TcM b) -> LocatedN a -> TcM (LocatedN b)
-- wrapLocMN fn (L loc a) = setSrcSpanA loc $ do { b <- fn a
--                                               ; return (L loc b) }

wrapLocFstM :: (a -> TcM (b,c)) -> Located a -> TcM (Located b, c)
wrapLocFstM fn (L loc a) =
  setSrcSpan loc $ do
    (b,c) <- fn a
    return (L loc b, c)

wrapLocFstMA :: (a -> TcM (b,c)) -> LocatedA a -> TcM (LocatedA b, c)
wrapLocFstMA fn (L loc a) =
  setSrcSpanA loc $ do
    (b,c) <- fn a
    return (L loc b, c)

wrapLocSndM :: (a -> TcM (b, c)) -> Located a -> TcM (b, Located c)
wrapLocSndM fn (L loc a) =
  setSrcSpan loc $ do
    (b,c) <- fn a
    return (b, L loc c)

wrapLocSndMA :: (a -> TcM (b, c)) -> LocatedA a -> TcM (b, LocatedA c)
wrapLocSndMA fn (L loc a) =
  setSrcSpanA loc $ do
    (b,c) <- fn a
    return (b, L loc c)

wrapLocM_ :: (a -> TcM ()) -> Located a -> TcM ()
wrapLocM_ fn (L loc a) = setSrcSpan loc (fn a)

wrapLocMA_ :: (a -> TcM ()) -> LocatedA a -> TcM ()
wrapLocMA_ fn (L loc a) = setSrcSpan (locA loc) (fn a)

-- Reporting errors

getErrsVar :: TcRn (TcRef Messages)
getErrsVar = do { env <- getLclEnv; return (tcl_errs env) }

setErrsVar :: TcRef Messages -> TcRn a -> TcRn a
setErrsVar v = updLclEnv (\ env -> env { tcl_errs =  v })

addErr :: MsgDoc -> TcRn ()
addErr msg = do { loc <- getSrcSpanM; addErrAt loc msg }

failWith :: MsgDoc -> TcRn a
failWith msg = addErr msg >> failM

failAt :: SrcSpan -> MsgDoc -> TcRn a
failAt loc msg = addErrAt loc msg >> failM

addErrAt :: SrcSpan -> MsgDoc -> TcRn ()
-- addErrAt is mainly (exclusively?) used by the renamer, where
-- tidying is not an issue, but it's all lazy so the extra
-- work doesn't matter
addErrAt loc msg = do { ctxt <- getErrCtxt
                      ; tidy_env <- tcInitTidyEnv
                      ; err_info <- mkErrInfo tidy_env ctxt
                      ; addLongErrAt loc msg err_info }

addErrs :: [(SrcSpan,MsgDoc)] -> TcRn ()
addErrs msgs = mapM_ add msgs
             where
               add (loc,msg) = addErrAt loc msg

checkErr :: Bool -> MsgDoc -> TcRn ()
-- Add the error if the bool is False
checkErr ok msg = unless ok (addErr msg)

addMessages :: Messages -> TcRn ()
addMessages msgs1
  = do { errs_var <- getErrsVar ;
         msgs0 <- readTcRef errs_var ;
         writeTcRef errs_var (unionMessages msgs0 msgs1) }

discardWarnings :: TcRn a -> TcRn a
-- Ignore warnings inside the thing inside;
-- used to ignore-unused-variable warnings inside derived code
discardWarnings thing_inside
  = do  { errs_var <- getErrsVar
        ; (old_warns, _) <- readTcRef errs_var

        ; result <- thing_inside

        -- Revert warnings to old_warns
        ; (_new_warns, new_errs) <- readTcRef errs_var
        ; writeTcRef errs_var (old_warns, new_errs)

        ; return result }

{-
************************************************************************
*                                                                      *
        Shared error message stuff: renamer and typechecker
*                                                                      *
************************************************************************
-}

mkLongErrAt :: SrcSpan -> MsgDoc -> MsgDoc -> TcRn ErrMsg
mkLongErrAt loc msg extra
  = do { dflags <- getDynFlags ;
         printer <- getPrintUnqualified dflags ;
         unit_state <- unitState <$> getDynFlags ;
         let msg' = pprWithUnitState unit_state msg in
         return $ mkLongErrMsg dflags loc printer msg' extra }

mkErrDocAt :: SrcSpan -> ErrDoc -> TcRn ErrMsg
mkErrDocAt loc errDoc
  = do { dflags <- getDynFlags ;
         printer <- getPrintUnqualified dflags ;
         unit_state <- unitState <$> getDynFlags ;
         let f = pprWithUnitState unit_state
             errDoc' = mapErrDoc f errDoc
         in
         return $ mkErrDoc dflags loc printer errDoc' }

addLongErrAt :: SrcSpan -> MsgDoc -> MsgDoc -> TcRn ()
addLongErrAt loc msg extra = mkLongErrAt loc msg extra >>= reportError

reportErrors :: [ErrMsg] -> TcM ()
reportErrors = mapM_ reportError

reportError :: ErrMsg -> TcRn ()
reportError err
  = do { traceTc "Adding error:" (pprLocErrMsg err) ;
         errs_var <- getErrsVar ;
         (warns, errs) <- readTcRef errs_var ;
         writeTcRef errs_var (warns, errs `snocBag` err) }

reportWarning :: WarnReason -> ErrMsg -> TcRn ()
reportWarning reason err
  = do { let warn = makeIntoWarning reason err
                    -- 'err' was built by mkLongErrMsg or something like that,
                    -- so it's of error severity.  For a warning we downgrade
                    -- its severity to SevWarning

       ; traceTc "Adding warning:" (pprLocErrMsg warn)
       ; errs_var <- getErrsVar
       ; (warns, errs) <- readTcRef errs_var
       ; writeTcRef errs_var (warns `snocBag` warn, errs) }


-----------------------
checkNoErrs :: TcM r -> TcM r
-- (checkNoErrs m) succeeds iff m succeeds and generates no errors
-- If m fails then (checkNoErrsTc m) fails.
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
        dflags <- getDynFlags ;
        if errorsFound dflags msgs then
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
ininings seem unobjectional, and they solve the immediate
problem. -}

getErrCtxt :: TcM [ErrCtxt]
getErrCtxt = do { env <- getLclEnv; return (tcl_ctxt env) }

setErrCtxt :: [ErrCtxt] -> TcM a -> TcM a
{-# INLINE setErrCtxt #-}   -- Note [Inlining addErrCtxt]
setErrCtxt ctxt = updLclEnv (\ env -> env { tcl_ctxt = ctxt })

-- | Add a fixed message to the error context. This message should not
-- do any tidying.
addErrCtxt :: MsgDoc -> TcM a -> TcM a
{-# INLINE addErrCtxt #-}   -- Note [Inlining addErrCtxt]
addErrCtxt msg = addErrCtxtM (\env -> return (env, msg))

-- | Add a message to the error context. This message may do tidying.
addErrCtxtM :: (TidyEnv -> TcM (TidyEnv, MsgDoc)) -> TcM a -> TcM a
{-# INLINE addErrCtxtM #-}  -- Note [Inlining addErrCtxt]
addErrCtxtM ctxt m = updCtxt (push_ctxt (False, ctxt)) m

-- | Add a fixed landmark message to the error context. A landmark
-- message is always sure to be reported, even if there is a lot of
-- context. It also doesn't count toward the maximum number of contexts
-- reported.
addLandmarkErrCtxt :: MsgDoc -> TcM a -> TcM a
{-# INLINE addLandmarkErrCtxt #-}  -- Note [Inlining addErrCtxt]
addLandmarkErrCtxt msg = addLandmarkErrCtxtM (\env -> return (env, msg))

-- | Variant of 'addLandmarkErrCtxt' that allows for monadic operations
-- and tidying.
addLandmarkErrCtxtM :: (TidyEnv -> TcM (TidyEnv, MsgDoc)) -> TcM a -> TcM a
{-# INLINE addLandmarkErrCtxtM #-}  -- Note [Inlining addErrCtxt]
addLandmarkErrCtxtM ctxt m = updCtxt (push_ctxt (True, ctxt)) m

push_ctxt :: (Bool, TidyEnv -> TcM (TidyEnv, MsgDoc))
          -> Bool -> [ErrCtxt] -> [ErrCtxt]
push_ctxt ctxt in_gen ctxts
  | in_gen    = ctxts
  | otherwise = ctxt : ctxts

updCtxt :: (Bool -> [ErrCtxt] -> [ErrCtxt]) -> TcM a -> TcM a
{-# INLINE updCtxt #-} -- Note [Inlining addErrCtxt]
-- Helper function for the above
-- The Bool is true if we are in generated code
updCtxt upd = updLclEnv (\ env@(TcLclEnv { tcl_ctxt = ctxt
                                         , tcl_in_gen_code = in_gen }) ->
                           env { tcl_ctxt = upd in_gen ctxt })

popErrCtxt :: TcM a -> TcM a
popErrCtxt = updCtxt (\ _ msgs -> case msgs of { [] -> []; (_ : ms) -> ms })

getCtLocM :: CtOrigin -> Maybe TypeOrKind -> TcM CtLoc
getCtLocM origin t_or_k
  = do { env <- getLclEnv
       ; return (CtLoc { ctl_origin = origin
                       , ctl_env    = env
                       , ctl_t_or_k = t_or_k
                       , ctl_depth  = initialSubGoalDepth }) }

setCtLocM :: CtLoc -> TcM a -> TcM a
-- Set the SrcSpan and error context from the CtLoc
setCtLocM (CtLoc { ctl_env = lcl }) thing_inside
  = updLclEnv (\env -> env { tcl_loc   = tcl_loc lcl
                           , tcl_bndrs = tcl_bndrs lcl
                           , tcl_ctxt  = tcl_ctxt lcl })
              thing_inside


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

capture_messages :: TcM r -> TcM (r, Messages)
-- capture_messages simply captures and returns the
--                  errors arnd warnings generated by thing_inside
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
                          ; dflags <- getDynFlags
                          ; let errs_found = errorsFound dflags msgs
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
       ; let lie_to_keep = case mb_res of
                             Nothing -> dropMisleading lie
                             Just {} -> lie

       ; return (mb_res, lie_to_keep) }

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
  = do { env0 <- getLclEnv
       ; local_usage_ref <- newTcRef zeroUE
       ; let env1 = env0 { tcl_usage = local_usage_ref }
       ; result <- setLclEnv env1 thing_inside
       ; local_usage <- readTcRef local_usage_ref
       ; return (local_usage,result) }

-- | @tcScalingUsage mult thing_inside@ runs @thing_inside@ and scales all the
-- usage information by @mult@.
tcScalingUsage :: Mult -> TcM a -> TcM a
tcScalingUsage mult thing_inside
  = do { (usage, result) <- tcCollectingUsage thing_inside
       ; traceTc "tcScalingUsage" (ppr mult)
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
tryTc :: TcRn a -> TcRn (Maybe a, Messages)
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
tryTcDiscardingErrs recover thing_inside
  = do { ((mb_res, lie), msgs) <- capture_messages    $
                                  capture_constraints $
                                  tcTryM thing_inside
        ; dflags <- getDynFlags
        ; case mb_res of
            Just res | not (errorsFound dflags msgs)
                     , not (insolubleWC lie)
              -> -- 'main' succeeded with no errors
                 do { addMessages msgs  -- msgs might still have warnings
                    ; emitConstraints lie
                    ; return res }

            _ -> -- 'main' failed, or produced an error message
                 recover     -- Discard all errors and warnings
                             -- and unsolved constraints entirely
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

addErrTc :: MsgDoc -> TcM ()
addErrTc err_msg = do { env0 <- tcInitTidyEnv
                      ; addErrTcM (env0, err_msg) }

addErrTcM :: (TidyEnv, MsgDoc) -> TcM ()
addErrTcM (tidy_env, err_msg)
  = do { ctxt <- getErrCtxt ;
         loc  <- getSrcSpanM ;
         add_err_tcm tidy_env err_msg loc ctxt }

-- The failWith functions add an error message and cause failure

failWithTc :: MsgDoc -> TcM a               -- Add an error message and fail
failWithTc err_msg
  = addErrTc err_msg >> failM

failWithTcM :: (TidyEnv, MsgDoc) -> TcM a   -- Add an error message and fail
failWithTcM local_and_msg
  = addErrTcM local_and_msg >> failM

checkTc :: Bool -> MsgDoc -> TcM ()         -- Check that the boolean is true
checkTc True  _   = return ()
checkTc False err = failWithTc err

checkTcM :: Bool -> (TidyEnv, MsgDoc) -> TcM ()
checkTcM True  _   = return ()
checkTcM False err = failWithTcM err

failIfTc :: Bool -> MsgDoc -> TcM ()         -- Check that the boolean is false
failIfTc False _   = return ()
failIfTc True  err = failWithTc err

failIfTcM :: Bool -> (TidyEnv, MsgDoc) -> TcM ()
   -- Check that the boolean is false
failIfTcM False _   = return ()
failIfTcM True  err = failWithTcM err


--         Warnings have no 'M' variant, nor failure

-- | Display a warning if a condition is met,
--   and the warning is enabled
warnIfFlag :: WarningFlag -> Bool -> MsgDoc -> TcRn ()
warnIfFlag warn_flag is_bad msg
  = do { warn_on <- woptM warn_flag
       ; when (warn_on && is_bad) $
         addWarn (Reason warn_flag) msg }

-- | Display a warning if a condition is met.
warnIf :: Bool -> MsgDoc -> TcRn ()
warnIf is_bad msg
  = when is_bad (addWarn NoReason msg)

-- | Display a warning if a condition is met.
warnTc :: WarnReason -> Bool -> MsgDoc -> TcM ()
warnTc reason warn_if_true warn_msg
  | warn_if_true = addWarnTc reason warn_msg
  | otherwise    = return ()

-- | Display a warning if a condition is met.
warnTcM :: WarnReason -> Bool -> (TidyEnv, MsgDoc) -> TcM ()
warnTcM reason warn_if_true warn_msg
  | warn_if_true = addWarnTcM reason warn_msg
  | otherwise    = return ()

-- | Display a warning in the current context.
addWarnTc :: WarnReason -> MsgDoc -> TcM ()
addWarnTc reason msg
 = do { env0 <- tcInitTidyEnv ;
      addWarnTcM reason (env0, msg) }

-- | Display a warning in a given context.
addWarnTcM :: WarnReason -> (TidyEnv, MsgDoc) -> TcM ()
addWarnTcM reason (env0, msg)
 = do { ctxt <- getErrCtxt ;
        err_info <- mkErrInfo env0 ctxt ;
        add_warn reason msg err_info }

-- | Display a warning for the current source location.
addWarn :: WarnReason -> MsgDoc -> TcRn ()
addWarn reason msg = add_warn reason msg Outputable.empty

-- | Display a warning for a given source location.
addWarnAt :: WarnReason -> SrcSpan -> MsgDoc -> TcRn ()
addWarnAt reason loc msg = add_warn_at reason loc msg Outputable.empty

-- | Display a warning, with an optional flag, for the current source
-- location.
add_warn :: WarnReason -> MsgDoc -> MsgDoc -> TcRn ()
add_warn reason msg extra_info
  = do { loc <- getSrcSpanM
       ; add_warn_at reason loc msg extra_info }

-- | Display a warning, with an optional flag, for a given location.
add_warn_at :: WarnReason -> SrcSpan -> MsgDoc -> MsgDoc -> TcRn ()
add_warn_at reason loc msg extra_info
  = do { dflags <- getDynFlags ;
         printer <- getPrintUnqualified dflags ;
         let { warn = mkLongWarnMsg dflags loc printer
                                    msg extra_info } ;
         reportWarning reason warn }


{-
-----------------------------------
        Other helper functions
-}

add_err_tcm :: TidyEnv -> MsgDoc -> SrcSpan
            -> [ErrCtxt]
            -> TcM ()
add_err_tcm tidy_env err_msg loc ctxt
 = do { err_info <- mkErrInfo tidy_env ctxt ;
        addLongErrAt loc err_msg err_info }

mkErrInfo :: TidyEnv -> [ErrCtxt] -> TcM SDoc
-- Tidy the error info, trimming excessive contexts
mkErrInfo env ctxts
--  = do
--       dbg <- hasPprDebug <$> getDynFlags
--       if dbg                -- In -dppr-debug style the output
--          then return empty  -- just becomes too voluminous
--          else go dbg 0 env ctxts
 = go False 0 env ctxts
 where
   go :: Bool -> Int -> TidyEnv -> [ErrCtxt] -> TcM SDoc
   go _ _ _   [] = return empty
   go dbg n env ((is_landmark, ctxt) : ctxts)
     | is_landmark || n < mAX_CONTEXTS -- Too verbose || dbg
     = do { (env', msg) <- ctxt env
          ; let n' = if is_landmark then n else n+1
          ; rest <- go dbg n' env' ctxts
          ; return (msg $$ rest) }
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

chooseUniqueOccTc :: (OccSet -> OccName) -> TcM OccName
chooseUniqueOccTc fn =
  do { env <- getGblEnv
     ; let dfun_n_var = tcg_dfun_n env
     ; set <- readTcRef dfun_n_var
     ; let occ = fn set
     ; writeTcRef dfun_n_var (extendOccSet set occ)
     ; return occ }

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

emitInsoluble :: Ct -> TcM ()
emitInsoluble ct
  = do { traceTc "emitInsoluble" (ppr ct)
       ; lie_var <- getConstraintVar
       ; updTcRef lie_var (`addInsols` unitBag ct) }

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

-- | Throw out any constraints emitted by the thing_inside
discardConstraints :: TcM a -> TcM a
discardConstraints thing_inside = fst <$> captureConstraints thing_inside

-- | The name says it all. The returned TcLevel is the *inner* TcLevel.
pushLevelAndCaptureConstraints :: TcM a -> TcM (TcLevel, WantedConstraints, a)
pushLevelAndCaptureConstraints thing_inside
  = do { env <- getLclEnv
       ; let tclvl' = pushTcLevel (tcl_tclvl env)
       ; traceTc "pushLevelAndCaptureConstraints {" (ppr tclvl')
       ; (res, lie) <- setLclEnv (env { tcl_tclvl = tclvl' }) $
                       captureConstraints thing_inside
       ; traceTc "pushLevelAndCaptureConstraints }" (ppr tclvl')
       ; return (tclvl', lie, res) }

pushTcLevelM_ :: TcM a -> TcM a
pushTcLevelM_ x = updLclEnv (\ env -> env { tcl_tclvl = pushTcLevel (tcl_tclvl env) }) x

pushTcLevelM :: TcM a -> TcM (TcLevel, a)
-- See Note [TcLevel assignment] in GHC.Tc.Utils.TcType
pushTcLevelM thing_inside
  = do { env <- getLclEnv
       ; let tclvl' = pushTcLevel (tcl_tclvl env)
       ; res <- setLclEnv (env { tcl_tclvl = tclvl' })
                          thing_inside
       ; return (tclvl', res) }

-- Returns pushed TcLevel
pushTcLevelsM :: Int -> TcM a -> TcM (a, TcLevel)
pushTcLevelsM num_levels thing_inside
  = do { env <- getLclEnv
       ; let tclvl' = nTimes num_levels pushTcLevel (tcl_tclvl env)
       ; res <- setLclEnv (env { tcl_tclvl = tclvl' }) $
                thing_inside
       ; return (res, tclvl') }

getTcLevel :: TcM TcLevel
getTcLevel = do { env <- getLclEnv
                ; return (tcl_tclvl env) }

setTcLevel :: TcLevel -> TcM a -> TcM a
setTcLevel tclvl thing_inside
  = updLclEnv (\env -> env { tcl_tclvl = tclvl }) thing_inside

isTouchableTcM :: TcTyVar -> TcM Bool
isTouchableTcM tv
  = do { lvl <- getTcLevel
       ; return (isTouchableMetaTyVar lvl tv) }

getLclTypeEnv :: TcM TcTypeEnv
getLclTypeEnv = do { env <- getLclEnv; return (tcl_env env) }

setLclTypeEnv :: TcLclEnv -> TcM a -> TcM a
-- Set the local type envt, but do *not* disturb other fields,
-- notably the lie_var
setLclTypeEnv lcl_env thing_inside
  = updLclEnv upd thing_inside
  where
    upd env = env { tcl_env = tcl_env lcl_env }

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
                         , hole_occ  = occ
                         , hole_ty   = mkTyVarTy tv
                         , hole_loc  = ct_loc }
       ; emitHole hole }
  where
    occ = mkTyVarOcc "_"
    sort | YesExtraConstraint <- extra_constraints = ConstraintHole
         | otherwise                               = TypeHole

emitNamedTypeHole :: (Name, TcTyVar) -> TcM ()
emitNamedTypeHole (name, tv)
  = do { ct_loc <- setSrcSpan (nameSrcSpan name) $
                   getCtLocM (TypeHoleOrigin occ) Nothing
       ; let hole = Hole { hole_sort = TypeHole
                         , hole_occ  = occ
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
  that led us to interrupte the constraint gathering process.

  One particular example "variable out of scope" Hole constraints. For
  example (#12529):
   f = p @ Int
  Here 'p' is out of scope, so we get an insoluble Hole constraint. But
  the visible type application fails in the monad (throws an exception).
  We must not discard the out-of-scope error.

  Also GHC.Tc.Solver.simplifyAndEmitFlatConstraints may fail having
  emitted some constraints with skolem-escape problems.

* If we discard too /few/ constraints, we may get the misleading
  class constraints mentioned above.  But we may /also/ end up taking
  constraints built at some inner level, and emitting them at some
  outer level, and then breaking the TcLevel invariants
  See Note [TcLevel and untouchable type variables] in GHC.Tc.Utils.TcType

So dropMisleading has a horridly ad-hoc structure.  It keeps only
/insoluble/ flat constraints (which are unlikely to very visibly trip
up on the TcLevel invariant, but all /implication/ constraints (except
the class constraints inside them).  The implication constraints are
OK because they set the ambient level before attempting to solve any
inner constraints.  Ugh! I hate this. But it seems to work.

However note that freshly-generated constraints like (Int ~ Bool), or
((a -> b) ~ Int) are all CNonCanonical, and hence won't be flagged as
insoluble.  The constraint solver does that.  So they'll be discarded.
That's probably ok; but see th/5358 as a not-so-good example:
   t1 :: Int
   t1 x = x   -- Manifestly wrong

   foo = $(...raises exception...)
We report the exception, but not the bug in t1.  Oh well.  Possible
solution: make GHC.Tc.Utils.Unify.uType spot manifestly-insoluble constraints.


************************************************************************
*                                                                      *
             Template Haskell context
*                                                                      *
************************************************************************
-}

recordThUse :: TcM ()
recordThUse = do { env <- getGblEnv; writeTcRef (tcg_th_used env) True }

recordThSpliceUse :: TcM ()
recordThSpliceUse = do { env <- getGblEnv; writeTcRef (tcg_th_splice_used env) True }

keepAlive :: Name -> TcRn ()     -- Record the name in the keep-alive set
keepAlive name
  = do { env <- getGblEnv
       ; traceRn "keep alive" (ppr name)
       ; updTcRef (tcg_keep env) (`extendNameSet` name) }

getStage :: TcM ThStage
getStage = do { env <- getLclEnv; return (tcl_th_ctxt env) }

getStageAndBindLevel :: Name -> TcRn (Maybe (TopLevelFlag, ThLevel, ThStage))
getStageAndBindLevel name
  = do { env <- getLclEnv;
       ; case lookupNameEnv (tcl_th_bndrs env) name of
           Nothing                  -> return Nothing
           Just (top_lvl, bind_lvl) -> return (Just (top_lvl, bind_lvl, tcl_th_ctxt env)) }

setStage :: ThStage -> TcM a -> TcRn a
setStage s = updLclEnv (\ env -> env { tcl_th_ctxt = s })

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
recordUnsafeInfer :: WarningMessages -> TcM ()
recordUnsafeInfer warns =
    getGblEnv >>= \env -> writeTcRef (tcg_safeInfer env) (False, warns)

-- | Figure out the final correct safe haskell mode
finalSafeMode :: DynFlags -> TcGblEnv -> IO SafeHaskellMode
finalSafeMode dflags tcg_env = do
    safeInf <- fst <$> readIORef (tcg_safeInfer tcg_env)
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
getLocalRdrEnv = do { env <- getLclEnv; return (tcl_rdr env) }

setLocalRdrEnv :: LocalRdrEnv -> RnM a -> RnM a
setLocalRdrEnv rdr_env thing_inside
  = updLclEnv (\env -> env {tcl_rdr = rdr_env}) thing_inside

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
        ; let !mod = tcg_semantic_mod tcg_env
              home_unit = hsc_home_unit hsc_env
              -- When we are instantiating a signature, we DEFINITELY
              -- do not want to knot tie.
              is_instantiate = isHomeUnitInstantiating home_unit
        ; let { if_env = IfGblEnv {
                            if_doc = text "initIfaceTcRn",
                            if_rec_types =
                                if is_instantiate
                                    then Nothing
                                    else Just (mod, get_type_env)
                         }
              ; get_type_env = readTcRef (tcg_type_env_var tcg_env) }
        ; setEnvs (if_env, ()) thing_inside }

-- Used when sucking in a ModIface into a ModDetails to put in
-- the HPT.  Notably, unlike initIfaceCheck, this does NOT use
-- hsc_type_env_var (since we're not actually going to typecheck,
-- so this variable will never get updated!)
initIfaceLoad :: HscEnv -> IfG a -> IO a
initIfaceLoad hsc_env do_this
 = do let gbl_env = IfGblEnv {
                        if_doc = text "initIfaceLoad",
                        if_rec_types = Nothing
                    }
      initTcRnIf 'i' hsc_env gbl_env () do_this

initIfaceCheck :: SDoc -> HscEnv -> IfG a -> IO a
-- Used when checking the up-to-date-ness of the old Iface
-- Initialise the environment with no useful info at all
initIfaceCheck doc hsc_env do_this
 = do let rec_types = case hsc_type_env_var hsc_env of
                         Just (mod,var) -> Just (mod, readTcRef var)
                         Nothing        -> Nothing
          gbl_env = IfGblEnv {
                        if_doc = text "initIfaceCheck" <+> doc,
                        if_rec_types = rec_types
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
failIfM :: MsgDoc -> IfL a
-- The Iface monad doesn't have a place to accumulate errors, so we
-- just fall over fast if one happens; it "shouldn't happen".
-- We use IfL here so that we can get context info out of the local env
failIfM msg
  = do  { env <- getLclEnv
        ; let full_msg = (if_loc env <> colon) $$ nest 2 msg
        ; dflags <- getDynFlags
        ; liftIO (putLogMsg dflags NoReason SevFatal
                   noSrcSpan $ withPprStyle defaultErrStyle full_msg)
        ; failM }

--------------------

-- | Run thing_inside in an interleaved thread.
-- It shares everything with the parent thread, so this is DANGEROUS.
--
-- It returns Nothing if the computation fails
--
-- It's used for lazily type-checking interface
-- signatures, which is pretty benign.
--
-- See Note [Masking exceptions in forkM_maybe]
forkM_maybe :: SDoc -> IfL a -> IfL (Maybe a)
forkM_maybe doc thing_inside
 = unsafeInterleaveM $ uninterruptibleMaskM_ $
    do { traceIf (text "Starting fork {" <+> doc)
       ; mb_res <- tryM $
                   updLclEnv (\env -> env { if_loc = if_loc env $$ doc }) $
                   thing_inside
       ; case mb_res of
            Right r  -> do  { traceIf (text "} ending fork" <+> doc)
                            ; return (Just r) }
            Left exn -> do {
                -- Bleat about errors in the forked thread, if -ddump-if-trace is on
                -- Otherwise we silently discard errors. Errors can legitimately
                -- happen when compiling interface signatures (see tcInterfaceSigs)
                  whenDOptM Opt_D_dump_if_trace $ do
                      dflags <- getDynFlags
                      let msg = hang (text "forkM failed:" <+> doc)
                                   2 (text (show exn))
                      liftIO $ putLogMsg dflags
                                         NoReason
                                         SevFatal
                                         noSrcSpan
                                         $ withPprStyle defaultErrStyle msg

                ; traceIf (text "} ending fork (badly)" <+> doc)
                ; return Nothing }
    }

forkM :: SDoc -> IfL a -> IfL a
forkM doc thing_inside
 = do   { mb_res <- forkM_maybe doc thing_inside
        ; return (case mb_res of
                        Nothing -> pgmError "Cannot continue after interface file error"
                                   -- pprPanic "forkM" doc
                        Just r  -> r) }

setImplicitEnvM :: TypeEnv -> IfL a -> IfL a
setImplicitEnvM tenv m = updLclEnv (\lcl -> lcl
                                     { if_implicits_env = Just tenv }) m

{-
Note [Masking exceptions in forkM_maybe]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When using GHC-as-API it must be possible to interrupt snippets of code
executed using runStmt (#1381). Since commit 02c4ab04 this is almost possible
by throwing an asynchronous interrupt to the GHC thread. However, there is a
subtle problem: runStmt first typechecks the code before running it, and the
exception might interrupt the type checker rather than the code. Moreover, the
typechecker might be inside an unsafeInterleaveIO (through forkM_maybe), and
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
