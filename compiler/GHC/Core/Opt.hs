{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[SimplCore]{Driver for simplifying @Core@ programs}
-}

{-# LANGUAGE CPP #-}

module GHC.Core.Opt ( CoreOptEnv (..), runCorePasses ) where

import GHC.Prelude

import GHC.Driver.Flags ( DumpFlag ( Opt_D_dump_str_signatures ) )

import GHC.Core
import GHC.Core.EndPass  ( EndPassConfig, endPassIO )
import GHC.Core.Opt.CSE  ( cseProgram )
import GHC.Core.Rules   ( ruleCheckProgram, getRules )
import GHC.Core.Ppr     ( pprCoreBindings )
import GHC.Core.Utils   ( dumpIdInfoOfProgram )
import GHC.Core.Lint    ( LintAnnotationsConfig, lintAnnots )
import GHC.Core.Opt.Config ( CoreToDo(..) )
import GHC.Core.Opt.Simplify ( simplifyPgm )
import GHC.Core.Opt.Simplify.Monad
import GHC.Core.Opt.Stats        ( SimplCountM, addCounts )
import GHC.Core.Opt.FloatIn      ( floatInwards )
import GHC.Core.Opt.FloatOut     ( floatOutwards )
import GHC.Core.Opt.LiberateCase ( liberateCase )
import GHC.Core.Opt.StaticArgs   ( doStaticArgs )
import GHC.Core.Opt.Specialise   ( specProgram )
import GHC.Core.Opt.SpecConstr   ( specConstrProgram )
import GHC.Core.Opt.DmdAnal
import GHC.Core.Opt.CprAnal      ( cprAnalProgram )
import GHC.Core.Opt.CallArity    ( callArityAnalProgram )
import GHC.Core.Opt.Exitify      ( exitifyProgram )
import GHC.Core.Opt.WorkWrap     ( wwTopBinds )
import GHC.Core.Opt.CallerCC     ( addCallerCostCentres )
import GHC.Core.LateCC           ( addLateCostCentresMG )
import GHC.Core.Seq (seqBinds)
import GHC.Core.FamInstEnv

import GHC.Exts ( SpecConstrAnnotation )

import GHC.Plugins.Monad

import GHC.Utils.Error  ( withTiming )
import GHC.Utils.Logger as Logger
import GHC.Utils.Outputable

import GHC.Unit.External ( ExternalPackageState(..) )
import GHC.Unit.Env
import GHC.Unit.Module.Env
import GHC.Unit.Module.ModGuts

import GHC.Types.Id.Info
import GHC.Types.Basic
import GHC.Types.Demand ( zapDmdEnvSig )
import GHC.Types.Unique.Supply ( mkSplitUniqSupply )
import GHC.Types.Var ( Var )
import GHC.Types.Name.Env ( NameEnv )

import Control.Monad

{-
************************************************************************
*                                                                      *
                  The CoreToDo interpreter
*                                                                      *
************************************************************************
-}

-- | @['CoreToDo']@ is the plan, this is everything we need to execute
-- that plan.
data CoreOptEnv = CoreOptEnv
  { co_logger        :: Logger
  , co_unitEnv       :: UnitEnv
  , co_liftCoreM     :: ModGuts -> CoreM ModGuts -> SimplCountM ModGuts
  , co_getAllRules   :: ModGuts -> IO RuleEnv
  , co_hptRuleBase   :: RuleBase
    -- ^ TODO(@mmhat, @ericson2314): Why does the simplifier need this?
    -- Why can't it just use 'co_getAllRules' above?
  , co_printUnqual   :: PrintUnqualified
  , co_visOrphans    :: ModuleSet
  , co_hasPprDebug   :: Bool
  , co_getEps        :: IO ExternalPackageState
  , co_extraVars     :: [Var]
  , co_specConstrAnn :: ModGuts -> IO (NameEnv SpecConstrAnnotation)
  , co_endPassCfg    :: CoreToDo -> EndPassConfig
  , co_lintAnnotationsCfg :: CoreToDo -> LintAnnotationsConfig
  }

-- | Run a core pipeline, as specified with a list of 'CoreToDo'.
--
-- In typical ussage, the "plannning" of what passes to run (i.e.
-- creation of the '[CoreToDo]') happens in
-- 'GHC.Driver.Config.Core.Opt'. Then this function "executes" that
-- plan.
runCorePasses :: CoreOptEnv
              -> [CoreToDo]
              -> ModGuts
              -> SimplCountM ModGuts
runCorePasses env passes guts
  = foldM do_pass guts passes
  where
    do_pass :: ModGuts -> CoreToDo -> SimplCountM ModGuts
    do_pass res CoreDoNothing = return res
    do_pass guts (CoreDoPasses ps) = runCorePasses env ps guts
    do_pass guts pass = do
      let end_pass_cfg = co_endPassCfg env  pass
      let lint_anno_cfg = co_lintAnnotationsCfg env pass
      let doCorePassWithDebug _debug_lvl nguts = do
            -- ERICSON2314 TO MMHAT:
            --
            -- This is rather intersting, this no longer works because
            -- of the planning vs non-planning separation! This means
            -- instead of doing
            --
            -- > | CoreToDoVariantBesidesDoPassses Config
            --
            -- We need to probably do
            --
            -- > | CoreToDoVariantBesidesDoPassses (DebugLevel -> Config)
            --
            -- so we can "defer" that one part of the planning for
            -- 'lintAnnots' to try in multiple ways.
            --
            -- Does that makes sense?

            -- let dflags' = (hsc_dflags hsc_env) { debugLevel = debug_lvl }
            --     hsc_env' = hsc_env { hsc_dflags = dflags' }
            doCorePass env pass nguts

      withTiming (co_logger env) (ppr pass <+> brackets (ppr this_mod)) (const ()) $ do
        guts' <- lintAnnots (co_logger env) lint_anno_cfg doCorePassWithDebug guts
        liftIO $ endPassIO (co_logger env) end_pass_cfg (mg_binds guts') (mg_rules guts')
        return guts'

    -- dflags = hsc_dflags hsc_env
    this_mod = mg_module guts

-- | Run a single core pass, as specified with a single 'CoreToDo'.
doCorePass :: CoreOptEnv
           -> CoreToDo
           -> ModGuts
           -> SimplCountM ModGuts
doCorePass env pass guts = do
  eps <- liftIO $ co_getEps env
  annos <- liftIO $ co_specConstrAnn env guts
  us <- liftIO $ mkSplitUniqSupply simplMask

  let this_mod = mg_module guts
  let external_rule_base = eps_rule_base eps
  let p_fam_env = eps_fam_inst_env eps
  let fam_envs = (p_fam_env, mg_fam_inst_env guts)
  let !read_ruleenv = co_getAllRules env guts

  case pass of
    CoreDoSimplify opts       -> {-# SCC "Simplify" #-} do
                                 (guts', sc) <- liftIO $ simplifyPgm (co_logger env) read_ruleenv (co_unitEnv env) opts guts
                                 addCounts sc
                                 return guts'

    CoreCSE                   -> {-# SCC "CommonSubExpr" #-}
                                 updateBinds cseProgram

    CoreLiberateCase opts     -> {-# SCC "LiberateCase" #-} do
                                 updateBinds (liberateCase opts)

    CoreDoFloatInwards opts   -> {-# SCC "FloatInwards" #-}
                                 updateBinds (floatInwards opts)

    CoreDoFloatOutwards opts  -> {-# SCC "FloatOutwards" #-}
                                 updateBindsM (floatOutwards (co_logger env) opts us)

    CoreDoStaticArgs          -> {-# SCC "StaticArgs" #-}
                                 updateBinds (doStaticArgs us)

    CoreDoCallArity           -> {-# SCC "CallArity" #-}
                                 updateBinds callArityAnalProgram

    CoreDoExitify             -> {-# SCC "Exitify" #-}
                                 updateBinds exitifyProgram

    CoreDoDemand opts         -> {-# SCC "DmdAnal" #-}
                                 updateBindsM (dmdAnal (co_logger env) (co_hasPprDebug env) opts fam_envs (mg_rules guts))

    CoreDoCpr                 -> {-# SCC "CprAnal" #-}
                                 updateBindsM (cprAnalProgram (co_logger env) fam_envs)

    CoreDoWorkerWrapper f     -> {-# SCC "WorkWrap" #-} do
                                 let opts = f (mg_module guts) fam_envs
                                 updateBinds (wwTopBinds opts us)

    CoreDoSpecialising f      -> {-# SCC "Specialise" #-} do
                                 let opts = f (co_printUnqual env)
                                 liftIO $ specProgram (co_logger env) opts (mg_loc guts) (co_visOrphans env) external_rule_base (co_hptRuleBase env) guts

    CoreDoSpecConstr opts     -> {-# SCC "SpecConstr" #-}
                                 return (specConstrProgram annos us opts this_mod guts)

    CoreAddCallerCcs opts     -> {-# SCC "AddCallerCcs" #-}
                                 return (addCallerCostCentres opts guts)

    CoreAddLateCcs opts       -> {-# SCC "AddLateCcs" #-}
                                 return (addLateCostCentresMG opts guts)

    CoreDoPrintCore           -> {-# SCC "PrintCore" #-} do
                                 liftIO $ printCore (co_logger env) (mg_binds guts)
                                 return guts

    CoreDoRuleCheck opts phase pat
                              -> {-# SCC "RuleCheck" #-}
                                 liftIO $ ruleCheckPass (co_logger env) opts (co_hptRuleBase env)  -- TODO: Own config record
                                                        (co_visOrphans env) phase pat guts

    CoreDoNothing             -> return guts

    CoreDoPasses passes       -> runCorePasses env passes guts

    CoreDoPluginPass _ p      -> {-# SCC "Plugin" #-} do
                                 co_liftCoreM env guts $ p guts
  where
    updateBinds f = return $ guts { mg_binds = f (mg_binds guts) }

    updateBindsM f = do
      b' <- liftIO $ f (mg_binds guts)
      return $ guts { mg_binds = b' }

{-
************************************************************************
*                                                                      *
\subsection{Core pass combinators}
*                                                                      *
************************************************************************
-}

printCore :: Logger -> CoreProgram -> IO ()
printCore logger binds
    = Logger.logDumpMsg logger "Print Core" (pprCoreBindings binds)

ruleCheckPass :: Logger
              -> RuleOpts
              -> RuleBase
              -> ModuleSet
              -> CompilerPhase
              -> String
              -> ModGuts
              -> IO ModGuts
ruleCheckPass logger ropts rb vis_orphs current_phase pat guts =
    withTiming logger (text "RuleCheck"<+>brackets (ppr $ mg_module guts))
                (const ()) $ do
        let rule_fn fn = getRules (RuleEnv [rb] vis_orphs) fn
                          ++ (mg_rules guts)
        logDumpMsg logger "Rule check" $
            ruleCheckProgram ropts current_phase pat rule_fn (mg_binds guts)
        return guts

dmdAnal :: Logger
        -> Bool
        -> DmdAnalOpts
        -> (FamInstEnv, FamInstEnv)
        -> [CoreRule]
        -> CoreProgram
        -> IO CoreProgram
dmdAnal logger has_ppr_debug opts fam_envs rules binds = do
  let binds_plus_dmds = dmdAnalProgram opts fam_envs rules binds
  -- TODO Ericson2314: Why are we gated on a specific dump flag and the master one??
  Logger.putDumpFileMaybe logger Opt_D_dump_str_signatures "Strictness signatures" FormatText $
    dumpIdInfoOfProgram has_ppr_debug (ppr . zapDmdEnvSig . dmdSigInfo) binds_plus_dmds
  -- See Note [Stamp out space leaks in demand analysis] in GHC.Core.Opt.DmdAnal
  seqBinds binds_plus_dmds `seq` return binds_plus_dmds
