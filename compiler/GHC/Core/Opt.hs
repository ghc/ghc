{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[SimplCore]{Driver for simplifying @Core@ programs}
-}

{-# LANGUAGE CPP #-}

module GHC.Core.Opt ( CoreOptEnv (..), runCorePasses ) where

import GHC.Prelude

import GHC.Core
import GHC.Core.EndPass  ( EndPassConfig, endPass )
import GHC.Core.Opt.CSE  ( cseProgram )
import GHC.Core.Ppr     ( pprCoreBindings )
import GHC.Core.Lint    ( LintAnnotationsConfig, DebugSetting(..), lintAnnots )
import GHC.Core.Opt.Config       ( CoreToDo(..) )
import GHC.Core.Opt.CallArity    ( callArityAnalProgram )
import GHC.Core.Opt.CallerCC     ( addCallerCostCentres )
import GHC.Core.Opt.CprAnal      ( cprAnalProgram )
import GHC.Core.Opt.DmdAnal
import GHC.Core.Opt.Exitify      ( exitifyProgram )
import GHC.Core.Opt.FloatIn      ( floatInwards )
import GHC.Core.Opt.FloatOut     ( floatOutwards )
import GHC.Core.Opt.LiberateCase ( liberateCase )
import GHC.Core.Opt.RuleCheck    ( ruleCheckPass )
import GHC.Core.Opt.Simplify     ( simplifyPgm )
import GHC.Core.Opt.Simplify.Monad
import GHC.Core.Opt.SpecConstr   ( specConstrProgram )
import GHC.Core.Opt.Specialise   ( specProgram )
import GHC.Core.Opt.StaticArgs   ( doStaticArgs )
import GHC.Core.Opt.Stats        ( SimplCountM, addCounts )
import GHC.Core.Opt.WorkWrap     ( wwTopBinds )
import GHC.Core.LateCC           ( addLateCostCentresMG )
import GHC.Core.Rules            ( extendRuleBaseList, extendRuleEnv )

import GHC.Exts ( SpecConstrAnnotation )

import GHC.Plugins.Monad

import GHC.Utils.Error  ( withTiming )
import GHC.Utils.Logger as Logger
import GHC.Utils.Outputable

import GHC.Unit.External ( ExternalPackageState(..) )
import GHC.Unit.Env
import GHC.Unit.Module.Env
import GHC.Unit.Module.ModGuts

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
  , co_debugSetting  :: DebugSetting
  , co_unitEnv       :: UnitEnv
  , co_liftCoreM     :: DebugSetting -> ModGuts -> CoreM ModGuts -> SimplCountM ModGuts
  , co_hptRuleBase   :: RuleBase
    -- ^ TODO(@mmhat, @ericson2314): Why does the simplifier need this?
    -- Why can't it just use 'co_getAllRules' above?
  , co_printUnqual   :: PrintUnqualified
  , co_visOrphans    :: ModuleSet
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
    do_pass guts pass = do
      let end_pass_cfg = co_endPassCfg env  pass
      let lint_anno_cfg = co_lintAnnotationsCfg env pass
      let doCorePassWithoutDebug debug_setting = let
            env' = env { co_debugSetting = debug_setting }
            in doCorePass env' pass

      withTiming (co_logger env) (ppr pass <+> brackets (ppr this_mod)) (const ()) $ do
        guts' <- lintAnnots (co_logger env) lint_anno_cfg doCorePassWithoutDebug guts
        liftIO $ endPass (co_logger env) end_pass_cfg (mg_binds guts') (mg_rules guts')
        return guts'

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
  let !read_ruleenv = readRuleEnv env guts

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
                                 liftIO $ demandAnalysis (co_logger env) opts fam_envs guts

    CoreDoCpr                 -> {-# SCC "CprAnal" #-}
                                 updateBindsM (cprAnalProgram (co_logger env) fam_envs)

    CoreDoWorkerWrapper opts  -> {-# SCC "WorkWrap" #-} do
                                 updateBinds (wwTopBinds opts us fam_envs this_mod)

    CoreDoSpecialising opts   -> {-# SCC "Specialise" #-} do
                                 liftIO $ specProgram (co_logger env) opts (co_printUnqual env) (mg_loc guts) (co_visOrphans env) external_rule_base (co_hptRuleBase env) guts

    CoreDoSpecConstr opts     -> {-# SCC "SpecConstr" #-}
                                 return (specConstrProgram annos us opts this_mod guts)

    CoreAddCallerCcs opts     -> {-# SCC "AddCallerCcs" #-}
                                 return (addCallerCostCentres opts guts)

    CoreAddLateCcs opts       -> {-# SCC "AddLateCcs" #-}
                                 return (addLateCostCentresMG opts guts)

    CoreDoPrintCore           -> {-# SCC "PrintCore" #-} do
                                 liftIO $ logDumpMsg (co_logger env) "Print Core" $
                                   pprCoreBindings (mg_binds guts)
                                 return guts

    CoreDoRuleCheck opts      -> {-# SCC "RuleCheck" #-}
                                 liftIO $ ruleCheckPass (co_logger env) opts (co_hptRuleBase env) (co_visOrphans env) guts

    CoreDoPluginPass _ p      -> {-# SCC "Plugin" #-}
                                 co_liftCoreM env (co_debugSetting env) guts $ p guts
  where
    updateBinds f = return $ guts { mg_binds = f (mg_binds guts) }

    updateBindsM f = do
      b' <- liftIO $ f (mg_binds guts)
      return $ guts { mg_binds = b' }

readRuleEnv :: CoreOptEnv -> ModGuts -> IO RuleEnv
readRuleEnv env guts = extendRuleEnv base_ruleenv <$> read_eps_rules
  where
    hpt_rule_base = co_hptRuleBase env
    -- Forcing this value to avoid unnessecary allocations.
    -- Not doing so results in +25.6% allocations of LargeRecord.
    !rule_base = extendRuleBaseList hpt_rule_base (mg_rules guts)
    vis_orphs = co_visOrphans env
    base_ruleenv = RuleEnv [rule_base] vis_orphs
    read_eps_rules = eps_rule_base <$> co_getEps env
