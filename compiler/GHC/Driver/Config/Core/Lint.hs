{-# LANGUAGE LambdaCase #-}
module GHC.Driver.Config.Core.Lint
  ( endPass
  , endPassHscEnvIO
  , lintPassResult
  , lintCoreBindings
  , lintInteractiveExpr
  , initEndPassConfig
  , initLintPassResultConfig
  , initLintConfig
  ) where

import GHC.Prelude

import qualified GHC.LanguageExtensions as LangExt

import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Driver.Config.Diagnostic

import GHC.Core
import GHC.Core.Ppr
import GHC.Core.Opt.Pipeline.Types
import GHC.Core.Opt.Utils ( SimplMode(..) )
import GHC.Plugins.Monad
import GHC.Core.Coercion

import GHC.Core.Lint

import GHC.Runtime.Context

import GHC.Data.Bag

import GHC.Types.Basic ( CompilerPhase(..) )

import GHC.Utils.Outputable as Outputable

{-
These functions are not CoreM monad stuff, but they probably ought to
be, and it makes a convenient place for them.  They print out stuff
before and after core passes, and do Core Lint when necessary.
-}

endPass :: CoreToDo -> CoreProgram -> [CoreRule] -> CoreM ()
endPass pass binds rules
  = do { hsc_env <- getHscEnv
       ; print_unqual <- getPrintUnqualified
       ; liftIO $ endPassHscEnvIO hsc_env
           print_unqual pass binds rules
       }

endPassHscEnvIO :: HscEnv -> PrintUnqualified
          -> CoreToDo -> CoreProgram -> [CoreRule] -> IO ()
endPassHscEnvIO hsc_env print_unqual pass binds rules
  = do { let dflags  = hsc_dflags hsc_env
       ; endPassIO
           (hsc_logger hsc_env)
           (initEndPassConfig (hsc_IC hsc_env) dflags print_unqual pass)
           binds rules
       }

lintPassResult :: HscEnv -> CoreToDo -> CoreProgram -> IO ()
lintPassResult hsc_env pass binds
  | not (gopt Opt_DoCoreLinting dflags)
  = return ()
  | otherwise
  = lintPassResult'
    (hsc_logger hsc_env)
    (initLintPassResultConfig (hsc_IC hsc_env) dflags pass)
    binds
  where
    dflags = hsc_dflags hsc_env

-- | Type-check a 'CoreProgram'. See Note [Core Lint guarantee].
lintCoreBindings :: DynFlags -> CoreToDo -> [Var] -> CoreProgram -> WarnsAndErrs
lintCoreBindings dflags coreToDo vars -- binds
  = lintCoreBindings' $ LintConfig
      { l_diagOpts = initDiagOpts dflags
      , l_platform = targetPlatform dflags
      , l_flags    = perPassFlags dflags coreToDo
      , l_vars     = vars
      }

lintInteractiveExpr :: SDoc -- ^ The source of the linted expression
                    -> HscEnv -> CoreExpr -> IO ()
lintInteractiveExpr what hsc_env expr
  | not (gopt Opt_DoCoreLinting dflags)
  = return ()
  | Just err <- lintExpr (initLintConfig dflags $ interactiveInScope $ hsc_IC hsc_env) expr
  = displayLintResults logger False what (pprCoreExpr expr) (emptyBag, err)
  | otherwise
  = return ()
  where
    dflags = hsc_dflags hsc_env
    logger = hsc_logger hsc_env

initEndPassConfig :: InteractiveContext -> DynFlags -> PrintUnqualified -> CoreToDo -> EndPassConfig
initEndPassConfig ic dflags print_unqual pass = EndPassConfig
  { ep_dumpCoreSizes = not (gopt Opt_SuppressCoreSizes dflags)
  , ep_lintPassResult = if gopt Opt_DoCoreLinting dflags
      then Just $ initLintPassResultConfig ic dflags pass
      else Nothing
  , ep_printUnqual = print_unqual
  , ep_dumpFlag = coreDumpFlag pass
  , ep_prettyPass = ppr pass
  , ep_passDetails = pprPassDetails pass
  }

coreDumpFlag :: CoreToDo -> Maybe DumpFlag
coreDumpFlag (CoreDoSimplify {})      = Just Opt_D_verbose_core2core
coreDumpFlag (CoreDoPluginPass {})    = Just Opt_D_verbose_core2core
coreDumpFlag CoreDoFloatInwards       = Just Opt_D_verbose_core2core
coreDumpFlag (CoreDoFloatOutwards {}) = Just Opt_D_verbose_core2core
coreDumpFlag CoreLiberateCase         = Just Opt_D_verbose_core2core
coreDumpFlag CoreDoStaticArgs         = Just Opt_D_verbose_core2core
coreDumpFlag CoreDoCallArity          = Just Opt_D_dump_call_arity
coreDumpFlag CoreDoExitify            = Just Opt_D_dump_exitify
coreDumpFlag CoreDoDemand             = Just Opt_D_dump_stranal
coreDumpFlag CoreDoCpr                = Just Opt_D_dump_cpranal
coreDumpFlag CoreDoWorkerWrapper      = Just Opt_D_dump_worker_wrapper
coreDumpFlag CoreDoSpecialising       = Just Opt_D_dump_spec
coreDumpFlag CoreDoSpecConstr         = Just Opt_D_dump_spec
coreDumpFlag CoreCSE                  = Just Opt_D_dump_cse
coreDumpFlag CoreDesugar              = Just Opt_D_dump_ds_preopt
coreDumpFlag CoreDesugarOpt           = Just Opt_D_dump_ds
coreDumpFlag CoreTidy                 = Just Opt_D_dump_simpl
coreDumpFlag CorePrep                 = Just Opt_D_dump_prep

coreDumpFlag CoreAddCallerCcs         = Nothing
coreDumpFlag CoreAddLateCcs           = Nothing
coreDumpFlag CoreDoPrintCore          = Nothing
coreDumpFlag (CoreDoRuleCheck {})     = Nothing
coreDumpFlag CoreDoNothing            = Nothing
coreDumpFlag (CoreDoPasses {})        = Nothing

initLintPassResultConfig :: InteractiveContext -> DynFlags -> CoreToDo -> LintPassResultConfig
initLintPassResultConfig ic dflags pass = LintPassResultConfig
  { lpr_diagOpts      = initDiagOpts dflags
  , lpr_platform      = targetPlatform dflags
  , lpr_makeLintFlags = perPassFlags dflags pass
  , lpr_showLintWarnings = showLintWarnings pass
  , lpr_passPpr = ppr pass
  , lpr_localsInScope = interactiveInScope ic
  }

showLintWarnings :: CoreToDo -> Bool
-- Disable Lint warnings on the first simplifier pass, because
-- there may be some INLINE knots still tied, which is tiresomely noisy
showLintWarnings = \case
  (CoreDoSimplify
    (CoreDoSimplifyOpts
      _
      (SimplMode { sm_phase = InitialPhase })))
    -> False
  _ -> True

perPassFlags :: DynFlags -> CoreToDo -> LintFlags
perPassFlags dflags pass
  = (defaultLintFlags dflags)
               { lf_check_global_ids = check_globals
               , lf_check_inline_loop_breakers = check_lbs
               , lf_check_static_ptrs = check_static_ptrs
               , lf_check_linearity = check_linearity
               , lf_check_fixed_rep = check_fixed_rep }
  where
    -- In the output of the desugarer, before optimisation,
    -- we have eta-expanded data constructors with representation-polymorphic
    -- bindings; so we switch off the representation-polymorphism checks.
    -- The very simple optimiser will beta-reduce them away.
    -- See Note [Checking for representation-polymorphic built-ins]
    -- in GHC.HsToCore.Expr.
    check_fixed_rep = case pass of
                        CoreDesugar -> False
                        _           -> True

    -- See Note [Checking for global Ids]
    check_globals = case pass of
                      CoreTidy -> False
                      CorePrep -> False
                      _        -> True

    -- See Note [Checking for INLINE loop breakers]
    check_lbs = case pass of
                      CoreDesugar    -> False
                      CoreDesugarOpt -> False
                      _              -> True

    -- See Note [Checking StaticPtrs]
    check_static_ptrs | not (xopt LangExt.StaticPointers dflags) = AllowAnywhere
                      | otherwise = case pass of
                          CoreDoFloatOutwards _ -> AllowAtTopLevel
                          CoreTidy              -> RejectEverywhere
                          CorePrep              -> AllowAtTopLevel
                          _                     -> AllowAnywhere

    -- See Note [Linting linearity]
    check_linearity = gopt Opt_DoLinearCoreLinting dflags || (
                        case pass of
                          CoreDesugar -> True
                          _ -> False)

initLintConfig :: DynFlags -> [Var] -> LintConfig
initLintConfig dflags vars =LintConfig
  { l_diagOpts = initDiagOpts dflags
  , l_platform = targetPlatform dflags
  , l_flags    = defaultLintFlags dflags
  , l_vars     = vars
  }

defaultLintFlags :: DynFlags -> LintFlags
defaultLintFlags dflags = LF { lf_check_global_ids = False
                             , lf_check_inline_loop_breakers = True
                             , lf_check_static_ptrs = AllowAnywhere
                             , lf_check_linearity = gopt Opt_DoLinearCoreLinting dflags
                             , lf_report_unsat_syns = True
                             , lf_check_fixed_rep = True
                             }
