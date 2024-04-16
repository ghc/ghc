module GHC.Driver.Config.Core.Lint
  ( endPass
  , endPassHscEnvIO
  , lintCoreBindings
  , initEndPassConfig
  , initLintPassResultConfig
  , initLintConfig
  ) where

import GHC.Prelude

import qualified GHC.LanguageExtensions as LangExt

import GHC.Driver.Env
import GHC.Driver.DynFlags
import GHC.Driver.Config.Diagnostic

import GHC.Core
import GHC.Core.Lint
import GHC.Core.Lint.Interactive
import GHC.Core.Opt.Pipeline.Types
import GHC.Core.Opt.Simplify ( SimplifyOpts(..) )
import GHC.Core.Opt.Simplify.Env ( SimplMode(..) )
import GHC.Core.Opt.Monad
import GHC.Core.Coercion

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
       ; name_ppr_ctx <- getNamePprCtx
       ; liftIO $ endPassHscEnvIO hsc_env
           name_ppr_ctx pass binds rules
       }

endPassHscEnvIO :: HscEnv -> NamePprCtx
          -> CoreToDo -> CoreProgram -> [CoreRule] -> IO ()
endPassHscEnvIO hsc_env name_ppr_ctx pass binds rules
  = do { let dflags  = hsc_dflags hsc_env
       ; endPassIO
           (hsc_logger hsc_env)
           (initEndPassConfig dflags (interactiveInScope $ hsc_IC hsc_env) name_ppr_ctx pass)
           binds rules
       }

-- | Type-check a 'CoreProgram'. See Note [Core Lint guarantee].
lintCoreBindings :: DynFlags -> CoreToDo -> [Var] -> CoreProgram -> WarnsAndErrs
lintCoreBindings dflags coreToDo vars -- binds
  = lintCoreBindings' $ LintConfig
      { l_diagOpts = initDiagOpts dflags
      , l_platform = targetPlatform dflags
      , l_flags    = perPassFlags dflags coreToDo
      , l_vars     = vars
      }

initEndPassConfig :: DynFlags -> [Var] -> NamePprCtx -> CoreToDo -> EndPassConfig
initEndPassConfig dflags extra_vars name_ppr_ctx pass = EndPassConfig
  { ep_dumpCoreSizes = not (gopt Opt_SuppressCoreSizes dflags)
  , ep_lintPassResult = if gopt Opt_DoCoreLinting dflags
      then Just $ initLintPassResultConfig dflags extra_vars pass
      else Nothing
  , ep_namePprCtx = name_ppr_ctx
  , ep_dumpFlag = coreDumpFlag pass
  , ep_prettyPass = ppr pass
  , ep_passDetails = pprPassDetails pass
  }

coreDumpFlag :: CoreToDo -> Maybe DumpFlag
coreDumpFlag (CoreDoSimplify {})      = Just Opt_D_verbose_core2core
coreDumpFlag (CoreDoPluginPass {})    = Just Opt_D_verbose_core2core
coreDumpFlag CoreDoFloatInwards       = Just Opt_D_dump_float_in
coreDumpFlag (CoreDoFloatOutwards {}) = Just Opt_D_dump_float_out
coreDumpFlag CoreLiberateCase         = Just Opt_D_dump_liberate_case
coreDumpFlag CoreDoStaticArgs         = Just Opt_D_dump_static_argument_transformation
coreDumpFlag CoreDoCallArity          = Just Opt_D_dump_call_arity
coreDumpFlag CoreDoExitify            = Just Opt_D_dump_exitify
coreDumpFlag (CoreDoDemand {})        = Just Opt_D_dump_dmdanal
coreDumpFlag CoreDoCpr                = Just Opt_D_dump_cpranal
coreDumpFlag CoreDoWorkerWrapper      = Just Opt_D_dump_worker_wrapper
coreDumpFlag CoreDoSpecialising       = Just Opt_D_dump_spec
coreDumpFlag CoreDoSpecConstr         = Just Opt_D_dump_spec_constr
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

initLintPassResultConfig :: DynFlags -> [Var] -> CoreToDo -> LintPassResultConfig
initLintPassResultConfig dflags extra_vars pass = LintPassResultConfig
  { lpr_diagOpts      = initDiagOpts dflags
  , lpr_platform      = targetPlatform dflags
  , lpr_makeLintFlags = perPassFlags dflags pass
  , lpr_showLintWarnings = showLintWarnings pass
  , lpr_passPpr = ppr pass
  , lpr_localsInScope = extra_vars
  }

showLintWarnings :: CoreToDo -> Bool
-- Disable Lint warnings on the first simplifier pass, because
-- there may be some INLINE knots still tied, which is tiresomely noisy
showLintWarnings (CoreDoSimplify cfg) = case sm_phase (so_mode cfg) of
  InitialPhase -> False
  _ -> True
showLintWarnings _ = True

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
    -- See Note [Representation-polymorphism checking built-ins] in GHC.Tc.Utils.Concrete
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
