module GHC.Driver.Config.Core.EndPass
  ( endPass
  , endPassDesugarBefore
  , endPassLintFlags
  , defaultLintFlags
  , lintPassResult
  , lintCoreBindings
  , initEndPassConfig
  , initLintAnnotationsConfig
  , initLintPassResultConfig
  , initLintConfig
  ) where

import GHC.Prelude

import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Driver.Config.Core.Lint

import GHC.Core
import GHC.Core.EndPass
import GHC.Core.Lint
import GHC.Core.Lint.Interactive
import GHC.Core.Opt.Pipeline.Types
import GHC.Core.Coercion

import GHC.Utils.Outputable as Outputable

{-
These functions are not CoreM monad stuff, but they probably ought to
be, and it makes a convenient place for them.  They print out stuff
before and after core passes, and do Core Lint when necessary.
-}

endPass :: HscEnv -> PrintUnqualified -> CoreToDo -> CoreProgram -> [CoreRule] -> IO ()
endPass hsc_env print_unqual pass binds rules = do
  let dflags  = hsc_dflags hsc_env
  endPassIO
    (hsc_logger hsc_env)
    (initEndPassConfig dflags (interactiveInScope $ hsc_IC hsc_env) print_unqual pass)
    binds rules

endPassDesugarBefore :: HscEnv -> PrintUnqualified -> CoreProgram -> [CoreRule] -> IO ()
endPassDesugarBefore hsc_env print_unqual binds rules = do
  let dflags  = hsc_dflags hsc_env
  endPassIO
    (hsc_logger hsc_env)
    (desugarBeforeConfig dflags (interactiveInScope $ hsc_IC hsc_env) print_unqual)
    binds rules

endPassLintFlags :: HscEnv -> PrintUnqualified -> Maybe DumpFlag -> LintFlags -> SDoc -> SDoc -> Bool -> CoreProgram -> [CoreRule] -> IO ()
endPassLintFlags hsc_env print_unqual dump_flag lint_flags pretty_pass pass_details show_lint_warnings binds rules = do
  let dflags  = hsc_dflags hsc_env
  endPassIO
    (hsc_logger hsc_env)
    (initEndPassConfig' dflags (interactiveInScope $ hsc_IC hsc_env) print_unqual dump_flag lint_flags pretty_pass pass_details show_lint_warnings)
    binds rules

initEndPassConfig :: DynFlags -> [Var] -> PrintUnqualified -> CoreToDo -> EndPassConfig
initEndPassConfig dflags extra_vars print_unqual pass =
  initEndPassConfig' dflags extra_vars print_unqual
        (coreDumpFlag pass)
        (perPassFlags dflags pass)
        (ppr pass)
        (pprPassDetails pass)
        (showLintWarnings pass)

desugarBeforeConfig :: DynFlags -> [Var] -> PrintUnqualified -> EndPassConfig
desugarBeforeConfig dflags extra_vars print_unqual =
  initEndPassConfig' dflags extra_vars print_unqual
        (Just Opt_D_dump_ds_preopt)
        (desugarBeforeFlags dflags)
        (text "Desugar (before optimization)")
        (Outputable.empty)
        True

initEndPassConfig'
  :: DynFlags -> [Var] -> PrintUnqualified -> Maybe DumpFlag -> LintFlags
  -> SDoc -> SDoc -> Bool -> EndPassConfig
initEndPassConfig' dflags extra_vars print_unqual dump_flag lint_flags pretty_pass pass_details show_lint_warnings = EndPassConfig
  { ep_dumpCoreSizes = not (gopt Opt_SuppressCoreSizes dflags)
  , ep_lintPassResult = if gopt Opt_DoCoreLinting dflags
      then Just $ initLintPassResultConfig dflags extra_vars
        lint_flags
        pretty_pass
        show_lint_warnings
      else Nothing
  , ep_printUnqual = print_unqual
  , ep_dumpFlag = dump_flag
  , ep_prettyPass = pretty_pass
  , ep_passDetails = pass_details
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
coreDumpFlag CoreDesugarOpt           = Just Opt_D_dump_ds
coreDumpFlag CoreTidy                 = Just Opt_D_dump_simpl
coreDumpFlag CorePrep                 = Just Opt_D_dump_prep

coreDumpFlag CoreAddCallerCcs         = Nothing
coreDumpFlag CoreAddLateCcs           = Nothing
coreDumpFlag CoreDoPrintCore          = Nothing
coreDumpFlag (CoreDoRuleCheck {})     = Nothing
coreDumpFlag CoreDoNothing            = Nothing
coreDumpFlag (CoreDoPasses {})        = Nothing
