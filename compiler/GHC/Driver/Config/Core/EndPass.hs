module GHC.Driver.Config.Core.EndPass
  ( initEndPassConfig
  ) where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Driver.Config.Core.Lint

import GHC.Core.EndPass
import GHC.Core.Opt.Pipeline.Types
import GHC.Core.Coercion

import GHC.Utils.Outputable as Outputable

initEndPassConfig :: DynFlags -> [Var] -> PrintUnqualified -> CoreToDo -> EndPassConfig
initEndPassConfig dflags extra_vars print_unqual pass = EndPassConfig
  { ep_dumpCoreSizes = not (gopt Opt_SuppressCoreSizes dflags)
  , ep_lintPassResult = maybeInitLintPassResultConfig dflags extra_vars
      (perPassFlags dflags pass)
      (ppr pass)
      (showLintWarnings pass)
  , ep_printUnqual = print_unqual
  , ep_dumpFlag = coreDumpFlag pass
  , ep_prettyPass = ppr pass
  , ep_passDetails = pprPassDetails pass
  }

coreDumpFlag :: CoreToDo -> Maybe DumpFlag
coreDumpFlag (CoreDoSimplify {})      = Just Opt_D_verbose_core2core
coreDumpFlag (CoreDoPluginPass {})    = Just Opt_D_verbose_core2core
coreDumpFlag (CoreDoFloatInwards {})  = Just Opt_D_verbose_core2core
coreDumpFlag (CoreDoFloatOutwards {}) = Just Opt_D_verbose_core2core
coreDumpFlag (CoreLiberateCase {})    = Just Opt_D_verbose_core2core
coreDumpFlag CoreDoStaticArgs         = Just Opt_D_verbose_core2core
coreDumpFlag CoreDoCallArity          = Just Opt_D_dump_call_arity
coreDumpFlag CoreDoExitify            = Just Opt_D_dump_exitify
coreDumpFlag CoreDoDemand             = Just Opt_D_dump_stranal
coreDumpFlag CoreDoCpr                = Just Opt_D_dump_cpranal
coreDumpFlag CoreDoWorkerWrapper      = Just Opt_D_dump_worker_wrapper
coreDumpFlag CoreDoSpecialising       = Just Opt_D_dump_spec
coreDumpFlag (CoreDoSpecConstr {})    = Just Opt_D_dump_spec
coreDumpFlag CoreCSE                  = Just Opt_D_dump_cse

coreDumpFlag (CoreAddCallerCcs {})    = Nothing
coreDumpFlag (CoreAddLateCcs {})      = Nothing
coreDumpFlag CoreDoPrintCore          = Nothing
coreDumpFlag (CoreDoRuleCheck {})     = Nothing
coreDumpFlag CoreDoNothing            = Nothing
coreDumpFlag (CoreDoPasses {})        = Nothing
