module GHC.Driver.Config.Core.Opt.Simplify
  ( initSimplifyExprOpts
  , initSimplifyOpts
  , initSimplMode
  , initGentleSimplMode
  ) where

import GHC.Prelude

import GHC.Core.Opt.Simplify ( SimplifyExprOpts(..), SimplifyOpts(..) )
import GHC.Core.Opt.Simplify.Env ( FloatEnable(..), SimplMode(..) )
import GHC.Core.Opt.Simplify.Monad ( TopEnvConfig(..) )

import GHC.Driver.Config ( initOptCoercionOpts )
import GHC.Driver.Config.Core.Lint ( defaultLintFlags, initLintPassResultConfig )
import GHC.Driver.Config.Core.Rules ( initRuleOpts )
import GHC.Driver.Config.Core.Opt.Arity ( initArityOpts )
import GHC.Driver.Session ( DynFlags(..), GeneralFlag(..), gopt )

import GHC.Runtime.Context ( InteractiveContext(..) )

import GHC.Types.Basic ( CompilerPhase(..) )
import GHC.Types.Var ( Var )

import GHC.Utils.Outputable

initSimplifyExprOpts :: DynFlags -> InteractiveContext -> SimplifyExprOpts
initSimplifyExprOpts dflags ic = SimplifyExprOpts
  { se_fam_inst = snd $ ic_instances ic
  , se_mode = (initSimplMode dflags InitialPhase "GHCi")
    { sm_inline = False
      -- Do not do any inlining, in case we expose some
      -- unboxed tuple stuff that confuses the bytecode
      -- interpreter
    }
  , se_top_env_cfg = TopEnvConfig
    { te_history_size = historySize dflags
    , te_tick_factor = simplTickFactor dflags
    }
  }

initSimplifyOpts :: DynFlags -> [Var] -> Int -> SimplMode -> SimplifyOpts
initSimplifyOpts dflags extra_vars iterations mode = SimplifyOpts
  { so_dump_core_sizes = not $ gopt Opt_SuppressCoreSizes dflags
  , so_iterations = iterations
  , so_mode = mode
  , so_pass_result_cfg = if gopt Opt_DoCoreLinting dflags
    then Just $ initLintPassResultConfig dflags extra_vars
      (defaultLintFlags dflags)
      (text "Simplifier")
      -- Disable Lint warnings on the first simplifier pass, because
      -- there may be some INLINE knots still tied, which is tiresomely noisy
      (sm_phase mode /= InitialPhase)
    else Nothing
  , so_top_env_cfg = TopEnvConfig
      { te_history_size = historySize dflags
      , te_tick_factor = simplTickFactor dflags
      }
  }

initSimplMode :: DynFlags -> CompilerPhase -> String -> SimplMode
initSimplMode dflags phase name = SimplMode
  { sm_names = [name]
  , sm_phase = phase
  , sm_rules = gopt Opt_EnableRewriteRules dflags
  , sm_eta_expand = gopt Opt_DoLambdaEtaExpansion dflags
  , sm_cast_swizzle = True
  , sm_inline = True
  , sm_uf_opts = unfoldingOpts dflags
  , sm_case_case = True
  , sm_pre_inline = gopt Opt_SimplPreInlining dflags
  , sm_float_enable = floatEnable dflags
  , sm_do_eta_reduction = gopt Opt_DoEtaReduction dflags
  , sm_arity_opts = initArityOpts dflags
  , sm_rule_opts = initRuleOpts dflags
  , sm_case_folding = gopt Opt_CaseFolding dflags
  , sm_case_merge = gopt Opt_CaseMerge dflags
  , sm_co_opt_opts = initOptCoercionOpts dflags
  }

initGentleSimplMode :: DynFlags -> SimplMode
initGentleSimplMode dflags = (initSimplMode dflags InitialPhase "Gentle")
  { -- Don't do case-of-case transformations.
    -- This makes full laziness work better
    sm_case_case = False
  }

floatEnable :: DynFlags -> FloatEnable
floatEnable dflags =
  case (gopt Opt_LocalFloatOut dflags, gopt Opt_LocalFloatOutTopLevel dflags) of
    (True, True) -> FloatEnabled
    (True, False)-> FloatNestedOnly
    (False, _)   -> FloatDisabled
