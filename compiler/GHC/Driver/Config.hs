-- | Subsystem configuration
module GHC.Driver.Config
   ( initOptCoercionOpts
   , initSimpleOpts
   , initEvalOpts
   )
where

import GHC.Prelude

import GHC.Driver.DynFlags
import GHC.Driver.Config.Core.Lint ( initLintConfig )
import GHC.Core.SimpleOpt
import GHC.Core.Coercion.Opt
import GHC.Runtime.Eval.Types ( EvalStep(..) )
import GHCi.Message (EvalOpts(..))

-- | Initialise coercion optimiser configuration from DynFlags
initOptCoercionOpts :: DynFlags -> OptCoercionOpts
initOptCoercionOpts dflags = OptCoercionOpts
   { optCoercionEnabled = not (hasNoOptCoercion dflags)
   , optCoercionLint    = lint_cfg
   }
  where
    lint_cfg
      | gopt Opt_DoCoreLinting dflags = Just (initLintConfig dflags [])
      | otherwise                     = Nothing

-- | Initialise Simple optimiser configuration from DynFlags
initSimpleOpts :: DynFlags -> SimpleOpts
initSimpleOpts dflags = SimpleOpts
   { so_uf_opts = unfoldingOpts dflags
   , so_co_opts = initOptCoercionOpts dflags
   , so_eta_red = gopt Opt_DoEtaReduction dflags
   , so_inline  = const True
   , so_can_drop = const True
   }

-- | Extract GHCi options from DynFlags and step
initEvalOpts :: DynFlags -> EvalStep -> EvalOpts
initEvalOpts dflags step =
  EvalOpts
    { useSandboxThread = gopt Opt_GhciSandbox dflags
    , singleStep       = singleStep
    , stepOut          = stepOut
    , breakOnException = gopt Opt_BreakOnException dflags
    , breakOnError     = gopt Opt_BreakOnError dflags
    }
  where
    (singleStep, stepOut) = case step of
      EvalStepSingle -> (True,  False)
      EvalStepOut    -> (False, True)
      EvalStepNone   -> (False, False)

