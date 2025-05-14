-- | Subsystem configuration
module GHC.Driver.Config
   ( initOptCoercionOpts
   , initSimpleOpts
   , initEvalOpts
   , EvalStep(..)
   )
where

import GHC.Prelude

import GHC.Driver.DynFlags
import GHC.Core.SimpleOpt
import GHC.Core.Coercion.Opt
import GHCi.Message (EvalOpts(..))

-- | Initialise coercion optimiser configuration from DynFlags
initOptCoercionOpts :: DynFlags -> OptCoercionOpts
initOptCoercionOpts dflags = OptCoercionOpts
   { optCoercionEnabled = not (hasNoOptCoercion dflags)
   }

-- | Initialise Simple optimiser configuration from DynFlags
initSimpleOpts :: DynFlags -> SimpleOpts
initSimpleOpts dflags = SimpleOpts
   { so_uf_opts = unfoldingOpts dflags
   , so_co_opts = initOptCoercionOpts dflags
   , so_eta_red = gopt Opt_DoEtaReduction dflags
   , so_inline  = True
   }

-- | Instruct the interpreter evaluation to break...
data EvalStep
  -- | ... at every breakpoint tick
  = EvalStepSingle
  -- | ... after every return stmt
  | EvalStepOut
  -- | ... only on explicit breakpoints
  | EvalStepNone

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

