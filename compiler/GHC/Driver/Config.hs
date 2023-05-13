-- | Subsystem configuration
module GHC.Driver.Config
   ( initOptCoercionOpts
   , initSimpleOpts
   , initEvalOpts
   )
where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Core.SimpleOpt
import GHC.Core.Coercion.Opt
import GHCi.Message (EvalOpts(..))

-- | Initialise coercion optimiser configuration from DynFlags
initOptCoercionOpts :: DynFlags -> OptCoercionOpts
initOptCoercionOpts dflags = OptCoercionOpts
   { optCoercionOpts
       = if hasNoOptCoercion dflags
         then Nothing
         else
            let dco_method =
                 if hasKeepDCoercions dflags
                 then OptDCos { skipDCoOpt = True }
                 else HydrateDCos
            in Just dco_method
   }

-- | Initialise Simple optimiser configuration from DynFlags
initSimpleOpts :: DynFlags -> SimpleOpts
initSimpleOpts dflags = SimpleOpts
   { so_uf_opts = unfoldingOpts dflags
   , so_co_opts = initOptCoercionOpts dflags
   , so_eta_red = gopt Opt_DoEtaReduction dflags
   }

-- | Extract GHCi options from DynFlags and step
initEvalOpts :: DynFlags -> Bool -> EvalOpts
initEvalOpts dflags step =
  EvalOpts
    { useSandboxThread = gopt Opt_GhciSandbox dflags
    , singleStep       = step
    , breakOnException = gopt Opt_BreakOnException dflags
    , breakOnError     = gopt Opt_BreakOnError dflags
    }

