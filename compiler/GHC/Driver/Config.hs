-- | Subsystem configuration
module GHC.Driver.Config
   ( initOptCoercionOpts
   , initSimpleOpts
   , initBCOOpts
   , initEvalOpts
   )
where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Core.SimpleOpt
import GHC.Core.Coercion.Opt
import GHC.Runtime.Interpreter (BCOOpts(..))
import GHCi.Message (EvalOpts(..))

import GHC.Conc (getNumProcessors)
import Control.Monad.IO.Class

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
   }

-- | Extract BCO options from DynFlags
initBCOOpts :: DynFlags -> IO BCOOpts
initBCOOpts dflags = do
  -- Serializing ResolvedBCO is expensive, so if we're in parallel mode
  -- (-j<n>) parallelise the serialization.
  n_jobs <- case parMakeCount dflags of
              Nothing -> liftIO getNumProcessors
              Just n  -> return n
  return $ BCOOpts n_jobs

-- | Extract GHCi options from DynFlags and step
initEvalOpts :: DynFlags -> Bool -> EvalOpts
initEvalOpts dflags step =
  EvalOpts
    { useSandboxThread = gopt Opt_GhciSandbox dflags
    , singleStep       = step
    , breakOnException = gopt Opt_BreakOnException dflags
    , breakOnError     = gopt Opt_BreakOnError dflags
    }

