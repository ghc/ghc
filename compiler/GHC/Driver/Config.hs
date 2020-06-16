-- | Subsystem configuration
module GHC.Driver.Config
   ( initOptCoercionOpts
   , initSimpleOptOpts
   )
where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Core.SimpleOpt
import GHC.Core.Coercion.Opt

-- | Initialise coercion optimiser configuration from DynFlags
initOptCoercionOpts :: DynFlags -> OptCoercionOpts
initOptCoercionOpts dflags = OptCoercionOpts
   { optCoercionEnabled = not (hasNoOptCoercion dflags)
   }

-- | Initialise Simple optimiser configuration from DynFlags
initSimpleOptOpts :: DynFlags -> SimpleOptOpts
initSimpleOptOpts dflags = SimpleOptOpts
   { so_uf_opts = unfoldingOpts dflags
   , so_co_opts = initOptCoercionOpts dflags
   }
