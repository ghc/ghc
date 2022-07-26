module GHC.Driver.Config.Core.Opt.SpecConstr where

import GHC.Driver.Session ( DynFlags(..), GeneralFlag( Opt_SpecConstrKeen )
                          , gopt, hasPprDebug )

import GHC.Core.Opt.SpecConstr ( SpecConstrOpts (..) )

initSpecConstrOpts :: DynFlags -> SpecConstrOpts
initSpecConstrOpts dflags = SpecConstrOpts
  { sc_max_args    = maxWorkerArgs dflags
  , sc_debug       = hasPprDebug dflags
  , sc_uf_opts     = unfoldingOpts dflags
  , sc_size        = specConstrThreshold dflags
  , sc_count       = specConstrCount     dflags
  , sc_recursive   = specConstrRecursive dflags
  , sc_keen        = gopt Opt_SpecConstrKeen dflags
  }
