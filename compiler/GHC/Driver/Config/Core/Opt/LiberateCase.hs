module GHC.Driver.Config.Core.Opt.LiberateCase
  ( initLiberateCaseOpts
  ) where

import GHC.Driver.DynFlags

import GHC.Core.Opt.LiberateCase ( LibCaseOpts(..) )

-- | Initialize configuration for the liberate case Core optimization
-- pass.
initLiberateCaseOpts :: DynFlags -> LibCaseOpts
initLiberateCaseOpts dflags = LibCaseOpts
  { lco_threshold = liberateCaseThreshold dflags
  , lco_unfolding_opts = unfoldingOpts dflags
  }
