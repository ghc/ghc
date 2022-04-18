module GHC.Driver.Config.Core.Opt.LiberateCase
  ( initLiberateCaseOpts
  ) where

import GHC.Driver.Session

import GHC.Core.Opt.LiberateCase ( LibCaseOpts(..) )

-- | Initialize configuration for the liberate case Core optomization
-- pass.
initLiberateCaseOpts :: DynFlags -> LibCaseOpts
initLiberateCaseOpts dflags = LibCaseOpts
  { lco_threshold = liberateCaseThreshold dflags
  , lco_unfolding_opts = unfoldingOpts dflags
  }
