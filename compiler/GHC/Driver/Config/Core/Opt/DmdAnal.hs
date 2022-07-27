module GHC.Driver.Config.Core.Opt.DmdAnal ( initDmdAnalOpts ) where

import GHC.Driver.Session

import GHC.Core.Opt.DmdAnal

initDmdAnalOpts :: DynFlags -> DmdAnalOpts
initDmdAnalOpts dflags = DmdAnalOpts
  { dmd_strict_dicts    = gopt Opt_DictsStrict dflags
  , dmd_unbox_width     = dmdUnboxWidth dflags
  , dmd_max_worker_args = maxWorkerArgs dflags
  }
