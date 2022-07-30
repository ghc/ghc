module GHC.Driver.Config.Core.Opt.WorkWrap
  ( initWorkWrapOpts
  ) where

import GHC.Driver.Config ( initSimpleOpts )
import GHC.Driver.Session ( DynFlags, GeneralFlag(..), gopt )

import GHC.Core.Opt.WorkWrap ( WwOpts(..) )

initWorkWrapOpts :: DynFlags -> WwOpts
initWorkWrapOpts dflags = MkWwOpts
  { wo_simple_opts       = initSimpleOpts dflags
  , wo_cpr_anal          = gopt Opt_CprAnal dflags
  , wo_unlift_strict     = gopt Opt_WorkerWrapperUnlift dflags
  }
