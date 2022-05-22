module GHC.Driver.Config.Core.Opt.CallerCC where

import GHC.Driver.Session

import GHC.Core.Opt.CallerCC ( CallerCCOpts (..) )

initCallerCCOpts :: DynFlags -> CallerCCOpts
initCallerCCOpts dflags = CallerCCOpts
  { cc_countEntries = gopt Opt_ProfCountEntries dflags
  , cc_filters = callerCcFilters dflags
  }
