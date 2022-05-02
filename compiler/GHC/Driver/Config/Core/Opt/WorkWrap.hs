module GHC.Driver.Config.Core.Opt.WorkWrap
  ( initWorkWrapOpts
  ) where

import GHC.Prelude ()

import GHC.Driver.Config (initSimpleOpts)
import GHC.Driver.Session

import GHC.Core.FamInstEnv
import GHC.Core.Opt.WorkWrap
import GHC.Unit.Types

initWorkWrapOpts :: Module -> DynFlags -> FamInstEnvs -> WwOpts
initWorkWrapOpts this_mod dflags fam_envs = MkWwOpts
  { wo_fam_envs          = fam_envs
  , wo_simple_opts       = initSimpleOpts dflags
  , wo_cpr_anal          = gopt Opt_CprAnal dflags
  , wo_module            = this_mod
  , wo_unlift_strict     = gopt Opt_WorkerWrapperUnlift dflags
  }
