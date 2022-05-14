module GHC.Driver.Config.HsToCore.Usage
  ( initUsageConfig
  )
where

import GHC.Driver.Env.Types
import GHC.Driver.Session

import GHC.HsToCore.Usage

initUsageConfig :: HscEnv -> UsageConfig
initUsageConfig hsc_env = UsageConfig
  { uc_safe_implicit_imps_req = safeImplicitImpsReq (hsc_dflags hsc_env)
  }
