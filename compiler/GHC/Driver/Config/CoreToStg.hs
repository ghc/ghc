module GHC.Driver.Config.CoreToStg where

import GHC.Driver.Config.Stg.Debug
import GHC.Driver.Session

import GHC.CoreToStg

initCoreToStgOpts :: DynFlags -> CoreToStgOpts
initCoreToStgOpts dflags = CoreToStgOpts
  { coreToStg_platform = targetPlatform dflags
  , coreToStg_ways = ways dflags
  , coreToStg_AutoSccsOnIndividualCafs = gopt Opt_AutoSccsOnIndividualCafs dflags
  , coreToStg_InfoTableMap = gopt Opt_InfoTableMap dflags
  , coreToStg_ExternalDynamicRefs = gopt Opt_ExternalDynamicRefs dflags
  , coreToStg_stgDebugOpts = initStgDebugOpts dflags
  }
