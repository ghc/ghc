module GHC.Driver.Config.Cmm.Builder
  ( initCmmBuilderConfig
  ) where

import GHC.Cmm.Builder.Config

import GHC.Driver.Config.StgToCmm.Ticky
import GHC.Driver.Session
import GHC.Utils.Outputable

import Prelude

initCmmBuilderConfig :: DynFlags -> CmmBuilderConfig
initCmmBuilderConfig dflags = CmmBuilderConfig
  -- settings
  { cmmBuilderProfile       = targetProfile dflags
  , cmmBuilderContext       = initSDocContext dflags defaultDumpStyle
  , cmmBuilderEmitDebugInfo = debugLevel      dflags > 0
  -- flags
  , cmmBuilderTickyCfg      = initCmmTickyConfig             dflags
  , cmmBuilderSCCProfiling  = sccProfilingEnabled            dflags
  , cmmBuilderEagerBlackHole = gopt Opt_EagerBlackHoling     dflags
  , cmmBuilderInfoTableMap  = gopt Opt_InfoTableMap          dflags
  , cmmBuilderOmitYields    = gopt Opt_OmitYields            dflags
  }
