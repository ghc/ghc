module GHC.Driver.Config.Cmm
  ( initCmmConfig
  ) where

import GHC.Cmm.Config

import GHC.Driver.DynFlags
import GHC.Driver.Backend

import GHC.Platform

import GHC.Prelude

initCmmConfig :: DynFlags -> CmmConfig
initCmmConfig dflags = CmmConfig
  { cmmProfile             = targetProfile                dflags
  , cmmOptControlFlow      = gopt Opt_CmmControlFlow      dflags
  , cmmDoLinting           = gopt Opt_DoCmmLinting        dflags
  , cmmOptElimCommonBlks   = gopt Opt_CmmElimCommonBlocks dflags
  , cmmOptSink             = gopt Opt_CmmSink             dflags
  , cmmOptThreadSanitizer  = gopt Opt_CmmThreadSanitizer dflags
  , cmmGenStackUnwindInstr = debugLevel dflags > 0
  , cmmExternalDynamicRefs = gopt Opt_ExternalDynamicRefs dflags
  , cmmDoCmmSwitchPlans    = not (backendHasNativeSwitch (backend dflags))
  , cmmSplitProcPoints     = not (backendSupportsUnsplitProcPoints (backend dflags))
                             || not (platformTablesNextToCode platform)
  }
  where platform                = targetPlatform dflags
