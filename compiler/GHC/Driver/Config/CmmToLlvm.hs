module GHC.Driver.Config.CmmToLlvm
  ( initLCGConfig
  ) where

import GHC.Prelude
import GHC.Driver.Session
import GHC.Platform
import GHC.CmmToLlvm.Config
import GHC.SysTools.Tasks
import GHC.Utils.Outputable
import GHC.Utils.Logger

-- | Initialize the Llvm code generator configuration from DynFlags
initLCGConfig :: Logger -> DynFlags -> IO LCGConfig
initLCGConfig logger dflags = do
  version <- figureLlvmVersion logger dflags
  pure $! LCGConfig {
    lcgPlatform               = targetPlatform dflags
    , lcgContext              = initSDocContext dflags (PprCode CStyle)
    , lcgFillUndefWithGarbage = gopt Opt_LlvmFillUndefWithGarbage dflags
    , lcgSplitSections        = gopt Opt_SplitSections dflags
    , lcgBmiVersion           = case platformArch (targetPlatform dflags) of
                                   ArchX86_64 -> bmiVersion dflags
                                   ArchX86    -> bmiVersion dflags
                                   _          -> Nothing
    , lcgLlvmVersion          = version
    , lcgDoWarn               = wopt Opt_WarnUnsupportedLlvmVersion dflags
    , lcgPlatformMisc         = platformMisc_llvmTarget $! platformMisc dflags
    , lcgLlvmConfig           = llvmConfig dflags
    }
