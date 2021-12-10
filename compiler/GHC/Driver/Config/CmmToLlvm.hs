module GHC.Driver.Config.CmmToLlvm
  ( initLlvmCgConfig
  ) where

import GHC.Prelude
import GHC.Driver.Session
import GHC.Platform
import GHC.CmmToLlvm.Config
import GHC.SysTools.Tasks
import GHC.Utils.Outputable
import GHC.Utils.Logger

-- | Initialize the Llvm code generator configuration from DynFlags
initLlvmCgConfig :: Logger -> DynFlags -> IO LlvmCgConfig
initLlvmCgConfig logger dflags = do
  version <- figureLlvmVersion logger dflags
  pure $! LlvmCgConfig {
    llvmCgPlatform               = targetPlatform dflags
    , llvmCgContext              = initSDocContext dflags (PprCode CStyle)
    , llvmCgFillUndefWithGarbage = gopt Opt_LlvmFillUndefWithGarbage dflags
    , llvmCgSplitSection         = gopt Opt_SplitSections dflags
    , llvmCgBmiVersion           = case platformArch (targetPlatform dflags) of
                                      ArchX86_64 -> bmiVersion dflags
                                      ArchX86    -> bmiVersion dflags
                                      _          -> Nothing
    , llvmCgLlvmVersion          = version
    , llvmCgDoWarn               = wopt Opt_WarnUnsupportedLlvmVersion dflags
    , llvmCgLlvmTarget           = platformMisc_llvmTarget $! platformMisc dflags
    , llvmCgLlvmConfig           = llvmConfig dflags
    }
