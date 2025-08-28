module GHC.Driver.Config.CmmToLlvm
  ( initLlvmCgConfig
  )
where

import GHC.Prelude
import GHC.Driver.DynFlags
import GHC.Driver.LlvmConfigCache
import GHC.Platform
import GHC.CmmToLlvm.Config
import GHC.SysTools.Tasks

import GHC.Utils.Outputable
import GHC.Utils.Logger

-- | Initialize the Llvm code generator configuration from DynFlags
initLlvmCgConfig :: Logger -> LlvmConfigCache -> DynFlags -> IO LlvmCgConfig
initLlvmCgConfig logger config_cache dflags = do
  version <- figureLlvmVersion logger dflags
  llvm_config <- readLlvmConfigCache config_cache
  pure $! LlvmCgConfig {
    llvmCgPlatform               = targetPlatform dflags
    , llvmCgContext              = initSDocContext dflags PprCode
    , llvmCgFillUndefWithGarbage = gopt Opt_LlvmFillUndefWithGarbage dflags
    , llvmCgSplitSection         = gopt Opt_SplitSections dflags
    , llvmCgAvxEnabled           = isAvxEnabled dflags
    , llvmCgBmiVersion           = case platformArch (targetPlatform dflags) of
                                      ArchX86_64 -> bmiVersion dflags
                                      ArchX86    -> bmiVersion dflags
                                      _          -> Nothing
    , llvmCgLlvmVersion          = version
    , llvmCgLlvmTarget           = platformMisc_llvmTarget $! platformMisc dflags
    , llvmCgLlvmConfig           = llvm_config
    }
