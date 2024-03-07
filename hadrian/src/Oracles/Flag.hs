{-# LANGUAGE MultiWayIf #-}

module Oracles.Flag (
    Flag (..), flag, getFlag,
    BuildFlag(..), buildFlag,
    targetSupportsSharedLibs,
    targetSupportsGhciObjects,
    targetSupportsThreadedRts,
    targetSupportsSMP,
    targetUseLibffiForAdjustors,
    arSupportsDashL,
    arSupportsAtFile
    ) where

import Hadrian.Oracles.TextFile
import Hadrian.Expression

import Base
import Oracles.Setting

import GHC.Toolchain.Target (Target(..))
import qualified GHC.Toolchain as Toolchain
import GHC.Platform.ArchOS

data Flag = CrossCompiling
          | UseGhcToolchain
data BuildFlag = CcLlvmBackend
          | GmpInTree
          | GmpFrameworkPref
          | UseSystemFfi
          | BootstrapThreadedRts
          | BootstrapEventLoggingRts
          | UseLibdw
          | UseLibnuma
          | UseLibzstd
          | StaticLibzstd
          | UseLibm
          | UseLibrt
          | UseLibdl
          | UseLibbfd
          | UseLibpthread
          | NeedLibatomic
          | TargetHasLibm

parseFlagResult :: String -> String -> Bool
parseFlagResult key value =
  if (value `notElem` ["YES", "NO", ""])
    then error $ "Configuration flag "
               ++ quote (key ++ " = " ++ value) ++ " cannot be parsed."
    else value == "YES"


-- Note, if a flag is set to empty string we treat it as set to NO. This seems
-- fragile, but some flags do behave like this.
flag :: Flag -> Action Bool
flag f = do
    let key = case f of
            CrossCompiling       -> "cross-compiling"
            UseGhcToolchain      -> "use-ghc-toolchain"
    parseFlagResult key <$> lookupSystemConfig key

buildFlag :: BuildFlag -> Stage -> Action Bool
buildFlag f st =
    let key = case f of
            CcLlvmBackend        -> "cc-llvm-backend"
            GmpInTree            -> "intree-gmp"
            GmpFrameworkPref     -> "gmp-framework-preferred"
            UseSystemFfi         -> "use-system-ffi"
            BootstrapThreadedRts -> "bootstrap-threaded-rts"
            BootstrapEventLoggingRts -> "bootstrap-event-logging-rts"
            UseLibdw             -> "use-lib-dw"
            UseLibnuma           -> "use-lib-numa"
            UseLibzstd           -> "use-lib-zstd"
            StaticLibzstd        -> "static-lib-zstd"
            UseLibm              -> "use-lib-m"
            UseLibrt             -> "use-lib-rt"
            UseLibdl             -> "use-lib-dl"
            UseLibbfd            -> "use-lib-bfd"
            UseLibpthread        -> "use-lib-pthread"
            NeedLibatomic        -> "need-libatomic"
            TargetHasLibm        -> "target-has-libm"
    in parseFlagResult key <$> (tgtConfig st key)
  where
    tgtConfig Stage0 {} = lookupHostBuildConfig
    tgtConfig Stage1 = lookupHostBuildConfig
    tgtConfig Stage2 = lookupTargetBuildConfig
    tgtConfig Stage3 = lookupTargetBuildConfig

-- | Get a configuration setting.
getFlag :: Flag -> Expr c b Bool
getFlag = expr . flag

-- | Does the target platform support object merging (and therefore we can build GHCi objects
-- when appropriate).
targetSupportsGhciObjects :: Stage -> Action Bool
targetSupportsGhciObjects stage = isJust <$> queryTargetTarget stage tgtMergeObjs

arSupportsDashL :: Stage -> Action Bool
arSupportsDashL stage = Toolchain.arSupportsDashL . tgtAr <$> targetStage stage

arSupportsAtFile :: Stage -> Action Bool
arSupportsAtFile stage = Toolchain.arSupportsAtFile . tgtAr <$> targetStage stage

targetSupportsSharedLibs :: Stage -> Action Bool
targetSupportsSharedLibs stage = do
    windows       <- isWinTarget stage
    wasm          <- anyTargetArch stage [ ArchWasm32 ]
    ppc_linux     <- (&&) <$> anyTargetArch stage [ ArchPPC ] <*> anyTargetOs stage [ OSLinux ]
    solaris       <- (&&) <$> anyTargetArch stage [ ArchX86 ] <*> anyTargetOs stage [ OSSolaris2 ]
    javascript    <- anyTargetArch stage [ ArchJavaScript ]
    return $ not (windows || wasm || javascript || ppc_linux || solaris)

-- | Does the target support threaded RTS?
targetSupportsThreadedRts :: Stage -> Action Bool
targetSupportsThreadedRts stage = do
    bad_arch <- anyTargetArch stage [ ArchWasm32, ArchJavaScript ]
    return $ not bad_arch

-- | Does the target support the -N RTS flag?
targetSupportsSMP :: Stage -> Action Bool
targetSupportsSMP stage = do
  unreg <- queryTargetTarget stage tgtUnregisterised
  armVer <- targetArmVersion stage
  goodArch <- (||) <$>
              anyTargetArch stage [ ArchX86
                            , ArchX86_64
                            , ArchPPC
                            , ArchPPC_64 ELF_V1
                            , ArchPPC_64 ELF_V2
                            , ArchAArch64
                            , ArchS390X
                            , ArchRISCV64
                            , ArchLoongArch64 ] <*> isArmTarget stage
  if   -- The THREADED_RTS requires `BaseReg` to be in a register and the
       -- Unregisterised mode doesn't allow that.
     | unreg                -> return False
       -- We don't support load/store barriers pre-ARMv7. See #10433.
     | Just ver <- armVer
     , ver < ARMv7          -> return False
     | goodArch             -> return True
     | otherwise            -> return False

targetUseLibffiForAdjustors :: Stage -> Action Bool
targetUseLibffiForAdjustors stage = queryTargetTarget stage tgtUseLibffiForAdjustors
