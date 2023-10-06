{-# LANGUAGE MultiWayIf #-}

module Oracles.Flag (
    Flag (..), flag, getFlag,
    targetRTSLinkerOnlySupportsSharedLibs,
    targetSupportsSharedLibs,
    targetSupportsGhciObjects,
    targetSupportsThreadedRts,
    targetSupportsSMP,
    useLibdw,
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
          | CcLlvmBackend
          | GmpInTree
          | GmpFrameworkPref
          | UseSystemFfi
          | BootstrapThreadedRts
          | BootstrapEventLoggingRts
          | UseLibnuma
          | UseLibzstd
          | StaticLibzstd
          | UseLibm
          | UseLibrt
          | UseLibdl
          | UseLibbfd
          | UseLibpthread
          | NeedLibatomic
          | UseGhcToolchain

-- Note, if a flag is set to empty string we treat it as set to NO. This seems
-- fragile, but some flags do behave like this.
flag :: Flag -> Action Bool
flag f = do
    let key = case f of
            CrossCompiling       -> "cross-compiling"
            CcLlvmBackend        -> "cc-llvm-backend"
            GmpInTree            -> "intree-gmp"
            GmpFrameworkPref     -> "gmp-framework-preferred"
            UseSystemFfi         -> "use-system-ffi"
            BootstrapThreadedRts -> "bootstrap-threaded-rts"
            BootstrapEventLoggingRts -> "bootstrap-event-logging-rts"
            UseLibnuma           -> "use-lib-numa"
            UseLibzstd           -> "use-lib-zstd"
            StaticLibzstd        -> "static-lib-zstd"
            UseLibm              -> "use-lib-m"
            UseLibrt             -> "use-lib-rt"
            UseLibdl             -> "use-lib-dl"
            UseLibbfd            -> "use-lib-bfd"
            UseLibpthread        -> "use-lib-pthread"
            NeedLibatomic        -> "need-libatomic"
            UseGhcToolchain      -> "use-ghc-toolchain"
    value <- lookupSystemConfig key
    when (value `notElem` ["YES", "NO", ""]) . error $ "Configuration flag "
        ++ quote (key ++ " = " ++ value) ++ " cannot be parsed."
    return $ value == "YES"

-- | Get a configuration setting.
getFlag :: Flag -> Expr c b Bool
getFlag = expr . flag

-- | Does the target platform support object merging (and therefore we can build GHCi objects
-- when appropriate).
targetSupportsGhciObjects :: Stage -> Action Bool
targetSupportsGhciObjects stage = do
  has_merge_objs <- isJust <$> queryTargetTarget stage tgtMergeObjs
  only_shared_libs <- targetRTSLinkerOnlySupportsSharedLibs stage
  pure $ has_merge_objs && not only_shared_libs

targetRTSLinkerOnlySupportsSharedLibs :: Stage -> Action Bool
targetRTSLinkerOnlySupportsSharedLibs s =
  queryTargetTarget s Toolchain.tgtRTSLinkerOnlySupportsSharedLibs

arSupportsDashL :: Stage -> Action Bool
arSupportsDashL stage = Toolchain.arSupportsDashL . tgtAr <$> targetStage stage

arSupportsAtFile :: Stage -> Action Bool
arSupportsAtFile stage = Toolchain.arSupportsAtFile . tgtAr <$> targetStage stage

targetSupportsSharedLibs :: Stage -> Action Bool
targetSupportsSharedLibs stage = do
    windows       <- isWinTarget stage
    ppc_linux     <- (&&) <$> anyTargetArch stage [ ArchPPC ] <*> anyTargetOs stage [ OSLinux ]
    solaris       <- (&&) <$> anyTargetArch stage [ ArchX86 ] <*> anyTargetOs stage [ OSSolaris2 ]
    javascript    <- anyTargetArch stage [ ArchJavaScript ]
    return $ not (windows || javascript || ppc_linux || solaris)

-- | Does the target support threaded RTS?
targetSupportsThreadedRts :: Stage -> Action Bool
targetSupportsThreadedRts stage = do
    bad_arch <- anyTargetArch stage [ ArchWasm32, ArchJavaScript ]
    return $ not bad_arch

-- | Does the target support the -N RTS flag?
targetSupportsSMP :: Stage -> Action Bool
targetSupportsSMP stage = queryTargetTarget stage Toolchain.tgtSupportsSMP

useLibdw :: Stage -> Action Bool
useLibdw stage = queryTargetTarget stage (isJust . tgtRTSWithLibdw)

targetUseLibffiForAdjustors :: Stage -> Action Bool
targetUseLibffiForAdjustors stage = queryTargetTarget stage tgtUseLibffiForAdjustors
