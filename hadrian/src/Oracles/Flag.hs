{-# LANGUAGE MultiWayIf #-}

module Oracles.Flag (
    Flag (..), flag, getFlag,
    platformSupportsSharedLibs,
    targetRTSLinkerOnlySupportsSharedLibs,
    targetSupportsThreadedRts,
    targetSupportsSMP,
    useLibffiForAdjustors, useLibdw,
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

targetRTSLinkerOnlySupportsSharedLibs :: Action Bool
targetRTSLinkerOnlySupportsSharedLibs = queryTargetTarget Toolchain.tgtRTSLinkerOnlySupportsSharedLibs

arSupportsDashL :: Stage -> Action Bool
arSupportsDashL stage = Toolchain.arSupportsDashL . tgtAr <$> targetStage stage

arSupportsAtFile :: Stage -> Action Bool
arSupportsAtFile stage = Toolchain.arSupportsAtFile . tgtAr <$> targetStage stage

platformSupportsSharedLibs :: Action Bool
-- FIXME: This is querying about the target but is named "platformXXX", targetSupportsSharedLibs would be better
platformSupportsSharedLibs = do
    windows       <- isWinTarget
    ppc_linux     <- (&&) <$> anyTargetArch [ ArchPPC ] <*> anyTargetOs [ OSLinux ]
    solaris       <- (&&) <$> anyTargetArch [ ArchX86 ] <*> anyTargetOs [ OSSolaris2 ]
    javascript    <- anyTargetArch     [ ArchJavaScript ]
    return $ not (windows || javascript || ppc_linux || solaris)

-- | Does the target support threaded RTS?
targetSupportsThreadedRts :: Action Bool
targetSupportsThreadedRts = do
    bad_arch <- anyTargetArch [ ArchWasm32, ArchJavaScript ]
    return $ not bad_arch

-- | Does the target support the -N RTS flag?
targetSupportsSMP :: Action Bool
targetSupportsSMP = queryTargetTarget Toolchain.tgtSupportsSMP

useLibffiForAdjustors :: Action Bool
useLibffiForAdjustors = queryTargetTarget tgtUseLibffiForAdjustors

useLibdw :: Action Bool
useLibdw = queryTargetTarget (isJust . tgtRTSWithLibdw)
