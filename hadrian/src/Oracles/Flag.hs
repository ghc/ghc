{-# LANGUAGE MultiWayIf #-}

module Oracles.Flag (
    Flag (..), flag, getFlag,
    targetRTSLinkerOnlySupportsSharedLibs,
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
          | CcLlvmBackend
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
            UseGhcToolchain      -> "use-ghc-toolchain"
    value <- lookupSystemConfig key
    when (value `notElem` ["YES", "NO", ""]) . error $ "Configuration flag "
        ++ quote (key ++ " = " ++ value) ++ " cannot be parsed."
    return $ value == "YES"

-- | Get a configuration setting.
getFlag :: Flag -> Expr c b Bool
getFlag = expr . flag

-- | Does the target RTS linker only support loading shared libraries?
-- If true, this has several implications:
-- 1. The GHC driver must not do loadArchive/loadObj etc and must
--    always do loadDLL, regardless of whether host GHC is dynamic or
--    not.
-- 2. The GHC driver will always enable -dynamic-too when compiling
--    vanilla way with TH codegen requirement.
-- 3. ghci will always enforce dynamic ways even if -dynamic or
--    -dynamic-too is not explicitly passed.
-- 4. Cabal must not build ghci objects since it's not supported by
--    the target.
-- 5. The testsuite driver will use dyn way for TH/ghci tests even
--    when host GHC is static.
-- 6. TH/ghci doesn't work if stage1 is built without shared libraries
--    (e.g. quickest/fully_static).
targetRTSLinkerOnlySupportsSharedLibs :: Stage -> Action Bool
targetRTSLinkerOnlySupportsSharedLibs stage = anyTargetArch stage [ ArchWasm32 ]

-- | Does the target platform support object merging (and therefore we can build GHCi objects
-- when appropriate).
targetSupportsGhciObjects :: Stage -> Action Bool
targetSupportsGhciObjects stage = do
  has_merge_objs <- isJust <$> queryTargetTarget stage tgtMergeObjs
  only_shared_libs <- targetRTSLinkerOnlySupportsSharedLibs stage
  pure $ has_merge_objs && not only_shared_libs

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

{-
-- | When cross compiling, enable for stage0 to get ghci
-- support. But when not cross compiling, disable for
-- stage0, otherwise we introduce extra dependencies
-- like haskeline etc, and mixing stageBoot/stage0 libs
-- can cause extra trouble (e.g. #25406)
--
-- Also checks whether the target supports GHCi.
ghcWithInterpreter :: Stage -> Action Bool
ghcWithInterpreter stage = do
    is_cross <- flag CrossCompiling
    goodOs <- anyTargetOs [ OSMinGW32, OSLinux, OSSolaris2 -- TODO "cygwin32"?,
                          , OSFreeBSD, OSDragonFly, OSNetBSD, OSOpenBSD
                          , OSDarwin, OSKFreeBSD
                          , OSWasi ]
    goodArch <- (||) <$>
                anyTargetArch [ ArchX86, ArchX86_64, ArchPPC
                              , ArchAArch64, ArchS390X
                              , ArchPPC_64 ELF_V1, ArchPPC_64 ELF_V2
                              , ArchRISCV64, ArchLoongArch64
                              , ArchWasm32 ]
                              <*> isArmTarget
    -- Maybe this should just be false for cross compilers. But for now
    -- I've kept the old behaviour where it will say yes. (See #25939)
    return $ goodOs && goodArch && (stage >= Stage1 || is_cross)
    -}

targetUseLibffiForAdjustors :: Stage -> Action Bool
targetUseLibffiForAdjustors stage = queryTargetTarget stage tgtUseLibffiForAdjustors
