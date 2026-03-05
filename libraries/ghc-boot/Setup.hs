{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
module Main where

import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Types.LocalBuildInfo
import Distribution.Verbosity
import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.Simple.Setup
#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Simple.LocalBuildInfo (interpretSymbolicPathLBI)
#endif

import System.IO
import System.Directory
import System.FilePath
import System.Environment
import Control.Monad
import Data.Char
import GHC.ResponseFile
import Distribution.System (Platform(..))

main :: IO ()
main = defaultMainWithHooks ghcHooks
  where
    ghcHooks = simpleUserHooks
      { confHook = \(gpd, hbi) cfg -> do
          let verbosity = fromFlagOrDefault minBound (configVerbosity cfg)
          lbi <- confHook simpleUserHooks (gpd, hbi) cfg
          gitCommitId <- lookupEnv "GIT_COMMIT_ID" >>= \case
            Just str -> return str
            Nothing -> do
              (git, progdb) <- requireProgram verbosity (simpleProgram "git") defaultProgramDb
              getProgramOutput verbosity git ["rev-parse", "HEAD"]
          info verbosity $ "Git Commit Id = " ++ gitCommitId
          let cfs = configFlags lbi
              cPa = configProgramArgs cfs ++ [("ghc", ["-D GIT_COMMIT_ID=" ++ gitCommitId])]
          return lbi { configFlags = cfs { configProgramArgs = cPa } }

      , postConf = \args cfg pd lbi -> do
          let verbosity = fromFlagOrDefault minBound (configVerbosity cfg)
          ghcAutogen verbosity lbi
          postConf simpleUserHooks args cfg pd lbi
      }

ghcAutogen :: Verbosity -> LocalBuildInfo -> IO ()
ghcAutogen verbosity lbi@LocalBuildInfo {hostPlatform, pkgDescrFile} = do
#if MIN_VERSION_Cabal(3,14,0)
  let fromSymPath = interpretSymbolicPathLBI lbi
#else
  let fromSymPath = id
#endif

  -- Get compiler/ root directory from the cabal file
  let Just compilerRoot = takeDirectory . fromSymPath <$> pkgDescrFile

  let platformHostFile = "GHC/Platform/Host.hs"
      platformHostPath = fromSymPath (autogenPackageModulesDir lbi) </> platformHostFile
  -- Write GHC.Platform.Host
  createDirectoryIfMissingVerbose verbosity True (takeDirectory platformHostPath)

  -- hostPlatform is listed in LocalBuildInfo as "the platform we are building for"
  let Platform arch os = hostPlatform

  rewriteFileEx verbosity platformHostPath $
    unlines
        [ "module GHC.Platform.Host where"
        , ""
        , "import GHC.Platform.ArchOS"
        , "import Distribution.System hiding (Arch, OS)"
        , ""
        , "hostPlatformArch :: Arch"
        , "hostPlatformArch = toArch " ++ show arch
        , ""
        , "hostPlatformOS   :: OS"
        , "hostPlatformOS   = toOS " ++ show os
        , ""
        , "hostPlatformArchOS :: ArchOS"
        , "hostPlatformArchOS = ArchOS hostPlatformArch hostPlatformOS"
        , ""
        , "toArch I386 = ArchX86"
        , "toArch X86_64 = ArchX86_64"
        , "toArch PPC = ArchPPC"
        , "toArch PPC64 = ArchPPC_64 ELF_V1"
        , "toArch PPC64LE = ArchPPC_64 ELF_V2"
        , "toArch Sparc = ArchUnknown -- ?"
        , "toArch Sparc64 = ArchUnknown -- ?"
        , "toArch Arm = ArchARM ARMv7 [] SOFT -- ?"
        , "toArch AArch64 = ArchAArch64"
        , "toArch Mips = ArchUnknown -- ?"
        , "toArch SH = ArchUnknown -- ?"
        , "toArch IA64 = ArchUnknown -- ?"
        , "toArch S390 = ArchUnknown -- ?"
        , "toArch S390X = ArchUnknown -- ?"
        , "toArch Alpha = ArchAlpha"
        , "toArch Hppa = ArchUnknown -- ?"
        , "toArch Rs6000 = ArchUnknown -- ?"
        , "toArch M68k = ArchUnknown -- ?"
        , "toArch Vax = ArchUnknown -- ?"
        , "toArch RISCV64 = ArchRISCV64"
        , "toArch LoongArch64 = ArchLoongArch64"
        , "toArch JavaScript = ArchJavaScript"
        , "toArch Wasm32 = ArchWasm32"
        , "toArch (OtherArch _) = ArchUnknown"
        , ""
        , "toOS Linux = OSLinux"
        , "toOS Windows = OSMinGW32"
        , "toOS OSX = OSDarwin"
        , "toOS FreeBSD = OSFreeBSD"
        , "toOS OpenBSD = OSOpenBSD"
        , "toOS NetBSD = OSNetBSD"
        , "toOS DragonFly = OSDragonFly"
        , "toOS Solaris = OSSolaris2"
        , "toOS AIX = OSAIX"
        , "toOS HPUX = OSUnknown -- ?"
        , "toOS IRIX = OSUnknown -- ?"
        , "toOS HaLVM = OSUnknown -- ?"
        , "toOS Hurd = OSHurd"
        , "toOS IOS = OSUnknown -- ?"
        , "toOS Android = OSUnknown -- ?"
        , "toOS Ghcjs = OSGhcjs"
        , "toOS Wasi = OSWasi"
        , "toOS Haiku = OSHaiku"
        , "toOS (OtherOS _) = OSUnknown"
        ]
