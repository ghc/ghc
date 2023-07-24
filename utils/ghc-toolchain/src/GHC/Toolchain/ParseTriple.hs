module GHC.Toolchain.ParseTriple ( parseTriple ) where

import Data.List (isPrefixOf)

import GHC.Platform.ArchOS

import GHC.Toolchain.Prelude
import GHC.Toolchain.CheckArm
import GHC.Toolchain.Tools.Cc

-- | Parse a triple `arch-vendor-os` into an 'ArchOS' and a vendor name 'String'
--
-- If the user specified a duple (`arch-os`) instead, the vendor name is 'Nothing'
parseTriple :: Cc -> String -> M (ArchOS, Maybe String)
parseTriple cc triple
  | [archName, osName] <- parts
  = do arch <- parseArch cc archName
       os   <- parseOs osName
       return (ArchOS arch os, Nothing)
  | [archName, vendorName, osName] <- parts
  = do arch <- parseArch cc archName
       os   <- parseOs osName
       return (ArchOS arch os, Just vendorName)

  | [archName, vendorName, osName, _abi] <- parts
  = do arch <- parseArch cc archName
       os   <- parseOs osName
       return (ArchOS arch os, Just vendorName)

  | otherwise
  = throwE $ "malformed triple " ++ triple
  where
    parts = splitOn '-' triple

parseArch :: Cc -> String -> M Arch
parseArch cc arch =
    case arch of
      "i386" -> pure ArchX86
      "x86_64" -> pure ArchX86_64
      "amd64" -> pure ArchX86_64
      "powerpc" -> pure ArchPPC
      "powerpc64" -> pure (ArchPPC_64 ELF_V1)
      "powerpc64le" -> pure (ArchPPC_64 ELF_V2)
      "s390x" -> pure ArchS390X
      "arm" -> findArmIsa cc
      _ | "armv" `isPrefixOf` arch -> findArmIsa cc
      "arm64" -> pure ArchAArch64
      "aarch64" -> pure ArchAArch64
      "alpha" -> pure ArchAlpha
      "mips" -> pure ArchMipseb
      "mipseb" -> pure ArchMipseb
      "mipsel" -> pure ArchMipsel
      "riscv64" -> pure ArchRISCV64
      "hppa" -> pure ArchUnknown
      "wasm32" -> pure ArchWasm32
      "javascript" -> pure ArchJavaScript
      _ -> throwE $ "Unknown architecture " ++ arch

parseOs :: String -> M OS
parseOs os =
    case os of
      "linux" -> pure OSLinux
      "linux-android" -> pure OSLinux
      "darwin" -> pure OSDarwin
      "ios" -> pure OSDarwin
      "watchos" -> pure OSDarwin
      "tvos" -> pure OSDarwin
      "solaris2" -> pure OSSolaris2
      "mingw32" -> pure OSMinGW32
      "freebsd" -> pure OSFreeBSD
      "dragonfly" -> pure OSDragonFly
      "kfreebsdgnu" -> pure OSKFreeBSD
      "openbsd" -> pure OSOpenBSD
      "netbsd" -> pure OSNetBSD
      "haiku" -> pure OSHaiku
      "nto-qnc" -> pure OSQNXNTO
      "aix" -> pure OSAIX
      "gnu" -> pure OSHurd
      "wasi" -> pure OSWasi
      "ghcjs" -> pure OSGhcjs
      _ -> throwE $ "Unknown operating system " ++ os

splitOn :: Char -> String -> [String]
splitOn sep = go
  where
    go "" = []
    go s  = a : go (drop 1 b)
      where
        (a,b) = break (== sep) s
