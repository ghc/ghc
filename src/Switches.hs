{-# LANGUAGE NoImplicitPrelude #-}

module Switches (
    buildHaddock, validating,
    IntegerLibraryImpl (..), integerLibraryImpl,
    supportsPackageKey, targetPlatforms, targetPlatform,
    targetOss, targetOs, targetArchs, dynamicGhcPrograms, ghcWithInterpreter,
    platformSupportsSharedLibs, crossCompiling,
    gccIsClang, gccLt46, windowsHost
    ) where

import Expression.Base

-- User-defined switches
buildHaddock :: BuildPredicate
buildHaddock = true

validating :: BuildPredicate
validating = false

-- Support for multiple integer library implementations
data IntegerLibraryImpl = IntegerGmp | IntegerGmp2 | IntegerSimple

integerLibraryImpl :: IntegerLibraryImpl
integerLibraryImpl = IntegerGmp2

-- Predicates based on configuration files
supportsPackageKey :: BuildPredicate
supportsPackageKey = configYes "supports-package-key"

targetPlatforms :: [String] -> BuildPredicate
targetPlatforms = configValues "target-platform-full"

targetPlatform :: String -> BuildPredicate
targetPlatform s = targetPlatforms [s]

targetOss :: [String] -> BuildPredicate
targetOss = configValues "target-os"

targetOs :: String -> BuildPredicate
targetOs s = targetOss [s]

targetArchs :: [String] -> BuildPredicate
targetArchs = configValues "target-arch"

solarisBrokenShld :: BuildPredicate
solarisBrokenShld = configYes "solaris-broken-shld"

platformSupportsSharedLibs :: BuildPredicate
platformSupportsSharedLibs =
    not (targetPlatforms [ "powerpc-unknown-linux"
                         , "x86_64-unknown-mingw32"
                         , "i386-unknown-mingw32" ]
        ||
        solarisBrokenShld && targetPlatform "i386-unknown-solaris2")

dynamicGhcPrograms :: BuildPredicate
dynamicGhcPrograms = configYes "dynamic-ghc-programs"

ghcWithInterpreter :: BuildPredicate
ghcWithInterpreter =
    targetOss [ "mingw32", "cygwin32", "linux", "solaris2"
              , "freebsd", "dragonfly", "netbsd", "openbsd"
              , "darwin", "kfreebsdgnu" ]
    &&
    targetArchs ["i386", "x86_64", "powerpc", "sparc", "sparc64", "arm"]

crossCompiling :: BuildPredicate
crossCompiling = configYes "cross-compiling"

gccIsClang :: BuildPredicate
gccIsClang = configYes "gcc-is-clang"

gccLt46 :: BuildPredicate
gccLt46 = configYes "gcc-lt-46"

windowsHost :: BuildPredicate
windowsHost = configValues "host-os-cpp" ["mingw32", "cygwin32"]
