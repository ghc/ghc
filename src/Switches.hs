module Switches (
    buildHaddock, validating,
    IntegerLibraryImpl (..), integerLibraryImpl,
    supportsPackageKey, targetPlatforms, targetPlatform,
    targetOss, targetOs, notTargetOs,
    targetArchs, dynamicGhcPrograms, ghcWithInterpreter,
    platformSupportsSharedLibs, crossCompiling,
    gccIsClang, gccLt46, windowsHost, notWindowsHost
    ) where

import Base
import Expression

-- User-defined switches
buildHaddock :: Monad m => Predicate m
buildHaddock = return True

validating :: Monad m => Predicate m
validating = return False

-- Support for multiple integer library implementations
data IntegerLibraryImpl = IntegerGmp | IntegerGmp2 | IntegerSimple

integerLibraryImpl :: IntegerLibraryImpl
integerLibraryImpl = IntegerGmp2

-- Predicates based on configuration files
supportsPackageKey :: Predicate Action
supportsPackageKey = configKeyYes "supports-package-key"

targetPlatforms :: [String] -> Predicate Action
targetPlatforms = configKeyValues "target-platform-full"

targetPlatform :: String -> Predicate Action
targetPlatform s = targetPlatforms [s]

targetOss :: [String] -> Predicate Action
targetOss = configKeyValues "target-os"

targetOs :: String -> Predicate Action
targetOs s = targetOss [s]

notTargetOs :: String -> Predicate Action
notTargetOs = fmap not . targetOs

targetArchs :: [String] -> Predicate Action
targetArchs = configKeyValues "target-arch"

platformSupportsSharedLibs :: Predicate Action
platformSupportsSharedLibs = do
    badPlatform   <- targetPlatforms [ "powerpc-unknown-linux"
                                     , "x86_64-unknown-mingw32"
                                     , "i386-unknown-mingw32" ]
    solaris       <- targetPlatform    "i386-unknown-solaris2"
    solarisBroken <- configKeyYes "solaris-broken-shld"
    return $ not (badPlatform || solaris && solarisBroken)

dynamicGhcPrograms :: Predicate Action
dynamicGhcPrograms = configKeyYes "dynamic-ghc-programs"

ghcWithInterpreter :: Predicate Action
ghcWithInterpreter = do
    goodOs <- targetOss [ "mingw32", "cygwin32", "linux", "solaris2"
                        , "freebsd", "dragonfly", "netbsd", "openbsd"
                        , "darwin", "kfreebsdgnu" ]
    goodArch <- targetArchs [ "i386", "x86_64", "powerpc", "sparc"
                            , "sparc64", "arm" ]
    return $ goodOs && goodArch

crossCompiling :: Predicate Action
crossCompiling = configKeyYes "cross-compiling"

gccIsClang :: Predicate Action
gccIsClang = configKeyYes "gcc-is-clang"

gccLt46 :: Predicate Action
gccLt46 = configKeyYes "gcc-lt-46"

windowsHost :: Predicate Action
windowsHost = configKeyValues "host-os-cpp" ["mingw32", "cygwin32"]

notWindowsHost :: Predicate Action
notWindowsHost = fmap not windowsHost
