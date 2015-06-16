module Switches (
    IntegerLibraryImpl (..), integerLibraryImpl,
    notStage, stage0, stage1, stage2,
    configKeyYes, configKeyNo, configKeyNonEmpty,
    supportsPackageKey, targetPlatforms, targetPlatform,
    targetOss, targetOs, notTargetOs,
    targetArchs, dynamicGhcPrograms, ghcWithInterpreter,
    platformSupportsSharedLibs, crossCompiling,
    gccIsClang, gccLt46, windowsHost, notWindowsHost
    ) where

import Base
import Expression

-- TODO: This setting should be moved to UserSettings.hs
-- TODO: Define three packages for integer library instead of one in Targets.hs
-- Support for multiple integer library implementations
data IntegerLibraryImpl = IntegerGmp | IntegerGmp2 | IntegerSimple

integerLibraryImpl :: IntegerLibraryImpl
integerLibraryImpl = IntegerGmp2

-- Derived predicates
notStage :: Stage -> Predicate
notStage = liftM not . stage

stage0 :: Predicate
stage0 = stage Stage0

stage1 :: Predicate
stage1 = stage Stage1

stage2 :: Predicate
stage2 = stage Stage2

configKeyYes :: String -> Predicate
configKeyYes key = configKeyValue key "YES"

configKeyNo :: String -> Predicate
configKeyNo key = configKeyValue key "NO"

configKeyNonEmpty :: String -> Predicate
configKeyNonEmpty key = liftM not $ configKeyValue key ""

-- Predicates based on configuration files
supportsPackageKey :: Predicate
supportsPackageKey = configKeyYes "supports-package-key"

targetPlatforms :: [String] -> Predicate
targetPlatforms = configKeyValues "target-platform-full"

targetPlatform :: String -> Predicate
targetPlatform s = targetPlatforms [s]

targetOss :: [String] -> Predicate
targetOss = configKeyValues "target-os"

targetOs :: String -> Predicate
targetOs s = targetOss [s]

notTargetOs :: String -> Predicate
notTargetOs = liftM not . targetOs

targetArchs :: [String] -> Predicate
targetArchs = configKeyValues "target-arch"

platformSupportsSharedLibs :: Predicate
platformSupportsSharedLibs = do
    badPlatform   <- targetPlatforms [ "powerpc-unknown-linux"
                                     , "x86_64-unknown-mingw32"
                                     , "i386-unknown-mingw32" ]
    solaris       <- targetPlatform    "i386-unknown-solaris2"
    solarisBroken <- configKeyYes "solaris-broken-shld"
    return $ not (badPlatform || solaris && solarisBroken)

dynamicGhcPrograms :: Predicate
dynamicGhcPrograms = configKeyYes "dynamic-ghc-programs"

ghcWithInterpreter :: Predicate
ghcWithInterpreter = do
    goodOs <- targetOss [ "mingw32", "cygwin32", "linux", "solaris2"
                        , "freebsd", "dragonfly", "netbsd", "openbsd"
                        , "darwin", "kfreebsdgnu" ]
    goodArch <- targetArchs [ "i386", "x86_64", "powerpc", "sparc"
                            , "sparc64", "arm" ]
    return $ goodOs && goodArch

crossCompiling :: Predicate
crossCompiling = configKeyYes "cross-compiling"

gccIsClang :: Predicate
gccIsClang = configKeyYes "gcc-is-clang"

gccLt46 :: Predicate
gccLt46 = configKeyYes "gcc-lt-46"

windowsHost :: Predicate
windowsHost = configKeyValues "host-os-cpp" ["mingw32", "cygwin32"]

notWindowsHost :: Predicate
notWindowsHost = liftM not windowsHost
