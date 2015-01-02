module Oracles.Option (
    Option (..),
    ghcWithInterpreter, platformSupportsSharedLibs, windowsHost
    ) where

import Base
import Oracles.Base

data Option = TargetOS | TargetArch | TargetPlatformFull
            | ConfCcArgs Stage | ConfGccLinkerArgs Stage | ConfLdLinkerArgs Stage | ConfCppArgs Stage
            | IconvIncludeDirs | IconvLibDirs | GmpIncludeDirs | GmpLibDirs
            | SrcHcOpts
            | HostOsCpp

instance ShowAction Option where
    showAction opt = askConfig $ case opt of 
        TargetOS                -> "target-os"
        TargetArch              -> "target-arch"
        TargetPlatformFull      -> "target-platform-full"
        ConfCcArgs        stage -> "conf-cc-args-stage-"         ++ (show . fromEnum) stage
        ConfCppArgs       stage -> "conf-cpp-args-stage-"        ++ (show . fromEnum) stage
        ConfGccLinkerArgs stage -> "conf-gcc-linker-args-stage-" ++ (show . fromEnum) stage
        ConfLdLinkerArgs  stage -> "conf-ld-linker-args-stage-"  ++ (show . fromEnum) stage
        IconvIncludeDirs        -> "iconv-include-dirs"
        IconvLibDirs            -> "iconv-lib-dirs"
        GmpIncludeDirs          -> "gmp-include-dirs"
        GmpLibDirs              -> "gmp-lib-dirs"
        SrcHcOpts               -> "src-hc-opts"
        HostOsCpp               -> "host-os-cpp"

ghcWithInterpreter :: Condition
ghcWithInterpreter = do
    os   <- showAction TargetOS
    arch <- showAction TargetArch
    return $
        os `elem` ["mingw32", "cygwin32", "linux", "solaris2", "freebsd", "dragonfly", "netbsd", "openbsd", "darwin", "kfreebsdgnu"]
        &&
        arch `elem` ["i386", "x86_64", "powerpc", "sparc", "sparc64", "arm"]

platformSupportsSharedLibs :: Condition
platformSupportsSharedLibs = do
    platform <- showAction TargetPlatformFull
    return $ platform `notElem` [ "powerpc-unknown-linux", "x86_64-unknown-mingw32", "i386-unknown-mingw32" ] -- TODO: i386-unknown-solaris2?

windowsHost :: Condition
windowsHost = do
    hostOsCpp <- showAction HostOsCpp
    return $ hostOsCpp `elem` ["mingw32", "cygwin32"]
