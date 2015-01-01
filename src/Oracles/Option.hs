module Oracles.Option (
    Option (..),
    option, argOption,
    ghcWithInterpreter, platformSupportsSharedLibs, windowsHost
    ) where

import Base
import Oracles.Base

data Option = TargetOS | TargetArch | TargetPlatformFull
            | ConfCcArgs Stage | ConfGccLinkerArgs Stage | ConfLdLinkerArgs Stage | ConfCppArgs Stage
            | IconvIncludeDirs | IconvLibDirs | GmpIncludeDirs | GmpLibDirs
            | SrcHcOpts
            | HostOsCpp

option :: Option -> Action String
option opt = askConfig $ case opt of 
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

argOption :: Option -> Args
argOption opt = do
    opt' <- option opt
    arg [opt']

ghcWithInterpreter :: Condition
ghcWithInterpreter = do
    os   <- option TargetOS
    arch <- option TargetArch
    return $
        os `elem` ["mingw32", "cygwin32", "linux", "solaris2", "freebsd", "dragonfly", "netbsd", "openbsd", "darwin", "kfreebsdgnu"]
        &&
        arch `elem` ["i386", "x86_64", "powerpc", "sparc", "sparc64", "arm"]

platformSupportsSharedLibs :: Condition
platformSupportsSharedLibs = do
    platform <- option TargetPlatformFull
    return $ platform `notElem` [ "powerpc-unknown-linux", "x86_64-unknown-mingw32", "i386-unknown-mingw32" ] -- TODO: i386-unknown-solaris2?

windowsHost :: Condition
windowsHost = do
    hostOsCpp <- option HostOsCpp
    return $ hostOsCpp `elem` ["mingw32", "cygwin32"]
