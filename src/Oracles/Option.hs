{-# LANGUAGE NoImplicitPrelude #-}
module Oracles.Option (
    Option (..),
    ghcWithInterpreter, platformSupportsSharedLibs, windowsHost, splitObjects
    ) where

import Base
import Oracles.Flag
import Oracles.Base

-- For each Option the files {default.config, user.config} contain
-- a line of the form 'target-os = mingw32'.
-- (showArgs TargetOS) is an action that consults the config files
-- and returns ["mingw32"].
-- TODO: separate single string options from multiple string ones.
data Option = TargetOS
            | TargetArch
            | TargetPlatformFull
            | ConfCcArgs Stage
            | ConfGccLinkerArgs Stage
            | ConfLdLinkerArgs Stage
            | ConfCppArgs Stage
            | IconvIncludeDirs
            | IconvLibDirs
            | GmpIncludeDirs
            | GmpLibDirs
            | SrcHcOpts
            | HostOsCpp
            | DynamicExtension
            | ProjectVersion

instance ShowArgs Option where
    showArgs opt = showArgs $ fmap words $ askConfig $ case opt of 
        TargetOS                -> "target-os"
        TargetArch              -> "target-arch"
        TargetPlatformFull      -> "target-platform-full"
        ConfCcArgs        stage -> "conf-cc-args-stage-"         ++ show stage
        ConfCppArgs       stage -> "conf-cpp-args-stage-"        ++ show stage
        ConfGccLinkerArgs stage -> "conf-gcc-linker-args-stage-" ++ show stage
        ConfLdLinkerArgs  stage -> "conf-ld-linker-args-stage-"  ++ show stage
        IconvIncludeDirs        -> "iconv-include-dirs"
        IconvLibDirs            -> "iconv-lib-dirs"
        GmpIncludeDirs          -> "gmp-include-dirs"
        GmpLibDirs              -> "gmp-lib-dirs"
        SrcHcOpts               -> "src-hc-opts"
        HostOsCpp               -> "host-os-cpp"
        DynamicExtension        -> "dynamic-extension"
        ProjectVersion          -> "project-version"

ghcWithInterpreter :: Condition
ghcWithInterpreter = do
    [os]   <- showArgs TargetOS
    [arch] <- showArgs TargetArch
    return $
        os `elem` ["mingw32", "cygwin32", "linux", "solaris2",
                   "freebsd", "dragonfly", "netbsd", "openbsd",
                   "darwin", "kfreebsdgnu"]
        &&
        arch `elem` ["i386", "x86_64", "powerpc", "sparc", "sparc64", "arm"]

platformSupportsSharedLibs :: Condition
platformSupportsSharedLibs = do
    [platform] <- showArgs TargetPlatformFull
    solarisBrokenShld <- test SolarisBrokenShld
    return $ notElem platform $
        ["powerpc-unknown-linux",
         "x86_64-unknown-mingw32",
         "i386-unknown-mingw32"] ++
        ["i386-unknown-solaris2" | solarisBrokenShld]

windowsHost :: Condition
windowsHost = do
    [hostOsCpp] <- showArgs HostOsCpp
    return $ hostOsCpp `elem` ["mingw32", "cygwin32"]

-- TODO: refactor helper Condition functions into a separate file
splitObjects :: Stage -> Condition
splitObjects stage = do
    [os]   <- showArgs TargetOS
    [arch] <- showArgs TargetArch
    splitObjectsBroken <- test SplitObjectsBroken
    ghcUnregisterised  <- test GhcUnregisterised
    return $ not splitObjectsBroken && not ghcUnregisterised
           && arch `elem` ["i386", "x86_64", "powerpc", "sparc"]
           && os   `elem` ["mingw32", "cygwin32", "linux", "darwin",
                           "solaris2", "freebsd", "dragonfly", "netbsd",
                           "openbsd"]
