{-# LANGUAGE NoImplicitPrelude #-}
module Oracles.Option (
    Option (..), MultiOption (..),
    ghcWithInterpreter, platformSupportsSharedLibs, windowsHost, splitObjects
    ) where

import Base
import Oracles.Flag
import Oracles.Base

-- For each Option the files {default.config, user.config} contain
-- a line of the form 'target-os = mingw32'.
-- (showArg TargetOs) is an action that consults the config files
-- and returns "mingw32".
--
-- MultiOption is used for multiple string options separated by spaces,
-- such as 'src-hc-args' = -H32m -O'.
-- (showArgs SrcHcArgs) therefore returns a list of strings ["-H32", "-O"].
data Option = TargetOs
            | TargetArch
            | TargetPlatformFull
            | HostOsCpp
            | DynamicExtension
            | ProjectVersion

data MultiOption = SrcHcArgs
                 | ConfCcArgs Stage
                 | ConfGccLinkerArgs Stage
                 | ConfLdLinkerArgs Stage
                 | ConfCppArgs Stage
                 | IconvIncludeDirs
                 | IconvLibDirs
                 | GmpIncludeDirs
                 | GmpLibDirs

instance ShowArg Option where
    showArg opt = askConfig $ case opt of
        TargetOs                -> "target-os"
        TargetArch              -> "target-arch"
        TargetPlatformFull      -> "target-platform-full"
        HostOsCpp               -> "host-os-cpp"
        DynamicExtension        -> "dynamic-extension"
        ProjectVersion          -> "project-version"

instance ShowArgs MultiOption where
    showArgs opt = showArgs $ fmap words $ askConfig $ case opt of
        SrcHcArgs               -> "src-hc-args"
        ConfCcArgs        stage -> "conf-cc-args"         ++ showStage stage
        ConfCppArgs       stage -> "conf-cpp-args"        ++ showStage stage
        ConfGccLinkerArgs stage -> "conf-gcc-linker-args" ++ showStage stage
        ConfLdLinkerArgs  stage -> "conf-ld-linker-args"  ++ showStage stage
        IconvIncludeDirs        -> "iconv-include-dirs"
        IconvLibDirs            -> "iconv-lib-dirs"
        GmpIncludeDirs          -> "gmp-include-dirs"
        GmpLibDirs              -> "gmp-lib-dirs"
      where
        showStage = ("-stage-" ++) . show

ghcWithInterpreter :: Condition
ghcWithInterpreter = do
    os   <- showArg TargetOs
    arch <- showArg TargetArch
    return $
        os `elem` ["mingw32", "cygwin32", "linux", "solaris2",
                   "freebsd", "dragonfly", "netbsd", "openbsd",
                   "darwin", "kfreebsdgnu"]
        &&
        arch `elem` ["i386", "x86_64", "powerpc", "sparc", "sparc64", "arm"]

platformSupportsSharedLibs :: Condition
platformSupportsSharedLibs = do
    platform <- showArg TargetPlatformFull
    solarisBrokenShld <- test SolarisBrokenShld
    return $ notElem platform $
        ["powerpc-unknown-linux",
         "x86_64-unknown-mingw32",
         "i386-unknown-mingw32"] ++
        ["i386-unknown-solaris2" | solarisBrokenShld]

windowsHost :: Condition
windowsHost = do
    hostOsCpp <- showArg HostOsCpp
    return $ hostOsCpp `elem` ["mingw32", "cygwin32"]

-- TODO: refactor helper Condition functions into a separate file
splitObjects :: Stage -> Condition
splitObjects stage = do
    arch <- showArg TargetArch
    os   <- showArg TargetOs
    not SplitObjectsBroken && not GhcUnregisterised
        && stage == Stage1
        && arch `elem` ["i386", "x86_64", "powerpc", "sparc"]
        && os   `elem` ["mingw32", "cygwin32", "linux", "darwin",
                       "solaris2", "freebsd", "dragonfly", "netbsd",
                       "openbsd"]
