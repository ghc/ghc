module Oracles.Option (
    Option (..), MultiOption (..), windowsHost
    ) where

import Base
import Oracles.Base

-- For each Option the file default.config contains a line of the
-- form 'target-os = mingw32'.
-- (showArg TargetOs) is an action that consults the config files
-- and returns "mingw32".
--
-- MultiOption is used for multiple string options separated by spaces,
-- such as 'src-hc-args = -H32m -O'.
-- (showArgs SrcHcArgs) therefore returns a list of strings ["-H32", "-O"].
data Option = TargetOs
            | TargetArch
            | TargetPlatformFull
            | HostOsCpp
            | DynamicExtension
            | ProjectVersion
            | GhcSourcePath

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
        GhcSourcePath           -> "ghc-source-path"

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
        showStage = ("-stage" ++) . show

windowsHost :: Action Bool
windowsHost = do
    hostOsCpp <- showArg HostOsCpp
    return $ hostOsCpp `elem` ["mingw32", "cygwin32"]
