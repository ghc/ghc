module Oracles.Setting (
    Setting (..), SettingMulti (..),
    setting, settingMulti,
    windowsHost
    ) where

import Stage
import Oracles.Base
import Development.Shake

-- Each Setting comes from the system.config file, e.g. 'target-os = mingw32'.
-- setting TargetOs looks up the config file and returns "mingw32".
--
-- SettingMulti is used for multiple string values separated by spaces, such
-- as 'src-hc-args = -H32m -O'.
-- settingMulti SrcHcArgs therefore returns a list of strings ["-H32", "-O"].
data Setting = TargetOs
             | TargetArch
             | TargetPlatformFull
             | HostOsCpp
             | DynamicExtension
             | ProjectVersion
             | GhcSourcePath

data SettingMulti = SrcHcArgs
                  | ConfCcArgs Stage
                  | ConfGccLinkerArgs Stage
                  | ConfLdLinkerArgs Stage
                  | ConfCppArgs Stage
                  | IconvIncludeDirs
                  | IconvLibDirs
                  | GmpIncludeDirs
                  | GmpLibDirs

setting :: Setting -> Action String
setting s = askConfig $ case s of
    TargetOs           -> "target-os"
    TargetArch         -> "target-arch"
    TargetPlatformFull -> "target-platform-full"
    HostOsCpp          -> "host-os-cpp"
    DynamicExtension   -> "dynamic-extension"
    ProjectVersion     -> "project-version"
    GhcSourcePath      -> "ghc-source-path"

settingMulti :: SettingMulti -> Action [String]
settingMulti s = fmap words $ askConfig $ case s of
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
    hostOsCpp <- setting HostOsCpp
    return $ hostOsCpp `elem` ["mingw32", "cygwin32"]
