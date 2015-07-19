module Oracles.Setting (
    Setting (..), SettingList (..),
    setting, settingList,
    windowsHost
    ) where

import Stage
import Oracles.Base

-- Each Setting comes from the system.config file, e.g. 'target-os = mingw32'.
-- setting TargetOs looks up the config file and returns "mingw32".
--
-- SettingList is used for multiple string values separated by spaces, such
-- as 'src-hc-args = -H32m -O'.
-- settingList SrcHcArgs therefore returns a list of strings ["-H32", "-O"].
data Setting = TargetOs
             | TargetArch
             | TargetPlatformFull
             | HostOsCpp
             | DynamicExtension
             | ProjectVersion
             | GhcSourcePath

data SettingList = SrcHcArgs
                 | ConfCcArgs Stage
                 | ConfGccLinkerArgs Stage
                 | ConfLdLinkerArgs Stage
                 | ConfCppArgs Stage
                 | IconvIncludeDirs
                 | IconvLibDirs
                 | GmpIncludeDirs
                 | GmpLibDirs

setting :: Setting -> Action String
setting key = askConfig $ case key of
    TargetOs           -> "target-os"
    TargetArch         -> "target-arch"
    TargetPlatformFull -> "target-platform-full"
    HostOsCpp          -> "host-os-cpp"
    DynamicExtension   -> "dynamic-extension"
    ProjectVersion     -> "project-version"
    GhcSourcePath      -> "ghc-source-path"

settingList :: SettingList -> Action [String]
settingList key = fmap words $ askConfig $ case key of
    SrcHcArgs               -> "src-hc-args"
    ConfCcArgs        stage -> "conf-cc-args-stage"         ++ show stage
    ConfCppArgs       stage -> "conf-cpp-args-stage"        ++ show stage
    ConfGccLinkerArgs stage -> "conf-gcc-linker-args-stage" ++ show stage
    ConfLdLinkerArgs  stage -> "conf-ld-linker-args-stage"  ++ show stage
    IconvIncludeDirs        -> "iconv-include-dirs"
    IconvLibDirs            -> "iconv-lib-dirs"
    GmpIncludeDirs          -> "gmp-include-dirs"
    GmpLibDirs              -> "gmp-lib-dirs"

windowsHost :: Action Bool
windowsHost = do
    hostOsCpp <- setting HostOsCpp
    return $ hostOsCpp `elem` ["mingw32", "cygwin32"]
