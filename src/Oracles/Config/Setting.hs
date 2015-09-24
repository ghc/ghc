module Oracles.Config.Setting (
    Setting (..), SettingList (..),
    setting, settingList, getSetting, getSettingList,
    anyTargetPlatform, anyTargetOs, anyTargetArch, anyHostOs, windowsHost,
    ghcWithInterpreter, ghcEnableTablesNextToCode, useLibFFIForAdjustors,
    ghcCanonVersion, cmdLineLengthLimit
    ) where

import Base
import Oracles.Config
import Stage

-- Each Setting comes from the system.config file, e.g. 'target-os = mingw32'.
-- setting TargetOs looks up the config file and returns "mingw32".
--
-- SettingList is used for multiple string values separated by spaces, such
-- as 'gmp-include-dirs = a b'.
-- settingList GmpIncludeDirs therefore returns a list of strings ["a", "b"].
data Setting = DynamicExtension
             | GhcMajorVersion
             | GhcMinorVersion
             | GhcPatchLevel
             | GhcVersion
             | GhcSourcePath
             | HostArch
             | HostOs
             | ProjectGitCommitId
             | ProjectName
             | ProjectVersion
             | ProjectVersionInt
             | ProjectPatchLevel
             | ProjectPatchLevel1
             | ProjectPatchLevel2
             | TargetArch
             | TargetOs
             | TargetPlatformFull

data SettingList = ConfCcArgs Stage
                 | ConfCppArgs Stage
                 | ConfGccLinkerArgs Stage
                 | ConfLdLinkerArgs Stage
                 | GmpIncludeDirs
                 | GmpLibDirs
                 | IconvIncludeDirs
                 | IconvLibDirs

setting :: Setting -> Action String
setting key = askConfig $ case key of
    DynamicExtension   -> "dynamic-extension"
    GhcMajorVersion    -> "ghc-major-version"
    GhcMinorVersion    -> "ghc-minor-version"
    GhcPatchLevel      -> "ghc-patch-level"
    GhcVersion         -> "ghc-version"
    GhcSourcePath      -> "ghc-source-path"
    HostArch           -> "host-arch"
    HostOs             -> "host-os"
    ProjectGitCommitId -> "project-git-commit-id"
    ProjectName        -> "project-name"
    ProjectVersion     -> "project-version"
    ProjectVersionInt  -> "project-version-int"
    ProjectPatchLevel  -> "project-patch-level"
    ProjectPatchLevel1 -> "project-patch-level1"
    ProjectPatchLevel2 -> "project-patch-level2"
    TargetArch         -> "target-arch"
    TargetOs           -> "target-os"
    TargetPlatformFull -> "target-platform-full"

settingList :: SettingList -> Action [String]
settingList key = fmap words $ askConfig $ case key of
    ConfCcArgs        stage -> "conf-cc-args-stage"         ++ show stage
    ConfCppArgs       stage -> "conf-cpp-args-stage"        ++ show stage
    ConfGccLinkerArgs stage -> "conf-gcc-linker-args-stage" ++ show stage
    ConfLdLinkerArgs  stage -> "conf-ld-linker-args-stage"  ++ show stage
    GmpIncludeDirs          -> "gmp-include-dirs"
    GmpLibDirs              -> "gmp-lib-dirs"
    IconvIncludeDirs        -> "iconv-include-dirs"
    IconvLibDirs            -> "iconv-lib-dirs"

getSetting :: Setting -> ReaderT a Action String
getSetting = lift . setting

getSettingList :: SettingList -> ReaderT a Action [String]
getSettingList = lift . settingList

matchSetting :: Setting -> [String] -> Action Bool
matchSetting key values = fmap (`elem` values) $ setting key

anyTargetPlatform :: [String] -> Action Bool
anyTargetPlatform = matchSetting TargetPlatformFull

anyTargetOs :: [String] -> Action Bool
anyTargetOs = matchSetting TargetOs

anyTargetArch :: [String] -> Action Bool
anyTargetArch = matchSetting TargetArch

anyHostOs :: [String] -> Action Bool
anyHostOs = matchSetting HostOs

windowsHost :: Action Bool
windowsHost = anyHostOs ["mingw32", "cygwin32"]

ghcWithInterpreter :: Action Bool
ghcWithInterpreter = do
    goodOs <- anyTargetOs [ "mingw32", "cygwin32", "linux", "solaris2"
                          , "freebsd", "dragonfly", "netbsd", "openbsd"
                          , "darwin", "kfreebsdgnu" ]
    goodArch <- anyTargetArch [ "i386", "x86_64", "powerpc", "sparc"
                              , "sparc64", "arm" ]
    return $ goodOs && goodArch

ghcEnableTablesNextToCode :: Action Bool
ghcEnableTablesNextToCode = notM $ anyTargetArch ["ia64", "powerpc64", "powerpc64le"]

useLibFFIForAdjustors :: Action Bool
useLibFFIForAdjustors = notM $ anyTargetArch ["i386", "x86_64"]

-- Canonicalised GHC version number, used for integer version comparisons. We
-- expand GhcMinorVersion to two digits by adding a leading zero if necessary.
ghcCanonVersion :: Action String
ghcCanonVersion = do
    ghcMajorVersion <- setting GhcMajorVersion
    ghcMinorVersion <- setting GhcMinorVersion
    let leadingZero = [ '0' | length ghcMinorVersion == 1 ]
    return $ ghcMajorVersion ++ leadingZero ++ ghcMinorVersion

-- Command lines have limited size on Windows. Since Windows 7 the limit is
-- 32768 characters (theoretically). In practice we use 31000 to leave some
-- breathing space for the builder's path & name, auxiliary flags, and other
-- overheads. Use this function to set limits for other OSs if necessary.
cmdLineLengthLimit :: Action Int
cmdLineLengthLimit = do
    windows <- windowsHost
    return $ if windows
             then 31000
             else 4194304 -- Cabal needs a bit more than 2MB!
