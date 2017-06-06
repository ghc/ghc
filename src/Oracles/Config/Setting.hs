module Oracles.Config.Setting (
    Setting (..), SettingList (..), setting, settingList, getSetting,
    getSettingList, anyTargetPlatform, anyTargetOs, anyTargetArch, anyHostOs,
    ghcWithInterpreter, ghcEnableTablesNextToCode, useLibFFIForAdjustors,
    ghcCanonVersion, cmdLineLengthLimit, iosHost, osxHost, windowsHost,
    relocatableBuild, installDocDir, installGhcLibDir
    ) where

import Control.Monad.Trans.Reader

import Base
import Oracles.Config
import Stage

-- TODO: Reduce the variety of similar flags (e.g. CPP and non-CPP versions).
-- | Each 'Setting' comes from @system.config@ file, e.g. 'target-os = mingw32'.
-- @setting TargetOs@ looks up the config file and returns "mingw32".
-- 'SettingList' is used for multiple string values separated by spaces, such
-- as @gmp-include-dirs = a b@.
-- @settingList GmpIncludeDirs@ therefore returns a list of strings ["a", "b"].
data Setting = BuildArch
             | BuildOs
             | BuildPlatform
             | BuildVendor
             | CcClangBackend
             | CcLlvmBackend
             | DynamicExtension
             | GhcMajorVersion
             | GhcMinorVersion
             | GhcPatchLevel
             | GhcVersion
             | GhcSourcePath
             | HostArch
             | HostOs
             | HostPlatform
             | HostVendor
             | ProjectGitCommitId
             | ProjectName
             | ProjectVersion
             | ProjectVersionInt
             | ProjectPatchLevel
             | ProjectPatchLevel1
             | ProjectPatchLevel2
             | TargetArch
             | TargetOs
             | TargetPlatform
             | TargetPlatformFull
             | TargetVendor
             | FfiIncludeDir
             | FfiLibDir
             | GmpIncludeDir
             | GmpLibDir
             | IconvIncludeDir
             | IconvLibDir
             -- Paths to where GHC is installed
             | InstallPrefix
             | InstallBinDir
             | InstallLibDir
             | InstallDataRootDir
             -- "install" utility
             | Install
             | InstallData
             | InstallProgram
             | InstallScript
             | InstallDir
             -- symbolic link
             | LnS

data SettingList = ConfCcArgs Stage
                 | ConfCppArgs Stage
                 | ConfGccLinkerArgs Stage
                 | ConfLdLinkerArgs Stage
                 | HsCppArgs

setting :: Setting -> Action String
setting key = unsafeAskConfig $ case key of
    BuildArch          -> "build-arch"
    BuildOs            -> "build-os"
    BuildPlatform      -> "build-platform"
    BuildVendor        -> "build-vendor"
    CcClangBackend     -> "cc-clang-backend"
    CcLlvmBackend      -> "cc-llvm-backend"
    DynamicExtension   -> "dynamic-extension"
    GhcMajorVersion    -> "ghc-major-version"
    GhcMinorVersion    -> "ghc-minor-version"
    GhcPatchLevel      -> "ghc-patch-level"
    GhcVersion         -> "ghc-version"
    GhcSourcePath      -> "ghc-source-path"
    HostArch           -> "host-arch"
    HostOs             -> "host-os"
    HostPlatform       -> "host-platform"
    HostVendor         -> "host-vendor"
    ProjectGitCommitId -> "project-git-commit-id"
    ProjectName        -> "project-name"
    ProjectVersion     -> "project-version"
    ProjectVersionInt  -> "project-version-int"
    ProjectPatchLevel  -> "project-patch-level"
    ProjectPatchLevel1 -> "project-patch-level1"
    ProjectPatchLevel2 -> "project-patch-level2"
    TargetArch         -> "target-arch"
    TargetOs           -> "target-os"
    TargetPlatform     -> "target-platform"
    TargetPlatformFull -> "target-platform-full"
    TargetVendor       -> "target-vendor"
    FfiIncludeDir      -> "ffi-include-dir"
    FfiLibDir          -> "ffi-lib-dir"
    GmpIncludeDir      -> "gmp-include-dir"
    GmpLibDir          -> "gmp-lib-dir"
    IconvIncludeDir    -> "iconv-include-dir"
    IconvLibDir        -> "iconv-lib-dir"
    InstallPrefix      -> "install-prefix"
    InstallBinDir      -> "install-bindir"
    InstallLibDir      -> "install-libdir"
    InstallDataRootDir -> "install-datarootdir"
    Install            -> "install"
    InstallDir         -> "install-dir"
    InstallProgram     -> "install-program"
    InstallScript      -> "install-script"
    InstallData        -> "install-data"
    LnS                -> "ln-s"

settingList :: SettingList -> Action [String]
settingList key = fmap words $ unsafeAskConfig $ case key of
    ConfCcArgs        stage -> "conf-cc-args-"         ++ stageString stage
    ConfCppArgs       stage -> "conf-cpp-args-"        ++ stageString stage
    ConfGccLinkerArgs stage -> "conf-gcc-linker-args-" ++ stageString stage
    ConfLdLinkerArgs  stage -> "conf-ld-linker-args-"  ++ stageString stage
    HsCppArgs               -> "hs-cpp-args"

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

iosHost :: Action Bool
iosHost = anyHostOs ["ios"]

osxHost :: Action Bool
osxHost = anyHostOs ["darwin"]

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

-- | Canonicalised GHC version number, used for integer version comparisons. We
-- expand GhcMinorVersion to two digits by adding a leading zero if necessary.
ghcCanonVersion :: Action String
ghcCanonVersion = do
    ghcMajorVersion <- setting GhcMajorVersion
    ghcMinorVersion <- setting GhcMinorVersion
    let leadingZero = [ '0' | length ghcMinorVersion == 1 ]
    return $ ghcMajorVersion ++ leadingZero ++ ghcMinorVersion

-- | Command lines have limited size on Windows. Since Windows 7 the limit is
-- 32768 characters (theoretically). In practice we use 31000 to leave some
-- breathing space for the builder's path & name, auxiliary flags, and other
-- overheads. Use this function to set limits for other OSs if necessary.
cmdLineLengthLimit :: Action Int
cmdLineLengthLimit = do
    windows <- windowsHost
    osx     <- osxHost
    return $ case (windows, osx) of
        -- Windows:
        (True, False) -> 31000
        -- On Mac OSX ARG_MAX is 262144, yet when using @xargs@ on OSX this is
        -- reduced by over 20 000. Hence, 200 000 seems like a sensible limit.
        (False, True) -> 200000
        -- On all other systems, we try this:
        _             -> 4194304 -- Cabal library needs a bit more than 2MB!

-- | On Windows we normally want to make a relocatable bindist,
-- to we ignore flags like libdir
-- ref: mk/config.mk.in:232
relocatableBuild :: Action Bool
relocatableBuild = windowsHost

installDocDir :: Action String
installDocDir = do
  version <- setting ProjectVersion
  (-/- ("doc/ghc-" ++ version)) <$> setting InstallDataRootDir

-- | Unix: override libdir and datadir to put ghc-specific stuff in
-- a subdirectory with the version number included.
-- ref: mk/install.mk:101
-- TODO: CroosCompilePrefix
installGhcLibDir :: Action String
installGhcLibDir = do
  r <- relocatableBuild
  libdir <- setting InstallLibDir
  if r then return libdir
       else do
         v <- setting ProjectVersion
         return (libdir -/- ("ghc-" ++ v))
