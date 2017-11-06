module Oracles.Setting (
    configFile, Setting (..), SettingList (..), setting, settingList, getSetting,
    getSettingList,  anyTargetPlatform, anyTargetOs, anyTargetArch, anyHostOs,
    ghcWithInterpreter, ghcEnableTablesNextToCode, useLibFFIForAdjustors,
    ghcCanonVersion, cmdLineLengthLimit, iosHost, osxHost, windowsHost,
    topDirectory, relocatableBuild, installDocDir, installGhcLibDir, libsuf
    ) where

import Hadrian.Expression
import Hadrian.Oracles.TextFile
import Hadrian.Oracles.Path

import Base

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
             | LlvmTarget
             | FfiIncludeDir
             | FfiLibDir
             | GmpIncludeDir
             | GmpLibDir
             | IconvIncludeDir
             | IconvLibDir
             | CursesLibDir
             -- Paths to where GHC is installed (ref: mk/install.mk)
             | InstallPrefix
             | InstallBinDir
             | InstallLibDir
             | InstallDataRootDir
             -- Command lines for invoking the @install@ utility
             | Install
             | InstallData
             | InstallProgram
             | InstallScript
             | InstallDir
             -- Command line for creating a symbolic link
             | LnS

data SettingList = ConfCcArgs Stage
                 | ConfCppArgs Stage
                 | ConfGccLinkerArgs Stage
                 | ConfLdLinkerArgs Stage
                 | HsCppArgs

-- | Maps 'Setting's to names in @cfg/system.config.in@.
setting :: Setting -> Action String
setting key = lookupValueOrError configFile $ case key of
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
    LlvmTarget         -> "llvm-target"
    FfiIncludeDir      -> "ffi-include-dir"
    FfiLibDir          -> "ffi-lib-dir"
    GmpIncludeDir      -> "gmp-include-dir"
    GmpLibDir          -> "gmp-lib-dir"
    IconvIncludeDir    -> "iconv-include-dir"
    IconvLibDir        -> "iconv-lib-dir"
    CursesLibDir       -> "curses-lib-dir"
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
settingList key = fmap words $ lookupValueOrError configFile $ case key of
    ConfCcArgs        stage -> "conf-cc-args-"         ++ stageString stage
    ConfCppArgs       stage -> "conf-cpp-args-"        ++ stageString stage
    ConfGccLinkerArgs stage -> "conf-gcc-linker-args-" ++ stageString stage
    ConfLdLinkerArgs  stage -> "conf-ld-linker-args-"  ++ stageString stage
    HsCppArgs               -> "hs-cpp-args"

-- | Get a configuration setting.
getSetting :: Setting -> Expr c b String
getSetting = expr . setting

-- | Get a list of configuration settings.
getSettingList :: SettingList -> Args c b
getSettingList = expr . settingList

matchSetting :: Setting -> [String] -> Action Bool
matchSetting key values = (`elem` values) <$> setting key

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

-- ref: https://ghc.haskell.org/trac/ghc/wiki/Building/Installing#HowGHCfindsitsfiles
-- | On Windows we normally build a relocatable installation, which assumes that
-- the library directory @libdir@ is in a fixed location relative to the GHC
-- binary, namely @../lib@.
relocatableBuild :: Action Bool
relocatableBuild = windowsHost

installDocDir :: Action String
installDocDir = do
    version <- setting ProjectVersion
    dataDir <- setting InstallDataRootDir
    return $ dataDir -/- ("doc/ghc-" ++ version)

-- | Path to the GHC source tree.
topDirectory :: Action FilePath
topDirectory = fixAbsolutePathOnWindows =<< setting GhcSourcePath

-- ref: mk/install.mk:101
-- TODO: CroosCompilePrefix
-- | Unix: override @libdir@ and @datadir@ to put GHC-specific files in a
-- subdirectory with the version number included.
installGhcLibDir :: Action String
installGhcLibDir = do
    rBuild <- relocatableBuild
    libdir <- setting InstallLibDir
    if rBuild then return libdir
         else do
             version <- setting ProjectVersion
             return $ libdir -/- ("ghc-" ++ version)

-- TODO: find out why we need version number in the dynamic suffix
-- The current theory: dynamic libraries are eventually placed in a single
-- giant directory in the load path of the dynamic linker, and hence we must
-- distinguish different versions of GHC. In contrast static libraries live
-- in their own per-package directory and hence do not need a unique filename.
-- We also need to respect the system's dynamic extension, e.g. .dll or .so.
libsuf :: Way -> Action String
libsuf way =
    if not (wayUnit Dynamic way)
    then return $ waySuffix way ++ ".a" -- e.g., _p.a
    else do
        extension <- setting DynamicExtension  -- e.g., .dll or .so
        version   <- setting ProjectVersion    -- e.g., 7.11.20141222
        let prefix = wayPrefix $ removeWayUnit Dynamic way
        -- e.g., p_ghc7.11.20141222.dll (the result)
        return $ prefix ++ "-ghc" ++ version ++ extension
