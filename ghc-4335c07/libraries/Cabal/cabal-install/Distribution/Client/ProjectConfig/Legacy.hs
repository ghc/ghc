{-# LANGUAGE RecordWildCards, NamedFieldPuns, DeriveGeneric #-}

-- | Project configuration, implementation in terms of legacy types.
--
module Distribution.Client.ProjectConfig.Legacy (

    -- * Project config in terms of legacy types
    LegacyProjectConfig,
    parseLegacyProjectConfig,
    showLegacyProjectConfig,

    -- * Conversion to and from legacy config types
    commandLineFlagsToProjectConfig,
    convertLegacyProjectConfig,
    convertLegacyGlobalConfig,
    convertToLegacyProjectConfig,

    -- * Internals, just for tests
    parsePackageLocationTokenQ,
    renderPackageLocationToken,
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Client.ProjectConfig.Types
import Distribution.Client.Types
         ( RemoteRepo(..), emptyRemoteRepo
         , AllowNewer(..), AllowOlder(..) )

import Distribution.Client.Config
         ( SavedConfig(..), remoteRepoFields )

import Distribution.Solver.Types.ConstraintSource

import Distribution.Package
import Distribution.PackageDescription
         ( SourceRepo(..), RepoKind(..)
         , dispFlagAssignment, parseFlagAssignment )
import Distribution.PackageDescription.Parse
         ( sourceRepoFieldDescrs )
import Distribution.Simple.Compiler
         ( OptimisationLevel(..), DebugInfoLevel(..) )
import Distribution.Simple.Setup
         ( Flag(Flag), toFlag, fromFlagOrDefault
         , ConfigFlags(..), configureOptions
         , HaddockFlags(..), haddockOptions, defaultHaddockFlags
         , programDbPaths', splitArgs
         )
import Distribution.Client.Setup
         ( GlobalFlags(..), globalCommand
         , ConfigExFlags(..), configureExOptions, defaultConfigExFlags
         , InstallFlags(..), installOptions, defaultInstallFlags )
import Distribution.Simple.Program
         ( programName, knownPrograms )
import Distribution.Simple.Program.Db
         ( ProgramDb, defaultProgramDb )
import Distribution.Simple.Utils
         ( lowercase )
import Distribution.Utils.NubList
         ( toNubList, fromNubList, overNubList )
import Distribution.Simple.LocalBuildInfo
         ( toPathTemplate, fromPathTemplate )

import Distribution.Text
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP
         ( ReadP, (+++), (<++) )
import qualified Text.Read as Read
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint
         ( Doc, ($+$) )
import qualified Distribution.ParseUtils as ParseUtils (field)
import Distribution.ParseUtils
         ( ParseResult(..), PError(..), syntaxError, PWarning(..), warning
         , simpleField, commaNewLineListField
         , showToken )
import Distribution.Client.ParseUtils
import Distribution.Simple.Command
         ( CommandUI(commandOptions), ShowOrParseArgs(..)
         , OptionField, option, reqArg' )

import qualified Data.Map as Map

------------------------------------------------------------------
-- Representing the project config file in terms of legacy types
--

-- | We already have parsers\/pretty-printers for almost all the fields in the
-- project config file, but they're in terms of the types used for the command
-- line flags for Setup.hs or cabal commands. We don't want to redefine them
-- all, at least not yet so for the moment we use the parsers at the old types
-- and use conversion functions.
--
-- Ultimately if\/when this project-based approach becomes the default then we
-- can redefine the parsers directly for the new types.
--
data LegacyProjectConfig = LegacyProjectConfig {
       legacyPackages          :: [String],
       legacyPackagesOptional  :: [String],
       legacyPackagesRepo      :: [SourceRepo],
       legacyPackagesNamed     :: [Dependency],

       legacySharedConfig      :: LegacySharedConfig,
       legacyLocalConfig       :: LegacyPackageConfig,
       legacySpecificConfig    :: MapMappend PackageName LegacyPackageConfig
     } deriving Generic

instance Monoid LegacyProjectConfig where
  mempty  = gmempty
  mappend = (<>)

instance Semigroup LegacyProjectConfig where
  (<>) = gmappend

data LegacyPackageConfig = LegacyPackageConfig {
       legacyConfigureFlags    :: ConfigFlags,
       legacyInstallPkgFlags   :: InstallFlags,
       legacyHaddockFlags      :: HaddockFlags
     } deriving Generic

instance Monoid LegacyPackageConfig where
  mempty  = gmempty
  mappend = (<>)

instance Semigroup LegacyPackageConfig where
  (<>) = gmappend

data LegacySharedConfig = LegacySharedConfig {
       legacyGlobalFlags       :: GlobalFlags,
       legacyConfigureShFlags  :: ConfigFlags,
       legacyConfigureExFlags  :: ConfigExFlags,
       legacyInstallFlags      :: InstallFlags
     } deriving Generic

instance Monoid LegacySharedConfig where
  mempty  = gmempty
  mappend = (<>)

instance Semigroup LegacySharedConfig where
  (<>) = gmappend


------------------------------------------------------------------
-- Converting from and to the legacy types
--

-- | Convert configuration from the @cabal configure@ or @cabal build@ command
-- line into a 'ProjectConfig' value that can combined with configuration from
-- other sources.
--
-- At the moment this uses the legacy command line flag types. See
-- 'LegacyProjectConfig' for an explanation.
--
commandLineFlagsToProjectConfig :: GlobalFlags
                                -> ConfigFlags  -> ConfigExFlags
                                -> InstallFlags -> HaddockFlags
                                -> ProjectConfig
commandLineFlagsToProjectConfig globalFlags configFlags configExFlags
                                installFlags haddockFlags =
    mempty {
      projectConfigBuildOnly     = convertLegacyBuildOnlyFlags
                                     globalFlags configFlags
                                     installFlags haddockFlags,
      projectConfigShared        = convertLegacyAllPackageFlags
                                     globalFlags configFlags
                                     configExFlags installFlags,
      projectConfigLocalPackages = convertLegacyPerPackageFlags
                                     configFlags installFlags haddockFlags
    }


-- | Convert from the types currently used for the user-wide @~/.cabal/config@
-- file into the 'ProjectConfig' type.
--
-- Only a subset of the 'ProjectConfig' can be represented in the user-wide
-- config. In particular it does not include packages that are in the project,
-- and it also doesn't support package-specific configuration (only
-- configuration that applies to all packages).
--
convertLegacyGlobalConfig :: SavedConfig -> ProjectConfig
convertLegacyGlobalConfig
    SavedConfig {
      savedGlobalFlags       = globalFlags,
      savedInstallFlags      = installFlags,
      savedConfigureFlags    = configFlags,
      savedConfigureExFlags  = configExFlags,
      savedUserInstallDirs   = _,
      savedGlobalInstallDirs = _,
      savedUploadFlags       = _,
      savedReportFlags       = _,
      savedHaddockFlags      = haddockFlags
    } =
    mempty {
      projectConfigShared        = configAllPackages,
      projectConfigLocalPackages = configLocalPackages,
      projectConfigBuildOnly     = configBuildOnly
    }
  where
    --TODO: [code cleanup] eliminate use of default*Flags here and specify the
    -- defaults in the various resolve functions in terms of the new types.
    configExFlags' = defaultConfigExFlags <> configExFlags
    installFlags'  = defaultInstallFlags  <> installFlags
    haddockFlags'  = defaultHaddockFlags  <> haddockFlags

    configLocalPackages = convertLegacyPerPackageFlags
                            configFlags installFlags' haddockFlags'
    configAllPackages   = convertLegacyAllPackageFlags
                            globalFlags configFlags
                            configExFlags' installFlags'
    configBuildOnly     = convertLegacyBuildOnlyFlags
                            globalFlags configFlags
                            installFlags' haddockFlags'


-- | Convert the project config from the legacy types to the 'ProjectConfig'
-- and associated types. See 'LegacyProjectConfig' for an explanation of the
-- approach.
--
convertLegacyProjectConfig :: LegacyProjectConfig -> ProjectConfig
convertLegacyProjectConfig
  LegacyProjectConfig {
    legacyPackages,
    legacyPackagesOptional,
    legacyPackagesRepo,
    legacyPackagesNamed,
    legacySharedConfig = LegacySharedConfig globalFlags configShFlags
                                            configExFlags installSharedFlags,
    legacyLocalConfig  = LegacyPackageConfig configFlags installPerPkgFlags
                                             haddockFlags,
    legacySpecificConfig
  } =

    ProjectConfig {
      projectPackages              = legacyPackages,
      projectPackagesOptional      = legacyPackagesOptional,
      projectPackagesRepo          = legacyPackagesRepo,
      projectPackagesNamed         = legacyPackagesNamed,

      projectConfigBuildOnly       = configBuildOnly,
      projectConfigShared          = configAllPackages,
      projectConfigProvenance      = mempty,
      projectConfigLocalPackages   = configLocalPackages,
      projectConfigSpecificPackage = fmap perPackage legacySpecificConfig
    }
  where
    configLocalPackages = convertLegacyPerPackageFlags
                            configFlags installPerPkgFlags haddockFlags
    configAllPackages   = convertLegacyAllPackageFlags
                            globalFlags (configFlags <> configShFlags)
                            configExFlags installSharedFlags
    configBuildOnly     = convertLegacyBuildOnlyFlags
                            globalFlags configShFlags
                            installSharedFlags haddockFlags

    perPackage (LegacyPackageConfig perPkgConfigFlags perPkgInstallFlags
                                    perPkgHaddockFlags) =
      convertLegacyPerPackageFlags
        perPkgConfigFlags perPkgInstallFlags perPkgHaddockFlags


-- | Helper used by other conversion functions that returns the
-- 'ProjectConfigShared' subset of the 'ProjectConfig'.
--
convertLegacyAllPackageFlags :: GlobalFlags -> ConfigFlags
                             -> ConfigExFlags -> InstallFlags
                             -> ProjectConfigShared
convertLegacyAllPackageFlags globalFlags configFlags
                             configExFlags installFlags =
    ProjectConfigShared{..}
  where
    GlobalFlags {
      globalConfigFile        = projectConfigConfigFile,
      globalSandboxConfigFile = _, -- ??
      globalRemoteRepos       = projectConfigRemoteRepos,
      globalLocalRepos        = projectConfigLocalRepos
    } = globalFlags

    ConfigFlags {
      configDistPref            = projectConfigDistDir,
      configHcFlavor            = projectConfigHcFlavor,
      configHcPath              = projectConfigHcPath,
      configHcPkg               = projectConfigHcPkg
    --configInstallDirs         = projectConfigInstallDirs,
    --configUserInstall         = projectConfigUserInstall,
    --configPackageDBs          = projectConfigPackageDBs,
    } = configFlags

    ConfigExFlags {
      configCabalVersion        = projectConfigCabalVersion,
      configExConstraints       = projectConfigConstraints,
      configPreferences         = projectConfigPreferences,
      configSolver              = projectConfigSolver,
      configAllowOlder          = projectConfigAllowOlder,
      configAllowNewer          = projectConfigAllowNewer
    } = configExFlags

    InstallFlags {
      installProjectFileName    = projectConfigProjectFile,
      installHaddockIndex       = projectConfigHaddockIndex,
    --installReinstall          = projectConfigReinstall,
    --installAvoidReinstalls    = projectConfigAvoidReinstalls,
    --installOverrideReinstall  = projectConfigOverrideReinstall,
      installIndexState         = projectConfigIndexState,
      installMaxBackjumps       = projectConfigMaxBackjumps,
    --installUpgradeDeps        = projectConfigUpgradeDeps,
      installReorderGoals       = projectConfigReorderGoals,
      installCountConflicts     = projectConfigCountConflicts,
      installPerComponent       = projectConfigPerComponent,
      installIndependentGoals   = projectConfigIndependentGoals,
    --installShadowPkgs         = projectConfigShadowPkgs,
      installStrongFlags        = projectConfigStrongFlags,
      installAllowBootLibInstalls = projectConfigAllowBootLibInstalls
    } = installFlags



-- | Helper used by other conversion functions that returns the
-- 'PackageConfig' subset of the 'ProjectConfig'.
--
convertLegacyPerPackageFlags :: ConfigFlags -> InstallFlags -> HaddockFlags
                             -> PackageConfig
convertLegacyPerPackageFlags configFlags installFlags haddockFlags =
    PackageConfig{..}
  where
    ConfigFlags {
      configProgramPaths,
      configProgramArgs,
      configProgramPathExtra    = packageConfigProgramPathExtra,
      configVanillaLib          = packageConfigVanillaLib,
      configProfLib             = packageConfigProfLib,
      configSharedLib           = packageConfigSharedLib,
      configStaticLib           = packageConfigStaticLib,
      configDynExe              = packageConfigDynExe,
      configProfExe             = packageConfigProfExe,
      configProf                = packageConfigProf,
      configProfDetail          = packageConfigProfDetail,
      configProfLibDetail       = packageConfigProfLibDetail,
      configConfigureArgs       = packageConfigConfigureArgs,
      configOptimization        = packageConfigOptimization,
      configProgPrefix          = packageConfigProgPrefix,
      configProgSuffix          = packageConfigProgSuffix,
      configGHCiLib             = packageConfigGHCiLib,
      configSplitObjs           = packageConfigSplitObjs,
      configStripExes           = packageConfigStripExes,
      configStripLibs           = packageConfigStripLibs,
      configExtraLibDirs        = packageConfigExtraLibDirs,
      configExtraFrameworkDirs  = packageConfigExtraFrameworkDirs,
      configExtraIncludeDirs    = packageConfigExtraIncludeDirs,
      configConfigurationsFlags = packageConfigFlagAssignment,
      configTests               = packageConfigTests,
      configBenchmarks          = packageConfigBenchmarks,
      configCoverage            = coverage,
      configLibCoverage         = libcoverage, --deprecated
      configDebugInfo           = packageConfigDebugInfo,
      configRelocatable         = packageConfigRelocatable
    } = configFlags
    packageConfigProgramPaths   = MapLast    (Map.fromList configProgramPaths)
    packageConfigProgramArgs    = MapMappend (Map.fromList configProgramArgs)

    packageConfigCoverage       = coverage <> libcoverage
    --TODO: defer this merging to the resolve phase

    InstallFlags {
      installDocumentation      = packageConfigDocumentation,
      installRunTests           = packageConfigRunTests
    } = installFlags

    HaddockFlags {
      haddockHoogle             = packageConfigHaddockHoogle,
      haddockHtml               = packageConfigHaddockHtml,
      haddockHtmlLocation       = packageConfigHaddockHtmlLocation,
      haddockForeignLibs        = packageConfigHaddockForeignLibs,
      haddockForHackage         = packageConfigHaddockForHackage,
      haddockExecutables        = packageConfigHaddockExecutables,
      haddockTestSuites         = packageConfigHaddockTestSuites,
      haddockBenchmarks         = packageConfigHaddockBenchmarks,
      haddockInternal           = packageConfigHaddockInternal,
      haddockCss                = packageConfigHaddockCss,
      haddockHscolour           = packageConfigHaddockHscolour,
      haddockHscolourCss        = packageConfigHaddockHscolourCss,
      haddockContents           = packageConfigHaddockContents
    } = haddockFlags



-- | Helper used by other conversion functions that returns the
-- 'ProjectConfigBuildOnly' subset of the 'ProjectConfig'.
--
convertLegacyBuildOnlyFlags :: GlobalFlags -> ConfigFlags
                            -> InstallFlags -> HaddockFlags
                            -> ProjectConfigBuildOnly
convertLegacyBuildOnlyFlags globalFlags configFlags
                              installFlags haddockFlags =
    ProjectConfigBuildOnly{..}
  where
    GlobalFlags {
      globalCacheDir          = projectConfigCacheDir,
      globalLogsDir           = projectConfigLogsDir,
      globalWorldFile         = _,
      globalHttpTransport     = projectConfigHttpTransport,
      globalIgnoreExpiry      = projectConfigIgnoreExpiry,
      globalStoreDir          = projectConfigStoreDir
    } = globalFlags

    ConfigFlags {
      configVerbosity           = projectConfigVerbosity
    } = configFlags

    InstallFlags {
      installDryRun             = projectConfigDryRun,
      installOnly               = _,
      installOnlyDeps           = projectConfigOnlyDeps,
      installRootCmd            = _,
      installSummaryFile        = projectConfigSummaryFile,
      installLogFile            = projectConfigLogFile,
      installBuildReports       = projectConfigBuildReports,
      installReportPlanningFailure = projectConfigReportPlanningFailure,
      installSymlinkBinDir      = projectConfigSymlinkBinDir,
      installOneShot            = projectConfigOneShot,
      installNumJobs            = projectConfigNumJobs,
      installKeepGoing          = projectConfigKeepGoing,
      installOfflineMode        = projectConfigOfflineMode
    } = installFlags

    HaddockFlags {
      haddockKeepTempFiles      = projectConfigKeepTempFiles --TODO: this ought to live elsewhere
    } = haddockFlags


convertToLegacyProjectConfig :: ProjectConfig -> LegacyProjectConfig
convertToLegacyProjectConfig
    projectConfig@ProjectConfig {
      projectPackages,
      projectPackagesOptional,
      projectPackagesRepo,
      projectPackagesNamed,
      projectConfigLocalPackages,
      projectConfigSpecificPackage
    } =
    LegacyProjectConfig {
      legacyPackages         = projectPackages,
      legacyPackagesOptional = projectPackagesOptional,
      legacyPackagesRepo     = projectPackagesRepo,
      legacyPackagesNamed    = projectPackagesNamed,
      legacySharedConfig     = convertToLegacySharedConfig projectConfig,
      legacyLocalConfig      = convertToLegacyAllPackageConfig projectConfig
                            <> convertToLegacyPerPackageConfig
                                 projectConfigLocalPackages,
      legacySpecificConfig   = fmap convertToLegacyPerPackageConfig
                                    projectConfigSpecificPackage
    }

convertToLegacySharedConfig :: ProjectConfig -> LegacySharedConfig
convertToLegacySharedConfig
    ProjectConfig {
      projectConfigBuildOnly     = ProjectConfigBuildOnly {..},
      projectConfigShared        = ProjectConfigShared {..}
    } =

    LegacySharedConfig {
      legacyGlobalFlags      = globalFlags,
      legacyConfigureShFlags = configFlags,
      legacyConfigureExFlags = configExFlags,
      legacyInstallFlags     = installFlags
    }
  where
    globalFlags = GlobalFlags {
      globalVersion           = mempty,
      globalNumericVersion    = mempty,
      globalConfigFile        = projectConfigConfigFile,
      globalSandboxConfigFile = mempty,
      globalConstraintsFile   = mempty,
      globalRemoteRepos       = projectConfigRemoteRepos,
      globalCacheDir          = projectConfigCacheDir,
      globalLocalRepos        = projectConfigLocalRepos,
      globalLogsDir           = projectConfigLogsDir,
      globalWorldFile         = mempty,
      globalRequireSandbox    = mempty,
      globalIgnoreSandbox     = mempty,
      globalIgnoreExpiry      = projectConfigIgnoreExpiry,
      globalHttpTransport     = projectConfigHttpTransport,
      globalNix               = mempty,
      globalStoreDir          = projectConfigStoreDir
    }

    configFlags = mempty {
      configVerbosity     = projectConfigVerbosity,
      configDistPref      = projectConfigDistDir
    }

    configExFlags = ConfigExFlags {
      configCabalVersion  = projectConfigCabalVersion,
      configExConstraints = projectConfigConstraints,
      configPreferences   = projectConfigPreferences,
      configSolver        = projectConfigSolver,
      configAllowOlder    = projectConfigAllowOlder,
      configAllowNewer    = projectConfigAllowNewer

    }

    installFlags = InstallFlags {
      installDocumentation     = mempty,
      installHaddockIndex      = projectConfigHaddockIndex,
      installDryRun            = projectConfigDryRun,
      installReinstall         = mempty, --projectConfigReinstall,
      installAvoidReinstalls   = mempty, --projectConfigAvoidReinstalls,
      installOverrideReinstall = mempty, --projectConfigOverrideReinstall,
      installMaxBackjumps      = projectConfigMaxBackjumps,
      installUpgradeDeps       = mempty, --projectConfigUpgradeDeps,
      installReorderGoals      = projectConfigReorderGoals,
      installCountConflicts    = projectConfigCountConflicts,
      installIndependentGoals  = projectConfigIndependentGoals,
      installShadowPkgs        = mempty, --projectConfigShadowPkgs,
      installStrongFlags       = projectConfigStrongFlags,
      installAllowBootLibInstalls = projectConfigAllowBootLibInstalls,
      installOnly              = mempty,
      installOnlyDeps          = projectConfigOnlyDeps,
      installIndexState        = projectConfigIndexState,
      installRootCmd           = mempty, --no longer supported
      installSummaryFile       = projectConfigSummaryFile,
      installLogFile           = projectConfigLogFile,
      installBuildReports      = projectConfigBuildReports,
      installReportPlanningFailure = projectConfigReportPlanningFailure,
      installSymlinkBinDir     = projectConfigSymlinkBinDir,
      installPerComponent      = projectConfigPerComponent,
      installOneShot           = projectConfigOneShot,
      installNumJobs           = projectConfigNumJobs,
      installKeepGoing         = projectConfigKeepGoing,
      installRunTests          = mempty,
      installOfflineMode       = projectConfigOfflineMode,
      installProjectFileName   = projectConfigProjectFile
    }


convertToLegacyAllPackageConfig :: ProjectConfig -> LegacyPackageConfig
convertToLegacyAllPackageConfig
    ProjectConfig {
      projectConfigBuildOnly = ProjectConfigBuildOnly {..},
      projectConfigShared    = ProjectConfigShared {..}
    } =

    LegacyPackageConfig {
      legacyConfigureFlags = configFlags,
      legacyInstallPkgFlags= mempty,
      legacyHaddockFlags   = haddockFlags
    }
  where
    configFlags = ConfigFlags {
      configArgs                = mempty,
      configPrograms_           = mempty,
      configProgramPaths        = mempty,
      configProgramArgs         = mempty,
      configProgramPathExtra    = mempty,
      configHcFlavor            = projectConfigHcFlavor,
      configHcPath              = projectConfigHcPath,
      configHcPkg               = projectConfigHcPkg,
      configInstantiateWith     = mempty,
      configVanillaLib          = mempty,
      configProfLib             = mempty,
      configSharedLib           = mempty,
      configStaticLib           = mempty,
      configDynExe              = mempty,
      configProfExe             = mempty,
      configProf                = mempty,
      configProfDetail          = mempty,
      configProfLibDetail       = mempty,
      configConfigureArgs       = mempty,
      configOptimization        = mempty,
      configProgPrefix          = mempty,
      configProgSuffix          = mempty,
      configInstallDirs         = mempty,
      configScratchDir          = mempty,
      configDistPref            = mempty,
      configCabalFilePath       = mempty,
      configVerbosity           = mempty,
      configUserInstall         = mempty, --projectConfigUserInstall,
      configPackageDBs          = mempty, --projectConfigPackageDBs,
      configGHCiLib             = mempty,
      configSplitObjs           = mempty,
      configStripExes           = mempty,
      configStripLibs           = mempty,
      configExtraLibDirs        = mempty,
      configExtraFrameworkDirs  = mempty,
      configConstraints         = mempty,
      configDependencies        = mempty,
      configExtraIncludeDirs    = mempty,
      configDeterministic       = mempty,
      configIPID                = mempty,
      configCID                 = mempty,
      configConfigurationsFlags = mempty,
      configTests               = mempty,
      configCoverage            = mempty, --TODO: don't merge
      configLibCoverage         = mempty, --TODO: don't merge
      configExactConfiguration  = mempty,
      configBenchmarks          = mempty,
      configFlagError           = mempty,                --TODO: ???
      configRelocatable         = mempty,
      configDebugInfo           = mempty,
      configUseResponseFiles    = mempty
    }

    haddockFlags = mempty {
      haddockKeepTempFiles = projectConfigKeepTempFiles
    }


convertToLegacyPerPackageConfig :: PackageConfig -> LegacyPackageConfig
convertToLegacyPerPackageConfig PackageConfig {..} =
    LegacyPackageConfig {
      legacyConfigureFlags  = configFlags,
      legacyInstallPkgFlags = installFlags,
      legacyHaddockFlags    = haddockFlags
    }
  where
    configFlags = ConfigFlags {
      configArgs                = mempty,
      configPrograms_           = configPrograms_ mempty,
      configProgramPaths        = Map.toList (getMapLast packageConfigProgramPaths),
      configProgramArgs         = Map.toList (getMapMappend packageConfigProgramArgs),
      configProgramPathExtra    = packageConfigProgramPathExtra,
      configHcFlavor            = mempty,
      configHcPath              = mempty,
      configHcPkg               = mempty,
      configInstantiateWith     = mempty,
      configVanillaLib          = packageConfigVanillaLib,
      configProfLib             = packageConfigProfLib,
      configSharedLib           = packageConfigSharedLib,
      configStaticLib           = packageConfigStaticLib,
      configDynExe              = packageConfigDynExe,
      configProfExe             = packageConfigProfExe,
      configProf                = packageConfigProf,
      configProfDetail          = packageConfigProfDetail,
      configProfLibDetail       = packageConfigProfLibDetail,
      configConfigureArgs       = packageConfigConfigureArgs,
      configOptimization        = packageConfigOptimization,
      configProgPrefix          = packageConfigProgPrefix,
      configProgSuffix          = packageConfigProgSuffix,
      configInstallDirs         = mempty,
      configScratchDir          = mempty,
      configDistPref            = mempty,
      configCabalFilePath       = mempty,
      configVerbosity           = mempty,
      configUserInstall         = mempty,
      configPackageDBs          = mempty,
      configGHCiLib             = packageConfigGHCiLib,
      configSplitObjs           = packageConfigSplitObjs,
      configStripExes           = packageConfigStripExes,
      configStripLibs           = packageConfigStripLibs,
      configExtraLibDirs        = packageConfigExtraLibDirs,
      configExtraFrameworkDirs  = packageConfigExtraFrameworkDirs,
      configConstraints         = mempty,
      configDependencies        = mempty,
      configExtraIncludeDirs    = packageConfigExtraIncludeDirs,
      configIPID                = mempty,
      configCID                 = mempty,
      configDeterministic       = mempty,
      configConfigurationsFlags = packageConfigFlagAssignment,
      configTests               = packageConfigTests,
      configCoverage            = packageConfigCoverage, --TODO: don't merge
      configLibCoverage         = packageConfigCoverage, --TODO: don't merge
      configExactConfiguration  = mempty,
      configBenchmarks          = packageConfigBenchmarks,
      configFlagError           = mempty,                --TODO: ???
      configRelocatable         = packageConfigRelocatable,
      configDebugInfo           = packageConfigDebugInfo,
      configUseResponseFiles    = mempty
    }

    installFlags = mempty {
      installDocumentation      = packageConfigDocumentation,
      installRunTests           = packageConfigRunTests
    }

    haddockFlags = HaddockFlags {
      haddockProgramPaths  = mempty,
      haddockProgramArgs   = mempty,
      haddockHoogle        = packageConfigHaddockHoogle,
      haddockHtml          = packageConfigHaddockHtml,
      haddockHtmlLocation  = packageConfigHaddockHtmlLocation,
      haddockForHackage    = packageConfigHaddockForHackage,
      haddockForeignLibs   = packageConfigHaddockForeignLibs,
      haddockExecutables   = packageConfigHaddockExecutables,
      haddockTestSuites    = packageConfigHaddockTestSuites,
      haddockBenchmarks    = packageConfigHaddockBenchmarks,
      haddockInternal      = packageConfigHaddockInternal,
      haddockCss           = packageConfigHaddockCss,
      haddockHscolour      = packageConfigHaddockHscolour,
      haddockHscolourCss   = packageConfigHaddockHscolourCss,
      haddockContents      = packageConfigHaddockContents,
      haddockDistPref      = mempty,
      haddockKeepTempFiles = mempty,
      haddockVerbosity     = mempty
    }


------------------------------------------------
-- Parsing and showing the project config file
--

parseLegacyProjectConfig :: String -> ParseResult LegacyProjectConfig
parseLegacyProjectConfig =
    parseConfig legacyProjectConfigFieldDescrs
                legacyPackageConfigSectionDescrs
                mempty

showLegacyProjectConfig :: LegacyProjectConfig -> String
showLegacyProjectConfig config =
    Disp.render $
    showConfig  legacyProjectConfigFieldDescrs
                legacyPackageConfigSectionDescrs
                config
  $+$
    Disp.text ""


legacyProjectConfigFieldDescrs :: [FieldDescr LegacyProjectConfig]
legacyProjectConfigFieldDescrs =

    [ newLineListField "packages"
        (Disp.text . renderPackageLocationToken) parsePackageLocationTokenQ
        legacyPackages
        (\v flags -> flags { legacyPackages = v })
    , newLineListField "optional-packages"
        (Disp.text . renderPackageLocationToken) parsePackageLocationTokenQ
        legacyPackagesOptional
        (\v flags -> flags { legacyPackagesOptional = v })
    , commaNewLineListField "extra-packages"
        disp parse
        legacyPackagesNamed
        (\v flags -> flags { legacyPackagesNamed = v })
    ]

 ++ map (liftField
           legacySharedConfig
           (\flags conf -> conf { legacySharedConfig = flags }))
        legacySharedConfigFieldDescrs

 ++ map (liftField
           legacyLocalConfig
           (\flags conf -> conf { legacyLocalConfig = flags }))
        legacyPackageConfigFieldDescrs

-- | This is a bit tricky since it has to cover globs which have embedded @,@
-- chars. But we don't just want to parse strictly as a glob since we want to
-- allow http urls which don't parse as globs, and possibly some
-- system-dependent file paths. So we parse fairly liberally as a token, but
-- we allow @,@ inside matched @{}@ braces.
--
parsePackageLocationTokenQ :: ReadP r String
parsePackageLocationTokenQ = parseHaskellString
                   Parse.<++ parsePackageLocationToken
  where
    parsePackageLocationToken :: ReadP r String
    parsePackageLocationToken = fmap fst (Parse.gather outerTerm)
      where
        outerTerm   = alternateEither1 outerToken (braces innerTerm)
        innerTerm   = alternateEither  innerToken (braces innerTerm)
        outerToken  = Parse.munch1 outerChar >> return ()
        innerToken  = Parse.munch1 innerChar >> return ()
        outerChar c = not (isSpace c || c == '{' || c == '}' || c == ',')
        innerChar c = not (isSpace c || c == '{' || c == '}')
        braces      = Parse.between (Parse.char '{') (Parse.char '}')

    alternateEither, alternateEither1,
      alternatePQs, alternate1PQs, alternateQsP, alternate1QsP
      :: ReadP r () -> ReadP r () -> ReadP r ()

    alternateEither1 p q = alternate1PQs p q +++ alternate1QsP q p
    alternateEither  p q = alternateEither1 p q +++ return ()
    alternate1PQs    p q = p >> alternateQsP q p
    alternatePQs     p q = alternate1PQs p q +++ return ()
    alternate1QsP    q p = Parse.many1 q >> alternatePQs p q
    alternateQsP     q p = alternate1QsP q p +++ return ()

renderPackageLocationToken :: String -> String
renderPackageLocationToken s | needsQuoting = show s
                             | otherwise    = s
  where
    needsQuoting  = not (ok 0 s)
                 || s == "." -- . on its own on a line has special meaning
                 || take 2 s == "--" -- on its own line is comment syntax
                 --TODO: [code cleanup] these "." and "--" escaping issues
                 -- ought to be dealt with systematically in ParseUtils.
    ok :: Int -> String -> Bool
    ok n []       = n == 0
    ok _ ('"':_)  = False
    ok n ('{':cs) = ok (n+1) cs
    ok n ('}':cs) = ok (n-1) cs
    ok n (',':cs) = (n > 0) && ok n cs
    ok _ (c:_)
      | isSpace c = False
    ok n (_  :cs) = ok n cs


legacySharedConfigFieldDescrs :: [FieldDescr LegacySharedConfig]
legacySharedConfigFieldDescrs =

  ( liftFields
      legacyGlobalFlags
      (\flags conf -> conf { legacyGlobalFlags = flags })
  . addFields
      [ newLineListField "local-repo"
          showTokenQ parseTokenQ
          (fromNubList . globalLocalRepos)
          (\v conf -> conf { globalLocalRepos = toNubList v })
      ]
  . filterFields
      [ "remote-repo-cache"
      , "logs-dir", "store-dir", "ignore-expiry", "http-transport"
      ]
  . commandOptionsToFields
  ) (commandOptions (globalCommand []) ParseArgs)
 ++
  ( liftFields
      legacyConfigureShFlags
      (\flags conf -> conf { legacyConfigureShFlags = flags })
  . filterFields ["verbose", "builddir" ]
  . commandOptionsToFields
  ) (configureOptions ParseArgs)
 ++
  ( liftFields
      legacyConfigureExFlags
      (\flags conf -> conf { legacyConfigureExFlags = flags })
  . addFields
      [ commaNewLineListField "constraints"
        (disp . fst) (fmap (\constraint -> (constraint, constraintSrc)) parse)
        configExConstraints (\v conf -> conf { configExConstraints = v })

      , commaNewLineListField "preferences"
        disp parse
        configPreferences (\v conf -> conf { configPreferences = v })

      , monoidField "allow-older"
        (maybe mempty disp) (fmap Just parse)
        (fmap unAllowOlder . configAllowOlder)
        (\v conf -> conf { configAllowOlder = fmap AllowOlder v })

      , monoidField "allow-newer"
        (maybe mempty disp) (fmap Just parse)
        (fmap unAllowNewer . configAllowNewer)
        (\v conf -> conf { configAllowNewer = fmap AllowNewer v })
      ]
  . filterFields
      [ "cabal-lib-version", "solver"
        -- not "constraint" or "preference", we use our own plural ones above
      ]
  . commandOptionsToFields
  ) (configureExOptions ParseArgs constraintSrc)
 ++
  ( liftFields
      legacyInstallFlags
      (\flags conf -> conf { legacyInstallFlags = flags })
  . addFields
      [ newLineListField "build-summary"
          (showTokenQ . fromPathTemplate) (fmap toPathTemplate parseTokenQ)
          (fromNubList . installSummaryFile)
          (\v conf -> conf { installSummaryFile = toNubList v })
      ]
  . filterFields
      [ "doc-index-file"
      , "root-cmd", "symlink-bindir"
      , "build-log"
      , "remote-build-reporting", "report-planning-failure"
      , "one-shot", "jobs", "keep-going", "offline", "per-component"
        -- solver flags:
      , "max-backjumps", "reorder-goals", "count-conflicts", "independent-goals"
      , "strong-flags" , "allow-boot-library-installs", "index-state"
      ]
  . commandOptionsToFields
  ) (installOptions ParseArgs)
  where
    constraintSrc = ConstraintSourceProjectConfig "TODO"


legacyPackageConfigFieldDescrs :: [FieldDescr LegacyPackageConfig]
legacyPackageConfigFieldDescrs =
  ( liftFields
      legacyConfigureFlags
      (\flags conf -> conf { legacyConfigureFlags = flags })
  . addFields
      [ newLineListField "extra-include-dirs"
          showTokenQ parseTokenQ
          configExtraIncludeDirs
          (\v conf -> conf { configExtraIncludeDirs = v })
      , newLineListField "extra-lib-dirs"
          showTokenQ parseTokenQ
          configExtraLibDirs
          (\v conf -> conf { configExtraLibDirs = v })
      , newLineListField "extra-framework-dirs"
          showTokenQ parseTokenQ
          configExtraFrameworkDirs
          (\v conf -> conf { configExtraFrameworkDirs = v })
      , newLineListField "extra-prog-path"
          showTokenQ parseTokenQ
          (fromNubList . configProgramPathExtra)
          (\v conf -> conf { configProgramPathExtra = toNubList v })
      , newLineListField "configure-options"
          showTokenQ parseTokenQ
          configConfigureArgs
          (\v conf -> conf { configConfigureArgs = v })
      , simpleField "flags"
          dispFlagAssignment parseFlagAssignment
          configConfigurationsFlags
          (\v conf -> conf { configConfigurationsFlags = v })
      ]
  . filterFields
      [ "with-compiler", "with-hc-pkg"
      , "program-prefix", "program-suffix"
      , "library-vanilla", "library-profiling"
      , "shared", "static", "executable-dynamic"
      , "profiling", "executable-profiling"
      , "profiling-detail", "library-profiling-detail"
      , "library-for-ghci", "split-objs"
      , "executable-stripping", "library-stripping"
      , "tests", "benchmarks"
      , "coverage", "library-coverage"
      , "relocatable"
        -- not "extra-include-dirs", "extra-lib-dirs", "extra-framework-dirs"
        -- or "extra-prog-path". We use corrected ones above that parse
        -- as list fields.
      ]
  . commandOptionsToFields
  ) (configureOptions ParseArgs)
 ++
    liftFields
      legacyConfigureFlags
      (\flags conf -> conf { legacyConfigureFlags = flags })
    [ overrideFieldCompiler
    , overrideFieldOptimization
    , overrideFieldDebugInfo
    ]
 ++
  ( liftFields
      legacyInstallPkgFlags
      (\flags conf -> conf { legacyInstallPkgFlags = flags })
  . filterFields
      [ "documentation", "run-tests"
      ]
  . commandOptionsToFields
  ) (installOptions ParseArgs)
 ++
  ( liftFields
      legacyHaddockFlags
      (\flags conf -> conf { legacyHaddockFlags = flags })
  . mapFieldNames
      ("haddock-"++)
  . addFields
      [ simpleField "for-hackage"
          -- TODO: turn this into a library function
          (fromFlagOrDefault Disp.empty . fmap disp) (Parse.option mempty (fmap toFlag parse))
          haddockForHackage (\v conf -> conf { haddockForHackage = v })
      ]
  . filterFields
      [ "hoogle", "html", "html-location"
      , "foreign-libraries"
      , "executables", "tests", "benchmarks", "all", "internal", "css"
      , "hyperlink-source", "hscolour-css"
      , "contents-location", "keep-temp-files"
      ]
  . commandOptionsToFields
  ) (haddockOptions ParseArgs)

  where
    overrideFieldCompiler =
      simpleField "compiler"
        (fromFlagOrDefault Disp.empty . fmap disp)
        (Parse.option mempty (fmap toFlag parse))
        configHcFlavor (\v flags -> flags { configHcFlavor = v })


    -- TODO: [code cleanup] The following is a hack. The "optimization" and
    -- "debug-info" fields are OptArg, and viewAsFieldDescr fails on that.
    -- Instead of a hand-written parser and printer, we should handle this case
    -- properly in the library.

    overrideFieldOptimization =
      liftField configOptimization
                (\v flags -> flags { configOptimization = v }) $
      let name = "optimization" in
      FieldDescr name
        (\f -> case f of
                 Flag NoOptimisation      -> Disp.text "False"
                 Flag NormalOptimisation  -> Disp.text "True"
                 Flag MaximumOptimisation -> Disp.text "2"
                 _                        -> Disp.empty)
        (\line str _ -> case () of
         _ |  str == "False" -> ParseOk [] (Flag NoOptimisation)
           |  str == "True"  -> ParseOk [] (Flag NormalOptimisation)
           |  str == "0"     -> ParseOk [] (Flag NoOptimisation)
           |  str == "1"     -> ParseOk [] (Flag NormalOptimisation)
           |  str == "2"     -> ParseOk [] (Flag MaximumOptimisation)
           | lstr == "false" -> ParseOk [caseWarning] (Flag NoOptimisation)
           | lstr == "true"  -> ParseOk [caseWarning] (Flag NormalOptimisation)
           | otherwise       -> ParseFailed (NoParse name line)
           where
             lstr = lowercase str
             caseWarning = PWarning $
               "The '" ++ name ++ "' field is case sensitive, use 'True' or 'False'.")

    overrideFieldDebugInfo =
      liftField configDebugInfo (\v flags -> flags { configDebugInfo = v }) $
      let name = "debug-info" in
      FieldDescr name
        (\f -> case f of
                 Flag NoDebugInfo      -> Disp.text "False"
                 Flag MinimalDebugInfo -> Disp.text "1"
                 Flag NormalDebugInfo  -> Disp.text "True"
                 Flag MaximalDebugInfo -> Disp.text "3"
                 _                     -> Disp.empty)
        (\line str _ -> case () of
         _ |  str == "False" -> ParseOk [] (Flag NoDebugInfo)
           |  str == "True"  -> ParseOk [] (Flag NormalDebugInfo)
           |  str == "0"     -> ParseOk [] (Flag NoDebugInfo)
           |  str == "1"     -> ParseOk [] (Flag MinimalDebugInfo)
           |  str == "2"     -> ParseOk [] (Flag NormalDebugInfo)
           |  str == "3"     -> ParseOk [] (Flag MaximalDebugInfo)
           | lstr == "false" -> ParseOk [caseWarning] (Flag NoDebugInfo)
           | lstr == "true"  -> ParseOk [caseWarning] (Flag NormalDebugInfo)
           | otherwise       -> ParseFailed (NoParse name line)
           where
             lstr = lowercase str
             caseWarning = PWarning $
               "The '" ++ name ++ "' field is case sensitive, use 'True' or 'False'.")


legacyPackageConfigSectionDescrs :: [SectionDescr LegacyProjectConfig]
legacyPackageConfigSectionDescrs =
    [ packageRepoSectionDescr
    , packageSpecificOptionsSectionDescr
    , liftSection
        legacyLocalConfig
        (\flags conf -> conf { legacyLocalConfig = flags })
        programOptionsSectionDescr
    , liftSection
        legacyLocalConfig
        (\flags conf -> conf { legacyLocalConfig = flags })
        programLocationsSectionDescr
    , liftSection
        legacySharedConfig
        (\flags conf -> conf { legacySharedConfig = flags }) $
      liftSection
        legacyGlobalFlags
        (\flags conf -> conf { legacyGlobalFlags = flags })
        remoteRepoSectionDescr
    ]

packageRepoSectionDescr :: SectionDescr LegacyProjectConfig
packageRepoSectionDescr =
    SectionDescr {
      sectionName        = "source-repository-package",
      sectionFields      = sourceRepoFieldDescrs,
      sectionSubsections = [],
      sectionGet         = map (\x->("", x))
                         . legacyPackagesRepo,
      sectionSet         =
        \lineno unused pkgrepo projconf -> do
          unless (null unused) $
            syntaxError lineno "the section 'source-repository-package' takes no arguments"
          return projconf {
            legacyPackagesRepo = legacyPackagesRepo projconf ++ [pkgrepo]
          },
      sectionEmpty       = SourceRepo {
                             repoKind     = RepoThis, -- hopefully unused
                             repoType     = Nothing,
                             repoLocation = Nothing,
                             repoModule   = Nothing,
                             repoBranch   = Nothing,
                             repoTag      = Nothing,
                             repoSubdir   = Nothing
                           }
    }

packageSpecificOptionsSectionDescr :: SectionDescr LegacyProjectConfig
packageSpecificOptionsSectionDescr =
    SectionDescr {
      sectionName        = "package",
      sectionFields      = legacyPackageConfigFieldDescrs
                        ++ programOptionsFieldDescrs
                             (configProgramArgs . legacyConfigureFlags)
                             (\args pkgconf -> pkgconf {
                                 legacyConfigureFlags = (legacyConfigureFlags pkgconf) {
                                   configProgramArgs  = args
                                 }
                               }
                             )
                        ++ liftFields
                             legacyConfigureFlags
                             (\flags pkgconf -> pkgconf {
                                 legacyConfigureFlags = flags
                               }
                             )
                             programLocationsFieldDescrs,
      sectionSubsections = [],
      sectionGet         = \projconf ->
                             [ (display pkgname, pkgconf)
                             | (pkgname, pkgconf) <-
                                 Map.toList . getMapMappend
                               . legacySpecificConfig $ projconf ],
      sectionSet         =
        \lineno pkgnamestr pkgconf projconf -> do
          pkgname <- case simpleParse pkgnamestr of
            Just pkgname -> return pkgname
            Nothing      -> syntaxError lineno $
                                "a 'package' section requires a package name "
                             ++ "as an argument"
          return projconf {
            legacySpecificConfig =
              MapMappend $
              Map.insertWith mappend pkgname pkgconf
                             (getMapMappend $ legacySpecificConfig projconf)
          },
      sectionEmpty       = mempty
    }

programOptionsFieldDescrs :: (a -> [(String, [String])])
                          -> ([(String, [String])] -> a -> a)
                          -> [FieldDescr a]
programOptionsFieldDescrs get' set =
    commandOptionsToFields
  $ programDbOptions
      defaultProgramDb
      ParseArgs get' set

programOptionsSectionDescr :: SectionDescr LegacyPackageConfig
programOptionsSectionDescr =
    SectionDescr {
      sectionName        = "program-options",
      sectionFields      = programOptionsFieldDescrs
                             configProgramArgs
                             (\args conf -> conf { configProgramArgs = args }),
      sectionSubsections = [],
      sectionGet         = (\x->[("", x)])
                         . legacyConfigureFlags,
      sectionSet         =
        \lineno unused confflags pkgconf -> do
          unless (null unused) $
            syntaxError lineno "the section 'program-options' takes no arguments"
          return pkgconf {
            legacyConfigureFlags = legacyConfigureFlags pkgconf <> confflags
          },
      sectionEmpty       = mempty
    }

programLocationsFieldDescrs :: [FieldDescr ConfigFlags]
programLocationsFieldDescrs =
     commandOptionsToFields
   $ programDbPaths'
       (++ "-location")
       defaultProgramDb
       ParseArgs
       configProgramPaths
       (\paths conf -> conf { configProgramPaths = paths })

programLocationsSectionDescr :: SectionDescr LegacyPackageConfig
programLocationsSectionDescr =
    SectionDescr {
      sectionName        = "program-locations",
      sectionFields      = programLocationsFieldDescrs,
      sectionSubsections = [],
      sectionGet         = (\x->[("", x)])
                         . legacyConfigureFlags,
      sectionSet         =
        \lineno unused confflags pkgconf -> do
          unless (null unused) $
            syntaxError lineno "the section 'program-locations' takes no arguments"
          return pkgconf {
            legacyConfigureFlags = legacyConfigureFlags pkgconf <> confflags
          },
      sectionEmpty       = mempty
    }


-- | For each known program @PROG@ in 'progDb', produce a @PROG-options@
-- 'OptionField'.
programDbOptions
  :: ProgramDb
  -> ShowOrParseArgs
  -> (flags -> [(String, [String])])
  -> ([(String, [String])] -> (flags -> flags))
  -> [OptionField flags]
programDbOptions progDb showOrParseArgs get' set =
  case showOrParseArgs of
    -- we don't want a verbose help text list so we just show a generic one:
    ShowArgs  -> [programOptions  "PROG"]
    ParseArgs -> map (programOptions . programName . fst)
                 (knownPrograms progDb)
  where
    programOptions prog =
      option "" [prog ++ "-options"]
        ("give extra options to " ++ prog)
        get' set
        (reqArg' "OPTS" (\args -> [(prog, splitArgs args)])
           (\progArgs -> [ joinsArgs args
                         | (prog', args) <- progArgs, prog==prog' ]))


    joinsArgs = unwords . map escape
    escape arg | any isSpace arg = "\"" ++ arg ++ "\""
               | otherwise       = arg


remoteRepoSectionDescr :: SectionDescr GlobalFlags
remoteRepoSectionDescr =
    SectionDescr {
      sectionName        = "repository",
      sectionFields      = remoteRepoFields,
      sectionSubsections = [],
      sectionGet         = map (\x->(remoteRepoName x, x)) . fromNubList
                         . globalRemoteRepos,
      sectionSet         =
        \lineno reponame repo0 conf -> do
          when (null reponame) $
            syntaxError lineno $ "a 'repository' section requires the "
                              ++ "repository name as an argument"
          let repo = repo0 { remoteRepoName = reponame }
          when (remoteRepoKeyThreshold repo
                 > length (remoteRepoRootKeys repo)) $
            warning $ "'key-threshold' for repository "
                   ++ show (remoteRepoName repo)
                   ++ " higher than number of keys"
          when (not (null (remoteRepoRootKeys repo))
                && remoteRepoSecure repo /= Just True) $
            warning $ "'root-keys' for repository "
                   ++ show (remoteRepoName repo)
                   ++ " non-empty, but 'secure' not set to True."
          return conf {
            globalRemoteRepos = overNubList (++[repo]) (globalRemoteRepos conf)
          },
      sectionEmpty       = emptyRemoteRepo ""
    }


-------------------------------
-- Local field utils
--

--TODO: [code cleanup] all these utils should move to Distribution.ParseUtils
-- either augmenting or replacing the ones there

--TODO: [code cleanup] this is a different definition from listField, like
-- commaNewLineListField it pretty prints on multiple lines
newLineListField :: String -> (a -> Doc) -> ReadP [a] a
                 -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
newLineListField = listFieldWithSep Disp.sep

--TODO: [code cleanup] local copy purely so we can use the fixed version
-- of parseOptCommaList below
listFieldWithSep :: ([Doc] -> Doc) -> String -> (a -> Doc) -> ReadP [a] a
                 -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
listFieldWithSep separator name showF readF get' set =
  liftField get' set' $
    ParseUtils.field name showF' (parseOptCommaList readF)
  where
    set' xs b = set (get' b ++ xs) b
    showF'    = separator . map showF

-- | Parser combinator for simple fields which uses the field type's
-- 'Monoid' instance for combining multiple occurences of the field.
monoidField :: Monoid a => String -> (a -> Doc) -> ReadP a a
            -> (b -> a) -> (a -> b -> b) -> FieldDescr b
monoidField name showF readF get' set =
  liftField get' set' $ ParseUtils.field name showF readF
  where
    set' xs b = set (get' b `mappend` xs) b

--TODO: [code cleanup] local redefinition that should replace the version in
-- D.ParseUtils. This version avoid parse ambiguity for list element parsers
-- that have multiple valid parses of prefixes.
parseOptCommaList :: ReadP r a -> ReadP r [a]
parseOptCommaList p = Parse.sepBy p sep
  where
    -- The separator must not be empty or it introduces ambiguity
    sep = (Parse.skipSpaces >> Parse.char ',' >> Parse.skipSpaces)
      +++ (Parse.satisfy isSpace >> Parse.skipSpaces)

--TODO: [code cleanup] local redefinition that should replace the version in
-- D.ParseUtils called showFilePath. This version escapes "." and "--" which
-- otherwise are special syntax.
showTokenQ :: String -> Doc
showTokenQ ""            = Disp.empty
showTokenQ x@('-':'-':_) = Disp.text (show x)
showTokenQ x@('.':[])    = Disp.text (show x)
showTokenQ x             = showToken x

-- This is just a copy of parseTokenQ, using the fixed parseHaskellString
parseTokenQ :: ReadP r String
parseTokenQ = parseHaskellString
          <++ Parse.munch1 (\x -> not (isSpace x) && x /= ',')

--TODO: [code cleanup] use this to replace the parseHaskellString in
-- Distribution.ParseUtils. It turns out Read instance for String accepts
-- the ['a', 'b'] syntax, which we do not want. In particular it messes
-- up any token starting with [].
parseHaskellString :: ReadP r String
parseHaskellString =
  Parse.readS_to_P $
    Read.readPrec_to_S (do Read.String s <- Read.lexP; return s) 0

-- Handy util
addFields :: [FieldDescr a]
          -> ([FieldDescr a] -> [FieldDescr a])
addFields = (++)
