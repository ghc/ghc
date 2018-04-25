{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

-- | Handling project configuration, types.
--
module Distribution.Client.ProjectConfig.Types (

    -- * Types for project config
    ProjectConfig(..),
    ProjectConfigBuildOnly(..),
    ProjectConfigShared(..),
    ProjectConfigProvenance(..),
    PackageConfig(..),

    -- * Resolving configuration
    SolverSettings(..),
    BuildTimeSettings(..),

    -- * Extra useful Monoids
    MapLast(..),
    MapMappend(..),
  ) where

import Distribution.Client.Types
         ( RemoteRepo, AllowNewer(..), AllowOlder(..) )
import Distribution.Client.Dependency.Types
         ( PreSolver )
import Distribution.Client.Targets
         ( UserConstraint )
import Distribution.Client.BuildReports.Types
         ( ReportLevel(..) )

import Distribution.Client.IndexUtils.Timestamp
         ( IndexState )

import Distribution.Solver.Types.Settings
import Distribution.Solver.Types.ConstraintSource

import Distribution.Package
         ( PackageName, PackageId, UnitId )
import Distribution.Types.Dependency
import Distribution.Version
         ( Version )
import Distribution.System
         ( Platform )
import Distribution.PackageDescription
         ( FlagAssignment, SourceRepo(..) )
import Distribution.Simple.Compiler
         ( Compiler, CompilerFlavor
         , OptimisationLevel(..), ProfDetailLevel, DebugInfoLevel(..) )
import Distribution.Simple.Setup
         ( Flag, HaddockTarget(..) )
import Distribution.Simple.InstallDirs
         ( PathTemplate )
import Distribution.Utils.NubList
         ( NubList )
import Distribution.Verbosity
         ( Verbosity )

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Distribution.Compat.Binary (Binary)
import Distribution.Compat.Semigroup
import GHC.Generics (Generic)
import Data.Typeable


-------------------------------
-- Project config types
--

-- | This type corresponds directly to what can be written in the
-- @cabal.project@ file. Other sources of configuration can also be injected
-- into this type, such as the user-wide @~/.cabal/config@ file and the
-- command line of @cabal configure@ or @cabal build@.
--
-- Since it corresponds to the external project file it is an instance of
-- 'Monoid' and all the fields can be empty. This also means there has to
-- be a step where we resolve configuration. At a minimum resolving means
-- applying defaults but it can also mean merging information from multiple
-- sources. For example for package-specific configuration the project file
-- can specify configuration that applies to all local packages, and then
-- additional configuration for a specific package.
--
-- Future directions: multiple profiles, conditionals. If we add these
-- features then the gap between configuration as written in the config file
-- and resolved settings we actually use will become even bigger.
--
data ProjectConfig
   = ProjectConfig {

       -- | Packages in this project, including local dirs, local .cabal files
       -- local and remote tarballs. When these are file globs, they must
       -- match at least one package.
       projectPackages              :: [String],

       -- | Like 'projectConfigPackageGlobs' but /optional/ in the sense that
       -- file globs are allowed to match nothing. The primary use case for
       -- this is to be able to say @optional-packages: */@ to automagically
       -- pick up deps that we unpack locally without erroring when
       -- there aren't any.
       projectPackagesOptional      :: [String],

       -- | Packages in this project from remote source repositories.
       projectPackagesRepo          :: [SourceRepo],

       -- | Packages in this project from hackage repositories.
       projectPackagesNamed         :: [Dependency],

       -- See respective types for an explanation of what these
       -- values are about:
       projectConfigBuildOnly       :: ProjectConfigBuildOnly,
       projectConfigShared          :: ProjectConfigShared,
       projectConfigProvenance      :: Set ProjectConfigProvenance,

       -- | Configuration to be applied to *local* packages; i.e.,
       -- any packages which are explicitly named in `cabal.project`.
       projectConfigLocalPackages   :: PackageConfig,
       projectConfigSpecificPackage :: MapMappend PackageName PackageConfig
     }
  deriving (Eq, Show, Generic, Typeable)

-- | That part of the project configuration that only affects /how/ we build
-- and not the /value/ of the things we build. This means this information
-- does not need to be tracked for changes since it does not affect the
-- outcome.
--
data ProjectConfigBuildOnly
   = ProjectConfigBuildOnly {
       projectConfigVerbosity             :: Flag Verbosity,
       projectConfigDryRun                :: Flag Bool,
       projectConfigOnlyDeps              :: Flag Bool,
       projectConfigSummaryFile           :: NubList PathTemplate,
       projectConfigLogFile               :: Flag PathTemplate,
       projectConfigBuildReports          :: Flag ReportLevel,
       projectConfigReportPlanningFailure :: Flag Bool,
       projectConfigSymlinkBinDir         :: Flag FilePath,
       projectConfigOneShot               :: Flag Bool,
       projectConfigNumJobs               :: Flag (Maybe Int),
       projectConfigKeepGoing             :: Flag Bool,
       projectConfigOfflineMode           :: Flag Bool,
       projectConfigKeepTempFiles         :: Flag Bool,
       projectConfigHttpTransport         :: Flag String,
       projectConfigIgnoreExpiry          :: Flag Bool,
       projectConfigCacheDir              :: Flag FilePath,
       projectConfigLogsDir               :: Flag FilePath,
       projectConfigStoreDir              :: Flag FilePath
     }
  deriving (Eq, Show, Generic)


-- | Project configuration that is shared between all packages in the project.
-- In particular this includes configuration that affects the solver.
--
data ProjectConfigShared
   = ProjectConfigShared {
       projectConfigDistDir           :: Flag FilePath,
       projectConfigConfigFile        :: Flag FilePath,
       projectConfigProjectFile       :: Flag FilePath,
       projectConfigHcFlavor          :: Flag CompilerFlavor,
       projectConfigHcPath            :: Flag FilePath,
       projectConfigHcPkg             :: Flag FilePath,
       projectConfigHaddockIndex      :: Flag PathTemplate,

       -- Things that only make sense for manual mode, not --local mode
       -- too much control!
     --projectConfigUserInstall       :: Flag Bool,
     --projectConfigInstallDirs       :: InstallDirs (Flag PathTemplate),
     --TODO: [required eventually] decide what to do with InstallDirs
     -- currently we don't allow it to be specified in the config file
     --projectConfigPackageDBs        :: [Maybe PackageDB],

       -- configuration used both by the solver and other phases
       projectConfigRemoteRepos       :: NubList RemoteRepo,     -- ^ Available Hackage servers.
       projectConfigLocalRepos        :: NubList FilePath,
       projectConfigIndexState        :: Flag IndexState,

       -- solver configuration
       projectConfigConstraints       :: [(UserConstraint, ConstraintSource)],
       projectConfigPreferences       :: [Dependency],
       projectConfigCabalVersion      :: Flag Version,  --TODO: [required eventually] unused
       projectConfigSolver            :: Flag PreSolver,
       projectConfigAllowOlder        :: Maybe AllowOlder,
       projectConfigAllowNewer        :: Maybe AllowNewer,
       projectConfigMaxBackjumps      :: Flag Int,
       projectConfigReorderGoals      :: Flag ReorderGoals,
       projectConfigCountConflicts    :: Flag CountConflicts,
       projectConfigStrongFlags       :: Flag StrongFlags,
       projectConfigAllowBootLibInstalls :: Flag AllowBootLibInstalls,
       projectConfigPerComponent      :: Flag Bool,
       projectConfigIndependentGoals  :: Flag IndependentGoals

       -- More things that only make sense for manual mode, not --local mode
       -- too much control!
     --projectConfigShadowPkgs        :: Flag Bool,
     --projectConfigReinstall         :: Flag Bool,
     --projectConfigAvoidReinstalls   :: Flag Bool,
     --projectConfigOverrideReinstall :: Flag Bool,
     --projectConfigUpgradeDeps       :: Flag Bool
     }
  deriving (Eq, Show, Generic)


-- | Specifies the provenance of project configuration, whether defaults were
-- used or if the configuration was read from an explicit file path.
data ProjectConfigProvenance

     -- | The configuration is implicit due to no explicit configuration
     -- being found. See 'Distribution.Client.ProjectConfig.readProjectConfig'
     -- for how implicit configuration is determined.
   = Implicit

     -- | The path the project configuration was explicitly read from.
     -- | The configuration was explicitly read from the specified 'FilePath'.
   | Explicit FilePath
  deriving (Eq, Ord, Show, Generic)


-- | Project configuration that is specific to each package, that is where we
-- can in principle have different values for different packages in the same
-- project.
--
data PackageConfig
   = PackageConfig {
       packageConfigProgramPaths        :: MapLast String FilePath,
       packageConfigProgramArgs         :: MapMappend String [String],
       packageConfigProgramPathExtra    :: NubList FilePath,
       packageConfigFlagAssignment      :: FlagAssignment,
       packageConfigVanillaLib          :: Flag Bool,
       packageConfigSharedLib           :: Flag Bool,
       packageConfigStaticLib           :: Flag Bool,
       packageConfigDynExe              :: Flag Bool,
       packageConfigProf                :: Flag Bool, --TODO: [code cleanup] sort out
       packageConfigProfLib             :: Flag Bool, --      this duplication
       packageConfigProfExe             :: Flag Bool, --      and consistency
       packageConfigProfDetail          :: Flag ProfDetailLevel,
       packageConfigProfLibDetail       :: Flag ProfDetailLevel,
       packageConfigConfigureArgs       :: [String],
       packageConfigOptimization        :: Flag OptimisationLevel,
       packageConfigProgPrefix          :: Flag PathTemplate,
       packageConfigProgSuffix          :: Flag PathTemplate,
       packageConfigExtraLibDirs        :: [FilePath],
       packageConfigExtraFrameworkDirs  :: [FilePath],
       packageConfigExtraIncludeDirs    :: [FilePath],
       packageConfigGHCiLib             :: Flag Bool,
       packageConfigSplitObjs           :: Flag Bool,
       packageConfigStripExes           :: Flag Bool,
       packageConfigStripLibs           :: Flag Bool,
       packageConfigTests               :: Flag Bool,
       packageConfigBenchmarks          :: Flag Bool,
       packageConfigCoverage            :: Flag Bool,
       packageConfigRelocatable         :: Flag Bool,
       packageConfigDebugInfo           :: Flag DebugInfoLevel,
       packageConfigRunTests            :: Flag Bool, --TODO: [required eventually] use this
       packageConfigDocumentation       :: Flag Bool, --TODO: [required eventually] use this
       packageConfigHaddockHoogle       :: Flag Bool, --TODO: [required eventually] use this
       packageConfigHaddockHtml         :: Flag Bool, --TODO: [required eventually] use this
       packageConfigHaddockHtmlLocation :: Flag String, --TODO: [required eventually] use this
       packageConfigHaddockForeignLibs  :: Flag Bool, --TODO: [required eventually] use this
       packageConfigHaddockExecutables  :: Flag Bool, --TODO: [required eventually] use this
       packageConfigHaddockTestSuites   :: Flag Bool, --TODO: [required eventually] use this
       packageConfigHaddockBenchmarks   :: Flag Bool, --TODO: [required eventually] use this
       packageConfigHaddockInternal     :: Flag Bool, --TODO: [required eventually] use this
       packageConfigHaddockCss          :: Flag FilePath, --TODO: [required eventually] use this
       packageConfigHaddockHscolour     :: Flag Bool, --TODO: [required eventually] use this
       packageConfigHaddockHscolourCss  :: Flag FilePath, --TODO: [required eventually] use this
       packageConfigHaddockContents     :: Flag PathTemplate, --TODO: [required eventually] use this
       packageConfigHaddockForHackage   :: Flag HaddockTarget
     }
  deriving (Eq, Show, Generic)

instance Binary ProjectConfig
instance Binary ProjectConfigBuildOnly
instance Binary ProjectConfigShared
instance Binary ProjectConfigProvenance
instance Binary PackageConfig


-- | Newtype wrapper for 'Map' that provides a 'Monoid' instance that takes
-- the last value rather than the first value for overlapping keys.
newtype MapLast k v = MapLast { getMapLast :: Map k v }
  deriving (Eq, Show, Functor, Generic, Binary, Typeable)

instance Ord k => Monoid (MapLast k v) where
  mempty  = MapLast Map.empty
  mappend = (<>)

instance Ord k => Semigroup (MapLast k v) where
  MapLast a <> MapLast b = MapLast $ Map.union b a
  -- rather than Map.union which is the normal Map monoid instance


-- | Newtype wrapper for 'Map' that provides a 'Monoid' instance that
-- 'mappend's values of overlapping keys rather than taking the first.
newtype MapMappend k v = MapMappend { getMapMappend :: Map k v }
  deriving (Eq, Show, Functor, Generic, Binary, Typeable)

instance (Semigroup v, Ord k) => Monoid (MapMappend k v) where
  mempty  = MapMappend Map.empty
  mappend = (<>)

instance (Semigroup v, Ord k) => Semigroup (MapMappend k v) where
  MapMappend a <> MapMappend b = MapMappend (Map.unionWith (<>) a b)
  -- rather than Map.union which is the normal Map monoid instance


instance Monoid ProjectConfig where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ProjectConfig where
  (<>) = gmappend


instance Monoid ProjectConfigBuildOnly where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ProjectConfigBuildOnly where
  (<>) = gmappend


instance Monoid ProjectConfigShared where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ProjectConfigShared where
  (<>) = gmappend


instance Monoid PackageConfig where
  mempty = gmempty
  mappend = (<>)

instance Semigroup PackageConfig where
  (<>) = gmappend

----------------------------------------
-- Resolving configuration to settings
--

-- | Resolved configuration for the solver. The idea is that this is easier to
-- use than the raw configuration because in the raw configuration everything
-- is optional (monoidial). In the 'BuildTimeSettings' every field is filled
-- in, if only with the defaults.
--
-- Use 'resolveSolverSettings' to make one from the project config (by
-- applying defaults etc).
--
data SolverSettings
   = SolverSettings {
       solverSettingRemoteRepos       :: [RemoteRepo],     -- ^ Available Hackage servers.
       solverSettingLocalRepos        :: [FilePath],
       solverSettingConstraints       :: [(UserConstraint, ConstraintSource)],
       solverSettingPreferences       :: [Dependency],
       solverSettingFlagAssignment    :: FlagAssignment, -- ^ For all local packages
       solverSettingFlagAssignments   :: Map PackageName FlagAssignment,
       solverSettingCabalVersion      :: Maybe Version,  --TODO: [required eventually] unused
       solverSettingSolver            :: PreSolver,
       solverSettingAllowOlder        :: AllowOlder,
       solverSettingAllowNewer        :: AllowNewer,
       solverSettingMaxBackjumps      :: Maybe Int,
       solverSettingReorderGoals      :: ReorderGoals,
       solverSettingCountConflicts    :: CountConflicts,
       solverSettingStrongFlags       :: StrongFlags,
       solverSettingAllowBootLibInstalls :: AllowBootLibInstalls,
       solverSettingIndexState        :: Maybe IndexState,
       solverSettingIndependentGoals  :: IndependentGoals
       -- Things that only make sense for manual mode, not --local mode
       -- too much control!
     --solverSettingShadowPkgs        :: Bool,
     --solverSettingReinstall         :: Bool,
     --solverSettingAvoidReinstalls   :: Bool,
     --solverSettingOverrideReinstall :: Bool,
     --solverSettingUpgradeDeps       :: Bool
     }
  deriving (Eq, Show, Generic, Typeable)

instance Binary SolverSettings


-- | Resolved configuration for things that affect how we build and not the
-- value of the things we build. The idea is that this is easier to use than
-- the raw configuration because in the raw configuration everything is
-- optional (monoidial). In the 'BuildTimeSettings' every field is filled in,
-- if only with the defaults.
--
-- Use 'resolveBuildTimeSettings' to make one from the project config (by
-- applying defaults etc).
--
data BuildTimeSettings
   = BuildTimeSettings {
       buildSettingDryRun                :: Bool,
       buildSettingOnlyDeps              :: Bool,
       buildSettingSummaryFile           :: [PathTemplate],
       buildSettingLogFile               :: Maybe (Compiler  -> Platform
                                                -> PackageId -> UnitId
                                                             -> FilePath),
       buildSettingLogVerbosity          :: Verbosity,
       buildSettingBuildReports          :: ReportLevel,
       buildSettingReportPlanningFailure :: Bool,
       buildSettingSymlinkBinDir         :: [FilePath],
       buildSettingOneShot               :: Bool,
       buildSettingNumJobs               :: Int,
       buildSettingKeepGoing             :: Bool,
       buildSettingOfflineMode           :: Bool,
       buildSettingKeepTempFiles         :: Bool,
       buildSettingRemoteRepos           :: [RemoteRepo],
       buildSettingLocalRepos            :: [FilePath],
       buildSettingCacheDir              :: FilePath,
       buildSettingHttpTransport         :: Maybe String,
       buildSettingIgnoreExpiry          :: Bool
     }

