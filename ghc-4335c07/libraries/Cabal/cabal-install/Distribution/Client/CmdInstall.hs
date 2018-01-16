{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

-- | cabal-install CLI command: build
--
module Distribution.Client.CmdInstall (
    -- * The @build@ CLI and action
    installCommand,
    installAction,

    -- * Internals exposed for testing
    TargetProblem(..),
    selectPackageTargets,
    selectComponentTarget
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Client.ProjectOrchestration
import Distribution.Client.CmdErrorMessages

import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags
         , applyFlagDefaults )
import qualified Distribution.Client.Setup as Client
import Distribution.Client.Types
         ( PackageSpecifier(NamedPackage), UnresolvedSourcePackage )
import Distribution.Client.ProjectPlanning.Types
         ( pkgConfigCompiler )
import Distribution.Client.ProjectConfig.Types
         ( ProjectConfig, ProjectConfigBuildOnly(..)
         , projectConfigLogsDir, projectConfigStoreDir, projectConfigShared
         , projectConfigBuildOnly, projectConfigDistDir
         , projectConfigConfigFile )
import Distribution.Client.Config
         ( defaultCabalDir )
import Distribution.Client.ProjectConfig
         ( readGlobalConfig, resolveBuildTimeSettings )
import Distribution.Client.DistDirLayout
         ( defaultDistDirLayout, distDirectory, mkCabalDirLayout
         , ProjectRoot(ProjectRootImplicit), distProjectCacheDirectory
         , storePackageDirectory, cabalStoreDirLayout )
import Distribution.Client.RebuildMonad
         ( runRebuild )
import Distribution.Client.InstallSymlink
         ( symlinkBinary )
import Distribution.Simple.Setup
         ( Flag(Flag), HaddockFlags, fromFlagOrDefault, flagToMaybe )
import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Simple.Compiler
         ( compilerId )
import Distribution.Types.PackageName
         ( mkPackageName )
import Distribution.Types.UnitId
         ( UnitId )
import Distribution.Types.UnqualComponentName
         ( UnqualComponentName, unUnqualComponentName )
import Distribution.Verbosity
         ( Verbosity, normal )
import Distribution.Simple.Utils
         ( wrapText, die', withTempDirectory, createDirectoryIfMissingVerbose )

import qualified Data.Map as Map
import System.Directory ( getTemporaryDirectory, makeAbsolute )
import System.FilePath ( (</>) )

import qualified Distribution.Client.CmdBuild as CmdBuild

installCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
installCommand = CommandUI
  { commandName         = "new-install"
  , commandSynopsis     = "Install packages."
  , commandUsage        = usageAlternatives "new-install" [ "[TARGETS] [FLAGS]" ]
  , commandDescription  = Just $ \_ -> wrapText $
         "Installs one or more packages. This is done by installing them "
      ++ "in the store and symlinking the executables in the directory "
      ++ "specified by the --symlink-bindir flag (`~/.cabal/bin/` by default). "
      ++ "If you want the installed executables to be available globally, "
      ++ "make sure that the PATH environment variable contains that directory. "
      ++ "\n\n"
      ++ "If TARGET is a library, it will be added to the global environment. "
      ++ "When doing this, cabal will try to build a plan that includes all "
      ++ "the previously installed libraries. This is currently not implemented."
  , commandNotes        = Just $ \pname ->
         "Examples:\n"
      ++ "  " ++ pname ++ " new-install\n"
      ++ "    Install the package in the current directory\n"
      ++ "  " ++ pname ++ " new-install pkgname\n"
      ++ "    Install the package named pkgname (fetching it from hackage if necessary)\n"
      ++ "  " ++ pname ++ " new-install ./pkgfoo\n"
      ++ "    Install the package in the ./pkgfoo directory\n"

      ++ cmdCommonHelpTextNewBuildBeta
  , commandOptions = commandOptions CmdBuild.buildCommand
  , commandDefaultFlags = commandDefaultFlags CmdBuild.buildCommand
  }


-- | The @install@ command actually serves four different needs. It installs:
-- * Nonlocal exes:
--   For example a program from hackage. The behavior is similar to the old
--   install command, except that now conflicts between separate runs of the
--   command are impossible thanks to the store.
--   Exes are installed in the store like a normal dependency, then they are
--   symlinked uin the directory specified by --symlink-bindir.
--   To do this we need a dummy projectBaseContext containing the targets as
--   estra packages and using a temporary dist directory.
-- * Nonlocal libraries (TODO see #4558)
-- * Local exes         (TODO see #4558)
-- * Local libraries    (TODO see #4558)
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
installAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
            -> [String] -> GlobalFlags -> IO ()
installAction (applyFlagDefaults -> (configFlags, configExFlags, installFlags, haddockFlags))
            targetStrings globalFlags = do
  -- We never try to build tests/benchmarks for remote packages.
  -- So we set them as disabled by default and error if they are explicitly
  -- enabled.
  when (configTests configFlags' == Flag True) $
    die' verbosity $ "--enable-tests was specified, but tests can't "
                  ++ "be enabled in a remote package"
  when (configBenchmarks configFlags' == Flag True) $
    die' verbosity $ "--enable-benchmarks was specified, but benchmarks can't "
                  ++ "be enabled in a remote package"

  -- We need a place to put a temporary dist directory
  globalTmp <- getTemporaryDirectory
  withTempDirectory
    verbosity
    globalTmp
    "cabal-install."
    $ \tmpDir -> do

    let packageNames = mkPackageName <$> targetStrings
        packageSpecifiers =
          (\pname -> NamedPackage pname []) <$> packageNames

    baseCtx <- establishDummyProjectBaseContext
                 verbosity
                 cliConfig
                 tmpDir
                 packageSpecifiers

    let targetSelectors = TargetPackageName <$> packageNames

    buildCtx <-
      runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do

            -- Interpret the targets on the command line as build targets
            targets <- either (reportTargetProblems verbosity) return
                     $ resolveTargets
                         selectPackageTargets
                         selectComponentTarget
                         TargetProblemCommon
                         elaboratedPlan
                         targetSelectors

            let elaboratedPlan' = pruneInstallPlanToTargets
                                    TargetActionBuild
                                    targets
                                    elaboratedPlan
            elaboratedPlan'' <-
              if buildSettingOnlyDeps (buildSettings baseCtx)
                then either (reportCannotPruneDependencies verbosity) return $
                     pruneInstallPlanToDependencies (Map.keysSet targets)
                                                    elaboratedPlan'
                else return elaboratedPlan'

            return (elaboratedPlan'', targets)

    printPlan verbosity baseCtx buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx

    let compiler = pkgConfigCompiler $ elaboratedShared buildCtx
    let mkPkgBinDir = (</> "bin") .
                      storePackageDirectory
                         (cabalStoreDirLayout $ cabalDirLayout baseCtx)
                         (compilerId compiler)

    -- If there are exes, symlink them
    let defaultSymlinkBindir = error "TODO: how do I get the default ~/.cabal (or ~/.local) directory? (use --symlink-bindir explicitly for now)" </> "bin"
    symlinkBindir <- makeAbsolute $ fromFlagOrDefault defaultSymlinkBindir (Client.installSymlinkBinDir installFlags)
    traverse_ (symlinkBuiltPackage mkPkgBinDir symlinkBindir)
          $ Map.toList $ targetsMap buildCtx
    runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes
  where
    configFlags' = disableTestsBenchsByDefault configFlags
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags')
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags' configExFlags
                  installFlags haddockFlags


-- | Disables tests and benchmarks if they weren't explicitly enabled.
disableTestsBenchsByDefault :: ConfigFlags -> ConfigFlags
disableTestsBenchsByDefault configFlags =
  configFlags { configTests = Flag False <> configTests configFlags
              , configBenchmarks = Flag False <> configBenchmarks configFlags }

-- | Symlink every exe from a package from the store to a given location
symlinkBuiltPackage :: (UnitId -> FilePath) -- ^ A function to get an UnitId's
                                            -- store directory
                    -> FilePath -- ^ Where to put the symlink
                    -> ( UnitId
                        , [(ComponentTarget, [TargetSelector PackageId])] )
                     -> IO ()
symlinkBuiltPackage mkSourceBinDir destDir (pkg, components) =
  traverse_ (symlinkBuiltExe (mkSourceBinDir pkg) destDir) exes
  where
    exes = catMaybes $ (exeMaybe . fst) <$> components
    exeMaybe (ComponentTarget (CExeName exe) _) = Just exe
    exeMaybe _ = Nothing

-- | Symlink a specific exe.
symlinkBuiltExe :: FilePath -> FilePath -> UnqualComponentName -> IO Bool
symlinkBuiltExe sourceDir destDir exe =
  symlinkBinary
    destDir
    sourceDir
    exe
    $ unUnqualComponentName exe

-- | Create a dummy project context, without a .cabal or a .cabal.project file
-- (a place where to put a temporary dist directory is still needed)
establishDummyProjectBaseContext :: Verbosity
                                 -> ProjectConfig
                                 -> FilePath -- ^ Where to put the dist directory
                                 -> [PackageSpecifier UnresolvedSourcePackage] -- ^ The packages to be included in the project
                                 -> IO ProjectBaseContext
establishDummyProjectBaseContext verbosity cliConfig tmpDir localPackages = do

    cabalDir <- defaultCabalDir

    -- Create the dist directories
    createDirectoryIfMissingVerbose verbosity True $ distDirectory distDirLayout
    createDirectoryIfMissingVerbose verbosity True $ distProjectCacheDirectory distDirLayout

    globalConfig <- runRebuild ""
                  $ readGlobalConfig verbosity
                  $ projectConfigConfigFile
                  $ projectConfigShared cliConfig
    let projectConfig = globalConfig <> cliConfig

    let ProjectConfigBuildOnly {
          projectConfigLogsDir,
          projectConfigStoreDir
        } = projectConfigBuildOnly projectConfig

        mlogsDir = flagToMaybe projectConfigLogsDir
        mstoreDir = flagToMaybe projectConfigStoreDir
        cabalDirLayout = mkCabalDirLayout cabalDir mstoreDir mlogsDir

        buildSettings = resolveBuildTimeSettings
                          verbosity cabalDirLayout
                          projectConfig

    return ProjectBaseContext {
      distDirLayout,
      cabalDirLayout,
      projectConfig,
      localPackages,
      buildSettings
    }
  where
    mdistDirectory = flagToMaybe
                   $ projectConfigDistDir
                   $ projectConfigShared cliConfig
    projectRoot = ProjectRootImplicit tmpDir
    distDirLayout = defaultDistDirLayout projectRoot
                                         mdistDirectory

-- | This defines what a 'TargetSelector' means for the @bench@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @build@ command select all components except non-buildable and disabled
-- tests\/benchmarks, fail if there are no such components
--
selectPackageTargets :: TargetSelector PackageId
                     -> [AvailableTarget k] -> Either TargetProblem [k]
selectPackageTargets targetSelector targets

    -- If there are any buildable targets then we select those
  | not (null targetsBuildable)
  = Right targetsBuildable

    -- If there are targets but none are buildable then we report those
  | not (null targets)
  = Left (TargetProblemNoneEnabled targetSelector targets')

    -- If there are no targets at all then we report that
  | otherwise
  = Left (TargetProblemNoTargets targetSelector)
  where
    targets'         = forgetTargetsDetail targets
    targetsBuildable = selectBuildableTargetsWith
                         (buildable targetSelector)
                         targets

    -- When there's a target filter like "pkg:tests" then we do select tests,
    -- but if it's just a target like "pkg" then we don't build tests unless
    -- they are requested by default (i.e. by using --enable-tests)
    buildable (TargetPackage _ _  Nothing) TargetNotRequestedByDefault = False
    buildable (TargetAllPackages  Nothing) TargetNotRequestedByDefault = False
    buildable _ _ = True

-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @build@ command we just need the basic checks on being buildable etc.
--
selectComponentTarget :: PackageId -> ComponentName -> SubComponentTarget
                      -> AvailableTarget k -> Either TargetProblem k
selectComponentTarget pkgid cname subtarget =
    either (Left . TargetProblemCommon) Right
  . selectComponentTargetBasic pkgid cname subtarget


-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's for the @build@ command.
--
data TargetProblem =
     TargetProblemCommon       TargetProblemCommon

     -- | The 'TargetSelector' matches targets but none are buildable
   | TargetProblemNoneEnabled (TargetSelector PackageId) [AvailableTarget ()]

     -- | There are no targets at all
   | TargetProblemNoTargets   (TargetSelector PackageId)
  deriving (Eq, Show)

reportTargetProblems :: Verbosity -> [TargetProblem] -> IO a
reportTargetProblems verbosity =
    die' verbosity . unlines . map renderTargetProblem

renderTargetProblem :: TargetProblem -> String
renderTargetProblem (TargetProblemCommon problem) =
    renderTargetProblemCommon "build" problem
renderTargetProblem (TargetProblemNoneEnabled targetSelector targets) =
    renderTargetProblemNoneEnabled "build" targetSelector targets
renderTargetProblem(TargetProblemNoTargets targetSelector) =
    renderTargetProblemNoTargets "build" targetSelector

reportCannotPruneDependencies :: Verbosity -> CannotPruneDependencies -> IO a
reportCannotPruneDependencies verbosity =
    die' verbosity . renderCannotPruneDependencies

