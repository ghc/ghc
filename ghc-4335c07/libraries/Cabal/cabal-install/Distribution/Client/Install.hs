{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Install
-- Copyright   :  (c) 2005 David Himmelstrup
--                    2007 Bjorn Bringert
--                    2007-2010 Duncan Coutts
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- High level interface to package installation.
-----------------------------------------------------------------------------
module Distribution.Client.Install (
    -- * High-level interface
    install,

    -- * Lower-level interface that allows to manipulate the install plan
    makeInstallContext,
    makeInstallPlan,
    processInstallPlan,
    InstallArgs,
    InstallContext,

    -- * Prune certain packages from the install plan
    pruneInstallPlan
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import qualified Data.Map as Map
import qualified Data.Set as S
import Control.Exception as Exception
         ( Exception(toException), bracket, catches
         , Handler(Handler), handleJust, IOException, SomeException )
#ifndef mingw32_HOST_OS
import Control.Exception as Exception
         ( Exception(fromException) )
#endif
import System.Exit
         ( ExitCode(..) )
import Distribution.Compat.Exception
         ( catchIO, catchExit )
import Control.Monad
         ( forM_, mapM )
import System.Directory
         ( getTemporaryDirectory, doesDirectoryExist, doesFileExist,
           createDirectoryIfMissing, removeFile, renameDirectory,
           getDirectoryContents )
import System.FilePath
         ( (</>), (<.>), equalFilePath, takeDirectory )
import System.IO
         ( openFile, IOMode(AppendMode), hClose )
import System.IO.Error
         ( isDoesNotExistError, ioeGetFileName )

import Distribution.Client.Targets
import Distribution.Client.Configure
         ( chooseCabalVersion, configureSetupScript, checkConfigExFlags )
import Distribution.Client.Dependency
import Distribution.Client.Dependency.Types
         ( Solver(..) )
import Distribution.Client.FetchUtils
import Distribution.Client.HttpUtils
         ( HttpTransport (..) )
import Distribution.Solver.Types.PackageFixedDeps
import qualified Distribution.Client.Haddock as Haddock (regenerateHaddockIndex)
import Distribution.Client.IndexUtils as IndexUtils
         ( getSourcePackagesAtIndexState, getInstalledPackages )
import qualified Distribution.Client.InstallPlan as InstallPlan
import qualified Distribution.Client.SolverInstallPlan as SolverInstallPlan
import Distribution.Client.InstallPlan (InstallPlan)
import Distribution.Client.SolverInstallPlan (SolverInstallPlan)
import Distribution.Client.Setup
         ( GlobalFlags(..), RepoContext(..)
         , ConfigFlags(..), configureCommand, filterConfigureFlags
         , ConfigExFlags(..), InstallFlags(..) )
import Distribution.Client.Config
         ( defaultCabalDir, defaultUserInstall )
import Distribution.Client.Sandbox.Timestamp
         ( withUpdateTimestamps )
import Distribution.Client.Sandbox.Types
         ( SandboxPackageInfo(..), UseSandbox(..), isUseSandbox
         , whenUsingSandbox )
import Distribution.Client.Tar (extractTarGzFile)
import Distribution.Client.Types as Source
import Distribution.Client.BuildReports.Types
         ( ReportLevel(..) )
import Distribution.Client.SetupWrapper
         ( setupWrapper, SetupScriptOptions(..), defaultSetupScriptOptions )
import qualified Distribution.Client.BuildReports.Anonymous as BuildReports
import qualified Distribution.Client.BuildReports.Storage as BuildReports
         ( storeAnonymous, storeLocal, fromInstallPlan, fromPlanningFailure )
import qualified Distribution.Client.InstallSymlink as InstallSymlink
         ( symlinkBinaries )
import qualified Distribution.Client.Win32SelfUpgrade as Win32SelfUpgrade
import qualified Distribution.Client.World as World
import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.Client.JobControl

import qualified Distribution.Solver.Types.ComponentDeps as CD
import           Distribution.Solver.Types.ConstraintSource
import           Distribution.Solver.Types.Settings
import           Distribution.Solver.Types.LabeledPackageConstraint
import           Distribution.Solver.Types.OptionalStanza
import qualified Distribution.Solver.Types.PackageIndex as SourcePackageIndex
import           Distribution.Solver.Types.PkgConfigDb
                   ( PkgConfigDb, readPkgConfigDb )
import           Distribution.Solver.Types.SourcePackage as SourcePackage

import Distribution.Utils.NubList
import Distribution.Simple.Compiler
         ( CompilerId(..), Compiler(compilerId), compilerFlavor
         , CompilerInfo(..), compilerInfo, PackageDB(..), PackageDBStack )
import Distribution.Simple.Program (ProgramDb)
import qualified Distribution.Simple.InstallDirs as InstallDirs
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.Setup
         ( haddockCommand, HaddockFlags(..)
         , buildCommand, BuildFlags(..), emptyBuildFlags
         , toFlag, fromFlag, fromFlagOrDefault, flagToMaybe, defaultDistPref )
import qualified Distribution.Simple.Setup as Cabal
         ( Flag(..)
         , copyCommand, CopyFlags(..), emptyCopyFlags
         , registerCommand, RegisterFlags(..), emptyRegisterFlags
         , testCommand, TestFlags(..), emptyTestFlags )
import Distribution.Simple.Utils
         ( createDirectoryIfMissingVerbose, comparing
         , writeFileAtomic, withUTF8FileContents )
import Distribution.Simple.InstallDirs as InstallDirs
         ( PathTemplate, fromPathTemplate, toPathTemplate, substPathTemplate
         , initialPathTemplateEnv, installDirsTemplateEnv )
import Distribution.Simple.Configure (interpretPackageDbFlags)
import Distribution.Simple.Register (registerPackage, defaultRegisterOptions)
import Distribution.Package
         ( PackageIdentifier(..), PackageId, packageName, packageVersion
         , Package(..), HasMungedPackageId(..), HasUnitId(..)
         , UnitId )
import Distribution.Types.Dependency
         ( Dependency(..), thisPackageVersion )
import Distribution.Types.MungedPackageId
import qualified Distribution.PackageDescription as PackageDescription
import Distribution.PackageDescription
         ( PackageDescription, GenericPackageDescription(..), Flag(..)
         , FlagAssignment, mkFlagAssignment, unFlagAssignment
         , showFlagValue, diffFlagAssignment, nullFlagAssignment )
import Distribution.PackageDescription.Configuration
         ( finalizePD )
import Distribution.ParseUtils
         ( showPWarning )
import Distribution.Version
         ( Version, VersionRange, foldVersionRange )
import Distribution.Simple.Utils as Utils
         ( notice, info, warn, debug, debugNoWrap, die'
         , withTempDirectory )
import Distribution.Client.Utils
         ( determineNumJobs, logDirChange, mergeBy, MergeResult(..)
         , tryCanonicalizePath )
import Distribution.System
         ( Platform, OS(Windows), buildOS )
import Distribution.Text
         ( display )
import Distribution.Verbosity as Verbosity
         ( Verbosity, modifyVerbosity, normal, verbose )
import Distribution.Simple.BuildPaths ( exeExtension )

--TODO:
-- * assign flags to packages individually
--   * complain about flags that do not apply to any package given as target
--     so flags do not apply to dependencies, only listed, can use flag
--     constraints for dependencies
--   * only record applicable flags in world file
-- * allow flag constraints
-- * allow installed constraints
-- * allow flag and installed preferences
-- * change world file to use cabal section syntax
--   * allow persistent configure flags for each package individually

-- ------------------------------------------------------------
-- * Top level user actions
-- ------------------------------------------------------------

-- | Installs the packages needed to satisfy a list of dependencies.
--
install
  :: Verbosity
  -> PackageDBStack
  -> RepoContext
  -> Compiler
  -> Platform
  -> ProgramDb
  -> UseSandbox
  -> Maybe SandboxPackageInfo
  -> GlobalFlags
  -> ConfigFlags
  -> ConfigExFlags
  -> InstallFlags
  -> HaddockFlags
  -> [UserTarget]
  -> IO ()
install verbosity packageDBs repos comp platform progdb useSandbox mSandboxPkgInfo
  globalFlags configFlags configExFlags installFlags haddockFlags
  userTargets0 = do

    unless (installRootCmd installFlags == Cabal.NoFlag) $
        warn verbosity $ "--root-cmd is no longer supported, "
        ++ "see https://github.com/haskell/cabal/issues/3353"
        ++ " (if you didn't type --root-cmd, comment out root-cmd"
        ++ " in your ~/.cabal/config file)"
    let userOrSandbox = fromFlag (configUserInstall configFlags)
                     || isUseSandbox useSandbox
    unless userOrSandbox $
        warn verbosity $ "the --global flag is deprecated -- "
        ++ "it is generally considered a bad idea to install packages "
        ++ "into the global store"

    installContext <- makeInstallContext verbosity args (Just userTargets0)
    planResult     <- foldProgress logMsg (return . Left) (return . Right) =<<
                      makeInstallPlan verbosity args installContext

    case planResult of
        Left message -> do
            reportPlanningFailure verbosity args installContext message
            die'' message
        Right installPlan ->
            processInstallPlan verbosity args installContext installPlan
  where
    args :: InstallArgs
    args = (packageDBs, repos, comp, platform, progdb, useSandbox,
            mSandboxPkgInfo, globalFlags, configFlags, configExFlags,
            installFlags, haddockFlags)

    die'' message = die' verbosity (message ++ if isUseSandbox useSandbox
                                   then installFailedInSandbox else [])
    -- TODO: use a better error message, remove duplication.
    installFailedInSandbox =
      "\nNote: when using a sandbox, all packages are required to have "
      ++ "consistent dependencies. "
      ++ "Try reinstalling/unregistering the offending packages or "
      ++ "recreating the sandbox."
    logMsg message rest = debugNoWrap verbosity message >> rest

-- TODO: Make InstallContext a proper data type with documented fields.
-- | Common context for makeInstallPlan and processInstallPlan.
type InstallContext = ( InstalledPackageIndex, SourcePackageDb
                      , PkgConfigDb
                      , [UserTarget], [PackageSpecifier UnresolvedSourcePackage]
                      , HttpTransport )

-- TODO: Make InstallArgs a proper data type with documented fields or just get
-- rid of it completely.
-- | Initial arguments given to 'install' or 'makeInstallContext'.
type InstallArgs = ( PackageDBStack
                   , RepoContext
                   , Compiler
                   , Platform
                   , ProgramDb
                   , UseSandbox
                   , Maybe SandboxPackageInfo
                   , GlobalFlags
                   , ConfigFlags
                   , ConfigExFlags
                   , InstallFlags
                   , HaddockFlags )

-- | Make an install context given install arguments.
makeInstallContext :: Verbosity -> InstallArgs -> Maybe [UserTarget]
                      -> IO InstallContext
makeInstallContext verbosity
  (packageDBs, repoCtxt, comp, _, progdb,_,_,
   globalFlags, _, configExFlags, installFlags, _) mUserTargets = do

    let idxState = flagToMaybe (installIndexState installFlags)

    installedPkgIndex <- getInstalledPackages verbosity comp packageDBs progdb
    sourcePkgDb       <- getSourcePackagesAtIndexState verbosity repoCtxt idxState
    pkgConfigDb       <- readPkgConfigDb      verbosity progdb

    checkConfigExFlags verbosity installedPkgIndex
                       (packageIndex sourcePkgDb) configExFlags
    transport <- repoContextGetTransport repoCtxt

    (userTargets, pkgSpecifiers) <- case mUserTargets of
      Nothing           ->
        -- We want to distinguish between the case where the user has given an
        -- empty list of targets on the command-line and the case where we
        -- specifically want to have an empty list of targets.
        return ([], [])
      Just userTargets0 -> do
        -- For install, if no target is given it means we use the current
        -- directory as the single target.
        let userTargets | null userTargets0 = [UserTargetLocalDir "."]
                        | otherwise         = userTargets0

        pkgSpecifiers <- resolveUserTargets verbosity repoCtxt
                         (fromFlag $ globalWorldFile globalFlags)
                         (packageIndex sourcePkgDb)
                         userTargets
        return (userTargets, pkgSpecifiers)

    return (installedPkgIndex, sourcePkgDb, pkgConfigDb, userTargets
           ,pkgSpecifiers, transport)

-- | Make an install plan given install context and install arguments.
makeInstallPlan :: Verbosity -> InstallArgs -> InstallContext
                -> IO (Progress String String SolverInstallPlan)
makeInstallPlan verbosity
  (_, _, comp, platform, _, _, mSandboxPkgInfo,
   _, configFlags, configExFlags, installFlags,
   _)
  (installedPkgIndex, sourcePkgDb, pkgConfigDb,
   _, pkgSpecifiers, _) = do

    solver <- chooseSolver verbosity (fromFlag (configSolver configExFlags))
              (compilerInfo comp)
    notice verbosity "Resolving dependencies..."
    return $ planPackages verbosity comp platform mSandboxPkgInfo solver
          configFlags configExFlags installFlags
          installedPkgIndex sourcePkgDb pkgConfigDb pkgSpecifiers

-- | Given an install plan, perform the actual installations.
processInstallPlan :: Verbosity -> InstallArgs -> InstallContext
                   -> SolverInstallPlan
                   -> IO ()
processInstallPlan verbosity
  args@(_,_, _, _, _, _, _, _, configFlags, _, installFlags, _)
  (installedPkgIndex, sourcePkgDb, _,
   userTargets, pkgSpecifiers, _) installPlan0 = do

    checkPrintPlan verbosity installedPkgIndex installPlan sourcePkgDb
      installFlags pkgSpecifiers

    unless (dryRun || nothingToInstall) $ do
      buildOutcomes <- performInstallations verbosity
                         args installedPkgIndex installPlan
      postInstallActions verbosity args userTargets installPlan buildOutcomes
  where
    installPlan = InstallPlan.configureInstallPlan configFlags installPlan0
    dryRun = fromFlag (installDryRun installFlags)
    nothingToInstall = null (fst (InstallPlan.ready installPlan))

-- ------------------------------------------------------------
-- * Installation planning
-- ------------------------------------------------------------

planPackages :: Verbosity
             -> Compiler
             -> Platform
             -> Maybe SandboxPackageInfo
             -> Solver
             -> ConfigFlags
             -> ConfigExFlags
             -> InstallFlags
             -> InstalledPackageIndex
             -> SourcePackageDb
             -> PkgConfigDb
             -> [PackageSpecifier UnresolvedSourcePackage]
             -> Progress String String SolverInstallPlan
planPackages verbosity comp platform mSandboxPkgInfo solver
             configFlags configExFlags installFlags
             installedPkgIndex sourcePkgDb pkgConfigDb pkgSpecifiers =

        resolveDependencies
          platform (compilerInfo comp) pkgConfigDb
          solver
          resolverParams

    >>= if onlyDeps then pruneInstallPlan pkgSpecifiers else return

  where
    resolverParams =

        setMaxBackjumps (if maxBackjumps < 0 then Nothing
                                             else Just maxBackjumps)

      . setIndependentGoals independentGoals

      . setReorderGoals reorderGoals

      . setCountConflicts countConflicts

      . setAvoidReinstalls avoidReinstalls

      . setShadowPkgs shadowPkgs

      . setStrongFlags strongFlags

      . setAllowBootLibInstalls allowBootLibInstalls

      . setSolverVerbosity verbosity

      . setPreferenceDefault (if upgradeDeps then PreferAllLatest
                                             else PreferLatestForSelected)

      . removeLowerBounds allowOlder
      . removeUpperBounds allowNewer

      . addPreferences
          -- preferences from the config file or command line
          [ PackageVersionPreference name ver
          | Dependency name ver <- configPreferences configExFlags ]

      . addConstraints
          -- version constraints from the config file or command line
            [ LabeledPackageConstraint (userToPackageConstraint pc) src
            | (pc, src) <- configExConstraints configExFlags ]

      . addConstraints
          --FIXME: this just applies all flags to all targets which
          -- is silly. We should check if the flags are appropriate
          [ let pc = PackageConstraint
                     (scopeToplevel $ pkgSpecifierTarget pkgSpecifier)
                     (PackagePropertyFlags flags)
            in LabeledPackageConstraint pc ConstraintSourceConfigFlagOrTarget
          | let flags = configConfigurationsFlags configFlags
          , not (nullFlagAssignment flags)
          , pkgSpecifier <- pkgSpecifiers ]

      . addConstraints
          [ let pc = PackageConstraint
                     (scopeToplevel $ pkgSpecifierTarget pkgSpecifier)
                     (PackagePropertyStanzas stanzas)
            in LabeledPackageConstraint pc ConstraintSourceConfigFlagOrTarget
          | pkgSpecifier <- pkgSpecifiers ]

      . maybe id applySandboxInstallPolicy mSandboxPkgInfo

      . (if reinstall then reinstallTargets else id)

        -- Don't solve for executables, the legacy install codepath
        -- doesn't understand how to install them
      . setSolveExecutables (SolveExecutables False)

      $ standardInstallPolicy
        installedPkgIndex sourcePkgDb pkgSpecifiers

    stanzas           = [ TestStanzas | testsEnabled ]
                     ++ [ BenchStanzas | benchmarksEnabled ]
    testsEnabled      = fromFlagOrDefault False $ configTests configFlags
    benchmarksEnabled = fromFlagOrDefault False $ configBenchmarks configFlags

    reinstall        = fromFlag (installOverrideReinstall installFlags) ||
                       fromFlag (installReinstall         installFlags)
    reorderGoals     = fromFlag (installReorderGoals      installFlags)
    countConflicts   = fromFlag (installCountConflicts    installFlags)
    independentGoals = fromFlag (installIndependentGoals  installFlags)
    avoidReinstalls  = fromFlag (installAvoidReinstalls   installFlags)
    shadowPkgs       = fromFlag (installShadowPkgs        installFlags)
    strongFlags      = fromFlag (installStrongFlags       installFlags)
    maxBackjumps     = fromFlag (installMaxBackjumps      installFlags)
    allowBootLibInstalls = fromFlag (installAllowBootLibInstalls installFlags)
    upgradeDeps      = fromFlag (installUpgradeDeps       installFlags)
    onlyDeps         = fromFlag (installOnlyDeps          installFlags)

    allowOlder       = fromMaybe (AllowOlder mempty)
                                 (configAllowOlder configExFlags)
    allowNewer       = fromMaybe (AllowNewer mempty)
                                 (configAllowNewer configExFlags)

-- | Remove the provided targets from the install plan.
pruneInstallPlan :: Package targetpkg
                 => [PackageSpecifier targetpkg]
                 -> SolverInstallPlan
                 -> Progress String String SolverInstallPlan
pruneInstallPlan pkgSpecifiers =
  -- TODO: this is a general feature and should be moved to D.C.Dependency
  -- Also, the InstallPlan.remove should return info more precise to the
  -- problem, rather than the very general PlanProblem type.
  either (Fail . explain) Done
  . SolverInstallPlan.remove (\pkg -> packageName pkg `elem` targetnames)
  where
    explain :: [SolverInstallPlan.SolverPlanProblem] -> String
    explain problems =
      "Cannot select only the dependencies (as requested by the "
      ++ "'--only-dependencies' flag), "
      ++ (case pkgids of
             [pkgid] -> "the package " ++ display pkgid ++ " is "
             _       -> "the packages "
                        ++ intercalate ", " (map display pkgids) ++ " are ")
      ++ "required by a dependency of one of the other targets."
      where
        pkgids =
          nub [ depid
              | SolverInstallPlan.PackageMissingDeps _ depids <- problems
              , depid <- depids
              , packageName depid `elem` targetnames ]

    targetnames  = map pkgSpecifierTarget pkgSpecifiers

-- ------------------------------------------------------------
-- * Informational messages
-- ------------------------------------------------------------

-- | Perform post-solver checks of the install plan and print it if
-- either requested or needed.
checkPrintPlan :: Verbosity
               -> InstalledPackageIndex
               -> InstallPlan
               -> SourcePackageDb
               -> InstallFlags
               -> [PackageSpecifier UnresolvedSourcePackage]
               -> IO ()
checkPrintPlan verbosity installed installPlan sourcePkgDb
  installFlags pkgSpecifiers = do

  -- User targets that are already installed.
  let preExistingTargets =
        [ p | let tgts = map pkgSpecifierTarget pkgSpecifiers,
              InstallPlan.PreExisting p <- InstallPlan.toList installPlan,
              packageName p `elem` tgts ]

  -- If there's nothing to install, we print the already existing
  -- target packages as an explanation.
  when nothingToInstall $
    notice verbosity $ unlines $
         "All the requested packages are already installed:"
       : map (display . packageId) preExistingTargets
      ++ ["Use --reinstall if you want to reinstall anyway."]

  let lPlan =
        [ (pkg, status)
        | pkg <- InstallPlan.executionOrder installPlan
        , let status = packageStatus installed pkg ]
  -- Are any packages classified as reinstalls?
  let reinstalledPkgs =
        [ ipkg
        | (_pkg, status) <- lPlan
        , ipkg <- extractReinstalls status ]
  -- Packages that are already broken.
  let oldBrokenPkgs =
          map Installed.installedUnitId
        . PackageIndex.reverseDependencyClosure installed
        . map (Installed.installedUnitId . fst)
        . PackageIndex.brokenPackages
        $ installed
  let excluded = reinstalledPkgs ++ oldBrokenPkgs
  -- Packages that are reverse dependencies of replaced packages are very
  -- likely to be broken. We exclude packages that are already broken.
  let newBrokenPkgs =
        filter (\ p -> not (Installed.installedUnitId p `elem` excluded))
               (PackageIndex.reverseDependencyClosure installed reinstalledPkgs)
  let containsReinstalls = not (null reinstalledPkgs)
  let breaksPkgs         = not (null newBrokenPkgs)

  let adaptedVerbosity
        | containsReinstalls
        , not overrideReinstall  = modifyVerbosity (max verbose) verbosity
        | otherwise              = verbosity

  -- We print the install plan if we are in a dry-run or if we are confronted
  -- with a dangerous install plan.
  when (dryRun || containsReinstalls && not overrideReinstall) $
    printPlan (dryRun || breaksPkgs && not overrideReinstall)
      adaptedVerbosity lPlan sourcePkgDb

  -- If the install plan is dangerous, we print various warning messages. In
  -- particular, if we can see that packages are likely to be broken, we even
  -- bail out (unless installation has been forced with --force-reinstalls).
  when containsReinstalls $ do
    if breaksPkgs
      then do
        (if dryRun || overrideReinstall then warn else die') verbosity $ unlines $
            "The following packages are likely to be broken by the reinstalls:"
          : map (display . mungedId) newBrokenPkgs
          ++ if overrideReinstall
               then if dryRun then [] else
                 ["Continuing even though " ++
                  "the plan contains dangerous reinstalls."]
               else
                 ["Use --force-reinstalls if you want to install anyway."]
      else unless dryRun $ warn verbosity
             "Note that reinstalls are always dangerous. Continuing anyway..."

  -- If we are explicitly told to not download anything, check that all packages
  -- are already fetched.
  let offline = fromFlagOrDefault False (installOfflineMode installFlags)
  when offline $ do
    let pkgs = [ confPkgSource cpkg
               | InstallPlan.Configured cpkg <- InstallPlan.toList installPlan ]
    notFetched <- fmap (map packageInfoId)
                  . filterM (fmap isNothing . checkFetched . packageSource)
                  $ pkgs
    unless (null notFetched) $
      die' verbosity $ "Can't download packages in offline mode. "
      ++ "Must download the following packages to proceed:\n"
      ++ intercalate ", " (map display notFetched)
      ++ "\nTry using 'cabal fetch'."

  where
    nothingToInstall = null (fst (InstallPlan.ready installPlan))

    dryRun            = fromFlag (installDryRun            installFlags)
    overrideReinstall = fromFlag (installOverrideReinstall installFlags)

data PackageStatus = NewPackage
                   | NewVersion [Version]
                   | Reinstall  [UnitId] [PackageChange]

type PackageChange = MergeResult MungedPackageId MungedPackageId

extractReinstalls :: PackageStatus -> [UnitId]
extractReinstalls (Reinstall ipids _) = ipids
extractReinstalls _                   = []

packageStatus :: InstalledPackageIndex
              -> ReadyPackage
              -> PackageStatus
packageStatus installedPkgIndex cpkg =
  case PackageIndex.lookupPackageName installedPkgIndex
                                      (packageName cpkg) of
    [] -> NewPackage
    ps ->  case filter ((== mungedId cpkg)
                        . mungedId) (concatMap snd ps) of
      []           -> NewVersion (map fst ps)
      pkgs@(pkg:_) -> Reinstall (map Installed.installedUnitId pkgs)
                                (changes pkg cpkg)

  where

    changes :: Installed.InstalledPackageInfo
            -> ReadyPackage
            -> [PackageChange]
    changes pkg (ReadyPackage pkg') = filter changed $
      mergeBy (comparing mungedName)
        -- deps of installed pkg
        (resolveInstalledIds $ Installed.depends pkg)
        -- deps of configured pkg
        (resolveInstalledIds $ CD.nonSetupDeps (depends pkg'))

    -- convert to source pkg ids via index
    resolveInstalledIds :: [UnitId] -> [MungedPackageId]
    resolveInstalledIds =
        nub
      . sort
      . map mungedId
      . mapMaybe (PackageIndex.lookupUnitId installedPkgIndex)

    changed (InBoth    pkgid pkgid') = pkgid /= pkgid'
    changed _                        = True

printPlan :: Bool -- is dry run
          -> Verbosity
          -> [(ReadyPackage, PackageStatus)]
          -> SourcePackageDb
          -> IO ()
printPlan dryRun verbosity plan sourcePkgDb = case plan of
  []   -> return ()
  pkgs
    | verbosity >= Verbosity.verbose -> notice verbosity $ unlines $
        ("In order, the following " ++ wouldWill ++ " be installed:")
      : map showPkgAndReason pkgs
    | otherwise -> notice verbosity $ unlines $
        ("In order, the following " ++ wouldWill
         ++ " be installed (use -v for more details):")
      : map showPkg pkgs
  where
    wouldWill | dryRun    = "would"
              | otherwise = "will"

    showPkg (pkg, _) = display (packageId pkg) ++
                       showLatest (pkg)

    showPkgAndReason (ReadyPackage pkg', pr) = display (packageId pkg') ++
          showLatest pkg' ++
          showFlagAssignment (nonDefaultFlags pkg') ++
          showStanzas (confPkgStanzas pkg') ++
          showDep pkg' ++
          case pr of
            NewPackage     -> " (new package)"
            NewVersion _   -> " (new version)"
            Reinstall _ cs -> " (reinstall)" ++ case cs of
                []   -> ""
                diff -> " (changes: "  ++ intercalate ", " (map change diff)
                        ++ ")"

    showLatest :: Package srcpkg => srcpkg -> String
    showLatest pkg = case mLatestVersion of
        Just latestVersion ->
            if packageVersion pkg < latestVersion
            then (" (latest: " ++ display latestVersion ++ ")")
            else ""
        Nothing -> ""
      where
        mLatestVersion :: Maybe Version
        mLatestVersion = case SourcePackageIndex.lookupPackageName
                                (packageIndex sourcePkgDb)
                                (packageName pkg) of
            [] -> Nothing
            x -> Just $ packageVersion $ last x

    toFlagAssignment :: [Flag] -> FlagAssignment
    toFlagAssignment =  mkFlagAssignment . map (\ f -> (flagName f, flagDefault f))

    nonDefaultFlags :: ConfiguredPackage loc -> FlagAssignment
    nonDefaultFlags cpkg =
      let defaultAssignment =
            toFlagAssignment
             (genPackageFlags (SourcePackage.packageDescription $
                               confPkgSource cpkg))
      in  confPkgFlags cpkg `diffFlagAssignment` defaultAssignment

    showStanzas :: [OptionalStanza] -> String
    showStanzas = concatMap ((" *" ++) . showStanza)

    showFlagAssignment :: FlagAssignment -> String
    showFlagAssignment = concatMap ((' ' :) . showFlagValue) . unFlagAssignment

    change (OnlyInLeft pkgid)        = display pkgid ++ " removed"
    change (InBoth     pkgid pkgid') = display pkgid ++ " -> "
                                    ++ display (mungedVersion pkgid')
    change (OnlyInRight      pkgid') = display pkgid' ++ " added"

    showDep pkg | Just rdeps <- Map.lookup (packageId pkg) revDeps
                  = " (via: " ++ unwords (map display rdeps) ++  ")"
                | otherwise = ""

    revDepGraphEdges :: [(PackageId, PackageId)]
    revDepGraphEdges = [ (rpid, packageId cpkg)
                       | (ReadyPackage cpkg, _) <- plan
                       , ConfiguredId rpid (Just PackageDescription.CLibName) _
                        <- CD.flatDeps (confPkgDeps cpkg) ]

    revDeps :: Map.Map PackageId [PackageId]
    revDeps = Map.fromListWith (++) (map (fmap (:[])) revDepGraphEdges)

-- ------------------------------------------------------------
-- * Post installation stuff
-- ------------------------------------------------------------

-- | Report a solver failure. This works slightly differently to
-- 'postInstallActions', as (by definition) we don't have an install plan.
reportPlanningFailure :: Verbosity -> InstallArgs -> InstallContext -> String
                      -> IO ()
reportPlanningFailure verbosity
  (_, _, comp, platform, _, _, _
  ,_, configFlags, _, installFlags, _)
  (_, sourcePkgDb, _, _, pkgSpecifiers, _)
  message = do

  when reportFailure $ do

    -- Only create reports for explicitly named packages
    let pkgids = filter
          (SourcePackageIndex.elemByPackageId (packageIndex sourcePkgDb)) $
          mapMaybe theSpecifiedPackage pkgSpecifiers

        buildReports = BuildReports.fromPlanningFailure platform
                       (compilerId comp) pkgids
                       (configConfigurationsFlags configFlags)

    unless (null buildReports) $
      info verbosity $
        "Solver failure will be reported for "
        ++ intercalate "," (map display pkgids)

    -- Save reports
    BuildReports.storeLocal (compilerInfo comp)
                            (fromNubList $ installSummaryFile installFlags)
                            buildReports platform

    -- Save solver log
    case logFile of
      Nothing -> return ()
      Just template -> forM_ pkgids $ \pkgid ->
        let env = initialPathTemplateEnv pkgid dummyIpid
                    (compilerInfo comp) platform
            path = fromPathTemplate $ substPathTemplate env template
        in  writeFile path message

  where
    reportFailure = fromFlag (installReportPlanningFailure installFlags)
    logFile = flagToMaybe (installLogFile installFlags)

    -- A IPID is calculated from the transitive closure of
    -- dependencies, but when the solver fails we don't have that.
    -- So we fail.
    dummyIpid = error "reportPlanningFailure: installed package ID not available"

-- | If a 'PackageSpecifier' refers to a single package, return Just that
-- package.
theSpecifiedPackage :: Package pkg => PackageSpecifier pkg -> Maybe PackageId
theSpecifiedPackage pkgSpec =
  case pkgSpec of
    NamedPackage name [PackagePropertyVersion version]
      -> PackageIdentifier name <$> trivialRange version
    NamedPackage _ _ -> Nothing
    SpecificSourcePackage pkg -> Just $ packageId pkg
  where
    -- | If a range includes only a single version, return Just that version.
    trivialRange :: VersionRange -> Maybe Version
    trivialRange = foldVersionRange
        Nothing
        Just     -- "== v"
        (\_ -> Nothing)
        (\_ -> Nothing)
        (\_ _ -> Nothing)
        (\_ _ -> Nothing)

-- | Various stuff we do after successful or unsuccessfully installing a bunch
-- of packages. This includes:
--
--  * build reporting, local and remote
--  * symlinking binaries
--  * updating indexes
--  * updating world file
--  * error reporting
--
postInstallActions :: Verbosity
                   -> InstallArgs
                   -> [UserTarget]
                   -> InstallPlan
                   -> BuildOutcomes
                   -> IO ()
postInstallActions verbosity
  (packageDBs, _, comp, platform, progdb, useSandbox, mSandboxPkgInfo
  ,globalFlags, configFlags, _, installFlags, _)
  targets installPlan buildOutcomes = do

  unless oneShot $
    World.insert verbosity worldFile
      --FIXME: does not handle flags
      [ World.WorldPkgInfo dep mempty
      | UserTargetNamed dep <- targets ]

  let buildReports = BuildReports.fromInstallPlan platform (compilerId comp)
                                                  installPlan buildOutcomes
  BuildReports.storeLocal (compilerInfo comp)
                          (fromNubList $ installSummaryFile installFlags)
                          buildReports
                          platform
  when (reportingLevel >= AnonymousReports) $
    BuildReports.storeAnonymous buildReports
  when (reportingLevel == DetailedReports) $
    storeDetailedBuildReports verbosity logsDir buildReports

  regenerateHaddockIndex verbosity packageDBs comp platform progdb useSandbox
                         configFlags installFlags buildOutcomes

  symlinkBinaries verbosity platform comp configFlags installFlags
                  installPlan buildOutcomes

  printBuildFailures verbosity buildOutcomes

  updateSandboxTimestampsFile verbosity useSandbox mSandboxPkgInfo
                              comp platform installPlan buildOutcomes

  where
    reportingLevel = fromFlag (installBuildReports installFlags)
    logsDir        = fromFlag (globalLogsDir globalFlags)
    oneShot        = fromFlag (installOneShot installFlags)
    worldFile      = fromFlag $ globalWorldFile globalFlags

storeDetailedBuildReports :: Verbosity -> FilePath
                          -> [(BuildReports.BuildReport, Maybe Repo)] -> IO ()
storeDetailedBuildReports verbosity logsDir reports = sequence_
  [ do dotCabal <- defaultCabalDir
       let logFileName = display (BuildReports.package report) <.> "log"
           logFile     = logsDir </> logFileName
           reportsDir  = dotCabal </> "reports" </> remoteRepoName remoteRepo
           reportFile  = reportsDir </> logFileName

       handleMissingLogFile $ do
         buildLog <- readFile logFile
         createDirectoryIfMissing True reportsDir -- FIXME
         writeFile reportFile (show (BuildReports.show report, buildLog))

  | (report, Just repo) <- reports
  , Just remoteRepo <- [maybeRepoRemote repo]
  , isLikelyToHaveLogFile (BuildReports.installOutcome report) ]

  where
    isLikelyToHaveLogFile BuildReports.ConfigureFailed {} = True
    isLikelyToHaveLogFile BuildReports.BuildFailed     {} = True
    isLikelyToHaveLogFile BuildReports.InstallFailed   {} = True
    isLikelyToHaveLogFile BuildReports.InstallOk       {} = True
    isLikelyToHaveLogFile _                               = False

    handleMissingLogFile = Exception.handleJust missingFile $ \ioe ->
      warn verbosity $ "Missing log file for build report: "
                    ++ fromMaybe ""  (ioeGetFileName ioe)

    missingFile ioe
      | isDoesNotExistError ioe  = Just ioe
    missingFile _                = Nothing


regenerateHaddockIndex :: Verbosity
                       -> [PackageDB]
                       -> Compiler
                       -> Platform
                       -> ProgramDb
                       -> UseSandbox
                       -> ConfigFlags
                       -> InstallFlags
                       -> BuildOutcomes
                       -> IO ()
regenerateHaddockIndex verbosity packageDBs comp platform progdb useSandbox
                       configFlags installFlags buildOutcomes
  | haddockIndexFileIsRequested && shouldRegenerateHaddockIndex = do

  defaultDirs <- InstallDirs.defaultInstallDirs
                   (compilerFlavor comp)
                   (fromFlag (configUserInstall configFlags))
                   True
  let indexFileTemplate = fromFlag (installHaddockIndex installFlags)
      indexFile = substHaddockIndexFileName defaultDirs indexFileTemplate

  notice verbosity $
     "Updating documentation index " ++ indexFile

  --TODO: might be nice if the install plan gave us the new InstalledPackageInfo
  installedPkgIndex <- getInstalledPackages verbosity comp packageDBs progdb
  Haddock.regenerateHaddockIndex verbosity installedPkgIndex progdb indexFile

  | otherwise = return ()
  where
    haddockIndexFileIsRequested =
         fromFlag (installDocumentation installFlags)
      && isJust (flagToMaybe (installHaddockIndex installFlags))

    -- We want to regenerate the index if some new documentation was actually
    -- installed. Since the index can be only per-user or per-sandbox (see
    -- #1337), we don't do it for global installs or special cases where we're
    -- installing into a specific db.
    shouldRegenerateHaddockIndex = (isUseSandbox useSandbox || normalUserInstall)
                                && someDocsWereInstalled buildOutcomes
      where
        someDocsWereInstalled = any installedDocs . Map.elems
        installedDocs (Right (BuildResult DocsOk _ _)) = True
        installedDocs _                                = False

        normalUserInstall     = (UserPackageDB `elem` packageDBs)
                             && all (not . isSpecificPackageDB) packageDBs
        isSpecificPackageDB (SpecificPackageDB _) = True
        isSpecificPackageDB _                     = False

    substHaddockIndexFileName defaultDirs = fromPathTemplate
                                          . substPathTemplate env
      where
        env  = env0 ++ installDirsTemplateEnv absoluteDirs
        env0 = InstallDirs.compilerTemplateEnv (compilerInfo comp)
            ++ InstallDirs.platformTemplateEnv platform
            ++ InstallDirs.abiTemplateEnv (compilerInfo comp) platform
        absoluteDirs = InstallDirs.substituteInstallDirTemplates
                         env0 templateDirs
        templateDirs = InstallDirs.combineInstallDirs fromFlagOrDefault
                         defaultDirs (configInstallDirs configFlags)


symlinkBinaries :: Verbosity
                -> Platform -> Compiler
                -> ConfigFlags
                -> InstallFlags
                -> InstallPlan
                -> BuildOutcomes
                -> IO ()
symlinkBinaries verbosity platform comp configFlags installFlags
                plan buildOutcomes = do
  failed <- InstallSymlink.symlinkBinaries platform comp
                                           configFlags installFlags
                                           plan buildOutcomes
  case failed of
    [] -> return ()
    [(_, exe, path)] ->
      warn verbosity $
           "could not create a symlink in " ++ bindir ++ " for "
        ++ display exe ++ " because the file exists there already but is not "
        ++ "managed by cabal. You can create a symlink for this executable "
        ++ "manually if you wish. The executable file has been installed at "
        ++ path
    exes ->
      warn verbosity $
           "could not create symlinks in " ++ bindir ++ " for "
        ++ intercalate ", " [ display exe | (_, exe, _) <- exes ]
        ++ " because the files exist there already and are not "
        ++ "managed by cabal. You can create symlinks for these executables "
        ++ "manually if you wish. The executable files have been installed at "
        ++ intercalate ", " [ path | (_, _, path) <- exes ]
  where
    bindir = fromFlag (installSymlinkBinDir installFlags)


printBuildFailures :: Verbosity -> BuildOutcomes -> IO ()
printBuildFailures verbosity buildOutcomes =
  case [ (pkgid, failure)
       | (pkgid, Left failure) <- Map.toList buildOutcomes ] of
    []     -> return ()
    failed -> die' verbosity . unlines
            $ "Error: some packages failed to install:"
            : [ display pkgid ++ printFailureReason reason
              | (pkgid, reason) <- failed ]
  where
    printFailureReason reason = case reason of
      DependentFailed pkgid -> " depends on " ++ display pkgid
                            ++ " which failed to install."
      DownloadFailed  e -> " failed while downloading the package."
                        ++ showException e
      UnpackFailed    e -> " failed while unpacking the package."
                        ++ showException e
      ConfigureFailed e -> " failed during the configure step."
                        ++ showException e
      BuildFailed     e -> " failed during the building phase."
                        ++ showException e
      TestsFailed     e -> " failed during the tests phase."
                        ++ showException e
      InstallFailed   e -> " failed during the final install step."
                        ++ showException e

      -- This will never happen, but we include it for completeness
      PlanningFailed -> " failed during the planning phase."

    showException e   =  " The exception was:\n  " ++ show e ++ maybeOOM e
#ifdef mingw32_HOST_OS
    maybeOOM _        = ""
#else
    maybeOOM e                    = maybe "" onExitFailure (fromException e)
    onExitFailure (ExitFailure n)
      | n == 9 || n == -9         =
      "\nThis may be due to an out-of-memory condition."
    onExitFailure _               = ""
#endif


-- | If we're working inside a sandbox and some add-source deps were installed,
-- update the timestamps of those deps.
updateSandboxTimestampsFile :: Verbosity -> UseSandbox -> Maybe SandboxPackageInfo
                            -> Compiler -> Platform
                            -> InstallPlan
                            -> BuildOutcomes
                            -> IO ()
updateSandboxTimestampsFile verbosity (UseSandbox sandboxDir)
                            (Just (SandboxPackageInfo _ _ _ allAddSourceDeps))
                            comp platform installPlan buildOutcomes =
  withUpdateTimestamps verbosity sandboxDir (compilerId comp) platform $ \_ -> do
    let allInstalled = [ pkg
                       | InstallPlan.Configured pkg
                            <- InstallPlan.toList installPlan
                       , case InstallPlan.lookupBuildOutcome pkg buildOutcomes of
                           Just (Right _success) -> True
                           _                     -> False
                       ]
        allSrcPkgs   = [ confPkgSource cpkg | cpkg <- allInstalled ]
        allPaths     = [ pth | LocalUnpackedPackage pth
                            <- map packageSource allSrcPkgs]
    allPathsCanonical <- mapM tryCanonicalizePath allPaths
    return $! filter (`S.member` allAddSourceDeps) allPathsCanonical

updateSandboxTimestampsFile _ _ _ _ _ _ _ = return ()

-- ------------------------------------------------------------
-- * Actually do the installations
-- ------------------------------------------------------------

data InstallMisc = InstallMisc {
    libVersion :: Maybe Version
  }

-- | If logging is enabled, contains location of the log file and the verbosity
-- level for logging.
type UseLogFile = Maybe (PackageIdentifier -> UnitId -> FilePath, Verbosity)

performInstallations :: Verbosity
                     -> InstallArgs
                     -> InstalledPackageIndex
                     -> InstallPlan
                     -> IO BuildOutcomes
performInstallations verbosity
  (packageDBs, repoCtxt, comp, platform, progdb, useSandbox, _,
   globalFlags, configFlags, configExFlags, installFlags, haddockFlags)
  installedPkgIndex installPlan = do

  -- With 'install -j' it can be a bit hard to tell whether a sandbox is used.
  whenUsingSandbox useSandbox $ \sandboxDir ->
    when parallelInstall $
      notice verbosity $ "Notice: installing into a sandbox located at "
                         ++ sandboxDir
  info verbosity $ "Number of threads used: " ++ (show numJobs) ++ "."

  jobControl   <- if parallelInstall then newParallelJobControl numJobs
                                     else newSerialJobControl
  fetchLimit   <- newJobLimit (min numJobs numFetchJobs)
  installLock  <- newLock -- serialise installation
  cacheLock    <- newLock -- serialise access to setup exe cache

  executeInstallPlan verbosity jobControl keepGoing useLogFile
                     installPlan $ \rpkg ->
    installReadyPackage platform cinfo configFlags
                        rpkg $ \configFlags' src pkg pkgoverride ->
      fetchSourcePackage verbosity repoCtxt fetchLimit src $ \src' ->
        installLocalPackage verbosity (packageId pkg) src' distPref $ \mpath ->
          installUnpackedPackage verbosity installLock numJobs
                                 (setupScriptOptions installedPkgIndex
                                  cacheLock rpkg)
                                 configFlags'
                                 installFlags haddockFlags comp progdb
                                 platform pkg rpkg pkgoverride mpath useLogFile

  where
    cinfo = compilerInfo comp

    numJobs         = determineNumJobs (installNumJobs installFlags)
    numFetchJobs    = 2
    parallelInstall = numJobs >= 2
    keepGoing       = fromFlag (installKeepGoing installFlags)
    distPref        = fromFlagOrDefault (useDistPref defaultSetupScriptOptions)
                      (configDistPref configFlags)

    setupScriptOptions index lock rpkg =
      configureSetupScript
        packageDBs
        comp
        platform
        progdb
        distPref
        (chooseCabalVersion configExFlags (libVersion miscOptions))
        (Just lock)
        parallelInstall
        index
        (Just rpkg)

    reportingLevel = fromFlag (installBuildReports installFlags)
    logsDir        = fromFlag (globalLogsDir globalFlags)

    -- Should the build output be written to a log file instead of stdout?
    useLogFile :: UseLogFile
    useLogFile = fmap ((\f -> (f, loggingVerbosity)) . substLogFileName)
                 logFileTemplate
      where
        installLogFile' = flagToMaybe $ installLogFile installFlags
        defaultTemplate = toPathTemplate $
                            logsDir </> "$compiler" </> "$libname" <.> "log"

        -- If the user has specified --remote-build-reporting=detailed, use the
        -- default log file location. If the --build-log option is set, use the
        -- provided location. Otherwise don't use logging, unless building in
        -- parallel (in which case the default location is used).
        logFileTemplate :: Maybe PathTemplate
        logFileTemplate
          | useDefaultTemplate = Just defaultTemplate
          | otherwise          = installLogFile'

        -- If the user has specified --remote-build-reporting=detailed or
        -- --build-log, use more verbose logging.
        loggingVerbosity :: Verbosity
        loggingVerbosity | overrideVerbosity = modifyVerbosity (max verbose) verbosity
                         | otherwise         = verbosity

        useDefaultTemplate :: Bool
        useDefaultTemplate
          | reportingLevel == DetailedReports = True
          | isJust installLogFile'            = False
          | parallelInstall                   = True
          | otherwise                         = False

        overrideVerbosity :: Bool
        overrideVerbosity
          | reportingLevel == DetailedReports = True
          | isJust installLogFile'            = True
          | parallelInstall                   = False
          | otherwise                         = False

    substLogFileName :: PathTemplate -> PackageIdentifier -> UnitId -> FilePath
    substLogFileName template pkg uid = fromPathTemplate
                                  . substPathTemplate env
                                  $ template
      where env = initialPathTemplateEnv (packageId pkg) uid
                    (compilerInfo comp) platform

    miscOptions  = InstallMisc {
      libVersion = flagToMaybe (configCabalVersion configExFlags)
    }


executeInstallPlan :: Verbosity
                   -> JobControl IO (UnitId, BuildOutcome)
                   -> Bool
                   -> UseLogFile
                   -> InstallPlan
                   -> (ReadyPackage -> IO BuildOutcome)
                   -> IO BuildOutcomes
executeInstallPlan verbosity jobCtl keepGoing useLogFile plan0 installPkg =
    InstallPlan.execute
      jobCtl keepGoing depsFailure plan0 $ \pkg -> do
        buildOutcome <- installPkg pkg
        printBuildResult (packageId pkg) (installedUnitId pkg) buildOutcome
        return buildOutcome

  where
    depsFailure = DependentFailed . packageId

    -- Print build log if something went wrong, and 'Installed $PKGID'
    -- otherwise.
    printBuildResult :: PackageId -> UnitId -> BuildOutcome -> IO ()
    printBuildResult pkgid uid buildOutcome = case buildOutcome of
        (Right _) -> notice verbosity $ "Installed " ++ display pkgid
        (Left _)  -> do
          notice verbosity $ "Failed to install " ++ display pkgid
          when (verbosity >= normal) $
            case useLogFile of
              Nothing                 -> return ()
              Just (mkLogFileName, _) -> do
                let logName = mkLogFileName pkgid uid
                putStr $ "Build log ( " ++ logName ++ " ):\n"
                printFile logName

    printFile :: FilePath -> IO ()
    printFile path = readFile path >>= putStr

-- | Call an installer for an 'SourcePackage' but override the configure
-- flags with the ones given by the 'ReadyPackage'. In particular the
-- 'ReadyPackage' specifies an exact 'FlagAssignment' and exactly
-- versioned package dependencies. So we ignore any previous partial flag
-- assignment or dependency constraints and use the new ones.
--
-- NB: when updating this function, don't forget to also update
-- 'configurePackage' in D.C.Configure.
installReadyPackage :: Platform -> CompilerInfo
                    -> ConfigFlags
                    -> ReadyPackage
                    -> (ConfigFlags -> UnresolvedPkgLoc
                                    -> PackageDescription
                                    -> PackageDescriptionOverride
                                    -> a)
                    -> a
installReadyPackage platform cinfo configFlags
                    (ReadyPackage (ConfiguredPackage ipid
                                    (SourcePackage _ gpkg source pkgoverride)
                                    flags stanzas deps))
                    installPkg =
  installPkg configFlags {
    configIPID = toFlag (display ipid),
    configConfigurationsFlags = flags,
    -- We generate the legacy constraints as well as the new style precise deps.
    -- In the end only one set gets passed to Setup.hs configure, depending on
    -- the Cabal version we are talking to.
    configConstraints  = [ thisPackageVersion srcid
                         | ConfiguredId srcid (Just PackageDescription.CLibName) _ipid
                            <- CD.nonSetupDeps deps ],
    configDependencies = [ (packageName srcid, dep_ipid)
                         | ConfiguredId srcid (Just PackageDescription.CLibName) dep_ipid
                            <- CD.nonSetupDeps deps ],
    -- Use '--exact-configuration' if supported.
    configExactConfiguration = toFlag True,
    configBenchmarks         = toFlag False,
    configTests              = toFlag (TestStanzas `elem` stanzas)
  } source pkg pkgoverride
  where
    pkg = case finalizePD flags (enableStanzas stanzas)
           (const True)
           platform cinfo [] gpkg of
      Left _ -> error "finalizePD ReadyPackage failed"
      Right (desc, _) -> desc

fetchSourcePackage
  :: Verbosity
  -> RepoContext
  -> JobLimit
  -> UnresolvedPkgLoc
  -> (ResolvedPkgLoc -> IO BuildOutcome)
  -> IO BuildOutcome
fetchSourcePackage verbosity repoCtxt fetchLimit src installPkg = do
  fetched <- checkFetched src
  case fetched of
    Just src' -> installPkg src'
    Nothing   -> onFailure DownloadFailed $ do
                   loc <- withJobLimit fetchLimit $
                            fetchPackage verbosity repoCtxt src
                   installPkg loc


installLocalPackage
  :: Verbosity
  -> PackageIdentifier -> ResolvedPkgLoc -> FilePath
  -> (Maybe FilePath -> IO BuildOutcome)
  -> IO BuildOutcome
installLocalPackage verbosity pkgid location distPref installPkg =

  case location of

    LocalUnpackedPackage dir ->
      installPkg (Just dir)

    LocalTarballPackage tarballPath ->
      installLocalTarballPackage verbosity
        pkgid tarballPath distPref installPkg

    RemoteTarballPackage _ tarballPath ->
      installLocalTarballPackage verbosity
        pkgid tarballPath distPref installPkg

    RepoTarballPackage _ _ tarballPath ->
      installLocalTarballPackage verbosity
        pkgid tarballPath distPref installPkg


installLocalTarballPackage
  :: Verbosity
  -> PackageIdentifier -> FilePath -> FilePath
  -> (Maybe FilePath -> IO BuildOutcome)
  -> IO BuildOutcome
installLocalTarballPackage verbosity pkgid
                           tarballPath distPref installPkg = do
  tmp <- getTemporaryDirectory
  withTempDirectory verbosity tmp "cabal-tmp" $ \tmpDirPath ->
    onFailure UnpackFailed $ do
      let relUnpackedPath = display pkgid
          absUnpackedPath = tmpDirPath </> relUnpackedPath
          descFilePath = absUnpackedPath
                     </> display (packageName pkgid) <.> "cabal"
      info verbosity $ "Extracting " ++ tarballPath
                    ++ " to " ++ tmpDirPath ++ "..."
      extractTarGzFile tmpDirPath relUnpackedPath tarballPath
      exists <- doesFileExist descFilePath
      unless exists $
        die' verbosity $ "Package .cabal file not found: " ++ show descFilePath
      maybeRenameDistDir absUnpackedPath
      installPkg (Just absUnpackedPath)

  where
    -- 'cabal sdist' puts pre-generated files in the 'dist'
    -- directory. This fails when a nonstandard build directory name
    -- is used (as is the case with sandboxes), so we need to rename
    -- the 'dist' dir here.
    --
    -- TODO: 'cabal get happy && cd sandbox && cabal install ../happy' still
    -- fails even with this workaround. We probably can live with that.
    maybeRenameDistDir :: FilePath -> IO ()
    maybeRenameDistDir absUnpackedPath = do
      let distDirPath    = absUnpackedPath </> defaultDistPref
          distDirPathTmp = absUnpackedPath </> (defaultDistPref ++ "-tmp")
          distDirPathNew = absUnpackedPath </> distPref
      distDirExists <- doesDirectoryExist distDirPath
      when (distDirExists
            && (not $ distDirPath `equalFilePath` distDirPathNew)) $ do
        -- NB: we need to handle the case when 'distDirPathNew' is a
        -- subdirectory of 'distDirPath' (e.g. the former is
        -- 'dist/dist-sandbox-3688fbc2' and the latter is 'dist').
        debug verbosity $ "Renaming '" ++ distDirPath ++ "' to '"
          ++ distDirPathTmp ++ "'."
        renameDirectory distDirPath distDirPathTmp
        when (distDirPath `isPrefixOf` distDirPathNew) $
          createDirectoryIfMissingVerbose verbosity False distDirPath
        debug verbosity $ "Renaming '" ++ distDirPathTmp ++ "' to '"
          ++ distDirPathNew ++ "'."
        renameDirectory distDirPathTmp distDirPathNew

installUnpackedPackage
  :: Verbosity
  -> Lock
  -> Int
  -> SetupScriptOptions
  -> ConfigFlags
  -> InstallFlags
  -> HaddockFlags
  -> Compiler
  -> ProgramDb
  -> Platform
  -> PackageDescription
  -> ReadyPackage
  -> PackageDescriptionOverride
  -> Maybe FilePath -- ^ Directory to change to before starting the installation.
  -> UseLogFile -- ^ File to log output to (if any)
  -> IO BuildOutcome
installUnpackedPackage verbosity installLock numJobs
                       scriptOptions
                       configFlags installFlags haddockFlags comp progdb
                       platform pkg rpkg pkgoverride workingDir useLogFile = do
  -- Override the .cabal file if necessary
  case pkgoverride of
    Nothing     -> return ()
    Just pkgtxt -> do
      let descFilePath = fromMaybe "." workingDir
                     </> display (packageName pkgid) <.> "cabal"
      info verbosity $
        "Updating " ++ display (packageName pkgid) <.> "cabal"
                    ++ " with the latest revision from the index."
      writeFileAtomic descFilePath pkgtxt

  -- Make sure that we pass --libsubdir etc to 'setup configure' (necessary if
  -- the setup script was compiled against an old version of the Cabal lib).
  configFlags' <- addDefaultInstallDirs configFlags
  -- Filter out flags not supported by the old versions of the Cabal lib.
  let configureFlags :: Version -> ConfigFlags
      configureFlags  = filterConfigureFlags configFlags' {
        configVerbosity = toFlag verbosity'
      }

  -- Path to the optional log file.
  mLogPath <- maybeLogPath

  logDirChange (maybe (const (return ())) appendFile mLogPath) workingDir $ do
    -- Configure phase
    onFailure ConfigureFailed $ do
      when (numJobs > 1) $ notice verbosity $
        "Configuring " ++ display pkgid ++ "..."
      setup configureCommand configureFlags mLogPath

    -- Build phase
      onFailure BuildFailed $ do
        when (numJobs > 1) $ notice verbosity $
          "Building " ++ display pkgid ++ "..."
        setup buildCommand' buildFlags mLogPath

    -- Doc generation phase
        docsResult <- if shouldHaddock
          then (do setup haddockCommand haddockFlags' mLogPath
                   return DocsOk)
                 `catchIO`   (\_ -> return DocsFailed)
                 `catchExit` (\_ -> return DocsFailed)
          else return DocsNotTried

    -- Tests phase
        onFailure TestsFailed $ do
          when (testsEnabled && PackageDescription.hasTests pkg) $
              setup Cabal.testCommand testFlags mLogPath

          let testsResult | testsEnabled = TestsOk
                          | otherwise = TestsNotTried

        -- Install phase
          onFailure InstallFailed $ criticalSection installLock $ do
            -- Actual installation
            withWin32SelfUpgrade verbosity uid configFlags
                                 cinfo platform pkg $ do
              setup Cabal.copyCommand copyFlags mLogPath

            -- Capture installed package configuration file, so that
            -- it can be incorporated into the final InstallPlan
            ipkgs <- genPkgConfs mLogPath
            let ipkgs' = case ipkgs of
                            [ipkg] -> [ipkg { Installed.installedUnitId = uid }]
                            _ -> ipkgs
            let packageDBs = interpretPackageDbFlags
                                (fromFlag (configUserInstall configFlags))
                                (configPackageDBs configFlags)
            forM_ ipkgs' $ \ipkg' ->
                registerPackage verbosity comp progdb
                                packageDBs ipkg'
                                defaultRegisterOptions

            return (Right (BuildResult docsResult testsResult (find ((==uid).installedUnitId) ipkgs')))

  where
    pkgid            = packageId pkg
    uid              = installedUnitId rpkg
    cinfo            = compilerInfo comp
    buildCommand'    = buildCommand progdb
    buildFlags   _   = emptyBuildFlags {
      buildDistPref  = configDistPref configFlags,
      buildVerbosity = toFlag verbosity'
    }
    shouldHaddock    = fromFlag (installDocumentation installFlags)
    haddockFlags' _   = haddockFlags {
      haddockVerbosity = toFlag verbosity',
      haddockDistPref  = configDistPref configFlags
    }
    testsEnabled = fromFlag (configTests configFlags)
                   && fromFlagOrDefault False (installRunTests installFlags)
    testFlags _ = Cabal.emptyTestFlags {
      Cabal.testDistPref = configDistPref configFlags
    }
    copyFlags _ = Cabal.emptyCopyFlags {
      Cabal.copyDistPref   = configDistPref configFlags,
      Cabal.copyDest       = toFlag InstallDirs.NoCopyDest,
      Cabal.copyVerbosity  = toFlag verbosity'
    }
    shouldRegister = PackageDescription.hasLibs pkg
    registerFlags _ = Cabal.emptyRegisterFlags {
      Cabal.regDistPref   = configDistPref configFlags,
      Cabal.regVerbosity  = toFlag verbosity'
    }
    verbosity' = maybe verbosity snd useLogFile
    tempTemplate name = name ++ "-" ++ display pkgid

    addDefaultInstallDirs :: ConfigFlags -> IO ConfigFlags
    addDefaultInstallDirs configFlags' = do
      defInstallDirs <- InstallDirs.defaultInstallDirs flavor userInstall False
      return $ configFlags' {
          configInstallDirs = fmap Cabal.Flag .
                              InstallDirs.substituteInstallDirTemplates env $
                              InstallDirs.combineInstallDirs fromFlagOrDefault
                              defInstallDirs (configInstallDirs configFlags)
          }
        where
          CompilerId flavor _ = compilerInfoId cinfo
          env         = initialPathTemplateEnv pkgid uid cinfo platform
          userInstall = fromFlagOrDefault defaultUserInstall
                        (configUserInstall configFlags')

    genPkgConfs :: Maybe FilePath
                     -> IO [Installed.InstalledPackageInfo]
    genPkgConfs mLogPath =
      if shouldRegister then do
        tmp <- getTemporaryDirectory
        withTempDirectory verbosity tmp (tempTemplate "pkgConf") $ \dir -> do
          let pkgConfDest = dir </> "pkgConf"
              registerFlags' version = (registerFlags version) {
                Cabal.regGenPkgConf = toFlag (Just pkgConfDest)
              }
          setup Cabal.registerCommand registerFlags' mLogPath
          is_dir <- doesDirectoryExist pkgConfDest
          let notHidden = not . isHidden
              isHidden name = "." `isPrefixOf` name
          if is_dir
            -- Sort so that each prefix of the package
            -- configurations is well formed
            then mapM (readPkgConf pkgConfDest) . sort . filter notHidden
                    =<< getDirectoryContents pkgConfDest
            else fmap (:[]) $ readPkgConf "." pkgConfDest
      else return []

    readPkgConf :: FilePath -> FilePath
                -> IO Installed.InstalledPackageInfo
    readPkgConf pkgConfDir pkgConfFile =
      (withUTF8FileContents (pkgConfDir </> pkgConfFile) $ \pkgConfText ->
        case Installed.parseInstalledPackageInfo pkgConfText of
          Installed.ParseFailed perror    -> pkgConfParseFailed perror
          Installed.ParseOk warns pkgConf -> do
            unless (null warns) $
              warn verbosity $ unlines (map (showPWarning pkgConfFile) warns)
            return pkgConf)

    pkgConfParseFailed :: Installed.PError -> IO a
    pkgConfParseFailed perror =
      die' verbosity $ "Couldn't parse the output of 'setup register --gen-pkg-config':"
            ++ show perror

    maybeLogPath :: IO (Maybe FilePath)
    maybeLogPath =
      case useLogFile of
         Nothing                 -> return Nothing
         Just (mkLogFileName, _) -> do
           let logFileName = mkLogFileName (packageId pkg) uid
               logDir      = takeDirectory logFileName
           unless (null logDir) $ createDirectoryIfMissing True logDir
           logFileExists <- doesFileExist logFileName
           when logFileExists $ removeFile logFileName
           return (Just logFileName)

    setup cmd flags mLogPath =
      Exception.bracket
      (traverse (\path -> openFile path AppendMode) mLogPath)
      (traverse_ hClose)
      (\logFileHandle ->
        setupWrapper verbosity
          scriptOptions { useLoggingHandle = logFileHandle
                        , useWorkingDir    = workingDir }
          (Just pkg)
          cmd flags [])


-- helper
onFailure :: (SomeException -> BuildFailure) -> IO BuildOutcome -> IO BuildOutcome
onFailure result action =
  action `catches`
    [ Handler $ \ioe  -> handler (ioe  :: IOException)
    , Handler $ \exit -> handler (exit :: ExitCode)
    ]
  where
    handler :: Exception e => e -> IO BuildOutcome
    handler = return . Left . result . toException


-- ------------------------------------------------------------
-- * Weird windows hacks
-- ------------------------------------------------------------

withWin32SelfUpgrade :: Verbosity
                     -> UnitId
                     -> ConfigFlags
                     -> CompilerInfo
                     -> Platform
                     -> PackageDescription
                     -> IO a -> IO a
withWin32SelfUpgrade _ _ _ _ _ _ action | buildOS /= Windows = action
withWin32SelfUpgrade verbosity uid configFlags cinfo platform pkg action = do

  defaultDirs <- InstallDirs.defaultInstallDirs
                   compFlavor
                   (fromFlag (configUserInstall configFlags))
                   (PackageDescription.hasLibs pkg)

  Win32SelfUpgrade.possibleSelfUpgrade verbosity
    (exeInstallPaths defaultDirs) action

  where
    pkgid = packageId pkg
    (CompilerId compFlavor _) = compilerInfoId cinfo

    exeInstallPaths defaultDirs =
      [ InstallDirs.bindir absoluteDirs </> exeName <.> exeExtension
      | exe <- PackageDescription.executables pkg
      , PackageDescription.buildable (PackageDescription.buildInfo exe)
      , let exeName = prefix ++ display (PackageDescription.exeName exe) ++ suffix
            prefix  = substTemplate prefixTemplate
            suffix  = substTemplate suffixTemplate ]
      where
        fromFlagTemplate = fromFlagOrDefault (InstallDirs.toPathTemplate "")
        prefixTemplate = fromFlagTemplate (configProgPrefix configFlags)
        suffixTemplate = fromFlagTemplate (configProgSuffix configFlags)
        templateDirs   = InstallDirs.combineInstallDirs fromFlagOrDefault
                           defaultDirs (configInstallDirs configFlags)
        absoluteDirs   = InstallDirs.absoluteInstallDirs
                           pkgid uid
                           cinfo InstallDirs.NoCopyDest
                           platform templateDirs
        substTemplate  = InstallDirs.fromPathTemplate
                       . InstallDirs.substPathTemplate env
          where env = InstallDirs.initialPathTemplateEnv pkgid uid
                      cinfo platform
