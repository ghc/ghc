{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Configure
-- Copyright   :  (c) David Himmelstrup 2005,
--                    Duncan Coutts 2005
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- High level interface to configuring a package.
-----------------------------------------------------------------------------
module Distribution.Client.Configure (
    configure,
    configureSetupScript,
    chooseCabalVersion,
    checkConfigExFlags,
    -- * Saved configure flags
    readConfigFlagsFrom, readConfigFlags,
    cabalConfigFlagsFile,
    writeConfigFlagsTo, writeConfigFlags,
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Client.Dependency
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.SolverInstallPlan (SolverInstallPlan)
import Distribution.Client.IndexUtils as IndexUtils
         ( getSourcePackages, getInstalledPackages )
import Distribution.Client.Setup
         ( ConfigExFlags(..), RepoContext(..)
         , configureCommand, configureExCommand, filterConfigureFlags )
import Distribution.Client.Types as Source
import Distribution.Client.SetupWrapper
         ( setupWrapper, SetupScriptOptions(..), defaultSetupScriptOptions )
import Distribution.Client.Targets
         ( userToPackageConstraint, userConstraintPackageName )
import Distribution.Client.JobControl (Lock)

import qualified Distribution.Solver.Types.ComponentDeps as CD
import           Distribution.Solver.Types.Settings
import           Distribution.Solver.Types.ConstraintSource
import           Distribution.Solver.Types.LabeledPackageConstraint
import           Distribution.Solver.Types.OptionalStanza
import           Distribution.Solver.Types.PackageIndex
                   ( PackageIndex, elemByPackageName )
import           Distribution.Solver.Types.PkgConfigDb
                   (PkgConfigDb, readPkgConfigDb)
import           Distribution.Solver.Types.SourcePackage

import Distribution.Simple.Compiler
         ( Compiler, CompilerInfo, compilerInfo, PackageDB(..), PackageDBStack )
import Distribution.Simple.Program (ProgramDb)
import Distribution.Client.SavedFlags ( readCommandFlags, writeCommandFlags )
import Distribution.Simple.Setup
         ( ConfigFlags(..)
         , fromFlag, toFlag, flagToMaybe, fromFlagOrDefault )
import Distribution.Simple.PackageIndex
         ( InstalledPackageIndex, lookupPackageName )
import Distribution.Package
         ( Package(..), packageName, PackageId )
import Distribution.Types.Dependency
         ( Dependency(..), thisPackageVersion )
import qualified Distribution.PackageDescription as PkgDesc
import Distribution.PackageDescription.Parsec
         ( readGenericPackageDescription )
import Distribution.PackageDescription.Configuration
         ( finalizePD )
import Distribution.Version
         ( Version, mkVersion, anyVersion, thisVersion
         , VersionRange, orLaterVersion )
import Distribution.Simple.Utils as Utils
         ( warn, notice, debug, die'
         , defaultPackageDesc )
import Distribution.System
         ( Platform )
import Distribution.Text ( display )
import Distribution.Verbosity as Verbosity
         ( Verbosity )

import System.FilePath ( (</>) )

-- | Choose the Cabal version such that the setup scripts compiled against this
-- version will support the given command-line flags.
chooseCabalVersion :: ConfigExFlags -> Maybe Version -> VersionRange
chooseCabalVersion configExFlags maybeVersion =
  maybe defaultVersionRange thisVersion maybeVersion
  where
    -- Cabal < 1.19.2 doesn't support '--exact-configuration' which is needed
    -- for '--allow-newer' to work.
    allowNewer = isRelaxDeps
                 (maybe mempty unAllowNewer $ configAllowNewer configExFlags)
    allowOlder = isRelaxDeps
                 (maybe mempty unAllowOlder $ configAllowOlder configExFlags)

    defaultVersionRange = if allowOlder || allowNewer
                          then orLaterVersion (mkVersion [1,19,2])
                          else anyVersion

-- | Configure the package found in the local directory
configure :: Verbosity
          -> PackageDBStack
          -> RepoContext
          -> Compiler
          -> Platform
          -> ProgramDb
          -> ConfigFlags
          -> ConfigExFlags
          -> [String]
          -> IO ()
configure verbosity packageDBs repoCtxt comp platform progdb
  configFlags configExFlags extraArgs = do

  installedPkgIndex <- getInstalledPackages verbosity comp packageDBs progdb
  sourcePkgDb       <- getSourcePackages    verbosity repoCtxt
  pkgConfigDb       <- readPkgConfigDb      verbosity progdb

  checkConfigExFlags verbosity installedPkgIndex
                     (packageIndex sourcePkgDb) configExFlags

  progress <- planLocalPackage verbosity comp platform configFlags configExFlags
                               installedPkgIndex sourcePkgDb pkgConfigDb

  notice verbosity "Resolving dependencies..."
  maybePlan <- foldProgress logMsg (return . Left) (return . Right)
                            progress
  case maybePlan of
    Left message -> do
      warn verbosity $
           "solver failed to find a solution:\n"
        ++ message
        ++ "\nTrying configure anyway."
      setupWrapper verbosity (setupScriptOptions installedPkgIndex Nothing)
        Nothing configureCommand (const configFlags) extraArgs

    Right installPlan0 ->
     let installPlan = InstallPlan.configureInstallPlan configFlags installPlan0
     in case fst (InstallPlan.ready installPlan) of
      [pkg@(ReadyPackage
              (ConfiguredPackage _ (SourcePackage _ _ (LocalUnpackedPackage _) _)
                                 _ _ _))] -> do
        configurePackage verbosity
          platform (compilerInfo comp)
          (setupScriptOptions installedPkgIndex (Just pkg))
          configFlags pkg extraArgs

      _ -> die' verbosity $ "internal error: configure install plan should have exactly "
              ++ "one local ready package."

  where
    setupScriptOptions :: InstalledPackageIndex
                       -> Maybe ReadyPackage
                       -> SetupScriptOptions
    setupScriptOptions =
      configureSetupScript
        packageDBs
        comp
        platform
        progdb
        (fromFlagOrDefault
           (useDistPref defaultSetupScriptOptions)
           (configDistPref configFlags))
        (chooseCabalVersion
           configExFlags
           (flagToMaybe (configCabalVersion configExFlags)))
        Nothing
        False

    logMsg message rest = debug verbosity message >> rest

configureSetupScript :: PackageDBStack
                     -> Compiler
                     -> Platform
                     -> ProgramDb
                     -> FilePath
                     -> VersionRange
                     -> Maybe Lock
                     -> Bool
                     -> InstalledPackageIndex
                     -> Maybe ReadyPackage
                     -> SetupScriptOptions
configureSetupScript packageDBs
                     comp
                     platform
                     progdb
                     distPref
                     cabalVersion
                     lock
                     forceExternal
                     index
                     mpkg
  = SetupScriptOptions {
      useCabalVersion          = cabalVersion
    , useCabalSpecVersion      = Nothing
    , useCompiler              = Just comp
    , usePlatform              = Just platform
    , usePackageDB             = packageDBs'
    , usePackageIndex          = index'
    , useProgramDb             = progdb
    , useDistPref              = distPref
    , useLoggingHandle         = Nothing
    , useWorkingDir            = Nothing
    , useExtraPathEnv          = []
    , setupCacheLock           = lock
    , useWin32CleanHack        = False
    , forceExternalSetupMethod = forceExternal
      -- If we have explicit setup dependencies, list them; otherwise, we give
      -- the empty list of dependencies; ideally, we would fix the version of
      -- Cabal here, so that we no longer need the special case for that in
      -- `compileSetupExecutable` in `externalSetupMethod`, but we don't yet
      -- know the version of Cabal at this point, but only find this there.
      -- Therefore, for now, we just leave this blank.
    , useDependencies          = fromMaybe [] explicitSetupDeps
    , useDependenciesExclusive = not defaultSetupDeps && isJust explicitSetupDeps
    , useVersionMacros         = not defaultSetupDeps && isJust explicitSetupDeps
    , isInteractive            = False
    }
  where
    -- When we are compiling a legacy setup script without an explicit
    -- setup stanza, we typically want to allow the UserPackageDB for
    -- finding the Cabal lib when compiling any Setup.hs even if we're doing
    -- a global install. However we also allow looking in a specific package
    -- db.
    packageDBs' :: PackageDBStack
    index'      :: Maybe InstalledPackageIndex
    (packageDBs', index') =
      case packageDBs of
        (GlobalPackageDB:dbs) | UserPackageDB `notElem` dbs
                              , Nothing <- explicitSetupDeps
            -> (GlobalPackageDB:UserPackageDB:dbs, Nothing)
        -- but if the user is using an odd db stack, don't touch it
        _otherwise -> (packageDBs, Just index)

    maybeSetupBuildInfo :: Maybe PkgDesc.SetupBuildInfo
    maybeSetupBuildInfo = do
      ReadyPackage cpkg <- mpkg
      let gpkg = packageDescription (confPkgSource cpkg)
      PkgDesc.setupBuildInfo (PkgDesc.packageDescription gpkg)

    -- Was a default 'custom-setup' stanza added by 'cabal-install' itself? If
    -- so, 'setup-depends' must not be exclusive. See #3199.
    defaultSetupDeps :: Bool
    defaultSetupDeps = maybe False PkgDesc.defaultSetupDepends
                       maybeSetupBuildInfo

    explicitSetupDeps :: Maybe [(InstalledPackageId, PackageId)]
    explicitSetupDeps = do
      -- Check if there is an explicit setup stanza.
      _buildInfo <- maybeSetupBuildInfo
      -- Return the setup dependencies computed by the solver
      ReadyPackage cpkg <- mpkg
      return [ ( cid, srcid )
             | ConfiguredId srcid (Just PkgDesc.CLibName) cid <- CD.setupDeps (confPkgDeps cpkg)
             ]

-- | Warn if any constraints or preferences name packages that are not in the
-- source package index or installed package index.
checkConfigExFlags :: Package pkg
                   => Verbosity
                   -> InstalledPackageIndex
                   -> PackageIndex pkg
                   -> ConfigExFlags
                   -> IO ()
checkConfigExFlags verbosity installedPkgIndex sourcePkgIndex flags = do
  unless (null unknownConstraints) $ warn verbosity $
             "Constraint refers to an unknown package: "
          ++ showConstraint (head unknownConstraints)
  unless (null unknownPreferences) $ warn verbosity $
             "Preference refers to an unknown package: "
          ++ display (head unknownPreferences)
  where
    unknownConstraints = filter (unknown . userConstraintPackageName . fst) $
                         configExConstraints flags
    unknownPreferences = filter (unknown . \(Dependency name _) -> name) $
                         configPreferences flags
    unknown pkg = null (lookupPackageName installedPkgIndex pkg)
               && not (elemByPackageName sourcePkgIndex pkg)
    showConstraint (uc, src) =
        display uc ++ " (" ++ showConstraintSource src ++ ")"

-- | Make an 'InstallPlan' for the unpacked package in the current directory,
-- and all its dependencies.
--
planLocalPackage :: Verbosity -> Compiler
                 -> Platform
                 -> ConfigFlags -> ConfigExFlags
                 -> InstalledPackageIndex
                 -> SourcePackageDb
                 -> PkgConfigDb
                 -> IO (Progress String String SolverInstallPlan)
planLocalPackage verbosity comp platform configFlags configExFlags
  installedPkgIndex (SourcePackageDb _ packagePrefs) pkgConfigDb = do
  pkg <- readGenericPackageDescription verbosity =<<
            case flagToMaybe (configCabalFilePath configFlags) of
                Nothing -> defaultPackageDesc verbosity
                Just fp -> return fp
  solver <- chooseSolver verbosity (fromFlag $ configSolver configExFlags)
            (compilerInfo comp)

  let -- We create a local package and ask to resolve a dependency on it
      localPkg = SourcePackage {
        packageInfoId             = packageId pkg,
        packageDescription        = pkg,
        packageSource             = LocalUnpackedPackage ".",
        packageDescrOverride      = Nothing
      }

      testsEnabled = fromFlagOrDefault False $ configTests configFlags
      benchmarksEnabled =
        fromFlagOrDefault False $ configBenchmarks configFlags

      resolverParams =
          removeLowerBounds
          (fromMaybe (AllowOlder mempty) $ configAllowOlder configExFlags)
        . removeUpperBounds
          (fromMaybe (AllowNewer mempty) $ configAllowNewer configExFlags)

        . addPreferences
            -- preferences from the config file or command line
            [ PackageVersionPreference name ver
            | Dependency name ver <- configPreferences configExFlags ]

        . addConstraints
            -- version constraints from the config file or command line
            -- TODO: should warn or error on constraints that are not on direct
            -- deps or flag constraints not on the package in question.
            [ LabeledPackageConstraint (userToPackageConstraint uc) src
            | (uc, src) <- configExConstraints configExFlags ]

        . addConstraints
            -- package flags from the config file or command line
            [ let pc = PackageConstraint
                       (scopeToplevel $ packageName pkg)
                       (PackagePropertyFlags $ configConfigurationsFlags configFlags)
              in LabeledPackageConstraint pc ConstraintSourceConfigFlagOrTarget
            ]

        . addConstraints
            -- '--enable-tests' and '--enable-benchmarks' constraints from
            -- the config file or command line
            [ let pc = PackageConstraint (scopeToplevel $ packageName pkg) .
                       PackagePropertyStanzas $
                       [ TestStanzas  | testsEnabled ] ++
                       [ BenchStanzas | benchmarksEnabled ]
              in LabeledPackageConstraint pc ConstraintSourceConfigFlagOrTarget
            ]

            -- Don't solve for executables, since we use an empty source
            -- package database and executables never show up in the
            -- installed package index
        . setSolveExecutables (SolveExecutables False)

        . setSolverVerbosity verbosity

        $ standardInstallPolicy
            installedPkgIndex
            -- NB: We pass in an *empty* source package database,
            -- because cabal configure assumes that all dependencies
            -- have already been installed
            (SourcePackageDb mempty packagePrefs)
            [SpecificSourcePackage localPkg]

  return (resolveDependencies platform (compilerInfo comp) pkgConfigDb solver resolverParams)


-- | Call an installer for an 'SourcePackage' but override the configure
-- flags with the ones given by the 'ReadyPackage'. In particular the
-- 'ReadyPackage' specifies an exact 'FlagAssignment' and exactly
-- versioned package dependencies. So we ignore any previous partial flag
-- assignment or dependency constraints and use the new ones.
--
-- NB: when updating this function, don't forget to also update
-- 'installReadyPackage' in D.C.Install.
configurePackage :: Verbosity
                 -> Platform -> CompilerInfo
                 -> SetupScriptOptions
                 -> ConfigFlags
                 -> ReadyPackage
                 -> [String]
                 -> IO ()
configurePackage verbosity platform comp scriptOptions configFlags
                 (ReadyPackage (ConfiguredPackage ipid spkg flags stanzas deps))
                 extraArgs =

  setupWrapper verbosity
    scriptOptions (Just pkg) configureCommand configureFlags extraArgs

  where
    gpkg = packageDescription spkg
    configureFlags   = filterConfigureFlags configFlags {
      configIPID = if isJust (flagToMaybe (configIPID configFlags))
                    -- Make sure cabal configure --ipid works.
                    then configIPID configFlags
                    else toFlag (display ipid),
      configConfigurationsFlags = flags,
      -- We generate the legacy constraints as well as the new style precise
      -- deps.  In the end only one set gets passed to Setup.hs configure,
      -- depending on the Cabal version we are talking to.
      configConstraints  = [ thisPackageVersion srcid
                           | ConfiguredId srcid (Just PkgDesc.CLibName) _uid <- CD.nonSetupDeps deps ],
      configDependencies = [ (packageName srcid, uid)
                           | ConfiguredId srcid (Just PkgDesc.CLibName) uid <- CD.nonSetupDeps deps ],
      -- Use '--exact-configuration' if supported.
      configExactConfiguration = toFlag True,
      configVerbosity          = toFlag verbosity,
      -- NB: if the user explicitly specified
      -- --enable-tests/--enable-benchmarks, always respect it.
      -- (But if they didn't, let solver decide.)
      configBenchmarks         = toFlag (BenchStanzas `elem` stanzas)
                                    `mappend` configBenchmarks configFlags,
      configTests              = toFlag (TestStanzas `elem` stanzas)
                                    `mappend` configTests configFlags
    }

    pkg = case finalizePD flags (enableStanzas stanzas)
           (const True)
           platform comp [] gpkg of
      Left _ -> error "finalizePD ReadyPackage failed"
      Right (desc, _) -> desc

-- -----------------------------------------------------------------------------
-- * Saved configure environments and flags
-- -----------------------------------------------------------------------------

-- | Read saved configure flags and restore the saved environment from the
-- specified files.
readConfigFlagsFrom :: FilePath  -- ^ path to saved flags file
                    -> IO (ConfigFlags, ConfigExFlags)
readConfigFlagsFrom flags = do
  readCommandFlags flags configureExCommand

-- | The path (relative to @--build-dir@) where the arguments to @configure@
-- should be saved.
cabalConfigFlagsFile :: FilePath -> FilePath
cabalConfigFlagsFile dist = dist </> "cabal-config-flags"

-- | Read saved configure flags and restore the saved environment from the
-- usual location.
readConfigFlags :: FilePath  -- ^ @--build-dir@
                -> IO (ConfigFlags, ConfigExFlags)
readConfigFlags dist =
  readConfigFlagsFrom (cabalConfigFlagsFile dist)

-- | Save the configure flags and environment to the specified files.
writeConfigFlagsTo :: FilePath  -- ^ path to saved flags file
                   -> Verbosity -> (ConfigFlags, ConfigExFlags)
                   -> IO ()
writeConfigFlagsTo file verb flags = do
  writeCommandFlags verb file configureExCommand flags

-- | Save the build flags to the usual location.
writeConfigFlags :: Verbosity
                 -> FilePath  -- ^ @--build-dir@
                 -> (ConfigFlags, ConfigExFlags) -> IO ()
writeConfigFlags verb dist =
  writeConfigFlagsTo (cabalConfigFlagsFile dist) verb
