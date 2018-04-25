-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Freeze
-- Copyright   :  (c) David Himmelstrup 2005
--                    Duncan Coutts 2011
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- The cabal freeze command
-----------------------------------------------------------------------------
module Distribution.Client.Freeze (
    freeze, getFreezePkgs
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Client.Config ( SavedConfig(..) )
import Distribution.Client.Types
import Distribution.Client.Targets
import Distribution.Client.Dependency
import Distribution.Client.IndexUtils as IndexUtils
         ( getSourcePackages, getInstalledPackages )
import Distribution.Client.SolverInstallPlan
         ( SolverInstallPlan, SolverPlanPackage )
import qualified Distribution.Client.SolverInstallPlan as SolverInstallPlan
import Distribution.Client.Setup
         ( GlobalFlags(..), FreezeFlags(..), ConfigExFlags(..)
         , RepoContext(..) )
import Distribution.Client.Sandbox.PackageEnvironment
         ( loadUserConfig, pkgEnvSavedConfig, showPackageEnvironment,
           userPackageEnvironmentFile )
import Distribution.Client.Sandbox.Types
         ( SandboxPackageInfo(..) )

import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.LabeledPackageConstraint
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PkgConfigDb
import Distribution.Solver.Types.SolverId

import Distribution.Package
         ( Package, packageId, packageName, packageVersion )
import Distribution.Simple.Compiler
         ( Compiler, compilerInfo, PackageDBStack )
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.Program
         ( ProgramDb )
import Distribution.Simple.Setup
         ( fromFlag, fromFlagOrDefault, flagToMaybe )
import Distribution.Simple.Utils
         ( die', notice, debug, writeFileAtomic )
import Distribution.System
         ( Platform )
import Distribution.Text
         ( display )
import Distribution.Verbosity
         ( Verbosity )

import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import Distribution.Version
         ( thisVersion )

-- ------------------------------------------------------------
-- * The freeze command
-- ------------------------------------------------------------

-- | Freeze all of the dependencies by writing a constraints section
-- constraining each dependency to an exact version.
--
freeze :: Verbosity
       -> PackageDBStack
       -> RepoContext
       -> Compiler
       -> Platform
       -> ProgramDb
       -> Maybe SandboxPackageInfo
       -> GlobalFlags
       -> FreezeFlags
       -> IO ()
freeze verbosity packageDBs repoCtxt comp platform progdb mSandboxPkgInfo
      globalFlags freezeFlags = do

    pkgs  <- getFreezePkgs
               verbosity packageDBs repoCtxt comp platform progdb mSandboxPkgInfo
               globalFlags freezeFlags

    if null pkgs
      then notice verbosity $ "No packages to be frozen. "
                           ++ "As this package has no dependencies."
      else if dryRun
             then notice verbosity $ unlines $
                     "The following packages would be frozen:"
                   : formatPkgs pkgs

             else freezePackages verbosity globalFlags pkgs

  where
    dryRun = fromFlag (freezeDryRun freezeFlags)

-- | Get the list of packages whose versions would be frozen by the @freeze@
-- command.
getFreezePkgs :: Verbosity
              -> PackageDBStack
              -> RepoContext
              -> Compiler
              -> Platform
              -> ProgramDb
              -> Maybe SandboxPackageInfo
              -> GlobalFlags
              -> FreezeFlags
              -> IO [SolverPlanPackage]
getFreezePkgs verbosity packageDBs repoCtxt comp platform progdb mSandboxPkgInfo
      globalFlags freezeFlags = do

    installedPkgIndex <- getInstalledPackages verbosity comp packageDBs progdb
    sourcePkgDb       <- getSourcePackages    verbosity repoCtxt
    pkgConfigDb       <- readPkgConfigDb      verbosity progdb

    pkgSpecifiers <- resolveUserTargets verbosity repoCtxt
                       (fromFlag $ globalWorldFile globalFlags)
                       (packageIndex sourcePkgDb)
                       [UserTargetLocalDir "."]

    sanityCheck pkgSpecifiers
    planPackages
               verbosity comp platform mSandboxPkgInfo freezeFlags
               installedPkgIndex sourcePkgDb pkgConfigDb pkgSpecifiers
  where
    sanityCheck pkgSpecifiers = do
      when (not . null $ [n | n@(NamedPackage _ _) <- pkgSpecifiers]) $
        die' verbosity $ "internal error: 'resolveUserTargets' returned "
           ++ "unexpected named package specifiers!"
      when (length pkgSpecifiers /= 1) $
        die' verbosity $ "internal error: 'resolveUserTargets' returned "
           ++ "unexpected source package specifiers!"

planPackages :: Verbosity
             -> Compiler
             -> Platform
             -> Maybe SandboxPackageInfo
             -> FreezeFlags
             -> InstalledPackageIndex
             -> SourcePackageDb
             -> PkgConfigDb
             -> [PackageSpecifier UnresolvedSourcePackage]
             -> IO [SolverPlanPackage]
planPackages verbosity comp platform mSandboxPkgInfo freezeFlags
             installedPkgIndex sourcePkgDb pkgConfigDb pkgSpecifiers = do

  solver <- chooseSolver verbosity
            (fromFlag (freezeSolver freezeFlags)) (compilerInfo comp)
  notice verbosity "Resolving dependencies..."

  installPlan <- foldProgress logMsg (die' verbosity) return $
                   resolveDependencies
                     platform (compilerInfo comp) pkgConfigDb
                     solver
                     resolverParams

  return $ pruneInstallPlan installPlan pkgSpecifiers

  where
    resolverParams =

        setMaxBackjumps (if maxBackjumps < 0 then Nothing
                                             else Just maxBackjumps)

      . setIndependentGoals independentGoals

      . setReorderGoals reorderGoals

      . setCountConflicts countConflicts

      . setShadowPkgs shadowPkgs

      . setStrongFlags strongFlags

      . setAllowBootLibInstalls allowBootLibInstalls

      . setSolverVerbosity verbosity

      . addConstraints
          [ let pkg = pkgSpecifierTarget pkgSpecifier
                pc = PackageConstraint (scopeToplevel pkg)
                                       (PackagePropertyStanzas stanzas)
            in LabeledPackageConstraint pc ConstraintSourceFreeze
          | pkgSpecifier <- pkgSpecifiers ]

      . maybe id applySandboxInstallPolicy mSandboxPkgInfo

      $ standardInstallPolicy installedPkgIndex sourcePkgDb pkgSpecifiers

    logMsg message rest = debug verbosity message >> rest

    stanzas = [ TestStanzas | testsEnabled ]
           ++ [ BenchStanzas | benchmarksEnabled ]
    testsEnabled      = fromFlagOrDefault False $ freezeTests freezeFlags
    benchmarksEnabled = fromFlagOrDefault False $ freezeBenchmarks freezeFlags

    reorderGoals     = fromFlag (freezeReorderGoals     freezeFlags)
    countConflicts   = fromFlag (freezeCountConflicts   freezeFlags)
    independentGoals = fromFlag (freezeIndependentGoals freezeFlags)
    shadowPkgs       = fromFlag (freezeShadowPkgs       freezeFlags)
    strongFlags      = fromFlag (freezeStrongFlags      freezeFlags)
    maxBackjumps     = fromFlag (freezeMaxBackjumps     freezeFlags)
    allowBootLibInstalls = fromFlag (freezeAllowBootLibInstalls freezeFlags)


-- | Remove all unneeded packages from an install plan.
--
-- A package is unneeded if it is either
--
-- 1) the package that we are freezing, or
--
-- 2) not a dependency (directly or transitively) of the package we are
--    freezing.  This is useful for removing previously installed packages
--    which are no longer required from the install plan.
--
-- Invariant: @pkgSpecifiers@ must refer to packages which are not
-- 'PreExisting' in the 'SolverInstallPlan'.
pruneInstallPlan :: SolverInstallPlan
                 -> [PackageSpecifier UnresolvedSourcePackage]
                 -> [SolverPlanPackage]
pruneInstallPlan installPlan pkgSpecifiers =
    removeSelf pkgIds $
    SolverInstallPlan.dependencyClosure installPlan pkgIds
  where
    pkgIds = [ PlannedId (packageId pkg)
             | SpecificSourcePackage pkg <- pkgSpecifiers ]
    removeSelf [thisPkg] = filter (\pp -> packageId pp /= packageId thisPkg)
    removeSelf _  = error $ "internal error: 'pruneInstallPlan' given "
                         ++ "unexpected package specifiers!"


freezePackages :: Package pkg => Verbosity -> GlobalFlags -> [pkg] -> IO ()
freezePackages verbosity globalFlags pkgs = do

    pkgEnv <- fmap (createPkgEnv . addFrozenConstraints) $
                   loadUserConfig verbosity ""
                   (flagToMaybe . globalConstraintsFile $ globalFlags)
    writeFileAtomic userPackageEnvironmentFile $ showPkgEnv pkgEnv
  where
    addFrozenConstraints config =
        config {
            savedConfigureExFlags = (savedConfigureExFlags config) {
                configExConstraints = map constraint pkgs
            }
        }
    constraint pkg =
        (pkgIdToConstraint $ packageId pkg
        ,ConstraintSourceUserConfig userPackageEnvironmentFile)
      where
        pkgIdToConstraint pkgId =
            UserConstraint (UserQualified UserQualToplevel (packageName pkgId))
                           (PackagePropertyVersion $ thisVersion (packageVersion pkgId))
    createPkgEnv config = mempty { pkgEnvSavedConfig = config }
    showPkgEnv = BS.Char8.pack . showPackageEnvironment


formatPkgs :: Package pkg => [pkg] -> [String]
formatPkgs = map $ showPkg . packageId
  where
    showPkg pid = name pid ++ " == " ++ version pid
    name = display . packageName
    version = display . packageVersion
