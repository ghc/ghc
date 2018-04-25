-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Fetch
-- Copyright   :  (c) David Himmelstrup 2005
--                    Duncan Coutts 2011
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- The cabal fetch command
-----------------------------------------------------------------------------
module Distribution.Client.Fetch (
    fetch,
  ) where

import Distribution.Client.Types
import Distribution.Client.Targets
import Distribution.Client.FetchUtils hiding (fetchPackage)
import Distribution.Client.Dependency
import Distribution.Client.IndexUtils as IndexUtils
         ( getSourcePackages, getInstalledPackages )
import qualified Distribution.Client.SolverInstallPlan as SolverInstallPlan
import Distribution.Client.Setup
         ( GlobalFlags(..), FetchFlags(..), RepoContext(..) )

import Distribution.Solver.Types.PkgConfigDb ( PkgConfigDb, readPkgConfigDb )
import Distribution.Solver.Types.SolverPackage
import Distribution.Solver.Types.SourcePackage

import Distribution.Package
         ( packageId )
import Distribution.Simple.Compiler
         ( Compiler, compilerInfo, PackageDBStack )
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.Program
         ( ProgramDb )
import Distribution.Simple.Setup
         ( fromFlag )
import Distribution.Simple.Utils
         ( die', notice, debug )
import Distribution.System
         ( Platform )
import Distribution.Text
         ( display )
import Distribution.Verbosity
         ( Verbosity )

import Control.Monad
         ( filterM )

-- ------------------------------------------------------------
-- * The fetch command
-- ------------------------------------------------------------

--TODO:
-- * add fetch -o support
-- * support tarball URLs via ad-hoc download cache (or in -o mode?)
-- * suggest using --no-deps, unpack or fetch -o if deps cannot be satisfied
-- * Port various flags from install:
--   * --updage-dependencies
--   * --constraint and --preference
--   * --only-dependencies, but note it conflicts with --no-deps


-- | Fetch a list of packages and their dependencies.
--
fetch :: Verbosity
      -> PackageDBStack
      -> RepoContext
      -> Compiler
      -> Platform
      -> ProgramDb
      -> GlobalFlags
      -> FetchFlags
      -> [UserTarget]
      -> IO ()
fetch verbosity _ _ _ _ _ _ _ [] =
    notice verbosity "No packages requested. Nothing to do."

fetch verbosity packageDBs repoCtxt comp platform progdb
      globalFlags fetchFlags userTargets = do

    mapM_ (checkTarget verbosity) userTargets

    installedPkgIndex <- getInstalledPackages verbosity comp packageDBs progdb
    sourcePkgDb       <- getSourcePackages    verbosity repoCtxt
    pkgConfigDb       <- readPkgConfigDb      verbosity progdb

    pkgSpecifiers <- resolveUserTargets verbosity repoCtxt
                       (fromFlag $ globalWorldFile globalFlags)
                       (packageIndex sourcePkgDb)
                       userTargets

    pkgs  <- planPackages
               verbosity comp platform fetchFlags
               installedPkgIndex sourcePkgDb pkgConfigDb pkgSpecifiers

    pkgs' <- filterM (fmap not . isFetched . packageSource) pkgs
    if null pkgs'
      --TODO: when we add support for remote tarballs then this message
      -- will need to be changed because for remote tarballs we fetch them
      -- at the earlier phase.
      then notice verbosity $ "No packages need to be fetched. "
                           ++ "All the requested packages are already local "
                           ++ "or cached locally."
      else if dryRun
             then notice verbosity $ unlines $
                     "The following packages would be fetched:"
                   : map (display . packageId) pkgs'

             else mapM_ (fetchPackage verbosity repoCtxt . packageSource) pkgs'

  where
    dryRun = fromFlag (fetchDryRun fetchFlags)

planPackages :: Verbosity
             -> Compiler
             -> Platform
             -> FetchFlags
             -> InstalledPackageIndex
             -> SourcePackageDb
             -> PkgConfigDb
             -> [PackageSpecifier UnresolvedSourcePackage]
             -> IO [UnresolvedSourcePackage]
planPackages verbosity comp platform fetchFlags
             installedPkgIndex sourcePkgDb pkgConfigDb pkgSpecifiers

  | includeDependencies = do
      solver <- chooseSolver verbosity
                (fromFlag (fetchSolver fetchFlags)) (compilerInfo comp)
      notice verbosity "Resolving dependencies..."
      installPlan <- foldProgress logMsg (die' verbosity) return $
                       resolveDependencies
                         platform (compilerInfo comp) pkgConfigDb
                         solver
                         resolverParams

      -- The packages we want to fetch are those packages the 'InstallPlan'
      -- that are in the 'InstallPlan.Configured' state.
      return
        [ solverPkgSource cpkg
        | (SolverInstallPlan.Configured cpkg)
            <- SolverInstallPlan.toList installPlan ]

  | otherwise =
      either (die' verbosity . unlines . map show) return $
        resolveWithoutDependencies resolverParams

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

        -- Reinstall the targets given on the command line so that the dep
        -- resolver will decide that they need fetching, even if they're
        -- already installed. Since we want to get the source packages of
        -- things we might have installed (but not have the sources for).
      . reinstallTargets

      $ standardInstallPolicy installedPkgIndex sourcePkgDb pkgSpecifiers

    includeDependencies = fromFlag (fetchDeps fetchFlags)
    logMsg message rest = debug verbosity message >> rest

    reorderGoals     = fromFlag (fetchReorderGoals     fetchFlags)
    countConflicts   = fromFlag (fetchCountConflicts   fetchFlags)
    independentGoals = fromFlag (fetchIndependentGoals fetchFlags)
    shadowPkgs       = fromFlag (fetchShadowPkgs       fetchFlags)
    strongFlags      = fromFlag (fetchStrongFlags      fetchFlags)
    maxBackjumps     = fromFlag (fetchMaxBackjumps     fetchFlags)
    allowBootLibInstalls = fromFlag (fetchAllowBootLibInstalls fetchFlags)


checkTarget :: Verbosity -> UserTarget -> IO ()
checkTarget verbosity target = case target of
    UserTargetRemoteTarball _uri
      -> die' verbosity $ "The 'fetch' command does not yet support remote tarballs. "
            ++ "In the meantime you can use the 'unpack' commands."
    _ -> return ()

fetchPackage :: Verbosity -> RepoContext -> PackageLocation a -> IO ()
fetchPackage verbosity repoCtxt pkgsrc = case pkgsrc of
    LocalUnpackedPackage _dir  -> return ()
    LocalTarballPackage  _file -> return ()

    RemoteTarballPackage _uri _ ->
      die' verbosity $ "The 'fetch' command does not yet support remote tarballs. "
         ++ "In the meantime you can use the 'unpack' commands."

    RepoTarballPackage repo pkgid _ -> do
      _ <- fetchRepoTarball verbosity repoCtxt repo pkgid
      return ()
