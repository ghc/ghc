-- | Functionality for invoking Haskell scripts with the correct
-- package database setup.
module Test.Cabal.Script (
    ScriptEnv(..),
    mkScriptEnv,
    runnerGhcArgs,
    runnerCommand,
    runghc,
) where

import Test.Cabal.Run

import Distribution.Backpack
import Distribution.Types.ModuleRenaming
import Distribution.Types.LocalBuildInfo
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.ComponentName
import Distribution.Types.UnqualComponentName
import Distribution.Utils.NubList
import Distribution.Simple.Program.Db
import Distribution.Simple.Program.Builtin
import Distribution.Simple.Program.GHC
import Distribution.Simple.Program
import Distribution.Simple.Compiler
import Distribution.Verbosity
import Distribution.System
import Distribution.Simple.Setup (Flag(..))

import System.Directory
import qualified Data.Monoid as M

-- | The runner environment, which contains all of the important
-- parameters for invoking GHC.  Mostly subset of 'LocalBuildInfo'.
data ScriptEnv = ScriptEnv
        { runnerProgramDb       :: ProgramDb
        , runnerPackageDbStack  :: PackageDBStack
        , runnerVerbosity       :: Verbosity
        , runnerPlatform        :: Platform
        , runnerCompiler        :: Compiler
        , runnerPackages        :: [(OpenUnitId, ModuleRenaming)]
        }

-- | Convert package database into absolute path, so that
-- if we change working directories in a subprocess we get the correct database.
canonicalizePackageDB :: PackageDB -> IO PackageDB
canonicalizePackageDB (SpecificPackageDB path)
    = SpecificPackageDB `fmap` canonicalizePath path
canonicalizePackageDB x = return x

-- | Create a 'ScriptEnv' from a 'LocalBuildInfo' configured with
-- the GHC that we want to use.
mkScriptEnv :: Verbosity -> LocalBuildInfo -> IO ScriptEnv
mkScriptEnv verbosity lbi = do
  package_db <- mapM canonicalizePackageDB (withPackageDB lbi)
  return $ ScriptEnv
    { runnerVerbosity       = verbosity
    , runnerProgramDb       = withPrograms  lbi
    , runnerPackageDbStack  = package_db
    , runnerPlatform        = hostPlatform  lbi
    , runnerCompiler        = compiler      lbi
    -- NB: the set of packages available to test.hs scripts will COINCIDE
    -- with the dependencies on the cabal-testsuite library
    , runnerPackages        = cabalTestsPackages   lbi
    }

-- | Compute the set of @-package-id@ flags which would be passed when
-- building the public library.  Assumes that the public library is
-- non-Backpack.
cabalTestsPackages :: LocalBuildInfo -> [(OpenUnitId, ModuleRenaming)]
cabalTestsPackages lbi =
    case componentNameCLBIs lbi (CExeName (mkUnqualComponentName "cabal-tests")) of
        [clbi] -> componentIncludes clbi
        _ -> error "cabalTestsPackages"

-- | Run a script with 'runghc', under the 'ScriptEnv'.
runghc :: ScriptEnv -> Maybe FilePath -> [(String, Maybe String)]
       -> FilePath -> [String] -> IO Result
runghc senv mb_cwd env_overrides script_path args = do
    (real_path, real_args) <- runnerCommand senv mb_cwd env_overrides script_path args
    run (runnerVerbosity senv) mb_cwd env_overrides real_path real_args

-- | Compute the command line which should be used to run a Haskell
-- script with 'runghc'.
runnerCommand :: ScriptEnv -> Maybe FilePath -> [(String, Maybe String)]
              -> FilePath -> [String] -> IO (FilePath, [String])
runnerCommand senv _mb_cwd _env_overrides script_path args = do
    (prog, _) <- requireProgram verbosity runghcProgram (runnerProgramDb senv)
    return (programPath prog,
            runghc_args ++ ["--"] ++ map ("--ghc-arg="++) ghc_args ++ [script_path] ++ args)
  where
    verbosity = runnerVerbosity senv
    runghc_args = []
    ghc_args = runnerGhcArgs senv

-- | Compute the GHC flags to invoke 'runghc' with under a 'ScriptEnv'.
runnerGhcArgs :: ScriptEnv -> [String]
runnerGhcArgs senv =
    renderGhcOptions (runnerCompiler senv) (runnerPlatform senv) ghc_options
  where
    ghc_options = M.mempty { ghcOptPackageDBs = runnerPackageDbStack senv
                           , ghcOptPackages   = toNubListR (runnerPackages senv)
                           -- Avoid picking stray module files that look
                           -- like our imports
                           , ghcOptSourcePathClear = Flag True }
