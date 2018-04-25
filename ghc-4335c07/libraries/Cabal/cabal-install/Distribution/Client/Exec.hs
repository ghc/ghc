-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Exec
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Implementation of the 'exec' command. Runs an arbitrary executable in an
-- environment suitable for making use of the sandbox.
-----------------------------------------------------------------------------

module Distribution.Client.Exec ( exec
                                ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import qualified Distribution.Simple.GHC   as GHC
import qualified Distribution.Simple.GHCJS as GHCJS

import Distribution.Client.Sandbox (getSandboxConfigFilePath)
import Distribution.Client.Sandbox.PackageEnvironment (sandboxPackageDBPath)
import Distribution.Client.Sandbox.Types              (UseSandbox (..))

import Distribution.Simple.Compiler    (Compiler, CompilerFlavor(..), compilerFlavor)
import Distribution.Simple.Program     (ghcProgram, ghcjsProgram, lookupProgram)
import Distribution.Simple.Program.Db  (ProgramDb, requireProgram, modifyProgramSearchPath)
import Distribution.Simple.Program.Find (ProgramSearchPathEntry(..))
import Distribution.Simple.Program.Run (programInvocation, runProgramInvocation)
import Distribution.Simple.Program.Types ( simpleProgram, ConfiguredProgram(..) )
import Distribution.Simple.Utils       (die', warn)

import Distribution.System    (Platform(..), OS(..), buildOS)
import Distribution.Verbosity (Verbosity)

import System.Directory ( doesDirectoryExist )
import System.Environment (lookupEnv)
import System.FilePath (searchPathSeparator, (</>))


-- | Execute the given command in the package's environment.
--
-- The given command is executed with GHC configured to use the correct
-- package database and with the sandbox bin directory added to the PATH.
exec :: Verbosity
     -> UseSandbox
     -> Compiler
     -> Platform
     -> ProgramDb
     -> [String]
     -> IO ()
exec verbosity useSandbox comp platform programDb extraArgs =
    case extraArgs of
        (exe:args) -> do
            program <- requireProgram' verbosity useSandbox programDb exe
            env <- environmentOverrides (programOverrideEnv program)
            let invocation = programInvocation
                                 program { programOverrideEnv = env }
                                 args
            runProgramInvocation verbosity invocation

        [] -> die' verbosity "Please specify an executable to run"
  where
    environmentOverrides env =
        case useSandbox of
            NoSandbox -> return env
            (UseSandbox sandboxDir) ->
                sandboxEnvironment verbosity sandboxDir comp platform programDb env


-- | Return the package's sandbox environment.
--
-- The environment sets GHC_PACKAGE_PATH so that GHC will use the sandbox.
sandboxEnvironment :: Verbosity
                   -> FilePath
                   -> Compiler
                   -> Platform
                   -> ProgramDb
                   -> [(String, Maybe String)] -- environment overrides so far
                   -> IO [(String, Maybe String)]
sandboxEnvironment verbosity sandboxDir comp platform programDb iEnv =
    case compilerFlavor comp of
      GHC   -> env GHC.getGlobalPackageDB   ghcProgram   "GHC_PACKAGE_PATH"
      GHCJS -> env GHCJS.getGlobalPackageDB ghcjsProgram "GHCJS_PACKAGE_PATH"
      _     -> die' verbosity "exec only works with GHC and GHCJS"
  where
    (Platform _ os) = platform
    ldPath = case os of
               OSX     -> "DYLD_LIBRARY_PATH"
               Windows -> "PATH"
               _       -> "LD_LIBRARY_PATH"
    env getGlobalPackageDB hcProgram packagePathEnvVar = do
        let Just program = lookupProgram hcProgram programDb
        gDb <- getGlobalPackageDB verbosity program
        sandboxConfigFilePath <- getSandboxConfigFilePath mempty
        let sandboxPackagePath   = sandboxPackageDBPath sandboxDir comp platform
            compilerPackagePaths = prependToSearchPath gDb sandboxPackagePath
        -- Packages database must exist, otherwise things will start
        -- failing in mysterious ways.
        exists <- doesDirectoryExist sandboxPackagePath
        unless exists $ warn verbosity $ "Package database is not a directory: "
                                           ++ sandboxPackagePath
        -- MASSIVE HACK.  We need this to be synchronized with installLibDir
        -- in defaultInstallDirs' in Distribution.Simple.InstallDirs,
        -- which has a special case for Windows (WHY? Who knows; it's been
        -- around as long as Windows exists.)  The sane thing to do here
        -- would be to read out the actual install dirs that were associated
        -- with the package in question, but that's not a well-formed question
        -- here because there is not actually install directory for the
        -- "entire" sandbox.  Since we want to kill this code in favor of
        -- new-build, I decided it wasn't worth fixing this "properly."
        -- Also, this doesn't handle LHC correctly but I don't care -- ezyang
        let extraLibPath =
                case buildOS of
                    Windows -> sandboxDir
                    _ -> sandboxDir </> "lib"
        -- 2016-11-26 Apologies for the spaghetti code here.
        -- Essentially we just want to add the sandbox's lib/ dir to
        -- whatever the library search path environment variable is:
        -- this allows running existing executables against foreign
        -- libraries (meaning Haskell code with a bunch of foreign
        -- exports). However, on Windows this variable is equal to the
        -- executable search path env var. And we try to keep not only
        -- what was already set in the environment, but also the
        -- additional directories we add below in requireProgram'. So
        -- the strategy is that we first take the environment
        -- overrides from requireProgram' below. If the library search
        -- path env is overridden (e.g. because we're on windows), we
        -- prepend the lib/ dir to the relevant override. If not, we
        -- want to avoid wiping the user's own settings, so we first
        -- read the env var's current value, and then prefix ours if
        -- the user had any set.
        iEnv' <-
          if any ((==ldPath) . fst) iEnv
            then return $ updateLdPath extraLibPath iEnv
            else do
              currentLibraryPath <- lookupEnv ldPath
              let updatedLdPath =
                    case currentLibraryPath of
                      Nothing -> Just extraLibPath
                      Just paths ->
                        Just $ extraLibPath ++ [searchPathSeparator] ++ paths
              return $ (ldPath, updatedLdPath) : iEnv

        -- Build the environment
        return $ [ (packagePathEnvVar, Just compilerPackagePaths)
                 , ("CABAL_SANDBOX_PACKAGE_PATH", Just compilerPackagePaths)
                 , ("CABAL_SANDBOX_CONFIG", Just sandboxConfigFilePath)
                 ] ++ iEnv'

    prependToSearchPath path newValue =
        newValue ++ [searchPathSeparator] ++ path

    updateLdPath path = map update
      where
        update (name, Just current)
          | name == ldPath = (ldPath, Just $ path ++ [searchPathSeparator] ++ current)
        update (name, Nothing)
          | name == ldPath = (ldPath, Just path)
        update x = x


-- | Check that a program is configured and available to be run. If
-- a sandbox is available check in the sandbox's directory.
requireProgram' :: Verbosity
                -> UseSandbox
                -> ProgramDb
                -> String
                -> IO ConfiguredProgram
requireProgram' verbosity useSandbox programDb exe = do
    (program, _) <- requireProgram
                        verbosity
                        (simpleProgram exe)
                        updateSearchPath
    return program
  where
    updateSearchPath =
        flip modifyProgramSearchPath programDb $ \searchPath ->
            case useSandbox of
                NoSandbox -> searchPath
                UseSandbox sandboxDir ->
                    ProgramSearchPathDir (sandboxDir </> "bin") : searchPath
