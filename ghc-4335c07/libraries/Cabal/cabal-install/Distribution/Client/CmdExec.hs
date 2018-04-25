-------------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Exec
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Implementation of the 'new-exec' command for running an arbitrary executable
-- in an environment suited to the part of the store built for a project.
-------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}
module Distribution.Client.CmdExec
  ( execAction
  , execCommand
  ) where

import Distribution.Client.DistDirLayout
  ( DistDirLayout(..)
  )
import Distribution.Client.InstallPlan
  ( GenericPlanPackage(..)
  , toGraph
  )
import Distribution.Client.Setup
  ( ConfigExFlags
  , ConfigFlags(configVerbosity)
  , GlobalFlags
  , InstallFlags
  )
import Distribution.Client.ProjectOrchestration
  ( ProjectBuildContext(..)
  , runProjectPreBuildPhase
  , establishProjectBaseContext
  , distDirLayout
  , commandLineFlagsToProjectConfig
  , ProjectBaseContext(..)
  )
import Distribution.Client.ProjectPlanOutput
  ( updatePostBuildProjectStatus
  , createPackageEnvironment
  , argsEquivalentOfGhcEnvironmentFile
  , PostBuildProjectStatus
  )
import qualified Distribution.Client.ProjectPlanning as Planning
import Distribution.Client.ProjectPlanning
  ( ElaboratedInstallPlan
  , ElaboratedSharedConfig(..)
  )
import Distribution.Simple.Command
  ( CommandUI(..)
  )
import Distribution.Simple.Program.Db
  ( modifyProgramSearchPath
  , requireProgram
  , configuredPrograms
  )
import Distribution.Simple.Program.Find
  ( ProgramSearchPathEntry(..)
  )
import Distribution.Simple.Program.Run
  ( programInvocation
  , runProgramInvocation
  )
import Distribution.Simple.Program.Types
  ( programOverrideEnv
  , programDefaultArgs
  , programPath
  , simpleProgram
  , ConfiguredProgram
  )
import Distribution.Simple.GHC
  ( getImplInfo
  , GhcImplInfo(supportsPkgEnvFiles) )
import Distribution.Simple.Setup
  ( HaddockFlags
  , fromFlagOrDefault
  )
import Distribution.Simple.Utils
  ( die'
  , info
  , withTempDirectory
  , wrapText
  )
import Distribution.Verbosity
  ( Verbosity
  , normal
  )

import qualified Distribution.Client.CmdBuild as CmdBuild

import Prelude ()
import Distribution.Client.Compat.Prelude

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M

execCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
execCommand = CommandUI
  { commandName = "new-exec"
  , commandSynopsis = "Give a command access to the store."
  , commandUsage = \pname ->
    "Usage: " ++ pname ++ " new-exec [FLAGS] [--] COMMAND [--] [ARGS]\n"
  , commandDescription = Just $ \pname -> wrapText $
       "During development it is often useful to run build tasks and perform"
    ++ " one-off program executions to experiment with the behavior of build"
    ++ " tools. It is convenient to run these tools in the same way " ++ pname
    ++ " itself would. The `" ++ pname ++ " new-exec` command provides a way to"
    ++ " do so.\n"
    ++ "\n"
    ++ "Compiler tools will be configured to see the same subset of the store"
    ++ " that builds would see. The PATH is modified to make all executables in"
    ++ " the dependency tree available (provided they have been built already)."
    ++ " Commands are also rewritten in the way cabal itself would. For"
    ++ " example, `" ++ pname ++ " new-exec ghc` will consult the configuration"
    ++ " to choose an appropriate version of ghc and to include any"
    ++ " ghc-specific flags requested."
  , commandNotes = Nothing
  , commandOptions = commandOptions CmdBuild.buildCommand
  , commandDefaultFlags = commandDefaultFlags CmdBuild.buildCommand
  }

execAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
           -> [String] -> GlobalFlags -> IO ()
execAction (configFlags, configExFlags, installFlags, haddockFlags)
           extraArgs globalFlags = do

  baseCtx <- establishProjectBaseContext verbosity cliConfig

  -- To set up the environment, we'd like to select the libraries in our
  -- dependency tree that we've already built. So first we set up an install
  -- plan, but we walk the dependency tree without first executing the plan.
  buildCtx <- runProjectPreBuildPhase
    verbosity
    baseCtx
    (\plan -> return (plan, M.empty))

  -- We use the build status below to decide what libraries to include in the
  -- compiler environment, but we don't want to actually build anything. So we
  -- pass mempty to indicate that nothing happened and we just want the current
  -- status.
  buildStatus <- updatePostBuildProjectStatus
    verbosity
    (distDirLayout baseCtx)
    (elaboratedPlanOriginal buildCtx)
    (pkgsBuildStatus buildCtx)
    mempty

  -- Some dependencies may have executables. Let's put those on the PATH.
  extraPaths <- pathAdditions verbosity baseCtx buildCtx
  let programDb = modifyProgramSearchPath
                  (map ProgramSearchPathDir extraPaths ++)
                . pkgConfigCompilerProgs
                . elaboratedShared
                $ buildCtx

  -- Now that we have the packages, set up the environment. We accomplish this
  -- by creating an environment file that selects the databases and packages we
  -- computed in the previous step, and setting an environment variable to
  -- point at the file.
  -- In case ghc is too old to support environment files,
  -- we pass the same info as arguments
  let compiler = pkgConfigCompiler $ elaboratedShared buildCtx
      envFilesSupported = supportsPkgEnvFiles (getImplInfo compiler)
  case extraArgs of
    [] -> die' verbosity "Please specify an executable to run"
    exe:args -> do
      (program, _) <- requireProgram verbosity (simpleProgram exe) programDb
      let argOverrides =
            argsEquivalentOfGhcEnvironmentFile
              compiler
              (distDirLayout baseCtx)
              (elaboratedPlanOriginal buildCtx)
              buildStatus
          programIsConfiguredCompiler = matchCompilerPath
                                          (elaboratedShared buildCtx)
                                          program
          argOverrides' =
            if envFilesSupported
            || not programIsConfiguredCompiler
            then []
            else argOverrides

      (if envFilesSupported
      then withTempEnvFile verbosity baseCtx buildCtx buildStatus
      else \f -> f []) $ \envOverrides -> do
        let program'   = withOverrides
                           envOverrides
                           argOverrides'
                           program
            invocation = programInvocation program' args
        runProgramInvocation verbosity invocation
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags configExFlags
                  installFlags haddockFlags
    withOverrides env args program = program
      { programOverrideEnv = programOverrideEnv program ++ env
      , programDefaultArgs = programDefaultArgs program ++ args}

matchCompilerPath :: ElaboratedSharedConfig -> ConfiguredProgram -> Bool
matchCompilerPath elaboratedShared program =
  programPath program
  `elem`
  (programPath <$> configuredCompilers)
  where
    configuredCompilers = configuredPrograms $ pkgConfigCompilerProgs elaboratedShared

-- | Execute an action with a temporary .ghc.environment file reflecting the
-- current environment. The action takes an environment containing the env
-- variable which points ghc to the file.
withTempEnvFile :: Verbosity
                -> ProjectBaseContext
                -> ProjectBuildContext
                -> PostBuildProjectStatus
                -> ([(String, Maybe String)] -> IO a)
                -> IO a
withTempEnvFile verbosity
                baseCtx
                buildCtx
                buildStatus
                action =
  withTempDirectory
   verbosity
   (distTempDirectory (distDirLayout baseCtx))
   "environment."
   (\tmpDir -> do
     envOverrides <- createPackageEnvironment
       verbosity
       tmpDir
       (elaboratedPlanToExecute buildCtx)
       (elaboratedShared buildCtx)
       buildStatus
     action envOverrides)

pathAdditions :: Verbosity -> ProjectBaseContext -> ProjectBuildContext -> IO [FilePath]
pathAdditions verbosity ProjectBaseContext{..}ProjectBuildContext{..} = do
  info verbosity . unlines $ "Including the following directories in PATH:"
                           : paths
  return paths
  where
  paths = S.toList
        $ binDirectories distDirLayout elaboratedShared elaboratedPlanToExecute

binDirectories
  :: DistDirLayout
  -> ElaboratedSharedConfig
  -> ElaboratedInstallPlan
  -> Set FilePath
binDirectories layout config = fromElaboratedInstallPlan where
  fromElaboratedInstallPlan = fromGraph . toGraph
  fromGraph = foldMap fromPlan
  fromSrcPkg = S.fromList . Planning.binDirectories layout config

  fromPlan (PreExisting _) = mempty
  fromPlan (Configured pkg) = fromSrcPkg pkg
  fromPlan (Installed pkg) = fromSrcPkg pkg

