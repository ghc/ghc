{-# LANGUAGE ViewPatterns #-}
-- | cabal-install CLI command: configure
--
module Distribution.Client.CmdConfigure (
    configureCommand,
    configureAction,
  ) where

import System.Directory
import Control.Monad
import qualified Data.Map as Map

import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectConfig
         ( writeProjectLocalExtraConfig )

import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags
         , applyFlagDefaults )
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault )
import Distribution.Verbosity
         ( normal )

import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Simple.Utils
         ( wrapText, notice )
import qualified Distribution.Client.Setup as Client

configureCommand :: CommandUI (ConfigFlags, ConfigExFlags
                              ,InstallFlags, HaddockFlags)
configureCommand = Client.installCommand {
  commandName         = "new-configure",
  commandSynopsis     = "Add extra project configuration",
  commandUsage        = usageAlternatives "new-configure" [ "[FLAGS]" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Adjust how the project is built by setting additional package flags "
     ++ "and other flags.\n\n"

     ++ "The configuration options are written to the 'cabal.project.local' "
     ++ "file (or '$project_file.local', if '--project-file' is specified) "
     ++ "which extends the configuration from the 'cabal.project' file "
     ++ "(if any). This combination is used as the project configuration for "
     ++ "all other commands (such as 'new-build', 'new-repl' etc) though it "
     ++ "can be extended/overridden on a per-command basis.\n\n"

     ++ "The new-configure command also checks that the project configuration "
     ++ "will work. In particular it checks that there is a consistent set of "
     ++ "dependencies for the project as a whole.\n\n"

     ++ "The 'cabal.project.local' file persists across 'new-clean' but is "
     ++ "overwritten on the next use of the 'new-configure' command. The "
     ++ "intention is that the 'cabal.project' file should be kept in source "
     ++ "control but the 'cabal.project.local' should not.\n\n"

     ++ "It is never necessary to use the 'new-configure' command. It is "
     ++ "merely a convenience in cases where you do not want to specify flags "
     ++ "to 'new-build' (and other commands) every time and yet do not want "
     ++ "to alter the 'cabal.project' persistently.",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-configure --with-compiler ghc-7.10.3\n"
     ++ "    Adjust the project configuration to use the given compiler\n"
     ++ "    program and check the resulting configuration works.\n"
     ++ "  " ++ pname ++ " new-configure\n"
     ++ "    Reset the local configuration to empty and check the overall\n"
     ++ "    project configuration works.\n\n"

     ++ cmdCommonHelpTextNewBuildBeta
   }

-- | To a first approximation, the @configure@ just runs the first phase of
-- the @build@ command where we bring the install plan up to date (thus
-- checking that it's possible).
--
-- The only difference is that @configure@ also allows the user to specify
-- some extra config flags which we save in the file @cabal.project.local@.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
configureAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
                -> [String] -> GlobalFlags -> IO ()
configureAction (applyFlagDefaults -> (configFlags, configExFlags, installFlags, haddockFlags))
                _extraArgs globalFlags = do
    --TODO: deal with _extraArgs, since flags with wrong syntax end up there

    baseCtx <- establishProjectBaseContext verbosity cliConfig

    -- Write out the @cabal.project.local@ so it gets picked up by the
    -- planning phase. If old config exists, then print the contents
    -- before overwriting
    exists <- doesFileExist "cabal.project.local"
    when exists $ do
        notice verbosity "'cabal.project.local' file already exists. Now overwriting it."
        copyFile "cabal.project.local" "cabal.project.local~"
    writeProjectLocalExtraConfig (distDirLayout baseCtx)
                                 cliConfig

    buildCtx <-
      runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan ->

            -- TODO: Select the same subset of targets as 'CmdBuild' would
            -- pick (ignoring, for example, executables in libraries
            -- we depend on). But we don't want it to fail, so actually we
            -- have to do it slightly differently from build.
            return (elaboratedPlan, Map.empty)

    let baseCtx' = baseCtx {
                      buildSettings = (buildSettings baseCtx) {
                        buildSettingDryRun = True
                      }
                    }

    -- TODO: Hmm, but we don't have any targets. Currently this prints
    -- what we would build if we were to build everything. Could pick
    -- implicit target like "."
    --
    -- TODO: should we say what's in the project (+deps) as a whole?
    printPlan verbosity baseCtx' buildCtx
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags configExFlags
                  installFlags haddockFlags

