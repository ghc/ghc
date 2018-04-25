{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Entry point to the default cabal-install front-end.
-----------------------------------------------------------------------------

module Main (main) where

import Distribution.Client.Setup
         ( GlobalFlags(..), globalCommand, withRepoContext
         , ConfigFlags(..)
         , ConfigExFlags(..), defaultConfigExFlags, configureExCommand
         , reconfigureCommand
         , configCompilerAux', configPackageDB'
         , BuildFlags(..), BuildExFlags(..), SkipAddSourceDepsCheck(..)
         , buildCommand, replCommand, testCommand, benchmarkCommand
         , InstallFlags(..), defaultInstallFlags
         , installCommand, upgradeCommand, uninstallCommand
         , FetchFlags(..), fetchCommand
         , FreezeFlags(..), freezeCommand
         , genBoundsCommand
         , OutdatedFlags(..), outdatedCommand
         , GetFlags(..), getCommand, unpackCommand
         , checkCommand
         , formatCommand
         , UpdateFlags(..), updateCommand
         , ListFlags(..), listCommand
         , InfoFlags(..), infoCommand
         , UploadFlags(..), uploadCommand
         , ReportFlags(..), reportCommand
         , runCommand
         , InitFlags(initVerbosity), initCommand
         , SDistFlags(..), SDistExFlags(..), sdistCommand
         , Win32SelfUpgradeFlags(..), win32SelfUpgradeCommand
         , ActAsSetupFlags(..), actAsSetupCommand
         , SandboxFlags(..), sandboxCommand
         , ExecFlags(..), execCommand
         , UserConfigFlags(..), userConfigCommand
         , reportCommand
         , manpageCommand
         )
import Distribution.Simple.Setup
         ( HaddockTarget(..)
         , DoctestFlags(..), doctestCommand
         , HaddockFlags(..), haddockCommand, defaultHaddockFlags
         , HscolourFlags(..), hscolourCommand
         , ReplFlags(..)
         , CopyFlags(..), copyCommand
         , RegisterFlags(..), registerCommand
         , CleanFlags(..), cleanCommand
         , TestFlags(..), BenchmarkFlags(..)
         , Flag(..), fromFlag, fromFlagOrDefault, flagToMaybe, toFlag
         , configAbsolutePaths
         )

import Prelude ()
import Distribution.Solver.Compat.Prelude hiding (get)

import Distribution.Client.SetupWrapper
         ( setupWrapper, SetupScriptOptions(..), defaultSetupScriptOptions )
import Distribution.Client.Config
         ( SavedConfig(..), loadConfig, defaultConfigFile, userConfigDiff
         , userConfigUpdate, createDefaultConfigFile, getConfigFilePath )
import Distribution.Client.Targets
         ( readUserTargets )
import qualified Distribution.Client.List as List
         ( list, info )


import qualified Distribution.Client.CmdConfigure as CmdConfigure
import qualified Distribution.Client.CmdUpdate    as CmdUpdate
import qualified Distribution.Client.CmdBuild     as CmdBuild
import qualified Distribution.Client.CmdRepl      as CmdRepl
import qualified Distribution.Client.CmdFreeze    as CmdFreeze
import qualified Distribution.Client.CmdHaddock   as CmdHaddock
import qualified Distribution.Client.CmdInstall   as CmdInstall
import qualified Distribution.Client.CmdRun       as CmdRun
import qualified Distribution.Client.CmdTest      as CmdTest
import qualified Distribution.Client.CmdBench     as CmdBench
import qualified Distribution.Client.CmdExec      as CmdExec

import Distribution.Client.Install            (install)
import Distribution.Client.Configure          (configure, writeConfigFlags)
import Distribution.Client.Update             (update)
import Distribution.Client.Exec               (exec)
import Distribution.Client.Fetch              (fetch)
import Distribution.Client.Freeze             (freeze)
import Distribution.Client.GenBounds          (genBounds)
import Distribution.Client.Outdated           (outdated)
import Distribution.Client.Check as Check     (check)
--import Distribution.Client.Clean            (clean)
import qualified Distribution.Client.Upload as Upload
import Distribution.Client.Run                (run, splitRunArgs)
import Distribution.Client.SrcDist            (sdist)
import Distribution.Client.Get                (get)
import Distribution.Client.Reconfigure        (Check(..), reconfigure)
import Distribution.Client.Nix                (nixInstantiate
                                              ,nixShell
                                              ,nixShellIfSandboxed)
import Distribution.Client.Sandbox            (sandboxInit
                                              ,sandboxAddSource
                                              ,sandboxDelete
                                              ,sandboxDeleteSource
                                              ,sandboxListSources
                                              ,sandboxHcPkg
                                              ,dumpPackageEnvironment

                                              ,loadConfigOrSandboxConfig
                                              ,findSavedDistPref
                                              ,initPackageDBIfNeeded
                                              ,maybeWithSandboxDirOnSearchPath
                                              ,maybeWithSandboxPackageInfo
                                              ,tryGetIndexFilePath
                                              ,sandboxBuildDir
                                              ,updateSandboxConfigFileFlag
                                              ,updateInstallDirs

                                              ,getPersistOrConfigCompiler)
import Distribution.Client.Sandbox.PackageEnvironment (setPackageDB)
import Distribution.Client.Sandbox.Timestamp  (maybeAddCompilerTimestampRecord)
import Distribution.Client.Sandbox.Types      (UseSandbox(..), whenUsingSandbox)
import Distribution.Client.Tar                (createTarGzFile)
import Distribution.Client.Types              (Password (..))
import Distribution.Client.Init               (initCabal)
import Distribution.Client.Manpage            (manpage)
import qualified Distribution.Client.Win32SelfUpgrade as Win32SelfUpgrade
import Distribution.Client.Utils              (determineNumJobs
#if defined(mingw32_HOST_OS)
                                              ,relaxEncodingErrors
#endif
                                              )

import Distribution.Package (packageId)
import Distribution.PackageDescription
         ( BuildType(..), Executable(..), buildable )
import Distribution.PackageDescription.Parsec ( readGenericPackageDescription )

import Distribution.PackageDescription.PrettyPrint
         ( writeGenericPackageDescription )
import qualified Distribution.Simple as Simple
import qualified Distribution.Make as Make
import qualified Distribution.Types.UnqualComponentName as Make
import Distribution.Simple.Build
         ( startInterpreter )
import Distribution.Simple.Command
         ( CommandParse(..), CommandUI(..), Command, CommandSpec(..)
         , CommandType(..), commandsRun, commandAddAction, hiddenCommand
         , commandFromSpec, commandShowOptions )
import Distribution.Simple.Compiler (Compiler(..), PackageDBStack)
import Distribution.Simple.Configure
         ( configCompilerAuxEx, ConfigStateFileError(..)
         , getPersistBuildConfig, interpretPackageDbFlags
         , tryGetPersistBuildConfig )
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Program (defaultProgramDb
                                   ,configureAllKnownPrograms
                                   ,simpleProgramInvocation
                                   ,getProgramInvocationOutput)
import Distribution.Simple.Program.Db (reconfigurePrograms)
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Utils
         ( cabalVersion, die', dieNoVerbosity, info, notice, topHandler
         , findPackageDesc, tryFindPackageDesc )
import Distribution.Text
         ( display )
import Distribution.Verbosity as Verbosity
         ( Verbosity, normal )
import Distribution.Version
         ( Version, mkVersion, orLaterVersion )
import qualified Paths_cabal_install (version)

import System.Environment       (getArgs, getProgName)
import System.Exit              (exitFailure, exitSuccess)
import System.FilePath          ( dropExtension, splitExtension
                                , takeExtension, (</>), (<.>))
import System.IO                ( BufferMode(LineBuffering), hSetBuffering
#ifdef mingw32_HOST_OS
                                , stderr
#endif
                                , stdout )
import System.Directory         (doesFileExist, getCurrentDirectory)
import Data.Monoid              (Any(..))
import Control.Exception        (SomeException(..), try)
import Control.Monad            (mapM_)

#ifdef MONOLITHIC
import qualified UnitTests
import qualified MemoryUsageTests
import qualified SolverQuickCheck
import qualified IntegrationTests2
import qualified System.Environment as Monolithic
#endif

-- | Entry point
--
main :: IO ()
#ifdef MONOLITHIC
main = do
    mb_exec <- Monolithic.lookupEnv "CABAL_INSTALL_MONOLITHIC_MODE"
    case mb_exec of
        Just "UnitTests"         -> UnitTests.main
        Just "MemoryUsageTests"  -> MemoryUsageTests.main
        Just "SolverQuickCheck"  -> SolverQuickCheck.main
        Just "IntegrationTests2" -> IntegrationTests2.main
        Just s -> error $ "Unrecognized mode '" ++ show s ++ "' in CABAL_INSTALL_MONOLITHIC_MODE"
        Nothing -> main'
#else
main = main'
#endif

main' :: IO ()
main' = do
  -- Enable line buffering so that we can get fast feedback even when piped.
  -- This is especially important for CI and build systems.
  hSetBuffering stdout LineBuffering
  -- The default locale encoding for Windows CLI is not UTF-8 and printing
  -- Unicode characters to it will fail unless we relax the handling of encoding
  -- errors when writing to stderr and stdout.
#ifdef mingw32_HOST_OS
  relaxEncodingErrors stdout
  relaxEncodingErrors stderr
#endif
  getArgs >>= mainWorker

mainWorker :: [String] -> IO ()
mainWorker args = topHandler $
  case commandsRun (globalCommand commands) commands args of
    CommandHelp   help                 -> printGlobalHelp help
    CommandList   opts                 -> printOptionsList opts
    CommandErrors errs                 -> printErrors errs
    CommandReadyToGo (globalFlags, commandParse)  ->
      case commandParse of
        _ | fromFlagOrDefault False (globalVersion globalFlags)
            -> printVersion
          | fromFlagOrDefault False (globalNumericVersion globalFlags)
            -> printNumericVersion
        CommandHelp     help           -> printCommandHelp help
        CommandList     opts           -> printOptionsList opts
        CommandErrors   errs           -> printErrors errs
        CommandReadyToGo action        -> do
          globalFlags' <- updateSandboxConfigFileFlag globalFlags
          action globalFlags'

  where
    printCommandHelp help = do
      pname <- getProgName
      putStr (help pname)
    printGlobalHelp help = do
      pname <- getProgName
      configFile <- defaultConfigFile
      putStr (help pname)
      putStr $ "\nYou can edit the cabal configuration file to set defaults:\n"
            ++ "  " ++ configFile ++ "\n"
      exists <- doesFileExist configFile
      unless exists $
          putStrLn $ "This file will be generated with sensible "
                  ++ "defaults if you run 'cabal update'."
    printOptionsList = putStr . unlines
    printErrors errs = dieNoVerbosity $ intercalate "\n" errs
    printNumericVersion = putStrLn $ display Paths_cabal_install.version
    printVersion        = putStrLn $ "cabal-install version "
                                  ++ display Paths_cabal_install.version
                                  ++ "\ncompiled using version "
                                  ++ display cabalVersion
                                  ++ " of the Cabal library "

    commands = map commandFromSpec commandSpecs
    commandSpecs =
      [ regularCmd installCommand installAction
      , regularCmd updateCommand updateAction
      , regularCmd listCommand listAction
      , regularCmd infoCommand infoAction
      , regularCmd fetchCommand fetchAction
      , regularCmd freezeCommand freezeAction
      , regularCmd getCommand getAction
      , hiddenCmd  unpackCommand unpackAction
      , regularCmd checkCommand checkAction
      , regularCmd sdistCommand sdistAction
      , regularCmd uploadCommand uploadAction
      , regularCmd reportCommand reportAction
      , regularCmd runCommand runAction
      , regularCmd initCommand initAction
      , regularCmd configureExCommand configureAction
      , regularCmd reconfigureCommand reconfigureAction
      , regularCmd buildCommand buildAction
      , regularCmd replCommand replAction
      , regularCmd sandboxCommand sandboxAction
      , regularCmd doctestCommand doctestAction
      , regularCmd haddockCommand haddockAction
      , regularCmd execCommand execAction
      , regularCmd userConfigCommand userConfigAction
      , regularCmd cleanCommand cleanAction
      , regularCmd genBoundsCommand genBoundsAction
      , regularCmd outdatedCommand outdatedAction
      , wrapperCmd copyCommand copyVerbosity copyDistPref
      , wrapperCmd hscolourCommand hscolourVerbosity hscolourDistPref
      , wrapperCmd registerCommand regVerbosity regDistPref
      , regularCmd testCommand testAction
      , regularCmd benchmarkCommand benchmarkAction
      , hiddenCmd  uninstallCommand uninstallAction
      , hiddenCmd  formatCommand formatAction
      , hiddenCmd  upgradeCommand upgradeAction
      , hiddenCmd  win32SelfUpgradeCommand win32SelfUpgradeAction
      , hiddenCmd  actAsSetupCommand actAsSetupAction
      , hiddenCmd  manpageCommand (manpageAction commandSpecs)

      , regularCmd  CmdConfigure.configureCommand CmdConfigure.configureAction
      , regularCmd  CmdUpdate.updateCommand       CmdUpdate.updateAction
      , regularCmd  CmdBuild.buildCommand         CmdBuild.buildAction
      , regularCmd  CmdRepl.replCommand           CmdRepl.replAction
      , regularCmd  CmdFreeze.freezeCommand       CmdFreeze.freezeAction
      , regularCmd  CmdHaddock.haddockCommand     CmdHaddock.haddockAction
      , regularCmd  CmdInstall.installCommand     CmdInstall.installAction
      , regularCmd  CmdRun.runCommand             CmdRun.runAction
      , regularCmd  CmdTest.testCommand           CmdTest.testAction
      , regularCmd  CmdBench.benchCommand         CmdBench.benchAction
      , regularCmd  CmdExec.execCommand           CmdExec.execAction
      ]

type Action = GlobalFlags -> IO ()

regularCmd :: CommandUI flags -> (flags -> [String] -> action)
           -> CommandSpec action
regularCmd ui action =
  CommandSpec ui ((flip commandAddAction) action) NormalCommand

hiddenCmd :: CommandUI flags -> (flags -> [String] -> action)
          -> CommandSpec action
hiddenCmd ui action =
  CommandSpec ui (\ui' -> hiddenCommand (commandAddAction ui' action))
  HiddenCommand

wrapperCmd :: Monoid flags => CommandUI flags -> (flags -> Flag Verbosity)
           -> (flags -> Flag String) -> CommandSpec Action
wrapperCmd ui verbosity distPref =
  CommandSpec ui (\ui' -> wrapperAction ui' verbosity distPref) NormalCommand

wrapperAction :: Monoid flags
              => CommandUI flags
              -> (flags -> Flag Verbosity)
              -> (flags -> Flag String)
              -> Command Action
wrapperAction command verbosityFlag distPrefFlag =
  commandAddAction command
    { commandDefaultFlags = mempty } $ \flags extraArgs globalFlags -> do
    let verbosity = fromFlagOrDefault normal (verbosityFlag flags)
    load <- try (loadConfigOrSandboxConfig verbosity globalFlags)
    let config = either (\(SomeException _) -> mempty) snd load
    distPref <- findSavedDistPref config (distPrefFlag flags)
    let setupScriptOptions = defaultSetupScriptOptions { useDistPref = distPref }
    setupWrapper verbosity setupScriptOptions Nothing
                 command (const flags) extraArgs

configureAction :: (ConfigFlags, ConfigExFlags)
                -> [String] -> Action
configureAction (configFlags, configExFlags) extraArgs globalFlags = do
  let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
  (useSandbox, config) <- updateInstallDirs (configUserInstall configFlags)
                          <$> loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (configDistPref configFlags)
  nixInstantiate verbosity distPref True globalFlags config
  nixShell verbosity distPref globalFlags config $ do
    let configFlags'   = savedConfigureFlags   config `mappend` configFlags
        configExFlags' = savedConfigureExFlags config `mappend` configExFlags
        globalFlags'   = savedGlobalFlags      config `mappend` globalFlags
    (comp, platform, progdb) <- configCompilerAuxEx configFlags'

    -- If we're working inside a sandbox and the user has set the -w option, we
    -- may need to create a sandbox-local package DB for this compiler and add a
    -- timestamp record for this compiler to the timestamp file.
    let configFlags''  = case useSandbox of
          NoSandbox               -> configFlags'
          (UseSandbox sandboxDir) -> setPackageDB sandboxDir
                                    comp platform configFlags'

    writeConfigFlags verbosity distPref (configFlags'', configExFlags')

    -- What package database(s) to use
    let packageDBs :: PackageDBStack
        packageDBs
          = interpretPackageDbFlags
            (fromFlag (configUserInstall configFlags''))
            (configPackageDBs configFlags'')

    whenUsingSandbox useSandbox $ \sandboxDir -> do
      initPackageDBIfNeeded verbosity configFlags'' comp progdb
      -- NOTE: We do not write the new sandbox package DB location to
      -- 'cabal.sandbox.config' here because 'configure -w' must not affect
      -- subsequent 'install' (for UI compatibility with non-sandboxed mode).

      indexFile     <- tryGetIndexFilePath verbosity config
      maybeAddCompilerTimestampRecord verbosity sandboxDir indexFile
        (compilerId comp) platform

    maybeWithSandboxDirOnSearchPath useSandbox $
      withRepoContext verbosity globalFlags' $ \repoContext ->
        configure verbosity packageDBs repoContext
                  comp platform progdb configFlags'' configExFlags' extraArgs

reconfigureAction :: (ConfigFlags, ConfigExFlags)
                  -> [String] -> Action
reconfigureAction flags@(configFlags, _) _ globalFlags = do
  let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
  (useSandbox, config) <- updateInstallDirs (configUserInstall configFlags)
                          <$> loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (configDistPref configFlags)
  let checkFlags = Check $ \_ saved -> do
        let flags' = saved <> flags
        unless (saved == flags') $ info verbosity message
        pure (Any True, flags')
        where
          -- This message is correct, but not very specific: it will list all
          -- of the new flags, even if some have not actually changed. The
          -- *minimal* set of changes is more difficult to determine.
          message =
            "flags changed: "
            ++ unwords (commandShowOptions configureExCommand flags)
  nixInstantiate verbosity distPref True globalFlags config
  _ <-
    reconfigure configureAction
    verbosity distPref useSandbox DontSkipAddSourceDepsCheck NoFlag
    checkFlags [] globalFlags config
  pure ()

buildAction :: (BuildFlags, BuildExFlags) -> [String] -> Action
buildAction (buildFlags, buildExFlags) extraArgs globalFlags = do
  let verbosity = fromFlagOrDefault normal (buildVerbosity buildFlags)
      noAddSource = fromFlagOrDefault DontSkipAddSourceDepsCheck
                    (buildOnly buildExFlags)
  (useSandbox, config) <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (buildDistPref buildFlags)
  -- Calls 'configureAction' to do the real work, so nothing special has to be
  -- done to support sandboxes.
  config' <-
    reconfigure configureAction
    verbosity distPref useSandbox noAddSource (buildNumJobs buildFlags)
    mempty [] globalFlags config
  nixShell verbosity distPref globalFlags config $ do
    maybeWithSandboxDirOnSearchPath useSandbox $
      build verbosity config' distPref buildFlags extraArgs


-- | Actually do the work of building the package. This is separate from
-- 'buildAction' so that 'testAction' and 'benchmarkAction' do not invoke
-- 'reconfigure' twice.
build :: Verbosity -> SavedConfig -> FilePath -> BuildFlags -> [String] -> IO ()
build verbosity config distPref buildFlags extraArgs =
  setupWrapper verbosity setupOptions Nothing
               (Cabal.buildCommand progDb) mkBuildFlags extraArgs
  where
    progDb       = defaultProgramDb
    setupOptions = defaultSetupScriptOptions { useDistPref = distPref }

    mkBuildFlags version = filterBuildFlags version config buildFlags'
    buildFlags' = buildFlags
      { buildVerbosity = toFlag verbosity
      , buildDistPref  = toFlag distPref
      }

-- | Make sure that we don't pass new flags to setup scripts compiled against
-- old versions of Cabal.
filterBuildFlags :: Version -> SavedConfig -> BuildFlags -> BuildFlags
filterBuildFlags version config buildFlags
  | version >= mkVersion [1,19,1] = buildFlags_latest
  -- Cabal < 1.19.1 doesn't support 'build -j'.
  | otherwise                      = buildFlags_pre_1_19_1
  where
    buildFlags_pre_1_19_1 = buildFlags {
      buildNumJobs = NoFlag
      }
    buildFlags_latest     = buildFlags {
      -- Take the 'jobs' setting '~/.cabal/config' into account.
      buildNumJobs = Flag . Just . determineNumJobs $
                     (numJobsConfigFlag `mappend` numJobsCmdLineFlag)
      }
    numJobsConfigFlag  = installNumJobs . savedInstallFlags $ config
    numJobsCmdLineFlag = buildNumJobs buildFlags


replAction :: (ReplFlags, BuildExFlags) -> [String] -> Action
replAction (replFlags, buildExFlags) extraArgs globalFlags = do
  let verbosity = fromFlagOrDefault normal (replVerbosity replFlags)
  (useSandbox, config) <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (replDistPref replFlags)
  cwd     <- getCurrentDirectory
  pkgDesc <- findPackageDesc cwd
  let
    -- There is a .cabal file in the current directory: start a REPL and load
    -- the project's modules.
    onPkgDesc = do
      let noAddSource = case replReload replFlags of
            Flag True -> SkipAddSourceDepsCheck
            _         -> fromFlagOrDefault DontSkipAddSourceDepsCheck
                        (buildOnly buildExFlags)

      -- Calls 'configureAction' to do the real work, so nothing special has to
      -- be done to support sandboxes.
      _ <-
        reconfigure configureAction
        verbosity distPref useSandbox noAddSource NoFlag
        mempty [] globalFlags config
      let progDb = defaultProgramDb
          setupOptions = defaultSetupScriptOptions
            { useCabalVersion = orLaterVersion $ mkVersion [1,18,0]
            , useDistPref     = distPref
            }
          replFlags'   = replFlags
            { replVerbosity = toFlag verbosity
            , replDistPref  = toFlag distPref
            }

      nixShell verbosity distPref globalFlags config $ do
        maybeWithSandboxDirOnSearchPath useSandbox $
          setupWrapper verbosity setupOptions Nothing
          (Cabal.replCommand progDb) (const replFlags') extraArgs

    -- No .cabal file in the current directory: just start the REPL (possibly
    -- using the sandbox package DB).
    onNoPkgDesc = do
      let configFlags = savedConfigureFlags config
      (comp, platform, programDb) <- configCompilerAux' configFlags
      programDb' <- reconfigurePrograms verbosity
                                        (replProgramPaths replFlags)
                                        (replProgramArgs replFlags)
                                        programDb
      nixShell verbosity distPref globalFlags config $ do
        startInterpreter verbosity programDb' comp platform
                        (configPackageDB' configFlags)

  either (const onNoPkgDesc) (const onPkgDesc) pkgDesc

installAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
              -> [String] -> Action
installAction (configFlags, _, installFlags, _) _ globalFlags
  | fromFlagOrDefault False (installOnly installFlags) = do
      let verb = fromFlagOrDefault normal (configVerbosity configFlags)
      (useSandbox, config) <- loadConfigOrSandboxConfig verb globalFlags
      dist <- findSavedDistPref config (configDistPref configFlags)
      let setupOpts = defaultSetupScriptOptions { useDistPref = dist }
      nixShellIfSandboxed verb dist globalFlags config useSandbox $
        setupWrapper
        verb setupOpts Nothing
        installCommand (const mempty) []

installAction
  (configFlags, configExFlags, installFlags, haddockFlags)
  extraArgs globalFlags = do
  let verb = fromFlagOrDefault normal (configVerbosity configFlags)
  (useSandbox, config) <- updateInstallDirs (configUserInstall configFlags)
                          <$> loadConfigOrSandboxConfig verb globalFlags

  let sandboxDist =
        case useSandbox of
          NoSandbox             -> NoFlag
          UseSandbox sandboxDir -> Flag $ sandboxBuildDir sandboxDir
  dist <- findSavedDistPref config
          (configDistPref configFlags `mappend` sandboxDist)

  nixShellIfSandboxed verb dist globalFlags config useSandbox $ do
    targets <- readUserTargets verb extraArgs

    -- TODO: It'd be nice if 'cabal install' picked up the '-w' flag passed to
    -- 'configure' when run inside a sandbox.  Right now, running
    --
    -- $ cabal sandbox init && cabal configure -w /path/to/ghc
    --   && cabal build && cabal install
    --
    -- performs the compilation twice unless you also pass -w to 'install'.
    -- However, this is the same behaviour that 'cabal install' has in the normal
    -- mode of operation, so we stick to it for consistency.

    let configFlags'    = maybeForceTests installFlags' $
                          savedConfigureFlags   config `mappend`
                          configFlags { configDistPref = toFlag dist }
        configExFlags'  = defaultConfigExFlags         `mappend`
                          savedConfigureExFlags config `mappend` configExFlags
        installFlags'   = defaultInstallFlags          `mappend`
                          savedInstallFlags     config `mappend` installFlags
        haddockFlags'   = defaultHaddockFlags          `mappend`
                          savedHaddockFlags     config `mappend`
                          haddockFlags { haddockDistPref = toFlag dist }
        globalFlags'    = savedGlobalFlags      config `mappend` globalFlags
    (comp, platform, progdb) <- configCompilerAux' configFlags'
    -- TODO: Redesign ProgramDB API to prevent such problems as #2241 in the
    -- future.
    progdb' <- configureAllKnownPrograms verb progdb

    -- If we're working inside a sandbox and the user has set the -w option, we
    -- may need to create a sandbox-local package DB for this compiler and add a
    -- timestamp record for this compiler to the timestamp file.
    configFlags'' <- case useSandbox of
      NoSandbox               -> configAbsolutePaths $ configFlags'
      (UseSandbox sandboxDir) -> return $ setPackageDB sandboxDir comp platform
                                                      configFlags'

    whenUsingSandbox useSandbox $ \sandboxDir -> do
      initPackageDBIfNeeded verb configFlags'' comp progdb'

      indexFile     <- tryGetIndexFilePath verb config
      maybeAddCompilerTimestampRecord verb sandboxDir indexFile
        (compilerId comp) platform

    -- TODO: Passing 'SandboxPackageInfo' to install unconditionally here means
    -- that 'cabal install some-package' inside a sandbox will sometimes reinstall
    -- modified add-source deps, even if they are not among the dependencies of
    -- 'some-package'. This can also prevent packages that depend on older
    -- versions of add-source'd packages from building (see #1362).
    maybeWithSandboxPackageInfo verb configFlags'' globalFlags'
                                comp platform progdb useSandbox $ \mSandboxPkgInfo ->
                                maybeWithSandboxDirOnSearchPath useSandbox $
      withRepoContext verb globalFlags' $ \repoContext ->
        install verb
                (configPackageDB' configFlags'')
                repoContext
                comp platform progdb'
                useSandbox mSandboxPkgInfo
                globalFlags' configFlags'' configExFlags'
                installFlags' haddockFlags'
                targets

      where
        -- '--run-tests' implies '--enable-tests'.
        maybeForceTests installFlags' configFlags' =
          if fromFlagOrDefault False (installRunTests installFlags')
          then configFlags' { configTests = toFlag True }
          else configFlags'

testAction :: (TestFlags, BuildFlags, BuildExFlags) -> [String] -> GlobalFlags
           -> IO ()
testAction (testFlags, buildFlags, buildExFlags) extraArgs globalFlags = do
  let verbosity      = fromFlagOrDefault normal (testVerbosity testFlags)
  (useSandbox, config) <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (testDistPref testFlags)
  let noAddSource    = fromFlagOrDefault DontSkipAddSourceDepsCheck
                      (buildOnly buildExFlags)
      buildFlags'    = buildFlags
                      { buildVerbosity = testVerbosity testFlags }
      checkFlags = Check $ \_ flags@(configFlags, configExFlags) ->
        if fromFlagOrDefault False (configTests configFlags)
          then pure (mempty, flags)
          else do
            info verbosity "reconfiguring to enable tests"
            let flags' = ( configFlags { configTests = toFlag True }
                        , configExFlags
                        )
            pure (Any True, flags')

  -- reconfigure also checks if we're in a sandbox and reinstalls add-source
  -- deps if needed.
  _ <-
    reconfigure configureAction
    verbosity distPref useSandbox noAddSource (buildNumJobs buildFlags')
    checkFlags [] globalFlags config
  nixShell verbosity distPref globalFlags config $ do
    let setupOptions   = defaultSetupScriptOptions { useDistPref = distPref }
        testFlags'     = testFlags { testDistPref = toFlag distPref }

    -- The package was just configured, so the LBI must be available.
    names <- componentNamesFromLBI verbosity distPref "test suites"
              (\c -> case c of { LBI.CTest{} -> True; _ -> False })
    let extraArgs'
          | null extraArgs = case names of
            ComponentNamesUnknown -> []
            ComponentNames names' -> [ Make.unUnqualComponentName name
                                    | LBI.CTestName name <- names' ]
          | otherwise      = extraArgs

    maybeWithSandboxDirOnSearchPath useSandbox $
      build verbosity config distPref buildFlags' extraArgs'

    maybeWithSandboxDirOnSearchPath useSandbox $
      setupWrapper verbosity setupOptions Nothing
      Cabal.testCommand (const testFlags') extraArgs'

data ComponentNames = ComponentNamesUnknown
                    | ComponentNames [LBI.ComponentName]

-- | Return the names of all buildable components matching a given predicate.
componentNamesFromLBI :: Verbosity -> FilePath -> String
                         -> (LBI.Component -> Bool)
                         -> IO ComponentNames
componentNamesFromLBI verbosity distPref targetsDescr compPred = do
  eLBI <- tryGetPersistBuildConfig distPref
  case eLBI of
    Left err -> case err of
      -- Note: the build config could have been generated by a custom setup
      -- script built against a different Cabal version, so it's crucial that
      -- we ignore the bad version error here.
      ConfigStateFileBadVersion _ _ _ -> return ComponentNamesUnknown
      _                               -> die' verbosity (show err)
    Right lbi -> do
      let pkgDescr = LBI.localPkgDescr lbi
          names    = map LBI.componentName
                     . filter (buildable . LBI.componentBuildInfo)
                     . filter compPred $
                     LBI.pkgComponents pkgDescr
      if null names
        then do notice verbosity $ "Package has no buildable "
                  ++ targetsDescr ++ "."
                exitSuccess -- See #3215.

        else return $! (ComponentNames names)

benchmarkAction :: (BenchmarkFlags, BuildFlags, BuildExFlags)
                   -> [String] -> GlobalFlags
                   -> IO ()
benchmarkAction
  (benchmarkFlags, buildFlags, buildExFlags)
  extraArgs globalFlags = do
  let verbosity      = fromFlagOrDefault normal
                       (benchmarkVerbosity benchmarkFlags)

  (useSandbox, config) <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (benchmarkDistPref benchmarkFlags)
  let buildFlags'    = buildFlags
                      { buildVerbosity = benchmarkVerbosity benchmarkFlags }
      noAddSource   = fromFlagOrDefault DontSkipAddSourceDepsCheck
                      (buildOnly buildExFlags)

  let checkFlags = Check $ \_ flags@(configFlags, configExFlags) ->
        if fromFlagOrDefault False (configBenchmarks configFlags)
          then pure (mempty, flags)
          else do
            info verbosity "reconfiguring to enable benchmarks"
            let flags' = ( configFlags { configBenchmarks = toFlag True }
                        , configExFlags
                        )
            pure (Any True, flags')


  -- reconfigure also checks if we're in a sandbox and reinstalls add-source
  -- deps if needed.
  config' <-
    reconfigure configureAction
    verbosity distPref useSandbox noAddSource (buildNumJobs buildFlags')
    checkFlags [] globalFlags config
  nixShell verbosity distPref globalFlags config $ do
    let setupOptions   = defaultSetupScriptOptions { useDistPref = distPref }
        benchmarkFlags'= benchmarkFlags { benchmarkDistPref = toFlag distPref }

    -- The package was just configured, so the LBI must be available.
    names <- componentNamesFromLBI verbosity distPref "benchmarks"
            (\c -> case c of { LBI.CBench{} -> True; _ -> False; })
    let extraArgs'
          | null extraArgs = case names of
            ComponentNamesUnknown -> []
            ComponentNames names' -> [ Make.unUnqualComponentName name
                                    | LBI.CBenchName name <- names']
          | otherwise      = extraArgs

    maybeWithSandboxDirOnSearchPath useSandbox $
      build verbosity config' distPref buildFlags' extraArgs'

    maybeWithSandboxDirOnSearchPath useSandbox $
      setupWrapper verbosity setupOptions Nothing
      Cabal.benchmarkCommand (const benchmarkFlags') extraArgs'

haddockAction :: HaddockFlags -> [String] -> Action
haddockAction haddockFlags extraArgs globalFlags = do
  let verbosity = fromFlag (haddockVerbosity haddockFlags)
  (useSandbox, config) <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (haddockDistPref haddockFlags)
  config' <-
    reconfigure configureAction
    verbosity distPref useSandbox DontSkipAddSourceDepsCheck NoFlag
    mempty [] globalFlags config
  nixShell verbosity distPref globalFlags config $ do
    let haddockFlags' = defaultHaddockFlags      `mappend`
                        savedHaddockFlags config' `mappend`
                        haddockFlags { haddockDistPref = toFlag distPref }
        setupScriptOptions = defaultSetupScriptOptions
                             { useDistPref = distPref }
    setupWrapper verbosity setupScriptOptions Nothing
      haddockCommand (const haddockFlags') extraArgs
    when (haddockForHackage haddockFlags == Flag ForHackage) $ do
      pkg <- fmap LBI.localPkgDescr (getPersistBuildConfig distPref)
      let dest = distPref </> name <.> "tar.gz"
          name = display (packageId pkg) ++ "-docs"
          docDir = distPref </> "doc" </> "html"
      createTarGzFile dest docDir name
      notice verbosity $ "Documentation tarball created: " ++ dest

doctestAction :: DoctestFlags -> [String] -> Action
doctestAction doctestFlags extraArgs _globalFlags = do
  let verbosity = fromFlag (doctestVerbosity doctestFlags)

  setupWrapper verbosity defaultSetupScriptOptions Nothing
    doctestCommand (const doctestFlags) extraArgs

cleanAction :: CleanFlags -> [String] -> Action
cleanAction cleanFlags extraArgs globalFlags = do
  load <- try (loadConfigOrSandboxConfig verbosity globalFlags)
  let config = either (\(SomeException _) -> mempty) snd load
  distPref <- findSavedDistPref config (cleanDistPref cleanFlags)
  let setupScriptOptions = defaultSetupScriptOptions
                           { useDistPref = distPref
                           , useWin32CleanHack = True
                           }
      cleanFlags' = cleanFlags { cleanDistPref = toFlag distPref }
  setupWrapper verbosity setupScriptOptions Nothing
               cleanCommand (const cleanFlags') extraArgs
  where
    verbosity = fromFlagOrDefault normal (cleanVerbosity cleanFlags)

listAction :: ListFlags -> [String] -> Action
listAction listFlags extraArgs globalFlags = do
  let verbosity = fromFlag (listVerbosity listFlags)
  (_useSandbox, config) <- loadConfigOrSandboxConfig verbosity
                           (globalFlags { globalRequireSandbox = Flag False })
  let configFlags' = savedConfigureFlags config
      configFlags  = configFlags' {
        configPackageDBs = configPackageDBs configFlags'
                           `mappend` listPackageDBs listFlags
        }
      globalFlags' = savedGlobalFlags    config `mappend` globalFlags
  (comp, _, progdb) <- configCompilerAux' configFlags
  withRepoContext verbosity globalFlags' $ \repoContext ->
    List.list verbosity
       (configPackageDB' configFlags)
       repoContext
       comp
       progdb
       listFlags
       extraArgs

infoAction :: InfoFlags -> [String] -> Action
infoAction infoFlags extraArgs globalFlags = do
  let verbosity = fromFlag (infoVerbosity infoFlags)
  targets <- readUserTargets verbosity extraArgs
  (_useSandbox, config) <- loadConfigOrSandboxConfig verbosity
                           (globalFlags { globalRequireSandbox = Flag False })
  let configFlags' = savedConfigureFlags config
      configFlags  = configFlags' {
        configPackageDBs = configPackageDBs configFlags'
                           `mappend` infoPackageDBs infoFlags
        }
      globalFlags' = savedGlobalFlags    config `mappend` globalFlags
  (comp, _, progdb) <- configCompilerAuxEx configFlags
  withRepoContext verbosity globalFlags' $ \repoContext ->
    List.info verbosity
       (configPackageDB' configFlags)
       repoContext
       comp
       progdb
       globalFlags'
       infoFlags
       targets

updateAction :: UpdateFlags -> [String] -> Action
updateAction updateFlags extraArgs globalFlags = do
  let verbosity = fromFlag (updateVerbosity updateFlags)
  unless (null extraArgs) $
    die' verbosity $ "'update' doesn't take any extra arguments: " ++ unwords extraArgs
  (_useSandbox, config) <- loadConfigOrSandboxConfig verbosity
                           (globalFlags { globalRequireSandbox = Flag False })
  let globalFlags' = savedGlobalFlags config `mappend` globalFlags
  withRepoContext verbosity globalFlags' $ \repoContext ->
    update verbosity updateFlags repoContext

upgradeAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
              -> [String] -> Action
upgradeAction (configFlags, _, _, _) _ _ = die' verbosity $
    "Use the 'cabal install' command instead of 'cabal upgrade'.\n"
 ++ "You can install the latest version of a package using 'cabal install'. "
 ++ "The 'cabal upgrade' command has been removed because people found it "
 ++ "confusing and it often led to broken packages.\n"
 ++ "If you want the old upgrade behaviour then use the install command "
 ++ "with the --upgrade-dependencies flag (but check first with --dry-run "
 ++ "to see what would happen). This will try to pick the latest versions "
 ++ "of all dependencies, rather than the usual behaviour of trying to pick "
 ++ "installed versions of all dependencies. If you do use "
 ++ "--upgrade-dependencies, it is recommended that you do not upgrade core "
 ++ "packages (e.g. by using appropriate --constraint= flags)."
 where
  verbosity = fromFlag (configVerbosity configFlags)

fetchAction :: FetchFlags -> [String] -> Action
fetchAction fetchFlags extraArgs globalFlags = do
  let verbosity = fromFlag (fetchVerbosity fetchFlags)
  targets <- readUserTargets verbosity extraArgs
  config <- loadConfig verbosity (globalConfigFile globalFlags)
  let configFlags  = savedConfigureFlags config
      globalFlags' = savedGlobalFlags config `mappend` globalFlags
  (comp, platform, progdb) <- configCompilerAux' configFlags
  withRepoContext verbosity globalFlags' $ \repoContext ->
    fetch verbosity
        (configPackageDB' configFlags)
        repoContext
        comp platform progdb globalFlags' fetchFlags
        targets

freezeAction :: FreezeFlags -> [String] -> Action
freezeAction freezeFlags _extraArgs globalFlags = do
  let verbosity = fromFlag (freezeVerbosity freezeFlags)
  (useSandbox, config) <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config NoFlag
  nixShell verbosity distPref globalFlags config $ do
    let configFlags  = savedConfigureFlags config
        globalFlags' = savedGlobalFlags config `mappend` globalFlags
    (comp, platform, progdb) <- configCompilerAux' configFlags

    maybeWithSandboxPackageInfo
      verbosity configFlags globalFlags'
      comp platform progdb useSandbox $ \mSandboxPkgInfo ->
        maybeWithSandboxDirOnSearchPath useSandbox $
        withRepoContext verbosity globalFlags' $ \repoContext ->
          freeze verbosity
            (configPackageDB' configFlags)
            repoContext
            comp platform progdb
            mSandboxPkgInfo
            globalFlags' freezeFlags

genBoundsAction :: FreezeFlags -> [String] -> GlobalFlags -> IO ()
genBoundsAction freezeFlags _extraArgs globalFlags = do
  let verbosity = fromFlag (freezeVerbosity freezeFlags)
  (useSandbox, config) <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config NoFlag
  nixShell verbosity distPref globalFlags config $ do
    let configFlags  = savedConfigureFlags config
        globalFlags' = savedGlobalFlags config `mappend` globalFlags
    (comp, platform, progdb) <- configCompilerAux' configFlags

    maybeWithSandboxPackageInfo
      verbosity configFlags globalFlags'
      comp platform progdb useSandbox $ \mSandboxPkgInfo ->
        maybeWithSandboxDirOnSearchPath useSandbox $
        withRepoContext verbosity globalFlags' $ \repoContext ->
          genBounds verbosity
                (configPackageDB' configFlags)
                repoContext
                comp platform progdb
                mSandboxPkgInfo
                globalFlags' freezeFlags

outdatedAction :: OutdatedFlags -> [String] -> GlobalFlags -> IO ()
outdatedAction outdatedFlags _extraArgs globalFlags = do
  let verbosity = fromFlag (outdatedVerbosity outdatedFlags)
  (_useSandbox, config) <- loadConfigOrSandboxConfig verbosity globalFlags
  let configFlags  = savedConfigureFlags config
      globalFlags' = savedGlobalFlags config `mappend` globalFlags
  (comp, platform, _progdb) <- configCompilerAux' configFlags
  withRepoContext verbosity globalFlags' $ \repoContext ->
    outdated verbosity outdatedFlags repoContext
             comp platform

uploadAction :: UploadFlags -> [String] -> Action
uploadAction uploadFlags extraArgs globalFlags = do
  config <- loadConfig verbosity (globalConfigFile globalFlags)
  let uploadFlags' = savedUploadFlags config `mappend` uploadFlags
      globalFlags' = savedGlobalFlags config `mappend` globalFlags
      tarfiles     = extraArgs
  when (null tarfiles && not (fromFlag (uploadDoc uploadFlags'))) $
    die' verbosity "the 'upload' command expects at least one .tar.gz archive."
  checkTarFiles extraArgs
  maybe_password <-
    case uploadPasswordCmd uploadFlags'
    of Flag (xs:xss) -> Just . Password <$>
                        getProgramInvocationOutput verbosity
                        (simpleProgramInvocation xs xss)
       _             -> pure $ flagToMaybe $ uploadPassword uploadFlags'
  withRepoContext verbosity globalFlags' $ \repoContext -> do
    if fromFlag (uploadDoc uploadFlags')
    then do
      when (length tarfiles > 1) $
       die' verbosity $ "the 'upload' command can only upload documentation "
             ++ "for one package at a time."
      tarfile <- maybe (generateDocTarball config) return $ listToMaybe tarfiles
      Upload.uploadDoc verbosity
                       repoContext
                       (flagToMaybe $ uploadUsername uploadFlags')
                       maybe_password
                       (fromFlag (uploadCandidate uploadFlags'))
                       tarfile
    else do
      Upload.upload verbosity
                    repoContext
                    (flagToMaybe $ uploadUsername uploadFlags')
                    maybe_password
                    (fromFlag (uploadCandidate uploadFlags'))
                    tarfiles
    where
    verbosity = fromFlag (uploadVerbosity uploadFlags)
    checkTarFiles tarfiles
      | not (null otherFiles)
      = die' verbosity $ "the 'upload' command expects only .tar.gz archives: "
           ++ intercalate ", " otherFiles
      | otherwise = sequence_
                      [ do exists <- doesFileExist tarfile
                           unless exists $ die' verbosity $ "file not found: " ++ tarfile
                      | tarfile <- tarfiles ]

      where otherFiles = filter (not . isTarGzFile) tarfiles
            isTarGzFile file = case splitExtension file of
              (file', ".gz") -> takeExtension file' == ".tar"
              _              -> False
    generateDocTarball config = do
      notice verbosity $
        "No documentation tarball specified. "
        ++ "Building a documentation tarball with default settings...\n"
        ++ "If you need to customise Haddock options, "
        ++ "run 'haddock --for-hackage' first "
        ++ "to generate a documentation tarball."
      haddockAction (defaultHaddockFlags { haddockForHackage = Flag ForHackage })
                    [] globalFlags
      distPref <- findSavedDistPref config NoFlag
      pkg <- fmap LBI.localPkgDescr (getPersistBuildConfig distPref)
      return $ distPref </> display (packageId pkg) ++ "-docs" <.> "tar.gz"

checkAction :: Flag Verbosity -> [String] -> Action
checkAction verbosityFlag extraArgs _globalFlags = do
  let verbosity = fromFlag verbosityFlag
  unless (null extraArgs) $
    die' verbosity $ "'check' doesn't take any extra arguments: " ++ unwords extraArgs
  allOk <- Check.check (fromFlag verbosityFlag)
  unless allOk exitFailure

formatAction :: Flag Verbosity -> [String] -> Action
formatAction verbosityFlag extraArgs _globalFlags = do
  let verbosity = fromFlag verbosityFlag
  path <- case extraArgs of
    [] -> do cwd <- getCurrentDirectory
             tryFindPackageDesc cwd
    (p:_) -> return p
  pkgDesc <- readGenericPackageDescription verbosity path
  -- Uses 'writeFileAtomic' under the hood.
  writeGenericPackageDescription path pkgDesc

uninstallAction :: Flag Verbosity -> [String] -> Action
uninstallAction verbosityFlag extraArgs _globalFlags = do
  let verbosity = fromFlag verbosityFlag
      package = case extraArgs of
        p:_ -> p
        _   -> "PACKAGE_NAME"
  die' verbosity $ "This version of 'cabal-install' does not support the 'uninstall' "
    ++ "operation. "
    ++ "It will likely be implemented at some point in the future; "
    ++ "in the meantime you're advised to use either 'ghc-pkg unregister "
    ++ package ++ "' or 'cabal sandbox hc-pkg -- unregister " ++ package ++ "'."


sdistAction :: (SDistFlags, SDistExFlags) -> [String] -> Action
sdistAction (sdistFlags, sdistExFlags) extraArgs globalFlags = do
  let verbosity = fromFlag (sDistVerbosity sdistFlags)
  unless (null extraArgs) $
    die' verbosity $ "'sdist' doesn't take any extra arguments: " ++ unwords extraArgs
  load <- try (loadConfigOrSandboxConfig verbosity globalFlags)
  let config = either (\(SomeException _) -> mempty) snd load
  distPref <- findSavedDistPref config (sDistDistPref sdistFlags)
  let sdistFlags' = sdistFlags { sDistDistPref = toFlag distPref }
  sdist sdistFlags' sdistExFlags

reportAction :: ReportFlags -> [String] -> Action
reportAction reportFlags extraArgs globalFlags = do
  let verbosity = fromFlag (reportVerbosity reportFlags)
  unless (null extraArgs) $
    die' verbosity $ "'report' doesn't take any extra arguments: " ++ unwords extraArgs
  config <- loadConfig verbosity (globalConfigFile globalFlags)
  let globalFlags' = savedGlobalFlags config `mappend` globalFlags
      reportFlags' = savedReportFlags config `mappend` reportFlags

  withRepoContext verbosity globalFlags' $ \repoContext ->
   Upload.report verbosity repoContext
    (flagToMaybe $ reportUsername reportFlags')
    (flagToMaybe $ reportPassword reportFlags')

runAction :: (BuildFlags, BuildExFlags) -> [String] -> Action
runAction (buildFlags, buildExFlags) extraArgs globalFlags = do
  let verbosity   = fromFlagOrDefault normal (buildVerbosity buildFlags)
  (useSandbox, config) <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (buildDistPref buildFlags)
  let noAddSource = fromFlagOrDefault DontSkipAddSourceDepsCheck
                    (buildOnly buildExFlags)
  -- reconfigure also checks if we're in a sandbox and reinstalls add-source
  -- deps if needed.
  config' <-
    reconfigure configureAction
    verbosity distPref useSandbox noAddSource (buildNumJobs buildFlags)
    mempty [] globalFlags config
  nixShell verbosity distPref globalFlags config $ do
    lbi <- getPersistBuildConfig distPref
    (exe, exeArgs) <- splitRunArgs verbosity lbi extraArgs

    maybeWithSandboxDirOnSearchPath useSandbox $
      build verbosity config' distPref buildFlags ["exe:" ++ display (exeName exe)]

    maybeWithSandboxDirOnSearchPath useSandbox $
      run verbosity lbi exe exeArgs

getAction :: GetFlags -> [String] -> Action
getAction getFlags extraArgs globalFlags = do
  let verbosity = fromFlag (getVerbosity getFlags)
  targets <- readUserTargets verbosity extraArgs
  (_useSandbox, config) <- loadConfigOrSandboxConfig verbosity
                           (globalFlags { globalRequireSandbox = Flag False })
  let globalFlags' = savedGlobalFlags config `mappend` globalFlags
  withRepoContext verbosity (savedGlobalFlags config) $ \repoContext ->
   get verbosity
    repoContext
    globalFlags'
    getFlags
    targets

unpackAction :: GetFlags -> [String] -> Action
unpackAction getFlags extraArgs globalFlags = do
  getAction getFlags extraArgs globalFlags

initAction :: InitFlags -> [String] -> Action
initAction initFlags extraArgs globalFlags = do
  let verbosity = fromFlag (initVerbosity initFlags)
  when (extraArgs /= []) $
    die' verbosity $ "'init' doesn't take any extra arguments: " ++ unwords extraArgs
  (_useSandbox, config) <- loadConfigOrSandboxConfig verbosity
                           (globalFlags { globalRequireSandbox = Flag False })
  let configFlags  = savedConfigureFlags config
  let globalFlags' = savedGlobalFlags    config `mappend` globalFlags
  (comp, _, progdb) <- configCompilerAux' configFlags
  withRepoContext verbosity globalFlags' $ \repoContext ->
    initCabal verbosity
            (configPackageDB' configFlags)
            repoContext
            comp
            progdb
            initFlags

sandboxAction :: SandboxFlags -> [String] -> Action
sandboxAction sandboxFlags extraArgs globalFlags = do
  let verbosity = fromFlag (sandboxVerbosity sandboxFlags)
  case extraArgs of
    -- Basic sandbox commands.
    ["init"] -> sandboxInit verbosity sandboxFlags globalFlags
    ["delete"] -> sandboxDelete verbosity sandboxFlags globalFlags
    ("add-source":extra) -> do
        when (noExtraArgs extra) $
          die' verbosity "The 'sandbox add-source' command expects at least one argument"
        sandboxAddSource verbosity extra sandboxFlags globalFlags
    ("delete-source":extra) -> do
        when (noExtraArgs extra) $
          die' verbosity ("The 'sandbox delete-source' command expects " ++
              "at least one argument")
        sandboxDeleteSource verbosity extra sandboxFlags globalFlags
    ["list-sources"] -> sandboxListSources verbosity sandboxFlags globalFlags

    -- More advanced commands.
    ("hc-pkg":extra) -> do
        when (noExtraArgs extra) $
            die' verbosity $ "The 'sandbox hc-pkg' command expects at least one argument"
        sandboxHcPkg verbosity sandboxFlags globalFlags extra
    ["buildopts"] -> die' verbosity "Not implemented!"

    -- Hidden commands.
    ["dump-pkgenv"]  -> dumpPackageEnvironment verbosity sandboxFlags globalFlags

    -- Error handling.
    [] -> die' verbosity $ "Please specify a subcommand (see 'help sandbox')"
    _  -> die' verbosity $ "Unknown 'sandbox' subcommand: " ++ unwords extraArgs

  where
    noExtraArgs = (<1) . length

execAction :: ExecFlags -> [String] -> Action
execAction execFlags extraArgs globalFlags = do
  let verbosity = fromFlag (execVerbosity execFlags)
  (useSandbox, config) <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (execDistPref execFlags)
  let configFlags = savedConfigureFlags config
      configFlags' = configFlags { configDistPref = Flag distPref }
  (comp, platform, progdb) <- getPersistOrConfigCompiler configFlags'
  exec verbosity useSandbox comp platform progdb extraArgs

userConfigAction :: UserConfigFlags -> [String] -> Action
userConfigAction ucflags extraArgs globalFlags = do
  let verbosity = fromFlag (userConfigVerbosity ucflags)
      force     = fromFlag (userConfigForce ucflags)
  case extraArgs of
    ("init":_) -> do
      path       <- configFile
      fileExists <- doesFileExist path
      if (not fileExists || (fileExists && force))
      then void $ createDefaultConfigFile verbosity path
      else die' verbosity $ path ++ " already exists."
    ("diff":_) -> mapM_ putStrLn =<< userConfigDiff globalFlags
    ("update":_) -> userConfigUpdate verbosity globalFlags
    -- Error handling.
    [] -> die' verbosity $ "Please specify a subcommand (see 'help user-config')"
    _  -> die' verbosity $ "Unknown 'user-config' subcommand: " ++ unwords extraArgs
  where configFile = getConfigFilePath (globalConfigFile globalFlags)

-- | See 'Distribution.Client.Install.withWin32SelfUpgrade' for details.
--
win32SelfUpgradeAction :: Win32SelfUpgradeFlags -> [String] -> Action
win32SelfUpgradeAction selfUpgradeFlags (pid:path:_extraArgs) _globalFlags = do
  let verbosity = fromFlag (win32SelfUpgradeVerbosity selfUpgradeFlags)
  Win32SelfUpgrade.deleteOldExeFile verbosity (read pid) path -- TODO: eradicateNoParse
win32SelfUpgradeAction _ _ _ = return ()

-- | Used as an entry point when cabal-install needs to invoke itself
-- as a setup script. This can happen e.g. when doing parallel builds.
--
actAsSetupAction :: ActAsSetupFlags -> [String] -> Action
actAsSetupAction actAsSetupFlags args _globalFlags =
  let bt = fromFlag (actAsSetupBuildType actAsSetupFlags)
  in case bt of
    Simple    -> Simple.defaultMainArgs args
    Configure -> Simple.defaultMainWithHooksArgs
                  Simple.autoconfUserHooks args
    Make      -> Make.defaultMainArgs args
    Custom               -> error "actAsSetupAction Custom"
    (UnknownBuildType _) -> error "actAsSetupAction UnknownBuildType"

manpageAction :: [CommandSpec action] -> Flag Verbosity -> [String] -> Action
manpageAction commands flagVerbosity extraArgs _ = do
  let verbosity = fromFlag flagVerbosity
  unless (null extraArgs) $
    die' verbosity $ "'manpage' doesn't take any extra arguments: " ++ unwords extraArgs
  pname <- getProgName
  let cabalCmd = if takeExtension pname == ".exe"
                 then dropExtension pname
                 else pname
  putStrLn $ manpage cabalCmd commands
