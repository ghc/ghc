{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Setup
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Distribution.Client.Setup
    ( globalCommand, GlobalFlags(..), defaultGlobalFlags
    , RepoContext(..), withRepoContext
    , configureCommand, ConfigFlags(..), filterConfigureFlags
    , configPackageDB', configCompilerAux'
    , configureExCommand, ConfigExFlags(..), defaultConfigExFlags
    , buildCommand, BuildFlags(..), BuildExFlags(..), SkipAddSourceDepsCheck(..)
    , replCommand, testCommand, benchmarkCommand
                        , configureExOptions, reconfigureCommand
    , installCommand, InstallFlags(..), installOptions, defaultInstallFlags
    , defaultSolver, defaultMaxBackjumps
    , listCommand, ListFlags(..)
    , updateCommand, UpdateFlags(..), defaultUpdateFlags
    , upgradeCommand
    , uninstallCommand
    , infoCommand, InfoFlags(..)
    , fetchCommand, FetchFlags(..)
    , freezeCommand, FreezeFlags(..)
    , genBoundsCommand
    , outdatedCommand, OutdatedFlags(..), IgnoreMajorVersionBumps(..)
    , getCommand, unpackCommand, GetFlags(..)
    , checkCommand
    , formatCommand
    , uploadCommand, UploadFlags(..), IsCandidate(..)
    , reportCommand, ReportFlags(..)
    , runCommand
    , initCommand, IT.InitFlags(..)
    , sdistCommand, SDistFlags(..), SDistExFlags(..), ArchiveFormat(..)
    , win32SelfUpgradeCommand, Win32SelfUpgradeFlags(..)
    , actAsSetupCommand, ActAsSetupFlags(..)
    , sandboxCommand, defaultSandboxLocation, SandboxFlags(..)
    , execCommand, ExecFlags(..), defaultExecFlags
    , userConfigCommand, UserConfigFlags(..)
    , manpageCommand

    , applyFlagDefaults
    , parsePackageArgs
    --TODO: stop exporting these:
    , showRepo
    , parseRepo
    , readRepo
    ) where

import Prelude ()
import Distribution.Client.Compat.Prelude hiding (get)

import Distribution.Client.Types
         ( Username(..), Password(..), RemoteRepo(..)
         , AllowNewer(..), AllowOlder(..), RelaxDeps(..)
         )
import Distribution.Client.BuildReports.Types
         ( ReportLevel(..) )
import Distribution.Client.Dependency.Types
         ( PreSolver(..) )
import Distribution.Client.IndexUtils.Timestamp
         ( IndexState(..) )
import qualified Distribution.Client.Init.Types as IT
         ( InitFlags(..), PackageType(..) )
import Distribution.Client.Targets
         ( UserConstraint, readUserConstraint )
import Distribution.Utils.NubList
         ( NubList, toNubList, fromNubList)

import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.Settings

import Distribution.Simple.Compiler ( Compiler, PackageDB, PackageDBStack )
import Distribution.Simple.Program (ProgramDb, defaultProgramDb)
import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import qualified Distribution.Simple.Command as Command
import Distribution.Simple.Configure
       ( configCompilerAuxEx, interpretPackageDbFlags, computeEffectiveProfiling )
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Setup
         ( ConfigFlags(..), BuildFlags(..), ReplFlags
         , TestFlags(..), BenchmarkFlags(..)
         , SDistFlags(..), HaddockFlags(..)
         , readPackageDbList, showPackageDbList
         , Flag(..), toFlag, flagToMaybe, flagToList, maybeToFlag
         , BooleanFlag(..), optionVerbosity
         , boolOpt, boolOpt', trueArg, falseArg
         , optionNumJobs )
import Distribution.Simple.InstallDirs
         ( PathTemplate, InstallDirs(..)
         , toPathTemplate, fromPathTemplate, combinePathTemplate )
import Distribution.Version
         ( Version, mkVersion, nullVersion, anyVersion, thisVersion )
import Distribution.Package
         ( PackageIdentifier, PackageName, packageName, packageVersion )
import Distribution.Types.Dependency
import Distribution.PackageDescription
         ( BuildType(..), RepoKind(..) )
import Distribution.System ( Platform )
import Distribution.Text
         ( Text(..), display )
import Distribution.ReadE
         ( ReadE(..), readP_to_E, succeedReadE )
import qualified Distribution.Compat.ReadP as Parse
         ( ReadP, char, munch1, pfail, sepBy1, (+++) )
import Distribution.ParseUtils
         ( readPToMaybe )
import Distribution.Verbosity
         ( Verbosity, lessVerbose, normal, verboseNoFlags, verboseNoTimestamp )
import Distribution.Simple.Utils
         ( wrapText, wrapLine )
import Distribution.Client.GlobalFlags
         ( GlobalFlags(..), defaultGlobalFlags
         , RepoContext(..), withRepoContext
         )

import Data.List
         ( deleteFirstsBy )
import System.FilePath
         ( (</>) )
import Network.URI
         ( parseAbsoluteURI, uriToString )

applyFlagDefaults :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
                  -> (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
applyFlagDefaults (configFlags, configExFlags, installFlags, haddockFlags) =
  ( commandDefaultFlags configureCommand <> configFlags
  , defaultConfigExFlags <> configExFlags
  , defaultInstallFlags <> installFlags
  , Cabal.defaultHaddockFlags <> haddockFlags
  )

globalCommand :: [Command action] -> CommandUI GlobalFlags
globalCommand commands = CommandUI {
    commandName         = "",
    commandSynopsis     =
         "Command line interface to the Haskell Cabal infrastructure.",
    commandUsage        = \pname ->
         "See http://www.haskell.org/cabal/ for more information.\n"
      ++ "\n"
      ++ "Usage: " ++ pname ++ " [GLOBAL FLAGS] [COMMAND [FLAGS]]\n",
    commandDescription  = Just $ \pname ->
      let
        commands' = commands ++ [commandAddAction helpCommandUI undefined]
        cmdDescs = getNormalCommandDescriptions commands'
        -- if new commands are added, we want them to appear even if they
        -- are not included in the custom listing below. Thus, we calculate
        -- the `otherCmds` list and append it under the `other` category.
        -- Alternatively, a new testcase could be added that ensures that
        -- the set of commands listed here is equal to the set of commands
        -- that are actually available.
        otherCmds = deleteFirstsBy (==) (map fst cmdDescs)
          [ "help"
          , "update"
          , "install"
          , "fetch"
          , "list"
          , "info"
          , "user-config"
          , "get"
          , "init"
          , "configure"
          , "reconfigure"
          , "build"
          , "clean"
          , "run"
          , "repl"
          , "test"
          , "bench"
          , "check"
          , "sdist"
          , "upload"
          , "report"
          , "freeze"
          , "gen-bounds"
          , "outdated"
          , "doctest"
          , "haddock"
          , "hscolour"
          , "copy"
          , "register"
          , "sandbox"
          , "exec"
          , "new-build"
          , "new-configure"
          , "new-repl"
          , "new-freeze"
          , "new-run"
          , "new-test"
          , "new-bench"
          , "new-haddock"
          ]
        maxlen    = maximum $ [length name | (name, _) <- cmdDescs]
        align str = str ++ replicate (maxlen - length str) ' '
        startGroup n = " ["++n++"]"
        par          = ""
        addCmd n     = case lookup n cmdDescs of
                         Nothing -> ""
                         Just d -> "  " ++ align n ++ "    " ++ d
        addCmdCustom n d = case lookup n cmdDescs of -- make sure that the
                                                  -- command still exists.
                         Nothing -> ""
                         Just _ -> "  " ++ align n ++ "    " ++ d
      in
         "Commands:\n"
      ++ unlines (
        [ startGroup "global"
        , addCmd "update"
        , addCmd "install"
        , par
        , addCmd "help"
        , addCmd "info"
        , addCmd "list"
        , addCmd "fetch"
        , addCmd "user-config"
        , par
        , startGroup "package"
        , addCmd "get"
        , addCmd "init"
        , par
        , addCmd "configure"
        , addCmd "build"
        , addCmd "clean"
        , par
        , addCmd "run"
        , addCmd "repl"
        , addCmd "test"
        , addCmd "bench"
        , par
        , addCmd "check"
        , addCmd "sdist"
        , addCmd "upload"
        , addCmd "report"
        , par
        , addCmd "freeze"
        , addCmd "gen-bounds"
        , addCmd "outdated"
        , addCmd "doctest"
        , addCmd "haddock"
        , addCmd "hscolour"
        , addCmd "copy"
        , addCmd "register"
        , addCmd "reconfigure"
        , par
        , startGroup "sandbox"
        , addCmd "sandbox"
        , addCmd "exec"
        , addCmdCustom "repl" "Open interpreter with access to sandbox packages."
        , par
        , startGroup "new-style projects (beta)"
        , addCmd "new-build"
        , addCmd "new-configure"
        , addCmd "new-repl"
        , addCmd "new-run"
        , addCmd "new-test"
        , addCmd "new-bench"
        , addCmd "new-freeze"
        , addCmd "new-haddock"
        ] ++ if null otherCmds then [] else par
                                           :startGroup "other"
                                           :[addCmd n | n <- otherCmds])
      ++ "\n"
      ++ "For more information about a command use:\n"
      ++ "   " ++ pname ++ " COMMAND --help\n"
      ++ "or " ++ pname ++ " help COMMAND\n"
      ++ "\n"
      ++ "To install Cabal packages from hackage use:\n"
      ++ "  " ++ pname ++ " install foo [--dry-run]\n"
      ++ "\n"
      ++ "Occasionally you need to update the list of available packages:\n"
      ++ "  " ++ pname ++ " update\n",
    commandNotes = Nothing,
    commandDefaultFlags = mempty,
    commandOptions = args
  }
  where
    args :: ShowOrParseArgs -> [OptionField GlobalFlags]
    args ShowArgs  = argsShown
    args ParseArgs = argsShown ++ argsNotShown

    -- arguments we want to show in the help
    argsShown :: [OptionField GlobalFlags]
    argsShown = [
       option ['V'] ["version"]
         "Print version information"
         globalVersion (\v flags -> flags { globalVersion = v })
         trueArg

      ,option [] ["numeric-version"]
         "Print just the version number"
         globalNumericVersion (\v flags -> flags { globalNumericVersion = v })
         trueArg

      ,option [] ["config-file"]
         "Set an alternate location for the config file"
         globalConfigFile (\v flags -> flags { globalConfigFile = v })
         (reqArgFlag "FILE")

      ,option [] ["sandbox-config-file"]
         "Set an alternate location for the sandbox config file (default: './cabal.sandbox.config')"
         globalSandboxConfigFile (\v flags -> flags { globalSandboxConfigFile = v })
         (reqArgFlag "FILE")

      ,option [] ["default-user-config"]
         "Set a location for a cabal.config file for projects without their own cabal.config freeze file."
         globalConstraintsFile (\v flags -> flags {globalConstraintsFile = v})
         (reqArgFlag "FILE")

      ,option [] ["require-sandbox"]
         "requiring the presence of a sandbox for sandbox-aware commands"
         globalRequireSandbox (\v flags -> flags { globalRequireSandbox = v })
         (boolOpt' ([], ["require-sandbox"]) ([], ["no-require-sandbox"]))

      ,option [] ["ignore-sandbox"]
         "Ignore any existing sandbox"
         globalIgnoreSandbox (\v flags -> flags { globalIgnoreSandbox = v })
         trueArg

      ,option [] ["ignore-expiry"]
         "Ignore expiry dates on signed metadata (use only in exceptional circumstances)"
         globalIgnoreExpiry (\v flags -> flags { globalIgnoreExpiry = v })
         trueArg

      ,option [] ["http-transport"]
         "Set a transport for http(s) requests. Accepts 'curl', 'wget', 'powershell', and 'plain-http'. (default: 'curl')"
         globalHttpTransport (\v flags -> flags { globalHttpTransport = v })
         (reqArgFlag "HttpTransport")
      ,option [] ["nix"]
         "Nix integration: run commands through nix-shell if a 'shell.nix' file exists"
         globalNix (\v flags -> flags { globalNix = v })
         (boolOpt [] [])
      ]

    -- arguments we don't want shown in the help
    argsNotShown :: [OptionField GlobalFlags]
    argsNotShown = [
       option [] ["remote-repo"]
         "The name and url for a remote repository"
         globalRemoteRepos (\v flags -> flags { globalRemoteRepos = v })
         (reqArg' "NAME:URL" (toNubList . maybeToList . readRepo) (map showRepo . fromNubList))

      ,option [] ["remote-repo-cache"]
         "The location where downloads from all remote repos are cached"
         globalCacheDir (\v flags -> flags { globalCacheDir = v })
         (reqArgFlag "DIR")

      ,option [] ["local-repo"]
         "The location of a local repository"
         globalLocalRepos (\v flags -> flags { globalLocalRepos = v })
         (reqArg' "DIR" (\x -> toNubList [x]) fromNubList)

      ,option [] ["logs-dir"]
         "The location to put log files"
         globalLogsDir (\v flags -> flags { globalLogsDir = v })
         (reqArgFlag "DIR")

      ,option [] ["world-file"]
         "The location of the world file"
         globalWorldFile (\v flags -> flags { globalWorldFile = v })
         (reqArgFlag "FILE")

      ,option [] ["store-dir"]
         "The location of the nix-local-build store"
         globalStoreDir (\v flags -> flags { globalStoreDir = v })
         (reqArgFlag "DIR")
      ]

-- ------------------------------------------------------------
-- * Config flags
-- ------------------------------------------------------------

configureCommand :: CommandUI ConfigFlags
configureCommand = c
  { commandDefaultFlags = mempty
  , commandNotes = Just $ \pname -> (case commandNotes c of
         Nothing -> ""
         Just n  -> n pname ++ "\n")
       ++ "Examples:\n"
       ++ "  " ++ pname ++ " configure\n"
       ++ "    Configure with defaults;\n"
       ++ "  " ++ pname ++ " configure --enable-tests -fcustomflag\n"
       ++ "    Configure building package including tests,\n"
       ++ "    with some package-specific flag.\n"
  }
 where
  c = Cabal.configureCommand defaultProgramDb

configureOptions ::  ShowOrParseArgs -> [OptionField ConfigFlags]
configureOptions = commandOptions configureCommand

-- | Given some 'ConfigFlags' for the version of Cabal that
-- cabal-install was built with, and a target older 'Version' of
-- Cabal that we want to pass these flags to, convert the
-- flags into a form that will be accepted by the older
-- Setup script.  Generally speaking, this just means filtering
-- out flags that the old Cabal library doesn't understand, but
-- in some cases it may also mean "emulating" a feature using
-- some more legacy flags.
filterConfigureFlags :: ConfigFlags -> Version -> ConfigFlags
filterConfigureFlags flags cabalLibVersion
  -- NB: we expect the latest version to be the most common case,
  -- so test it first.
  | cabalLibVersion >= mkVersion [2,1,0]  = flags_latest
  -- The naming convention is that flags_version gives flags with
  -- all flags *introduced* in version eliminated.
  -- It is NOT the latest version of Cabal library that
  -- these flags work for; version of introduction is a more
  -- natural metric.
  | cabalLibVersion < mkVersion [1,3,10] = flags_1_3_10
  | cabalLibVersion < mkVersion [1,10,0] = flags_1_10_0
  | cabalLibVersion < mkVersion [1,12,0] = flags_1_12_0
  | cabalLibVersion < mkVersion [1,14,0] = flags_1_14_0
  | cabalLibVersion < mkVersion [1,18,0] = flags_1_18_0
  | cabalLibVersion < mkVersion [1,19,1] = flags_1_19_1
  | cabalLibVersion < mkVersion [1,19,2] = flags_1_19_2
  | cabalLibVersion < mkVersion [1,21,1] = flags_1_21_1
  | cabalLibVersion < mkVersion [1,22,0] = flags_1_22_0
  | cabalLibVersion < mkVersion [1,23,0] = flags_1_23_0
  | cabalLibVersion < mkVersion [1,25,0] = flags_1_25_0
  | cabalLibVersion < mkVersion [2,1,0]  = flags_2_1_0
  | otherwise = flags_latest
  where
    flags_latest = flags        {
      -- Cabal >= 1.19.1 uses '--dependency' and does not need '--constraint'.
      configConstraints = []
      }

    flags_2_1_0 = flags_latest {
      -- Cabal < 2.1 doesn't know about -v +timestamp modifier
        configVerbosity   = fmap verboseNoTimestamp (configVerbosity flags_latest)
      -- Cabal < 2.1 doesn't know about --<enable|disable>-static
      , configStaticLib   = NoFlag
      }

    flags_1_25_0 = flags_2_1_0 {
      -- Cabal < 1.25.0 doesn't know about --dynlibdir.
      configInstallDirs = configInstallDirs_1_25_0,
      -- Cabal < 1.25 doesn't have extended verbosity syntax
      configVerbosity   = fmap verboseNoFlags (configVerbosity flags_2_1_0),
      -- Cabal < 1.25 doesn't support --deterministic
      configDeterministic = mempty
      }
    configInstallDirs_1_25_0 = let dirs = configInstallDirs flags in
        dirs { dynlibdir = NoFlag
             , libexecsubdir = NoFlag
             , libexecdir = maybeToFlag $
                 combinePathTemplate <$> flagToMaybe (libexecdir dirs)
                                     <*> flagToMaybe (libexecsubdir dirs)
             }
    -- Cabal < 1.23 doesn't know about '--profiling-detail'.
    -- Cabal < 1.23 has a hacked up version of 'enable-profiling'
    -- which we shouldn't use.
    (tryLibProfiling, tryExeProfiling) = computeEffectiveProfiling flags
    flags_1_23_0 = flags_1_25_0 { configProfDetail    = NoFlag
                                , configProfLibDetail = NoFlag
                                , configIPID          = NoFlag
                                , configProf          = NoFlag
                                , configProfExe       = Flag tryExeProfiling
                                , configProfLib       = Flag tryLibProfiling
                                }

    -- Cabal < 1.22 doesn't know about '--disable-debug-info'.
    flags_1_22_0 = flags_1_23_0 { configDebugInfo = NoFlag }

    -- Cabal < 1.21.1 doesn't know about 'disable-relocatable'
    -- Cabal < 1.21.1 doesn't know about 'enable-profiling'
    -- (but we already dealt with it in flags_1_23_0)
    flags_1_21_1 =
      flags_1_22_0 { configRelocatable = NoFlag
                   , configCoverage = NoFlag
                   , configLibCoverage = configCoverage flags
                   }
    -- Cabal < 1.19.2 doesn't know about '--exact-configuration' and
    -- '--enable-library-stripping'.
    flags_1_19_2 = flags_1_21_1 { configExactConfiguration = NoFlag
                                , configStripLibs = NoFlag }
    -- Cabal < 1.19.1 uses '--constraint' instead of '--dependency'.
    flags_1_19_1 = flags_1_19_2 { configDependencies = []
                                , configConstraints  = configConstraints flags }
    -- Cabal < 1.18.0 doesn't know about --extra-prog-path and --sysconfdir.
    flags_1_18_0 = flags_1_19_1 { configProgramPathExtra = toNubList []
                                , configInstallDirs = configInstallDirs_1_18_0}
    configInstallDirs_1_18_0 = (configInstallDirs flags_1_19_1) { sysconfdir = NoFlag }
    -- Cabal < 1.14.0 doesn't know about '--disable-benchmarks'.
    flags_1_14_0 = flags_1_18_0 { configBenchmarks  = NoFlag }
    -- Cabal < 1.12.0 doesn't know about '--enable/disable-executable-dynamic'
    -- and '--enable/disable-library-coverage'.
    flags_1_12_0 = flags_1_14_0 { configLibCoverage = NoFlag
                                , configDynExe      = NoFlag }
    -- Cabal < 1.10.0 doesn't know about '--disable-tests'.
    flags_1_10_0 = flags_1_12_0 { configTests       = NoFlag }
    -- Cabal < 1.3.10 does not grok the '--constraints' flag.
    flags_1_3_10 = flags_1_10_0 { configConstraints = [] }

-- | Get the package database settings from 'ConfigFlags', accounting for
-- @--package-db@ and @--user@ flags.
configPackageDB' :: ConfigFlags -> PackageDBStack
configPackageDB' cfg =
    interpretPackageDbFlags userInstall (configPackageDBs cfg)
  where
    userInstall = Cabal.fromFlagOrDefault True (configUserInstall cfg)

-- | Configure the compiler, but reduce verbosity during this step.
configCompilerAux' :: ConfigFlags -> IO (Compiler, Platform, ProgramDb)
configCompilerAux' configFlags =
  configCompilerAuxEx configFlags
    --FIXME: make configCompilerAux use a sensible verbosity
    { configVerbosity = fmap lessVerbose (configVerbosity configFlags) }

-- ------------------------------------------------------------
-- * Config extra flags
-- ------------------------------------------------------------

-- | cabal configure takes some extra flags beyond runghc Setup configure
--
data ConfigExFlags = ConfigExFlags {
    configCabalVersion :: Flag Version,
    configExConstraints:: [(UserConstraint, ConstraintSource)],
    configPreferences  :: [Dependency],
    configSolver       :: Flag PreSolver,
    configAllowNewer   :: Maybe AllowNewer,
    configAllowOlder   :: Maybe AllowOlder
  }
  deriving (Eq, Generic)

defaultConfigExFlags :: ConfigExFlags
defaultConfigExFlags = mempty { configSolver     = Flag defaultSolver }

configureExCommand :: CommandUI (ConfigFlags, ConfigExFlags)
configureExCommand = configureCommand {
    commandDefaultFlags = (mempty, defaultConfigExFlags),
    commandOptions      = \showOrParseArgs ->
         liftOptions fst setFst
         (filter ((`notElem` ["constraint", "dependency", "exact-configuration"])
                  . optionName) $ configureOptions  showOrParseArgs)
      ++ liftOptions snd setSnd
         (configureExOptions showOrParseArgs ConstraintSourceCommandlineFlag)
  }
  where
    setFst a (_,b) = (a,b)
    setSnd b (a,_) = (a,b)

configureExOptions :: ShowOrParseArgs
                   -> ConstraintSource
                   -> [OptionField ConfigExFlags]
configureExOptions _showOrParseArgs src =
  [ option [] ["cabal-lib-version"]
      ("Select which version of the Cabal lib to use to build packages "
      ++ "(useful for testing).")
      configCabalVersion (\v flags -> flags { configCabalVersion = v })
      (reqArg "VERSION" (readP_to_E ("Cannot parse cabal lib version: "++)
                                    (fmap toFlag parse))
                        (map display . flagToList))
  , option [] ["constraint"]
      "Specify constraints on a package (version, installed/source, flags)"
      configExConstraints (\v flags -> flags { configExConstraints = v })
      (reqArg "CONSTRAINT"
              ((\x -> [(x, src)]) `fmap` ReadE readUserConstraint)
              (map $ display . fst))

  , option [] ["preference"]
      "Specify preferences (soft constraints) on the version of a package"
      configPreferences (\v flags -> flags { configPreferences = v })
      (reqArg "CONSTRAINT"
              (readP_to_E (const "dependency expected")
                          (fmap (\x -> [x]) parse))
              (map display))

  , optionSolver configSolver (\v flags -> flags { configSolver = v })

  , option [] ["allow-older"]
    ("Ignore lower bounds in all dependencies or DEPS")
    (fmap unAllowOlder . configAllowOlder)
    (\v flags -> flags { configAllowOlder = fmap AllowOlder v})
    (optArg "DEPS"
     (readP_to_E ("Cannot parse the list of packages: " ++) relaxDepsParser)
     (Just RelaxDepsAll) relaxDepsPrinter)

  , option [] ["allow-newer"]
    ("Ignore upper bounds in all dependencies or DEPS")
    (fmap unAllowNewer . configAllowNewer)
    (\v flags -> flags { configAllowNewer = fmap AllowNewer v})
    (optArg "DEPS"
     (readP_to_E ("Cannot parse the list of packages: " ++) relaxDepsParser)
     (Just RelaxDepsAll) relaxDepsPrinter)

  ]


relaxDepsParser :: Parse.ReadP r (Maybe RelaxDeps)
relaxDepsParser =
  (Just . RelaxDepsSome) `fmap` Parse.sepBy1 parse (Parse.char ',')

relaxDepsPrinter :: (Maybe RelaxDeps) -> [Maybe String]
relaxDepsPrinter Nothing                     = []
relaxDepsPrinter (Just RelaxDepsAll)         = [Nothing]
relaxDepsPrinter (Just (RelaxDepsSome pkgs)) = map (Just . display) $ pkgs


instance Monoid ConfigExFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ConfigExFlags where
  (<>) = gmappend

reconfigureCommand :: CommandUI (ConfigFlags, ConfigExFlags)
reconfigureCommand
  = configureExCommand
    { commandName         = "reconfigure"
    , commandSynopsis     = "Reconfigure the package if necessary."
    , commandDescription  = Just $ \pname -> wrapText $
         "Run `configure` with the most recently used flags, or append FLAGS "
         ++ "to the most recently used configuration. "
         ++ "Accepts the same flags as `" ++ pname ++ " configure'. "
         ++ "If the package has never been configured, the default flags are "
         ++ "used."
    , commandNotes        = Just $ \pname ->
        "Examples:\n"
        ++ "  " ++ pname ++ " reconfigure\n"
        ++ "    Configure with the most recently used flags.\n"
        ++ "  " ++ pname ++ " reconfigure -w PATH\n"
        ++ "    Reconfigure with the most recently used flags,\n"
        ++ "    but use the compiler at PATH.\n\n"
    , commandUsage        = usageAlternatives "reconfigure" [ "[FLAGS]" ]
    , commandDefaultFlags = mempty
    }

-- ------------------------------------------------------------
-- * Build flags
-- ------------------------------------------------------------

data SkipAddSourceDepsCheck =
  SkipAddSourceDepsCheck | DontSkipAddSourceDepsCheck
  deriving Eq

data BuildExFlags = BuildExFlags {
  buildOnly     :: Flag SkipAddSourceDepsCheck
} deriving Generic

buildExOptions :: ShowOrParseArgs -> [OptionField BuildExFlags]
buildExOptions _showOrParseArgs =
  option [] ["only"]
  "Don't reinstall add-source dependencies (sandbox-only)"
  buildOnly (\v flags -> flags { buildOnly = v })
  (noArg (Flag SkipAddSourceDepsCheck))

  : []

buildCommand :: CommandUI (BuildFlags, BuildExFlags)
buildCommand = parent {
    commandDefaultFlags = (commandDefaultFlags parent, mempty),
    commandOptions      =
      \showOrParseArgs -> liftOptions fst setFst
                          (commandOptions parent showOrParseArgs)
                          ++
                          liftOptions snd setSnd (buildExOptions showOrParseArgs)
  }
  where
    setFst a (_,b) = (a,b)
    setSnd b (a,_) = (a,b)

    parent = Cabal.buildCommand defaultProgramDb

instance Monoid BuildExFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup BuildExFlags where
  (<>) = gmappend

-- ------------------------------------------------------------
-- * Repl command
-- ------------------------------------------------------------

replCommand :: CommandUI (ReplFlags, BuildExFlags)
replCommand = parent {
    commandDefaultFlags = (commandDefaultFlags parent, mempty),
    commandOptions      =
      \showOrParseArgs -> liftOptions fst setFst
                          (commandOptions parent showOrParseArgs)
                          ++
                          liftOptions snd setSnd (buildExOptions showOrParseArgs)
  }
  where
    setFst a (_,b) = (a,b)
    setSnd b (a,_) = (a,b)

    parent = Cabal.replCommand defaultProgramDb

-- ------------------------------------------------------------
-- * Test command
-- ------------------------------------------------------------

testCommand :: CommandUI (TestFlags, BuildFlags, BuildExFlags)
testCommand = parent {
  commandDefaultFlags = (commandDefaultFlags parent,
                         Cabal.defaultBuildFlags, mempty),
  commandOptions      =
    \showOrParseArgs -> liftOptions get1 set1
                        (commandOptions parent showOrParseArgs)
                        ++
                        liftOptions get2 set2
                        (Cabal.buildOptions progDb showOrParseArgs)
                        ++
                        liftOptions get3 set3 (buildExOptions showOrParseArgs)
  }
  where
    get1 (a,_,_) = a; set1 a (_,b,c) = (a,b,c)
    get2 (_,b,_) = b; set2 b (a,_,c) = (a,b,c)
    get3 (_,_,c) = c; set3 c (a,b,_) = (a,b,c)

    parent = Cabal.testCommand
    progDb = defaultProgramDb

-- ------------------------------------------------------------
-- * Bench command
-- ------------------------------------------------------------

benchmarkCommand :: CommandUI (BenchmarkFlags, BuildFlags, BuildExFlags)
benchmarkCommand = parent {
  commandDefaultFlags = (commandDefaultFlags parent,
                         Cabal.defaultBuildFlags, mempty),
  commandOptions      =
    \showOrParseArgs -> liftOptions get1 set1
                        (commandOptions parent showOrParseArgs)
                        ++
                        liftOptions get2 set2
                        (Cabal.buildOptions progDb showOrParseArgs)
                        ++
                        liftOptions get3 set3 (buildExOptions showOrParseArgs)
  }
  where
    get1 (a,_,_) = a; set1 a (_,b,c) = (a,b,c)
    get2 (_,b,_) = b; set2 b (a,_,c) = (a,b,c)
    get3 (_,_,c) = c; set3 c (a,b,_) = (a,b,c)

    parent = Cabal.benchmarkCommand
    progDb = defaultProgramDb

-- ------------------------------------------------------------
-- * Fetch command
-- ------------------------------------------------------------

data FetchFlags = FetchFlags {
--    fetchOutput    :: Flag FilePath,
      fetchDeps      :: Flag Bool,
      fetchDryRun    :: Flag Bool,
      fetchSolver           :: Flag PreSolver,
      fetchMaxBackjumps     :: Flag Int,
      fetchReorderGoals     :: Flag ReorderGoals,
      fetchCountConflicts   :: Flag CountConflicts,
      fetchIndependentGoals :: Flag IndependentGoals,
      fetchShadowPkgs       :: Flag ShadowPkgs,
      fetchStrongFlags      :: Flag StrongFlags,
      fetchAllowBootLibInstalls :: Flag AllowBootLibInstalls,
      fetchVerbosity :: Flag Verbosity
    }

defaultFetchFlags :: FetchFlags
defaultFetchFlags = FetchFlags {
--  fetchOutput    = mempty,
    fetchDeps      = toFlag True,
    fetchDryRun    = toFlag False,
    fetchSolver           = Flag defaultSolver,
    fetchMaxBackjumps     = Flag defaultMaxBackjumps,
    fetchReorderGoals     = Flag (ReorderGoals False),
    fetchCountConflicts   = Flag (CountConflicts True),
    fetchIndependentGoals = Flag (IndependentGoals False),
    fetchShadowPkgs       = Flag (ShadowPkgs False),
    fetchStrongFlags      = Flag (StrongFlags False),
    fetchAllowBootLibInstalls = Flag (AllowBootLibInstalls False),
    fetchVerbosity = toFlag normal
   }

fetchCommand :: CommandUI FetchFlags
fetchCommand = CommandUI {
    commandName         = "fetch",
    commandSynopsis     = "Downloads packages for later installation.",
    commandUsage        = usageAlternatives "fetch" [ "[FLAGS] PACKAGES"
                                                    ],
    commandDescription  = Just $ \_ ->
          "Note that it currently is not possible to fetch the dependencies for a\n"
       ++ "package in the current directory.\n",
    commandNotes        = Nothing,
    commandDefaultFlags = defaultFetchFlags,
    commandOptions      = \ showOrParseArgs -> [
         optionVerbosity fetchVerbosity (\v flags -> flags { fetchVerbosity = v })

--     , option "o" ["output"]
--         "Put the package(s) somewhere specific rather than the usual cache."
--         fetchOutput (\v flags -> flags { fetchOutput = v })
--         (reqArgFlag "PATH")

       , option [] ["dependencies", "deps"]
           "Resolve and fetch dependencies (default)"
           fetchDeps (\v flags -> flags { fetchDeps = v })
           trueArg

       , option [] ["no-dependencies", "no-deps"]
           "Ignore dependencies"
           fetchDeps (\v flags -> flags { fetchDeps = v })
           falseArg

       , option [] ["dry-run"]
           "Do not install anything, only print what would be installed."
           fetchDryRun (\v flags -> flags { fetchDryRun = v })
           trueArg

       ] ++

       optionSolver      fetchSolver           (\v flags -> flags { fetchSolver           = v }) :
       optionSolverFlags showOrParseArgs
                         fetchMaxBackjumps     (\v flags -> flags { fetchMaxBackjumps     = v })
                         fetchReorderGoals     (\v flags -> flags { fetchReorderGoals     = v })
                         fetchCountConflicts   (\v flags -> flags { fetchCountConflicts   = v })
                         fetchIndependentGoals (\v flags -> flags { fetchIndependentGoals = v })
                         fetchShadowPkgs       (\v flags -> flags { fetchShadowPkgs       = v })
                         fetchStrongFlags      (\v flags -> flags { fetchStrongFlags      = v })
                         fetchAllowBootLibInstalls (\v flags -> flags { fetchAllowBootLibInstalls = v })

  }

-- ------------------------------------------------------------
-- * Freeze command
-- ------------------------------------------------------------

data FreezeFlags = FreezeFlags {
      freezeDryRun           :: Flag Bool,
      freezeTests            :: Flag Bool,
      freezeBenchmarks       :: Flag Bool,
      freezeSolver           :: Flag PreSolver,
      freezeMaxBackjumps     :: Flag Int,
      freezeReorderGoals     :: Flag ReorderGoals,
      freezeCountConflicts   :: Flag CountConflicts,
      freezeIndependentGoals :: Flag IndependentGoals,
      freezeShadowPkgs       :: Flag ShadowPkgs,
      freezeStrongFlags      :: Flag StrongFlags,
      freezeAllowBootLibInstalls :: Flag AllowBootLibInstalls,
      freezeVerbosity        :: Flag Verbosity
    }

defaultFreezeFlags :: FreezeFlags
defaultFreezeFlags = FreezeFlags {
    freezeDryRun           = toFlag False,
    freezeTests            = toFlag False,
    freezeBenchmarks       = toFlag False,
    freezeSolver           = Flag defaultSolver,
    freezeMaxBackjumps     = Flag defaultMaxBackjumps,
    freezeReorderGoals     = Flag (ReorderGoals False),
    freezeCountConflicts   = Flag (CountConflicts True),
    freezeIndependentGoals = Flag (IndependentGoals False),
    freezeShadowPkgs       = Flag (ShadowPkgs False),
    freezeStrongFlags      = Flag (StrongFlags False),
    freezeAllowBootLibInstalls = Flag (AllowBootLibInstalls False),
    freezeVerbosity        = toFlag normal
   }

freezeCommand :: CommandUI FreezeFlags
freezeCommand = CommandUI {
    commandName         = "freeze",
    commandSynopsis     = "Freeze dependencies.",
    commandDescription  = Just $ \_ -> wrapText $
         "Calculates a valid set of dependencies and their exact versions. "
      ++ "If successful, saves the result to the file `cabal.config`.\n"
      ++ "\n"
      ++ "The package versions specified in `cabal.config` will be used for "
      ++ "any future installs.\n"
      ++ "\n"
      ++ "An existing `cabal.config` is ignored and overwritten.\n",
    commandNotes        = Nothing,
    commandUsage        = usageFlags "freeze",
    commandDefaultFlags = defaultFreezeFlags,
    commandOptions      = \ showOrParseArgs -> [
         optionVerbosity freezeVerbosity
         (\v flags -> flags { freezeVerbosity = v })

       , option [] ["dry-run"]
           "Do not freeze anything, only print what would be frozen"
           freezeDryRun (\v flags -> flags { freezeDryRun = v })
           trueArg

       , option [] ["tests"]
           ("freezing of the dependencies of any tests suites "
            ++ "in the package description file.")
           freezeTests (\v flags -> flags { freezeTests = v })
           (boolOpt [] [])

       , option [] ["benchmarks"]
           ("freezing of the dependencies of any benchmarks suites "
            ++ "in the package description file.")
           freezeBenchmarks (\v flags -> flags { freezeBenchmarks = v })
           (boolOpt [] [])

       ] ++

       optionSolver
         freezeSolver           (\v flags -> flags { freezeSolver           = v }):
       optionSolverFlags showOrParseArgs
                         freezeMaxBackjumps     (\v flags -> flags { freezeMaxBackjumps     = v })
                         freezeReorderGoals     (\v flags -> flags { freezeReorderGoals     = v })
                         freezeCountConflicts   (\v flags -> flags { freezeCountConflicts   = v })
                         freezeIndependentGoals (\v flags -> flags { freezeIndependentGoals = v })
                         freezeShadowPkgs       (\v flags -> flags { freezeShadowPkgs       = v })
                         freezeStrongFlags      (\v flags -> flags { freezeStrongFlags      = v })
                         freezeAllowBootLibInstalls (\v flags -> flags { freezeAllowBootLibInstalls = v })

  }

-- ------------------------------------------------------------
-- * 'gen-bounds' command
-- ------------------------------------------------------------

genBoundsCommand :: CommandUI FreezeFlags
genBoundsCommand = CommandUI {
    commandName         = "gen-bounds",
    commandSynopsis     = "Generate dependency bounds.",
    commandDescription  = Just $ \_ -> wrapText $
         "Generates bounds for all dependencies that do not currently have them. "
      ++ "Generated bounds are printed to stdout.  "
      ++ "You can then paste them into your .cabal file.\n"
      ++ "\n",
    commandNotes        = Nothing,
    commandUsage        = usageFlags "gen-bounds",
    commandDefaultFlags = defaultFreezeFlags,
    commandOptions      = \ _ -> [
     optionVerbosity freezeVerbosity (\v flags -> flags { freezeVerbosity = v })
     ]
  }

-- ------------------------------------------------------------
-- * 'outdated' command
-- ------------------------------------------------------------

data IgnoreMajorVersionBumps = IgnoreMajorVersionBumpsNone
                             | IgnoreMajorVersionBumpsAll
                             | IgnoreMajorVersionBumpsSome [PackageName]

instance Monoid IgnoreMajorVersionBumps where
  mempty  = IgnoreMajorVersionBumpsNone
  mappend = (<>)

instance Semigroup IgnoreMajorVersionBumps where
  IgnoreMajorVersionBumpsNone       <> r                               = r
  l@IgnoreMajorVersionBumpsAll      <> _                               = l
  l@(IgnoreMajorVersionBumpsSome _) <> IgnoreMajorVersionBumpsNone     = l
  (IgnoreMajorVersionBumpsSome   _) <> r@IgnoreMajorVersionBumpsAll    = r
  (IgnoreMajorVersionBumpsSome   a) <> (IgnoreMajorVersionBumpsSome b) =
    IgnoreMajorVersionBumpsSome (a ++ b)

data OutdatedFlags = OutdatedFlags {
  outdatedVerbosity     :: Flag Verbosity,
  outdatedFreezeFile    :: Flag Bool,
  outdatedNewFreezeFile :: Flag Bool,
  outdatedSimpleOutput  :: Flag Bool,
  outdatedExitCode      :: Flag Bool,
  outdatedQuiet         :: Flag Bool,
  outdatedIgnore        :: [PackageName],
  outdatedMinor         :: Maybe IgnoreMajorVersionBumps
  }

defaultOutdatedFlags :: OutdatedFlags
defaultOutdatedFlags = OutdatedFlags {
  outdatedVerbosity     = toFlag normal,
  outdatedFreezeFile    = mempty,
  outdatedNewFreezeFile = mempty,
  outdatedSimpleOutput  = mempty,
  outdatedExitCode      = mempty,
  outdatedQuiet         = mempty,
  outdatedIgnore        = mempty,
  outdatedMinor         = mempty
  }

outdatedCommand :: CommandUI OutdatedFlags
outdatedCommand = CommandUI {
  commandName = "outdated",
  commandSynopsis = "Check for outdated dependencies",
  commandDescription  = Just $ \_ -> wrapText $
    "Checks for outdated dependencies in the package description file "
    ++ "or freeze file",
  commandNotes = Nothing,
  commandUsage = usageFlags "outdated",
  commandDefaultFlags = defaultOutdatedFlags,
  commandOptions      = \ _ -> [
    optionVerbosity outdatedVerbosity
      (\v flags -> flags { outdatedVerbosity = v })

    ,option [] ["freeze-file"]
     "Act on the freeze file"
     outdatedFreezeFile (\v flags -> flags { outdatedFreezeFile = v })
     trueArg

    ,option [] ["new-freeze-file"]
     "Act on the new-style freeze file"
     outdatedNewFreezeFile (\v flags -> flags { outdatedNewFreezeFile = v })
     trueArg

    ,option [] ["simple-output"]
     "Only print names of outdated dependencies, one per line"
     outdatedSimpleOutput (\v flags -> flags { outdatedSimpleOutput = v })
     trueArg

    ,option [] ["exit-code"]
     "Exit with non-zero when there are outdated dependencies"
     outdatedExitCode (\v flags -> flags { outdatedExitCode = v })
     trueArg

    ,option ['q'] ["quiet"]
     "Don't print any output. Implies '--exit-code' and '-v0'"
     outdatedQuiet (\v flags -> flags { outdatedQuiet = v })
     trueArg

   ,option [] ["ignore"]
    "Packages to ignore"
    outdatedIgnore (\v flags -> flags { outdatedIgnore = v })
    (reqArg "PKGS" pkgNameListParser (map display))

   ,option [] ["minor"]
    "Ignore major version bumps for these packages"
    outdatedMinor (\v flags -> flags { outdatedMinor = v })
    (optArg "PKGS" ignoreMajorVersionBumpsParser
      (Just IgnoreMajorVersionBumpsAll) ignoreMajorVersionBumpsPrinter)
   ]
  }
  where
    ignoreMajorVersionBumpsPrinter :: (Maybe IgnoreMajorVersionBumps)
                                   -> [Maybe String]
    ignoreMajorVersionBumpsPrinter Nothing = []
    ignoreMajorVersionBumpsPrinter (Just IgnoreMajorVersionBumpsNone)= []
    ignoreMajorVersionBumpsPrinter (Just IgnoreMajorVersionBumpsAll) = [Nothing]
    ignoreMajorVersionBumpsPrinter (Just (IgnoreMajorVersionBumpsSome pkgs)) =
      map (Just . display) $ pkgs

    ignoreMajorVersionBumpsParser  =
      (Just . IgnoreMajorVersionBumpsSome) `fmap` pkgNameListParser

    pkgNameListParser = readP_to_E
      ("Couldn't parse the list of package names: " ++)
      (Parse.sepBy1 parse (Parse.char ','))

-- ------------------------------------------------------------
-- * Update command
-- ------------------------------------------------------------

data UpdateFlags
    = UpdateFlags {
        updateVerbosity  :: Flag Verbosity,
        updateIndexState :: Flag IndexState
    } deriving Generic

defaultUpdateFlags :: UpdateFlags
defaultUpdateFlags
    = UpdateFlags {
        updateVerbosity  = toFlag normal,
        updateIndexState = toFlag IndexStateHead
    }

updateCommand  :: CommandUI UpdateFlags
updateCommand = CommandUI {
    commandName         = "update",
    commandSynopsis     = "Updates list of known packages.",
    commandDescription  = Just $ \_ ->
      "For all known remote repositories, download the package list.\n",
    commandNotes        = Just $ \_ ->
      relevantConfigValuesText ["remote-repo"
                               ,"remote-repo-cache"
                               ,"local-repo"],
    commandUsage        = usageFlags "update",
    commandDefaultFlags = defaultUpdateFlags,
    commandOptions      = \_ -> [
        optionVerbosity updateVerbosity (\v flags -> flags { updateVerbosity = v }),
        option [] ["index-state"]
          ("Update the source package index to its state as it existed at a previous time. " ++
           "Accepts unix-timestamps (e.g. '@1474732068'), ISO8601 UTC timestamps " ++
           "(e.g. '2016-09-24T17:47:48Z'), or 'HEAD' (default: 'HEAD').")
          updateIndexState (\v flags -> flags { updateIndexState = v })
          (reqArg "STATE" (readP_to_E (const $ "index-state must be a  " ++
                                       "unix-timestamps (e.g. '@1474732068'), " ++
                                       "a ISO8601 UTC timestamp " ++
                                       "(e.g. '2016-09-24T17:47:48Z'), or 'HEAD'")
                                      (toFlag `fmap` parse))
                          (flagToList . fmap display))
    ]
  }

-- ------------------------------------------------------------
-- * Other commands
-- ------------------------------------------------------------

upgradeCommand  :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
upgradeCommand = configureCommand {
    commandName         = "upgrade",
    commandSynopsis     = "(command disabled, use install instead)",
    commandDescription  = Nothing,
    commandUsage        = usageFlagsOrPackages "upgrade",
    commandDefaultFlags = (mempty, mempty, mempty, mempty),
    commandOptions      = commandOptions installCommand
  }

{-
cleanCommand  :: CommandUI ()
cleanCommand = makeCommand name shortDesc longDesc emptyFlags options
  where
    name       = "clean"
    shortDesc  = "Removes downloaded files"
    longDesc   = Nothing
    emptyFlags = ()
    options _  = []
-}

checkCommand  :: CommandUI (Flag Verbosity)
checkCommand = CommandUI {
    commandName         = "check",
    commandSynopsis     = "Check the package for common mistakes.",
    commandDescription  = Just $ \_ -> wrapText $
         "Expects a .cabal package file in the current directory.\n"
      ++ "\n"
      ++ "The checks correspond to the requirements to packages on Hackage. "
      ++ "If no errors and warnings are reported, Hackage will accept this "
      ++ "package.\n",
    commandNotes        = Nothing,
    commandUsage        = \pname -> "Usage: " ++ pname ++ " check\n",
    commandDefaultFlags = toFlag normal,
    commandOptions      = \_ -> []
  }

formatCommand  :: CommandUI (Flag Verbosity)
formatCommand = CommandUI {
    commandName         = "format",
    commandSynopsis     = "Reformat the .cabal file using the standard style.",
    commandDescription  = Nothing,
    commandNotes        = Nothing,
    commandUsage        = usageAlternatives "format" ["[FILE]"],
    commandDefaultFlags = toFlag normal,
    commandOptions      = \_ -> []
  }

uninstallCommand  :: CommandUI (Flag Verbosity)
uninstallCommand = CommandUI {
    commandName         = "uninstall",
    commandSynopsis     = "Warn about 'uninstall' not being implemented.",
    commandDescription  = Nothing,
    commandNotes        = Nothing,
    commandUsage        = usageAlternatives "uninstall" ["PACKAGES"],
    commandDefaultFlags = toFlag normal,
    commandOptions      = \_ -> []
  }

manpageCommand :: CommandUI (Flag Verbosity)
manpageCommand = CommandUI {
    commandName         = "manpage",
    commandSynopsis     = "Outputs manpage source.",
    commandDescription  = Just $ \_ ->
      "Output manpage source to STDOUT.\n",
    commandNotes        = Nothing,
    commandUsage        = usageFlags "manpage",
    commandDefaultFlags = toFlag normal,
    commandOptions      = \_ -> [optionVerbosity id const]
  }

runCommand :: CommandUI (BuildFlags, BuildExFlags)
runCommand = CommandUI {
    commandName         = "run",
    commandSynopsis     = "Builds and runs an executable.",
    commandDescription  = Just $ \pname -> wrapText $
         "Builds and then runs the specified executable. If no executable is "
      ++ "specified, but the package contains just one executable, that one "
      ++ "is built and executed.\n"
      ++ "\n"
      ++ "Use `" ++ pname ++ " test --show-details=streaming` to run a "
      ++ "test-suite and get its full output.\n",
    commandNotes        = Just $ \pname ->
          "Examples:\n"
       ++ "  " ++ pname ++ " run\n"
       ++ "    Run the only executable in the current package;\n"
       ++ "  " ++ pname ++ " run foo -- --fooflag\n"
       ++ "    Works similar to `./foo --fooflag`.\n",
    commandUsage        = usageAlternatives "run"
        ["[FLAGS] [EXECUTABLE] [-- EXECUTABLE_FLAGS]"],
    commandDefaultFlags = mempty,
    commandOptions      =
      \showOrParseArgs -> liftOptions fst setFst
                          (commandOptions parent showOrParseArgs)
                          ++
                          liftOptions snd setSnd
                          (buildExOptions showOrParseArgs)
  }
  where
    setFst a (_,b) = (a,b)
    setSnd b (a,_) = (a,b)

    parent = Cabal.buildCommand defaultProgramDb

-- ------------------------------------------------------------
-- * Report flags
-- ------------------------------------------------------------

data ReportFlags = ReportFlags {
    reportUsername  :: Flag Username,
    reportPassword  :: Flag Password,
    reportVerbosity :: Flag Verbosity
  } deriving Generic

defaultReportFlags :: ReportFlags
defaultReportFlags = ReportFlags {
    reportUsername  = mempty,
    reportPassword  = mempty,
    reportVerbosity = toFlag normal
  }

reportCommand :: CommandUI ReportFlags
reportCommand = CommandUI {
    commandName         = "report",
    commandSynopsis     = "Upload build reports to a remote server.",
    commandDescription  = Nothing,
    commandNotes        = Just $ \_ ->
         "You can store your Hackage login in the ~/.cabal/config file\n",
    commandUsage        = usageAlternatives "report" ["[FLAGS]"],
    commandDefaultFlags = defaultReportFlags,
    commandOptions      = \_ ->
      [optionVerbosity reportVerbosity (\v flags -> flags { reportVerbosity = v })

      ,option ['u'] ["username"]
        "Hackage username."
        reportUsername (\v flags -> flags { reportUsername = v })
        (reqArg' "USERNAME" (toFlag . Username)
                            (flagToList . fmap unUsername))

      ,option ['p'] ["password"]
        "Hackage password."
        reportPassword (\v flags -> flags { reportPassword = v })
        (reqArg' "PASSWORD" (toFlag . Password)
                            (flagToList . fmap unPassword))
      ]
  }

instance Monoid ReportFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ReportFlags where
  (<>) = gmappend

-- ------------------------------------------------------------
-- * Get flags
-- ------------------------------------------------------------

data GetFlags = GetFlags {
    getDestDir          :: Flag FilePath,
    getPristine         :: Flag Bool,
    getIndexState       :: Flag IndexState,
    getSourceRepository :: Flag (Maybe RepoKind),
    getVerbosity        :: Flag Verbosity
  } deriving Generic

defaultGetFlags :: GetFlags
defaultGetFlags = GetFlags {
    getDestDir          = mempty,
    getPristine         = mempty,
    getIndexState       = mempty,
    getSourceRepository = mempty,
    getVerbosity        = toFlag normal
   }

getCommand :: CommandUI GetFlags
getCommand = CommandUI {
    commandName         = "get",
    commandSynopsis     = "Download/Extract a package's source code (repository).",
    commandDescription  = Just $ \_ -> wrapText $
          "Creates a local copy of a package's source code. By default it gets "
       ++ "the source\ntarball and unpacks it in a local subdirectory. "
       ++ "Alternatively, with -s it will\nget the code from the source "
       ++ "repository specified by the package.\n",
    commandNotes        = Just $ \pname ->
          "Examples:\n"
       ++ "  " ++ pname ++ " get hlint\n"
       ++ "    Download the latest stable version of hlint;\n"
       ++ "  " ++ pname ++ " get lens --source-repository=head\n"
       ++ "    Download the source repository (i.e. git clone from github).\n",
    commandUsage        = usagePackages "get",
    commandDefaultFlags = defaultGetFlags,
    commandOptions      = \_ -> [
        optionVerbosity getVerbosity (\v flags -> flags { getVerbosity = v })

       ,option "d" ["destdir"]
         "Where to place the package source, defaults to the current directory."
         getDestDir (\v flags -> flags { getDestDir = v })
         (reqArgFlag "PATH")

       ,option "s" ["source-repository"]
         "Copy the package's source repository (ie git clone, darcs get, etc as appropriate)."
         getSourceRepository (\v flags -> flags { getSourceRepository = v })
        (optArg "[head|this|...]" (readP_to_E (const "invalid source-repository")
                                              (fmap (toFlag . Just) parse))
                                  (Flag Nothing)
                                  (map (fmap show) . flagToList))

      , option [] ["index-state"]
          ("Use source package index state as it existed at a previous time. " ++
           "Accepts unix-timestamps (e.g. '@1474732068'), ISO8601 UTC timestamps " ++
           "(e.g. '2016-09-24T17:47:48Z'), or 'HEAD' (default: 'HEAD'). " ++
           "This determines which package versions are available as well as " ++
           ".cabal file revision is selected (unless --pristine is used).")
          getIndexState (\v flags -> flags { getIndexState = v })
          (reqArg "STATE" (readP_to_E (const $ "index-state must be a  " ++
                                       "unix-timestamps (e.g. '@1474732068'), " ++
                                       "a ISO8601 UTC timestamp " ++
                                       "(e.g. '2016-09-24T17:47:48Z'), or 'HEAD'")
                                      (toFlag `fmap` parse))
                          (flagToList . fmap display))

       , option [] ["pristine"]
           ("Unpack the original pristine tarball, rather than updating the "
           ++ ".cabal file with the latest revision from the package archive.")
           getPristine (\v flags -> flags { getPristine = v })
           trueArg
       ]
  }

-- 'cabal unpack' is a deprecated alias for 'cabal get'.
unpackCommand :: CommandUI GetFlags
unpackCommand = getCommand {
  commandName  = "unpack",
  commandUsage = usagePackages "unpack"
  }

instance Monoid GetFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup GetFlags where
  (<>) = gmappend

-- ------------------------------------------------------------
-- * List flags
-- ------------------------------------------------------------

data ListFlags = ListFlags {
    listInstalled    :: Flag Bool,
    listSimpleOutput :: Flag Bool,
    listVerbosity    :: Flag Verbosity,
    listPackageDBs   :: [Maybe PackageDB]
  } deriving Generic

defaultListFlags :: ListFlags
defaultListFlags = ListFlags {
    listInstalled    = Flag False,
    listSimpleOutput = Flag False,
    listVerbosity    = toFlag normal,
    listPackageDBs   = []
  }

listCommand  :: CommandUI ListFlags
listCommand = CommandUI {
    commandName         = "list",
    commandSynopsis     = "List packages matching a search string.",
    commandDescription  = Just $ \_ -> wrapText $
         "List all packages, or all packages matching one of the search"
      ++ " strings.\n"
      ++ "\n"
      ++ "If there is a sandbox in the current directory and "
      ++ "config:ignore-sandbox is False, use the sandbox package database. "
      ++ "Otherwise, use the package database specified with --package-db. "
      ++ "If not specified, use the user package database.\n",
    commandNotes        = Just $ \pname ->
         "Examples:\n"
      ++ "  " ++ pname ++ " list pandoc\n"
      ++ "    Will find pandoc, pandoc-citeproc, pandoc-lens, ...\n",
    commandUsage        = usageAlternatives "list" [ "[FLAGS]"
                                                   , "[FLAGS] STRINGS"],
    commandDefaultFlags = defaultListFlags,
    commandOptions      = \_ -> [
        optionVerbosity listVerbosity (\v flags -> flags { listVerbosity = v })

        , option [] ["installed"]
            "Only print installed packages"
            listInstalled (\v flags -> flags { listInstalled = v })
            trueArg

        , option [] ["simple-output"]
            "Print in a easy-to-parse format"
            listSimpleOutput (\v flags -> flags { listSimpleOutput = v })
            trueArg

        , option "" ["package-db"]
          (   "Append the given package database to the list of package"
           ++ " databases used (to satisfy dependencies and register into)."
           ++ " May be a specific file, 'global' or 'user'. The initial list"
           ++ " is ['global'], ['global', 'user'], or ['global', $sandbox],"
           ++ " depending on context. Use 'clear' to reset the list to empty."
           ++ " See the user guide for details.")
          listPackageDBs (\v flags -> flags { listPackageDBs = v })
          (reqArg' "DB" readPackageDbList showPackageDbList)

        ]
  }

instance Monoid ListFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ListFlags where
  (<>) = gmappend

-- ------------------------------------------------------------
-- * Info flags
-- ------------------------------------------------------------

data InfoFlags = InfoFlags {
    infoVerbosity  :: Flag Verbosity,
    infoPackageDBs :: [Maybe PackageDB]
  } deriving Generic

defaultInfoFlags :: InfoFlags
defaultInfoFlags = InfoFlags {
    infoVerbosity  = toFlag normal,
    infoPackageDBs = []
  }

infoCommand  :: CommandUI InfoFlags
infoCommand = CommandUI {
    commandName         = "info",
    commandSynopsis     = "Display detailed information about a particular package.",
    commandDescription  = Just $ \_ -> wrapText $
         "If there is a sandbox in the current directory and "
      ++ "config:ignore-sandbox is False, use the sandbox package database. "
      ++ "Otherwise, use the package database specified with --package-db. "
      ++ "If not specified, use the user package database.\n",
    commandNotes        = Nothing,
    commandUsage        = usageAlternatives "info" ["[FLAGS] PACKAGES"],
    commandDefaultFlags = defaultInfoFlags,
    commandOptions      = \_ -> [
        optionVerbosity infoVerbosity (\v flags -> flags { infoVerbosity = v })

        , option "" ["package-db"]
          (   "Append the given package database to the list of package"
           ++ " databases used (to satisfy dependencies and register into)."
           ++ " May be a specific file, 'global' or 'user'. The initial list"
           ++ " is ['global'], ['global', 'user'], or ['global', $sandbox],"
           ++ " depending on context. Use 'clear' to reset the list to empty."
           ++ " See the user guide for details.")
          infoPackageDBs (\v flags -> flags { infoPackageDBs = v })
          (reqArg' "DB" readPackageDbList showPackageDbList)

        ]
  }

instance Monoid InfoFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup InfoFlags where
  (<>) = gmappend

-- ------------------------------------------------------------
-- * Install flags
-- ------------------------------------------------------------

-- | Install takes the same flags as configure along with a few extras.
--
data InstallFlags = InstallFlags {
    installDocumentation    :: Flag Bool,
    installHaddockIndex     :: Flag PathTemplate,
    installDryRun           :: Flag Bool,
    installMaxBackjumps     :: Flag Int,
    installReorderGoals     :: Flag ReorderGoals,
    installCountConflicts   :: Flag CountConflicts,
    installIndependentGoals :: Flag IndependentGoals,
    installShadowPkgs       :: Flag ShadowPkgs,
    installStrongFlags      :: Flag StrongFlags,
    installAllowBootLibInstalls :: Flag AllowBootLibInstalls,
    installReinstall        :: Flag Bool,
    installAvoidReinstalls  :: Flag AvoidReinstalls,
    installOverrideReinstall :: Flag Bool,
    installUpgradeDeps      :: Flag Bool,
    installOnly             :: Flag Bool,
    installOnlyDeps         :: Flag Bool,
    installIndexState       :: Flag IndexState,
    installRootCmd          :: Flag String,
    installSummaryFile      :: NubList PathTemplate,
    installLogFile          :: Flag PathTemplate,
    installBuildReports     :: Flag ReportLevel,
    installReportPlanningFailure :: Flag Bool,
    installSymlinkBinDir    :: Flag FilePath,
    installPerComponent     :: Flag Bool,
    installOneShot          :: Flag Bool,
    installNumJobs          :: Flag (Maybe Int),
    installKeepGoing        :: Flag Bool,
    installRunTests         :: Flag Bool,
    installOfflineMode      :: Flag Bool,
    -- | The cabal project file name; defaults to @cabal.project@.
    -- Th name itself denotes the cabal project file name, but it also
    -- is the base of auxiliary project files, such as
    -- @cabal.project.local@ and @cabal.project.freeze@ which are also
    -- read and written out in some cases.  If the path is not found
    -- in the current working directory, we will successively probe
    -- relative to parent directories until this name is found.
    installProjectFileName   :: Flag FilePath
  }
  deriving (Eq, Generic)

instance Binary InstallFlags

defaultInstallFlags :: InstallFlags
defaultInstallFlags = InstallFlags {
    installDocumentation   = Flag False,
    installHaddockIndex    = Flag docIndexFile,
    installDryRun          = Flag False,
    installMaxBackjumps    = Flag defaultMaxBackjumps,
    installReorderGoals    = Flag (ReorderGoals False),
    installCountConflicts  = Flag (CountConflicts True),
    installIndependentGoals= Flag (IndependentGoals False),
    installShadowPkgs      = Flag (ShadowPkgs False),
    installStrongFlags     = Flag (StrongFlags False),
    installAllowBootLibInstalls = Flag (AllowBootLibInstalls False),
    installReinstall       = Flag False,
    installAvoidReinstalls = Flag (AvoidReinstalls False),
    installOverrideReinstall = Flag False,
    installUpgradeDeps     = Flag False,
    installOnly            = Flag False,
    installOnlyDeps        = Flag False,
    installIndexState      = mempty,
    installRootCmd         = mempty,
    installSummaryFile     = mempty,
    installLogFile         = mempty,
    installBuildReports    = Flag NoReports,
    installReportPlanningFailure = Flag False,
    installSymlinkBinDir   = mempty,
    installPerComponent    = Flag True,
    installOneShot         = Flag False,
    installNumJobs         = mempty,
    installKeepGoing       = Flag False,
    installRunTests        = mempty,
    installOfflineMode     = Flag False,
    installProjectFileName = mempty
  }
  where
    docIndexFile = toPathTemplate ("$datadir" </> "doc"
                                   </> "$arch-$os-$compiler" </> "index.html")

defaultMaxBackjumps :: Int
defaultMaxBackjumps = 2000

defaultSolver :: PreSolver
defaultSolver = AlwaysModular

allSolvers :: String
allSolvers = intercalate ", " (map display ([minBound .. maxBound] :: [PreSolver]))

installCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
installCommand = CommandUI {
  commandName         = "install",
  commandSynopsis     = "Install packages.",
  commandUsage        = usageAlternatives "install" [ "[FLAGS]"
                                                    , "[FLAGS] PACKAGES"
                                                    ],
  commandDescription  = Just $ \_ -> wrapText $
        "Installs one or more packages. By default, the installed package"
     ++ " will be registered in the user's package database or, if a sandbox"
     ++ " is present in the current directory, inside the sandbox.\n"
     ++ "\n"
     ++ "If PACKAGES are specified, downloads and installs those packages."
     ++ " Otherwise, install the package in the current directory (and/or its"
     ++ " dependencies) (there must be exactly one .cabal file in the current"
     ++ " directory).\n"
     ++ "\n"
     ++ "When using a sandbox, the flags for `install` only affect the"
     ++ " current command and have no effect on future commands. (To achieve"
     ++ " that, `configure` must be used.)\n"
     ++ " In contrast, without a sandbox, the flags to `install` are saved and"
     ++ " affect future commands such as `build` and `repl`. See the help for"
     ++ " `configure` for a list of commands being affected.\n"
     ++ "\n"
     ++ "Installed executables will by default (and without a sandbox)"
     ++ " be put into `~/.cabal/bin/`."
     ++ " If you want installed executable to be available globally, make"
     ++ " sure that the PATH environment variable contains that directory.\n"
     ++ "When using a sandbox, executables will be put into"
     ++ " `$SANDBOX/bin/` (by default: `./.cabal-sandbox/bin/`).\n"
     ++ "\n"
     ++ "When specifying --bindir, consider also specifying --datadir;"
     ++ " this way the sandbox can be deleted and the executable should"
     ++ " continue working as long as bindir and datadir are left untouched.",
  commandNotes        = Just $ \pname ->
        ( case commandNotes
               $ Cabal.configureCommand defaultProgramDb
          of Just desc -> desc pname ++ "\n"
             Nothing   -> ""
        )
     ++ "Examples:\n"
     ++ "  " ++ pname ++ " install                 "
     ++ "    Package in the current directory\n"
     ++ "  " ++ pname ++ " install foo             "
     ++ "    Package from the hackage server\n"
     ++ "  " ++ pname ++ " install foo-1.0         "
     ++ "    Specific version of a package\n"
     ++ "  " ++ pname ++ " install 'foo < 2'       "
     ++ "    Constrained package version\n"
     ++ "  " ++ pname ++ " install haddock --bindir=$HOME/hask-bin/ --datadir=$HOME/hask-data/\n"
     ++ "  " ++ (map (const ' ') pname)
                      ++ "                         "
     ++ "    Change installation destination\n",
  commandDefaultFlags = (mempty, mempty, mempty, mempty),
  commandOptions      = \showOrParseArgs ->
       liftOptions get1 set1
       (filter ((`notElem` ["constraint", "dependency"
                           , "exact-configuration"])
                . optionName) $
                              configureOptions   showOrParseArgs)
    ++ liftOptions get2 set2 (configureExOptions showOrParseArgs ConstraintSourceCommandlineFlag)
    ++ liftOptions get3 set3 (installOptions     showOrParseArgs)
    ++ liftOptions get4 set4 (haddockOptions     showOrParseArgs)
  }
  where
    get1 (a,_,_,_) = a; set1 a (_,b,c,d) = (a,b,c,d)
    get2 (_,b,_,_) = b; set2 b (a,_,c,d) = (a,b,c,d)
    get3 (_,_,c,_) = c; set3 c (a,b,_,d) = (a,b,c,d)
    get4 (_,_,_,d) = d; set4 d (a,b,c,_) = (a,b,c,d)

haddockOptions :: ShowOrParseArgs -> [OptionField HaddockFlags]
haddockOptions showOrParseArgs
  = [ opt { optionName = "haddock-" ++ name,
            optionDescr = [ fmapOptFlags (\(_, lflags) -> ([], map ("haddock-" ++) lflags)) descr
                          | descr <- optionDescr opt] }
    | opt <- commandOptions Cabal.haddockCommand showOrParseArgs
    , let name = optionName opt
    , name `elem` ["hoogle", "html", "html-location"
                  ,"executables", "tests", "benchmarks", "all", "internal", "css"
                  ,"hyperlink-source", "hscolour-css"
                  ,"contents-location", "for-hackage"]
    ]
  where
    fmapOptFlags :: (OptFlags -> OptFlags) -> OptDescr a -> OptDescr a
    fmapOptFlags modify (ReqArg d f p r w)    = ReqArg d (modify f) p r w
    fmapOptFlags modify (OptArg d f p r i w)  = OptArg d (modify f) p r i w
    fmapOptFlags modify (ChoiceOpt xs)        = ChoiceOpt [(d, modify f, i, w) | (d, f, i, w) <- xs]
    fmapOptFlags modify (BoolOpt d f1 f2 r w) = BoolOpt d (modify f1) (modify f2) r w

installOptions ::  ShowOrParseArgs -> [OptionField InstallFlags]
installOptions showOrParseArgs =
      [ option "" ["documentation"]
          "building of documentation"
          installDocumentation (\v flags -> flags { installDocumentation = v })
          (boolOpt [] [])

      , option [] ["doc-index-file"]
          "A central index of haddock API documentation (template cannot use $pkgid)"
          installHaddockIndex (\v flags -> flags { installHaddockIndex = v })
          (reqArg' "TEMPLATE" (toFlag.toPathTemplate)
                              (flagToList . fmap fromPathTemplate))

      , option [] ["dry-run"]
          "Do not install anything, only print what would be installed."
          installDryRun (\v flags -> flags { installDryRun = v })
          trueArg
      ] ++

      optionSolverFlags showOrParseArgs
                        installMaxBackjumps     (\v flags -> flags { installMaxBackjumps     = v })
                        installReorderGoals     (\v flags -> flags { installReorderGoals     = v })
                        installCountConflicts   (\v flags -> flags { installCountConflicts   = v })
                        installIndependentGoals (\v flags -> flags { installIndependentGoals = v })
                        installShadowPkgs       (\v flags -> flags { installShadowPkgs       = v })
                        installStrongFlags      (\v flags -> flags { installStrongFlags      = v })
                        installAllowBootLibInstalls (\v flags -> flags { installAllowBootLibInstalls = v }) ++

      [ option [] ["reinstall"]
          "Install even if it means installing the same version again."
          installReinstall (\v flags -> flags { installReinstall = v })
          (yesNoOpt showOrParseArgs)

      , option [] ["avoid-reinstalls"]
          "Do not select versions that would destructively overwrite installed packages."
          (fmap asBool . installAvoidReinstalls)
          (\v flags -> flags { installAvoidReinstalls = fmap AvoidReinstalls v })
          (yesNoOpt showOrParseArgs)

      , option [] ["force-reinstalls"]
          "Reinstall packages even if they will most likely break other installed packages."
          installOverrideReinstall (\v flags -> flags { installOverrideReinstall = v })
          (yesNoOpt showOrParseArgs)

      , option [] ["upgrade-dependencies"]
          "Pick the latest version for all dependencies, rather than trying to pick an installed version."
          installUpgradeDeps (\v flags -> flags { installUpgradeDeps = v })
          (yesNoOpt showOrParseArgs)

      , option [] ["only-dependencies"]
          "Install only the dependencies necessary to build the given packages"
          installOnlyDeps (\v flags -> flags { installOnlyDeps = v })
          (yesNoOpt showOrParseArgs)

      , option [] ["dependencies-only"]
          "A synonym for --only-dependencies"
          installOnlyDeps (\v flags -> flags { installOnlyDeps = v })
          (yesNoOpt showOrParseArgs)

      , option [] ["index-state"]
          ("Use source package index state as it existed at a previous time. " ++
           "Accepts unix-timestamps (e.g. '@1474732068'), ISO8601 UTC timestamps " ++
           "(e.g. '2016-09-24T17:47:48Z'), or 'HEAD' (default: 'HEAD').")
          installIndexState (\v flags -> flags { installIndexState = v })
          (reqArg "STATE" (readP_to_E (const $ "index-state must be a  " ++
                                       "unix-timestamps (e.g. '@1474732068'), " ++
                                       "a ISO8601 UTC timestamp " ++
                                       "(e.g. '2016-09-24T17:47:48Z'), or 'HEAD'")
                                      (toFlag `fmap` parse))
                          (flagToList . fmap display))

      , option [] ["root-cmd"]
          "(No longer supported, do not use.)"
          installRootCmd (\v flags -> flags { installRootCmd = v })
          (reqArg' "COMMAND" toFlag flagToList)

      , option [] ["symlink-bindir"]
          "Add symlinks to installed executables into this directory."
           installSymlinkBinDir (\v flags -> flags { installSymlinkBinDir = v })
           (reqArgFlag "DIR")

      , option [] ["build-summary"]
          "Save build summaries to file (name template can use $pkgid, $compiler, $os, $arch)"
          installSummaryFile (\v flags -> flags { installSummaryFile = v })
          (reqArg' "TEMPLATE" (\x -> toNubList [toPathTemplate x]) (map fromPathTemplate . fromNubList))

      , option [] ["build-log"]
          "Log all builds to file (name template can use $pkgid, $compiler, $os, $arch)"
          installLogFile (\v flags -> flags { installLogFile = v })
          (reqArg' "TEMPLATE" (toFlag.toPathTemplate)
                              (flagToList . fmap fromPathTemplate))

      , option [] ["remote-build-reporting"]
          "Generate build reports to send to a remote server (none, anonymous or detailed)."
          installBuildReports (\v flags -> flags { installBuildReports = v })
          (reqArg "LEVEL" (readP_to_E (const $ "report level must be 'none', "
                                            ++ "'anonymous' or 'detailed'")
                                      (toFlag `fmap` parse))
                          (flagToList . fmap display))

      , option [] ["report-planning-failure"]
          "Generate build reports when the dependency solver fails. This is used by the Hackage build bot."
          installReportPlanningFailure (\v flags -> flags { installReportPlanningFailure = v })
          trueArg

      , option "" ["per-component"]
          "Per-component builds when possible"
          installPerComponent (\v flags -> flags { installPerComponent = v })
          (boolOpt [] [])

      , option [] ["one-shot"]
          "Do not record the packages in the world file."
          installOneShot (\v flags -> flags { installOneShot = v })
          (yesNoOpt showOrParseArgs)

      , option [] ["run-tests"]
          "Run package test suites during installation."
          installRunTests (\v flags -> flags { installRunTests = v })
          trueArg

      , optionNumJobs
        installNumJobs (\v flags -> flags { installNumJobs = v })

      , option [] ["keep-going"]
          "After a build failure, continue to build other unaffected packages."
          installKeepGoing (\v flags -> flags { installKeepGoing = v })
          trueArg

      , option [] ["offline"]
          "Don't download packages from the Internet."
          installOfflineMode (\v flags -> flags { installOfflineMode = v })
          (yesNoOpt showOrParseArgs)

      , option [] ["project-file"]
          "Set the name of the cabal.project file to search for in parent directories"
          installProjectFileName (\v flags -> flags {installProjectFileName = v})
          (reqArgFlag "FILE")
      ] ++ case showOrParseArgs of      -- TODO: remove when "cabal install"
                                        -- avoids
          ParseArgs ->
            [ option [] ["only"]
              "Only installs the package in the current directory."
              installOnly (\v flags -> flags { installOnly = v })
              trueArg ]
          _ -> []


instance Monoid InstallFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup InstallFlags where
  (<>) = gmappend

-- ------------------------------------------------------------
-- * Upload flags
-- ------------------------------------------------------------

-- | Is this a candidate package or a package to be published?
data IsCandidate = IsCandidate | IsPublished
                 deriving Eq

data UploadFlags = UploadFlags {
    uploadCandidate   :: Flag IsCandidate,
    uploadDoc         :: Flag Bool,
    uploadUsername    :: Flag Username,
    uploadPassword    :: Flag Password,
    uploadPasswordCmd :: Flag [String],
    uploadVerbosity   :: Flag Verbosity
  } deriving Generic

defaultUploadFlags :: UploadFlags
defaultUploadFlags = UploadFlags {
    uploadCandidate   = toFlag IsCandidate,
    uploadDoc         = toFlag False,
    uploadUsername    = mempty,
    uploadPassword    = mempty,
    uploadPasswordCmd = mempty,
    uploadVerbosity   = toFlag normal
  }

uploadCommand :: CommandUI UploadFlags
uploadCommand = CommandUI {
    commandName         = "upload",
    commandSynopsis     = "Uploads source packages or documentation to Hackage.",
    commandDescription  = Nothing,
    commandNotes        = Just $ \_ ->
         "You can store your Hackage login in the ~/.cabal/config file\n"
      ++ relevantConfigValuesText ["username", "password"],
    commandUsage        = \pname ->
         "Usage: " ++ pname ++ " upload [FLAGS] TARFILES\n",
    commandDefaultFlags = defaultUploadFlags,
    commandOptions      = \_ ->
      [optionVerbosity uploadVerbosity
       (\v flags -> flags { uploadVerbosity = v })

      ,option [] ["publish"]
        "Publish the package instead of uploading it as a candidate."
        uploadCandidate (\v flags -> flags { uploadCandidate = v })
        (noArg (Flag IsPublished))

      ,option ['d'] ["documentation"]
        ("Upload documentation instead of a source package. "
        ++ "By default, this uploads documentation for a package candidate. "
        ++ "To upload documentation for "
        ++ "a published package, combine with --publish.")
        uploadDoc (\v flags -> flags { uploadDoc = v })
        trueArg

      ,option ['u'] ["username"]
        "Hackage username."
        uploadUsername (\v flags -> flags { uploadUsername = v })
        (reqArg' "USERNAME" (toFlag . Username)
                            (flagToList . fmap unUsername))

      ,option ['p'] ["password"]
        "Hackage password."
        uploadPassword (\v flags -> flags { uploadPassword = v })
        (reqArg' "PASSWORD" (toFlag . Password)
                            (flagToList . fmap unPassword))

      ,option ['P'] ["password-command"]
        "Command to get Hackage password."
        uploadPasswordCmd (\v flags -> flags { uploadPasswordCmd = v })
        (reqArg' "PASSWORD" (Flag . words) (fromMaybe [] . flagToMaybe))
      ]
  }

instance Monoid UploadFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup UploadFlags where
  (<>) = gmappend

-- ------------------------------------------------------------
-- * Init flags
-- ------------------------------------------------------------

emptyInitFlags :: IT.InitFlags
emptyInitFlags  = mempty

defaultInitFlags :: IT.InitFlags
defaultInitFlags  = emptyInitFlags { IT.initVerbosity = toFlag normal }

initCommand :: CommandUI IT.InitFlags
initCommand = CommandUI {
    commandName = "init",
    commandSynopsis = "Create a new .cabal package file (interactively).",
    commandDescription = Just $ \_ -> wrapText $
         "Cabalise a project by creating a .cabal, Setup.hs, and "
      ++ "optionally a LICENSE file.\n"
      ++ "\n"
      ++ "Calling init with no arguments (recommended) uses an "
      ++ "interactive mode, which will try to guess as much as "
      ++ "possible and prompt you for the rest.  Command-line "
      ++ "arguments are provided for scripting purposes. "
      ++ "If you don't want interactive mode, be sure to pass "
      ++ "the -n flag.\n",
    commandNotes = Nothing,
    commandUsage = \pname ->
         "Usage: " ++ pname ++ " init [FLAGS]\n",
    commandDefaultFlags = defaultInitFlags,
    commandOptions = \_ ->
      [ option ['n'] ["non-interactive"]
        "Non-interactive mode."
        IT.nonInteractive (\v flags -> flags { IT.nonInteractive = v })
        trueArg

      , option ['q'] ["quiet"]
        "Do not generate log messages to stdout."
        IT.quiet (\v flags -> flags { IT.quiet = v })
        trueArg

      , option [] ["no-comments"]
        "Do not generate explanatory comments in the .cabal file."
        IT.noComments (\v flags -> flags { IT.noComments = v })
        trueArg

      , option ['m'] ["minimal"]
        "Generate a minimal .cabal file, that is, do not include extra empty fields.  Also implies --no-comments."
        IT.minimal (\v flags -> flags { IT.minimal = v })
        trueArg

      , option [] ["overwrite"]
        "Overwrite any existing .cabal, LICENSE, or Setup.hs files without warning."
        IT.overwrite (\v flags -> flags { IT.overwrite = v })
        trueArg

      , option [] ["package-dir"]
        "Root directory of the package (default = current directory)."
        IT.packageDir (\v flags -> flags { IT.packageDir = v })
        (reqArgFlag "DIRECTORY")

      , option ['p'] ["package-name"]
        "Name of the Cabal package to create."
        IT.packageName (\v flags -> flags { IT.packageName = v })
        (reqArg "PACKAGE" (readP_to_E ("Cannot parse package name: "++)
                                      (toFlag `fmap` parse))
                          (flagToList . fmap display))

      , option [] ["version"]
        "Initial version of the package."
        IT.version (\v flags -> flags { IT.version = v })
        (reqArg "VERSION" (readP_to_E ("Cannot parse package version: "++)
                                      (toFlag `fmap` parse))
                          (flagToList . fmap display))

      , option [] ["cabal-version"]
        "Required version of the Cabal library."
        IT.cabalVersion (\v flags -> flags { IT.cabalVersion = v })
        (reqArg "VERSION_RANGE" (readP_to_E ("Cannot parse Cabal version range: "++)
                                            (toFlag `fmap` parse))
                                (flagToList . fmap display))

      , option ['l'] ["license"]
        "Project license."
        IT.license (\v flags -> flags { IT.license = v })
        (reqArg "LICENSE" (readP_to_E ("Cannot parse license: "++)
                                      (toFlag `fmap` parse))
                          (flagToList . fmap display))

      , option ['a'] ["author"]
        "Name of the project's author."
        IT.author (\v flags -> flags { IT.author = v })
        (reqArgFlag "NAME")

      , option ['e'] ["email"]
        "Email address of the maintainer."
        IT.email (\v flags -> flags { IT.email = v })
        (reqArgFlag "EMAIL")

      , option ['u'] ["homepage"]
        "Project homepage and/or repository."
        IT.homepage (\v flags -> flags { IT.homepage = v })
        (reqArgFlag "URL")

      , option ['s'] ["synopsis"]
        "Short project synopsis."
        IT.synopsis (\v flags -> flags { IT.synopsis = v })
        (reqArgFlag "TEXT")

      , option ['c'] ["category"]
        "Project category."
        IT.category (\v flags -> flags { IT.category = v })
        (reqArg' "CATEGORY" (\s -> toFlag $ maybe (Left s) Right (readMaybe s))
                            (flagToList . fmap (either id show)))

      , option ['x'] ["extra-source-file"]
        "Extra source file to be distributed with tarball."
        IT.extraSrc (\v flags -> flags { IT.extraSrc = v })
        (reqArg' "FILE" (Just . (:[]))
                        (fromMaybe []))

      , option [] ["is-library"]
        "Build a library."
        IT.packageType (\v flags -> flags { IT.packageType = v })
        (noArg (Flag IT.Library))

      , option [] ["is-executable"]
        "Build an executable."
        IT.packageType
        (\v flags -> flags { IT.packageType = v })
        (noArg (Flag IT.Executable))

      , option [] ["main-is"]
        "Specify the main module."
        IT.mainIs
        (\v flags -> flags { IT.mainIs = v })
        (reqArgFlag "FILE")

      , option [] ["language"]
        "Specify the default language."
        IT.language
        (\v flags -> flags { IT.language = v })
        (reqArg "LANGUAGE" (readP_to_E ("Cannot parse language: "++)
                                       (toFlag `fmap` parse))
                          (flagToList . fmap display))

      , option ['o'] ["expose-module"]
        "Export a module from the package."
        IT.exposedModules
        (\v flags -> flags { IT.exposedModules = v })
        (reqArg "MODULE" (readP_to_E ("Cannot parse module name: "++)
                                     ((Just . (:[])) `fmap` parse))
                         (maybe [] (fmap display)))

      , option [] ["extension"]
        "Use a LANGUAGE extension (in the other-extensions field)."
        IT.otherExts
        (\v flags -> flags { IT.otherExts = v })
        (reqArg "EXTENSION" (readP_to_E ("Cannot parse extension: "++)
                                        ((Just . (:[])) `fmap` parse))
                            (maybe [] (fmap display)))

      , option ['d'] ["dependency"]
        "Package dependency."
        IT.dependencies (\v flags -> flags { IT.dependencies = v })
        (reqArg "PACKAGE" (readP_to_E ("Cannot parse dependency: "++)
                                      ((Just . (:[])) `fmap` parse))
                          (maybe [] (fmap display)))

      , option [] ["source-dir"]
        "Directory containing package source."
        IT.sourceDirs (\v flags -> flags { IT.sourceDirs = v })
        (reqArg' "DIR" (Just . (:[]))
                       (fromMaybe []))

      , option [] ["build-tool"]
        "Required external build tool."
        IT.buildTools (\v flags -> flags { IT.buildTools = v })
        (reqArg' "TOOL" (Just . (:[]))
                        (fromMaybe []))

      , optionVerbosity IT.initVerbosity (\v flags -> flags { IT.initVerbosity = v })
      ]
  }

-- ------------------------------------------------------------
-- * SDist flags
-- ------------------------------------------------------------

-- | Extra flags to @sdist@ beyond runghc Setup sdist
--
data SDistExFlags = SDistExFlags {
    sDistFormat    :: Flag ArchiveFormat
  }
  deriving (Show, Generic)

data ArchiveFormat = TargzFormat | ZipFormat -- ...
  deriving (Show, Eq)

defaultSDistExFlags :: SDistExFlags
defaultSDistExFlags = SDistExFlags {
    sDistFormat  = Flag TargzFormat
  }

sdistCommand :: CommandUI (SDistFlags, SDistExFlags)
sdistCommand = Cabal.sdistCommand {
    commandDefaultFlags = (commandDefaultFlags Cabal.sdistCommand, defaultSDistExFlags),
    commandOptions      = \showOrParseArgs ->
         liftOptions fst setFst (commandOptions Cabal.sdistCommand showOrParseArgs)
      ++ liftOptions snd setSnd sdistExOptions
  }
  where
    setFst a (_,b) = (a,b)
    setSnd b (a,_) = (a,b)

    sdistExOptions =
      [option [] ["archive-format"] "archive-format"
         sDistFormat (\v flags -> flags { sDistFormat = v })
         (choiceOpt
            [ (Flag TargzFormat, ([], ["targz"]),
                 "Produce a '.tar.gz' format archive (default and required for uploading to hackage)")
            , (Flag ZipFormat,   ([], ["zip"]),
                 "Produce a '.zip' format archive")
            ])
      ]

instance Monoid SDistExFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup SDistExFlags where
  (<>) = gmappend

-- ------------------------------------------------------------
-- * Win32SelfUpgrade flags
-- ------------------------------------------------------------

data Win32SelfUpgradeFlags = Win32SelfUpgradeFlags {
  win32SelfUpgradeVerbosity :: Flag Verbosity
} deriving Generic

defaultWin32SelfUpgradeFlags :: Win32SelfUpgradeFlags
defaultWin32SelfUpgradeFlags = Win32SelfUpgradeFlags {
  win32SelfUpgradeVerbosity = toFlag normal
}

win32SelfUpgradeCommand :: CommandUI Win32SelfUpgradeFlags
win32SelfUpgradeCommand = CommandUI {
  commandName         = "win32selfupgrade",
  commandSynopsis     = "Self-upgrade the executable on Windows",
  commandDescription  = Nothing,
  commandNotes        = Nothing,
  commandUsage        = \pname ->
    "Usage: " ++ pname ++ " win32selfupgrade PID PATH\n",
  commandDefaultFlags = defaultWin32SelfUpgradeFlags,
  commandOptions      = \_ ->
      [optionVerbosity win32SelfUpgradeVerbosity
       (\v flags -> flags { win32SelfUpgradeVerbosity = v})
      ]
}

instance Monoid Win32SelfUpgradeFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup Win32SelfUpgradeFlags where
  (<>) = gmappend

-- ------------------------------------------------------------
-- * ActAsSetup flags
-- ------------------------------------------------------------

data ActAsSetupFlags = ActAsSetupFlags {
    actAsSetupBuildType :: Flag BuildType
} deriving Generic

defaultActAsSetupFlags :: ActAsSetupFlags
defaultActAsSetupFlags = ActAsSetupFlags {
    actAsSetupBuildType = toFlag Simple
}

actAsSetupCommand :: CommandUI ActAsSetupFlags
actAsSetupCommand = CommandUI {
  commandName         = "act-as-setup",
  commandSynopsis     = "Run as-if this was a Setup.hs",
  commandDescription  = Nothing,
  commandNotes        = Nothing,
  commandUsage        = \pname ->
    "Usage: " ++ pname ++ " act-as-setup\n",
  commandDefaultFlags = defaultActAsSetupFlags,
  commandOptions      = \_ ->
      [option "" ["build-type"]
         "Use the given build type."
         actAsSetupBuildType (\v flags -> flags { actAsSetupBuildType = v })
         (reqArg "BUILD-TYPE" (readP_to_E ("Cannot parse build type: "++)
                               (fmap toFlag parse))
                              (map display . flagToList))
      ]
}

instance Monoid ActAsSetupFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ActAsSetupFlags where
  (<>) = gmappend

-- ------------------------------------------------------------
-- * Sandbox-related flags
-- ------------------------------------------------------------

data SandboxFlags = SandboxFlags {
  sandboxVerbosity :: Flag Verbosity,
  sandboxSnapshot  :: Flag Bool, -- FIXME: this should be an 'add-source'-only
                                 -- flag.
  sandboxLocation  :: Flag FilePath
} deriving Generic

defaultSandboxLocation :: FilePath
defaultSandboxLocation = ".cabal-sandbox"

defaultSandboxFlags :: SandboxFlags
defaultSandboxFlags = SandboxFlags {
  sandboxVerbosity = toFlag normal,
  sandboxSnapshot  = toFlag False,
  sandboxLocation  = toFlag defaultSandboxLocation
  }

sandboxCommand :: CommandUI SandboxFlags
sandboxCommand = CommandUI {
  commandName         = "sandbox",
  commandSynopsis     = "Create/modify/delete a sandbox.",
  commandDescription  = Just $ \pname -> concat
    [ paragraph $ "Sandboxes are isolated package databases that can be used"
      ++ " to prevent dependency conflicts that arise when many different"
      ++ " packages are installed in the same database (i.e. the user's"
      ++ " database in the home directory)."
    , paragraph $ "A sandbox in the current directory (created by"
      ++ " `sandbox init`) will be used instead of the user's database for"
      ++ " commands such as `install` and `build`. Note that (a directly"
      ++ " invoked) GHC will not automatically be aware of sandboxes;"
      ++ " only if called via appropriate " ++ pname
      ++ " commands, e.g. `repl`, `build`, `exec`."
    , paragraph $ "Currently, " ++ pname ++ " will not search for a sandbox"
      ++ " in folders above the current one, so cabal will not see the sandbox"
      ++ " if you are in a subfolder of a sandbox."
    , paragraph "Subcommands:"
    , headLine "init:"
    , indentParagraph $ "Initialize a sandbox in the current directory."
      ++ " An existing package database will not be modified, but settings"
      ++ " (such as the location of the database) can be modified this way."
    , headLine "delete:"
    , indentParagraph $ "Remove the sandbox; deleting all the packages"
      ++ " installed inside."
    , headLine "add-source:"
    , indentParagraph $ "Make one or more local packages available in the"
      ++ " sandbox. PATHS may be relative or absolute."
      ++ " Typical usecase is when you need"
      ++ " to make a (temporary) modification to a dependency: You download"
      ++ " the package into a different directory, make the modification,"
      ++ " and add that directory to the sandbox with `add-source`."
    , indentParagraph $ "Unless given `--snapshot`, any add-source'd"
      ++ " dependency that was modified since the last build will be"
      ++ " re-installed automatically."
    , headLine "delete-source:"
    , indentParagraph $ "Remove an add-source dependency; however, this will"
      ++ " not delete the package(s) that have been installed in the sandbox"
      ++ " from this dependency. You can either unregister the package(s) via"
      ++ " `" ++ pname ++ " sandbox hc-pkg unregister` or re-create the"
      ++ " sandbox (`sandbox delete; sandbox init`)."
    , headLine "list-sources:"
    , indentParagraph $ "List the directories of local packages made"
      ++ " available via `" ++ pname ++ " add-source`."
    , headLine "hc-pkg:"
    , indentParagraph $ "Similar to `ghc-pkg`, but for the sandbox package"
      ++ " database. Can be used to list specific/all packages that are"
      ++ " installed in the sandbox. For subcommands, see the help for"
      ++ " ghc-pkg. Affected by the compiler version specified by `configure`."
    ],
  commandNotes        = Just $ \pname ->
       relevantConfigValuesText ["require-sandbox"
                                ,"ignore-sandbox"]
    ++ "\n"
    ++ "Examples:\n"
    ++ "  Set up a sandbox with one local dependency, located at ../foo:\n"
    ++ "    " ++ pname ++ " sandbox init\n"
    ++ "    " ++ pname ++ " sandbox add-source ../foo\n"
    ++ "    " ++ pname ++ " install --only-dependencies\n"
    ++ "  Reset the sandbox:\n"
    ++ "    " ++ pname ++ " sandbox delete\n"
    ++ "    " ++ pname ++ " sandbox init\n"
    ++ "    " ++ pname ++ " install --only-dependencies\n"
    ++ "  List the packages in the sandbox:\n"
    ++ "    " ++ pname ++ " sandbox hc-pkg list\n"
    ++ "  Unregister the `broken` package from the sandbox:\n"
    ++ "    " ++ pname ++ " sandbox hc-pkg -- --force unregister broken\n",
  commandUsage        = usageAlternatives "sandbox"
    [ "init          [FLAGS]"
    , "delete        [FLAGS]"
    , "add-source    [FLAGS] PATHS"
    , "delete-source [FLAGS] PATHS"
    , "list-sources  [FLAGS]"
    , "hc-pkg        [FLAGS] [--] COMMAND [--] [ARGS]"
    ],

  commandDefaultFlags = defaultSandboxFlags,
  commandOptions      = \_ ->
    [ optionVerbosity sandboxVerbosity
      (\v flags -> flags { sandboxVerbosity = v })

    , option [] ["snapshot"]
      "Take a snapshot instead of creating a link (only applies to 'add-source')"
      sandboxSnapshot (\v flags -> flags { sandboxSnapshot = v })
      trueArg

    , option [] ["sandbox"]
      "Sandbox location (default: './.cabal-sandbox')."
      sandboxLocation (\v flags -> flags { sandboxLocation = v })
      (reqArgFlag "DIR")
    ]
  }

instance Monoid SandboxFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup SandboxFlags where
  (<>) = gmappend

-- ------------------------------------------------------------
-- * Exec Flags
-- ------------------------------------------------------------

data ExecFlags = ExecFlags {
  execVerbosity :: Flag Verbosity,
  execDistPref  :: Flag FilePath
} deriving Generic

defaultExecFlags :: ExecFlags
defaultExecFlags = ExecFlags {
  execVerbosity = toFlag normal,
  execDistPref  = NoFlag
  }

execCommand :: CommandUI ExecFlags
execCommand = CommandUI {
  commandName         = "exec",
  commandSynopsis     = "Give a command access to the sandbox package repository.",
  commandDescription  = Just $ \pname -> wrapText $
       -- TODO: this is too GHC-focused for my liking..
       "A directly invoked GHC will not automatically be aware of any"
    ++ " sandboxes: the GHC_PACKAGE_PATH environment variable controls what"
    ++ " GHC uses. `" ++ pname ++ " exec` can be used to modify this variable:"
    ++ " COMMAND will be executed in a modified environment and thereby uses"
    ++ " the sandbox package database.\n"
    ++ "\n"
    ++ "If there is no sandbox, behaves as identity (executing COMMAND).\n"
    ++ "\n"
    ++ "Note that other " ++ pname ++ " commands change the environment"
    ++ " variable appropriately already, so there is no need to wrap those"
    ++ " in `" ++ pname ++ " exec`. But with `" ++ pname ++ " exec`, the user"
    ++ " has more control and can, for example, execute custom scripts which"
    ++ " indirectly execute GHC.\n"
    ++ "\n"
    ++ "Note that `" ++ pname ++ " repl` is different from `" ++ pname
    ++ " exec -- ghci` as the latter will not forward any additional flags"
    ++ " being defined in the local package to ghci.\n"
    ++ "\n"
    ++ "See `" ++ pname ++ " sandbox`.\n",
  commandNotes        = Just $ \pname ->
       "Examples:\n"
    ++ "  " ++ pname ++ " exec -- ghci -Wall\n"
    ++ "    Start a repl session with sandbox packages and all warnings;\n"
    ++ "  " ++ pname ++ " exec gitit -- -f gitit.cnf\n"
    ++ "    Give gitit access to the sandbox packages, and pass it a flag;\n"
    ++ "  " ++ pname ++ " exec runghc Foo.hs\n"
    ++ "    Execute runghc on Foo.hs with runghc configured to use the\n"
    ++ "    sandbox package database (if a sandbox is being used).\n",
  commandUsage        = \pname ->
       "Usage: " ++ pname ++ " exec [FLAGS] [--] COMMAND [--] [ARGS]\n",

  commandDefaultFlags = defaultExecFlags,
  commandOptions      = \showOrParseArgs ->
    [ optionVerbosity execVerbosity
      (\v flags -> flags { execVerbosity = v })
    , Cabal.optionDistPref
       execDistPref (\d flags -> flags { execDistPref = d })
       showOrParseArgs
    ]
  }

instance Monoid ExecFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ExecFlags where
  (<>) = gmappend

-- ------------------------------------------------------------
-- * UserConfig flags
-- ------------------------------------------------------------

data UserConfigFlags = UserConfigFlags {
  userConfigVerbosity :: Flag Verbosity,
  userConfigForce     :: Flag Bool
} deriving Generic

instance Monoid UserConfigFlags where
  mempty = UserConfigFlags {
    userConfigVerbosity = toFlag normal,
    userConfigForce     = toFlag False
    }
  mappend = (<>)

instance Semigroup UserConfigFlags where
  (<>) = gmappend

userConfigCommand :: CommandUI UserConfigFlags
userConfigCommand = CommandUI {
  commandName         = "user-config",
  commandSynopsis     = "Display and update the user's global cabal configuration.",
  commandDescription  = Just $ \_ -> wrapText $
       "When upgrading cabal, the set of configuration keys and their default"
    ++ " values may change. This command provides means to merge the existing"
    ++ " config in ~/.cabal/config"
    ++ " (i.e. all bindings that are actually defined and not commented out)"
    ++ " and the default config of the new version.\n"
    ++ "\n"
    ++ "init: Creates a new config file at either ~/.cabal/config or as"
    ++ " specified by --config-file, if given. An existing file won't be "
    ++ " overwritten unless -f or --force is given.\n"
    ++ "diff: Shows a pseudo-diff of the user's ~/.cabal/config file and"
    ++ " the default configuration that would be created by cabal if the"
    ++ " config file did not exist.\n"
    ++ "update: Applies the pseudo-diff to the configuration that would be"
    ++ " created by default, and write the result back to ~/.cabal/config.",

  commandNotes        = Nothing,
  commandUsage        = usageAlternatives "user-config" ["init", "diff", "update"],
  commandDefaultFlags = mempty,
  commandOptions      = \ _ -> [
   optionVerbosity userConfigVerbosity (\v flags -> flags { userConfigVerbosity = v })
 , option ['f'] ["force"]
     "Overwrite the config file if it already exists."
     userConfigForce (\v flags -> flags { userConfigForce = v })
     trueArg
   ]
  }

-- ------------------------------------------------------------
-- * GetOpt Utils
-- ------------------------------------------------------------

reqArgFlag :: ArgPlaceHolder ->
              MkOptDescr (b -> Flag String) (Flag String -> b -> b) b
reqArgFlag ad = reqArg ad (succeedReadE Flag) flagToList

liftOptions :: (b -> a) -> (a -> b -> b)
            -> [OptionField a] -> [OptionField b]
liftOptions get set = map (liftOption get set)

yesNoOpt :: ShowOrParseArgs -> MkOptDescr (b -> Flag Bool) (Flag Bool -> b -> b) b
yesNoOpt ShowArgs sf lf = trueArg sf lf
yesNoOpt _        sf lf = Command.boolOpt' flagToMaybe Flag (sf, lf) ([], map ("no-" ++) lf) sf lf

optionSolver :: (flags -> Flag PreSolver)
             -> (Flag PreSolver -> flags -> flags)
             -> OptionField flags
optionSolver get set =
  option [] ["solver"]
    ("Select dependency solver to use (default: " ++ display defaultSolver ++ "). Choices: " ++ allSolvers ++ ".")
    get set
    (reqArg "SOLVER" (readP_to_E (const $ "solver must be one of: " ++ allSolvers)
                                 (toFlag `fmap` parse))
                     (flagToList . fmap display))

optionSolverFlags :: ShowOrParseArgs
                  -> (flags -> Flag Int   ) -> (Flag Int    -> flags -> flags)
                  -> (flags -> Flag ReorderGoals)     -> (Flag ReorderGoals     -> flags -> flags)
                  -> (flags -> Flag CountConflicts)   -> (Flag CountConflicts   -> flags -> flags)
                  -> (flags -> Flag IndependentGoals) -> (Flag IndependentGoals -> flags -> flags)
                  -> (flags -> Flag ShadowPkgs)       -> (Flag ShadowPkgs       -> flags -> flags)
                  -> (flags -> Flag StrongFlags)      -> (Flag StrongFlags      -> flags -> flags)
                  -> (flags -> Flag AllowBootLibInstalls) -> (Flag AllowBootLibInstalls -> flags -> flags)
                  -> [OptionField flags]
optionSolverFlags showOrParseArgs getmbj setmbj getrg setrg getcc setcc getig setig
                  getsip setsip getstrfl setstrfl getib setib =
  [ option [] ["max-backjumps"]
      ("Maximum number of backjumps allowed while solving (default: " ++ show defaultMaxBackjumps ++ "). Use a negative number to enable unlimited backtracking. Use 0 to disable backtracking completely.")
      getmbj setmbj
      (reqArg "NUM" (readP_to_E ("Cannot parse number: "++) (fmap toFlag parse))
                    (map show . flagToList))
  , option [] ["reorder-goals"]
      "Try to reorder goals according to certain heuristics. Slows things down on average, but may make backtracking faster for some packages."
      (fmap asBool . getrg)
      (setrg . fmap ReorderGoals)
      (yesNoOpt showOrParseArgs)
  , option [] ["count-conflicts"]
      "Try to speed up solving by preferring goals that are involved in a lot of conflicts (default)."
      (fmap asBool . getcc)
      (setcc . fmap CountConflicts)
      (yesNoOpt showOrParseArgs)
  , option [] ["independent-goals"]
      "Treat several goals on the command line as independent. If several goals depend on the same package, different versions can be chosen."
      (fmap asBool . getig)
      (setig . fmap IndependentGoals)
      (yesNoOpt showOrParseArgs)
  , option [] ["shadow-installed-packages"]
      "If multiple package instances of the same version are installed, treat all but one as shadowed."
      (fmap asBool . getsip)
      (setsip . fmap ShadowPkgs)
      (yesNoOpt showOrParseArgs)
  , option [] ["strong-flags"]
      "Do not defer flag choices (this used to be the default in cabal-install <= 1.20)."
      (fmap asBool . getstrfl)
      (setstrfl . fmap StrongFlags)
      (yesNoOpt showOrParseArgs)
  , option [] ["allow-boot-library-installs"]
      "Allow cabal to install base, ghc-prim, integer-simple, integer-gmp, and template-haskell."
      (fmap asBool . getib)
      (setib . fmap AllowBootLibInstalls)
      (yesNoOpt showOrParseArgs)
  ]

usageFlagsOrPackages :: String -> String -> String
usageFlagsOrPackages name pname =
     "Usage: " ++ pname ++ " " ++ name ++ " [FLAGS]\n"
  ++ "   or: " ++ pname ++ " " ++ name ++ " [PACKAGES]\n"

usagePackages :: String -> String -> String
usagePackages name pname =
     "Usage: " ++ pname ++ " " ++ name ++ " [PACKAGES]\n"

usageFlags :: String -> String -> String
usageFlags name pname =
  "Usage: " ++ pname ++ " " ++ name ++ " [FLAGS]\n"

--TODO: do we want to allow per-package flags?
parsePackageArgs :: [String] -> Either String [Dependency]
parsePackageArgs = parsePkgArgs []
  where
    parsePkgArgs ds [] = Right (reverse ds)
    parsePkgArgs ds (arg:args) =
      case readPToMaybe parseDependencyOrPackageId arg of
        Just dep -> parsePkgArgs (dep:ds) args
        Nothing  -> Left $
         show arg ++ " is not valid syntax for a package name or"
                  ++ " package dependency."

parseDependencyOrPackageId :: Parse.ReadP r Dependency
parseDependencyOrPackageId = parse Parse.+++ liftM pkgidToDependency parse
  where
    pkgidToDependency :: PackageIdentifier -> Dependency
    pkgidToDependency p = case packageVersion p of
      v | v == nullVersion -> Dependency (packageName p) anyVersion
        | otherwise        -> Dependency (packageName p) (thisVersion v)

showRepo :: RemoteRepo -> String
showRepo repo = remoteRepoName repo ++ ":"
             ++ uriToString id (remoteRepoURI repo) []

readRepo :: String -> Maybe RemoteRepo
readRepo = readPToMaybe parseRepo

parseRepo :: Parse.ReadP r RemoteRepo
parseRepo = do
  name   <- Parse.munch1 (\c -> isAlphaNum c || c `elem` "_-.")
  _      <- Parse.char ':'
  uriStr <- Parse.munch1 (\c -> isAlphaNum c || c `elem` "+-=._/*()@'$:;&!?~")
  uri    <- maybe Parse.pfail return (parseAbsoluteURI uriStr)
  return RemoteRepo {
    remoteRepoName           = name,
    remoteRepoURI            = uri,
    remoteRepoSecure         = Nothing,
    remoteRepoRootKeys       = [],
    remoteRepoKeyThreshold   = 0,
    remoteRepoShouldTryHttps = False
  }

-- ------------------------------------------------------------
-- * Helpers for Documentation
-- ------------------------------------------------------------

headLine :: String -> String
headLine = unlines
         . map unwords
         . wrapLine 79
         . words

paragraph :: String -> String
paragraph = (++"\n")
          . unlines
          . map unwords
          . wrapLine 79
          . words

indentParagraph :: String -> String
indentParagraph = unlines
                . (flip (++)) [""]
                . map (("  "++).unwords)
                . wrapLine 77
                . words

relevantConfigValuesText :: [String] -> String
relevantConfigValuesText vs =
     "Relevant global configuration keys:\n"
  ++ concat ["  " ++ v ++ "\n" |v <- vs]
