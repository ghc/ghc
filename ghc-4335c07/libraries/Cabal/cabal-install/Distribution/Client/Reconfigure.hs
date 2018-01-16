module Distribution.Client.Reconfigure ( Check(..), reconfigure ) where

import Control.Monad ( unless, when )
import Data.Maybe ( isJust )
import Data.Monoid hiding ( (<>) )
import System.Directory ( doesFileExist )

import Distribution.Compat.Semigroup

import Distribution.Verbosity

import Distribution.Simple.Configure ( localBuildInfoFile )
import Distribution.Simple.Setup ( Flag, flagToMaybe, toFlag )
import Distribution.Simple.Utils
       ( existsAndIsMoreRecentThan, defaultPackageDesc, info )

import Distribution.Client.Config ( SavedConfig(..) )
import Distribution.Client.Configure ( readConfigFlags )
import Distribution.Client.Nix ( findNixExpr, inNixShell, nixInstantiate )
import Distribution.Client.Sandbox
       ( WereDepsReinstalled(..), findSavedDistPref, getSandboxConfigFilePath
       , maybeReinstallAddSourceDeps, updateInstallDirs )
import Distribution.Client.Sandbox.PackageEnvironment
       ( userPackageEnvironmentFile )
import Distribution.Client.Sandbox.Types ( UseSandbox(..) )
import Distribution.Client.Setup
       ( ConfigFlags(..), ConfigExFlags, GlobalFlags(..)
       , SkipAddSourceDepsCheck(..) )


-- | @Check@ represents a function to check some condition on type @a@. The
-- returned 'Any' is 'True' if any part of the condition failed.
newtype Check a = Check {
  runCheck :: Any          -- Did any previous check fail?
           -> a            -- value returned by previous checks
           -> IO (Any, a)  -- Did this check fail? What value is returned?
}

instance Semigroup (Check a) where
  (<>) c d = Check $ \any0 a0 -> do
    (any1, a1) <- runCheck c any0 a0
    (any2, a2) <- runCheck d (any0 <> any1) a1
    return (any0 <> any1 <> any2, a2)

instance Monoid (Check a) where
  mempty = Check $ \_ a -> return (mempty, a)
  mappend = (<>)


-- | Re-configure the package in the current directory if needed. Deciding
-- when to reconfigure and with which options is convoluted:
--
-- If we are reconfiguring, we must always run @configure@ with the
-- verbosity option we are given; however, that a previous configuration
-- uses a different verbosity setting is not reason enough to reconfigure.
--
-- The package should be configured to use the same \"dist\" prefix as
-- given to the @build@ command, otherwise the build will probably
-- fail. Not only does this determine the \"dist\" prefix setting if we
-- need to reconfigure anyway, but an existing configuration should be
-- invalidated if its \"dist\" prefix differs.
--
-- If the package has never been configured (i.e., there is no
-- LocalBuildInfo), we must configure first, using the default options.
--
-- If the package has been configured, there will be a 'LocalBuildInfo'.
-- If there no package description file, we assume that the
-- 'PackageDescription' is up to date, though the configuration may need
-- to be updated for other reasons (see above). If there is a package
-- description file, and it has been modified since the 'LocalBuildInfo'
-- was generated, then we need to reconfigure.
--
-- The caller of this function may also have specific requirements
-- regarding the flags the last configuration used. For example,
-- 'testAction' requires that the package be configured with test suites
-- enabled. The caller may pass the required settings to this function
-- along with a function to check the validity of the saved 'ConfigFlags';
-- these required settings will be checked first upon determining that
-- a previous configuration exists.
reconfigure
  :: ((ConfigFlags, ConfigExFlags) -> [String] -> GlobalFlags -> IO ())
     -- ^ configure action
  -> Verbosity
     -- ^ Verbosity setting
  -> FilePath
     -- ^ \"dist\" prefix
  -> UseSandbox
  -> SkipAddSourceDepsCheck
     -- ^ Should we skip the timestamp check for modified
     -- add-source dependencies?
  -> Flag (Maybe Int)
     -- ^ -j flag for reinstalling add-source deps.
  -> Check (ConfigFlags, ConfigExFlags)
     -- ^ Check that the required flags are set.
     -- If they are not set, provide a message explaining the
     -- reason for reconfiguration.
  -> [String]     -- ^ Extra arguments
  -> GlobalFlags  -- ^ Global flags
  -> SavedConfig
  -> IO SavedConfig
reconfigure
  configureAction
  verbosity
  dist
  useSandbox
  skipAddSourceDepsCheck
  numJobsFlag
  check
  extraArgs
  globalFlags
  config
  = do

  savedFlags@(_, _) <- readConfigFlags dist

  useNix <- fmap isJust (findNixExpr globalFlags config)
  alreadyInNixShell <- inNixShell

  if useNix && not alreadyInNixShell
    then do

      -- If we are using Nix, we must reinstantiate the derivation outside
      -- the shell. Eventually, the caller will invoke 'nixShell' which will
      -- rerun cabal inside the shell. That will bring us back to 'reconfigure',
      -- but inside the shell we'll take the second branch, below.

      -- This seems to have a problem: won't 'configureAction' call 'nixShell'
      -- yet again, spawning an infinite tree of subprocesses?
      -- No, because 'nixShell' doesn't spawn a new process if it is already
      -- running in a Nix shell.

      nixInstantiate verbosity dist False globalFlags config
      return config

    else do

      let checks =
            checkVerb
            <> checkDist
            <> checkOutdated
            <> check
            <> checkAddSourceDeps
      (Any force, flags@(configFlags, _)) <- runCheck checks mempty savedFlags

      let (_, config') =
            updateInstallDirs
            (configUserInstall configFlags)
            (useSandbox, config)

      when force $ configureAction flags extraArgs globalFlags
      return config'

  where

    -- Changing the verbosity does not require reconfiguration, but the new
    -- verbosity should be used if reconfiguring.
    checkVerb = Check $ \_ (configFlags, configExFlags) -> do
      let configFlags' = configFlags { configVerbosity = toFlag verbosity}
      return (mempty, (configFlags', configExFlags))

    -- Reconfiguration is required if @--build-dir@ changes.
    checkDist = Check $ \_ (configFlags, configExFlags) -> do
      -- Always set the chosen @--build-dir@ before saving the flags,
      -- or bad things could happen.
      savedDist <- findSavedDistPref config (configDistPref configFlags)
      let distChanged = dist /= savedDist
      when distChanged $ info verbosity "build directory changed"
      let configFlags' = configFlags { configDistPref = toFlag dist }
      return (Any distChanged, (configFlags', configExFlags))

    checkOutdated = Check $ \_ flags@(configFlags, _) -> do
      let buildConfig = localBuildInfoFile dist

      -- Has the package ever been configured? If not, reconfiguration is
      -- required.
      configured <- doesFileExist buildConfig
      unless configured $ info verbosity "package has never been configured"

      -- Is the configuration older than the sandbox configuration file?
      -- If so, reconfiguration is required.
      sandboxConfig <- getSandboxConfigFilePath globalFlags
      sandboxConfigNewer <- existsAndIsMoreRecentThan sandboxConfig buildConfig
      when sandboxConfigNewer $
        info verbosity "sandbox was created after the package was configured"

      -- Is the @cabal.config@ file newer than @dist/setup.config@? Then we need
      -- to force reconfigure. Note that it's possible to use @cabal.config@
      -- even without sandboxes.
      userPackageEnvironmentFileModified <-
        existsAndIsMoreRecentThan userPackageEnvironmentFile buildConfig
      when userPackageEnvironmentFileModified $
        info verbosity ("user package environment file ('"
        ++ userPackageEnvironmentFile ++ "') was modified")

      -- Is the configuration older than the package description?
      descrFile <- maybe (defaultPackageDesc verbosity) return
                   (flagToMaybe (configCabalFilePath configFlags))
      outdated <- existsAndIsMoreRecentThan descrFile buildConfig
      when outdated $ info verbosity (descrFile ++ " was changed")

      let failed =
            Any outdated
            <> Any userPackageEnvironmentFileModified
            <> Any sandboxConfigNewer
            <> Any (not configured)
      return (failed, flags)

    checkAddSourceDeps = Check $ \(Any force') flags@(configFlags, _) -> do
      let (_, config') =
            updateInstallDirs
            (configUserInstall configFlags)
            (useSandbox, config)

          skipAddSourceDepsCheck'
            | force'    = SkipAddSourceDepsCheck
            | otherwise = skipAddSourceDepsCheck

      when (skipAddSourceDepsCheck' == SkipAddSourceDepsCheck) $
        info verbosity "skipping add-source deps check"

      -- Were any add-source dependencies reinstalled in the sandbox?
      depsReinstalled <-
        case skipAddSourceDepsCheck' of
          DontSkipAddSourceDepsCheck ->
            maybeReinstallAddSourceDeps
            verbosity numJobsFlag configFlags globalFlags
            (useSandbox, config')
          SkipAddSourceDepsCheck -> do
            return NoDepsReinstalled

      case depsReinstalled of
        NoDepsReinstalled -> return (mempty, flags)
        ReinstalledSomeDeps -> do
          info verbosity "some add-source dependencies were reinstalled"
          return (Any True, flags)
