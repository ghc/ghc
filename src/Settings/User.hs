module Settings.User (
    buildRootPath, trackBuildSystem, userArgs, userPackages, userLibraryWays,
    userRtsWays, userKnownPackages, integerLibrary, buildHaddock, validating,
    ghciWithDebugger, ghcProfiled, ghcDebugged, dynamicGhcPrograms,
    turnWarningsIntoErrors, splitObjects, verboseCommands, putBuild, putSuccess
    ) where

-- Import the actual user settings from the module UserSettings.
-- The user can put an UserSettings.hs file into the hadrian root
-- folder that takes precedence over the default UserSettings.hs
-- file located in src/.
import UserSettings
