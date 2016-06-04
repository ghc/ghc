-- If you want to customise your build you should copy this file from
-- hadrian/src/UserSettings.hs to hadrian/UserSettings.hs and edit your copy.
-- If you don't copy the file your changes will be tracked by git and you can
-- accidentally commit them.
module UserSettings (
    buildRootPath, trackBuildSystem, userArgs, userPackages, userLibraryWays,
    userRtsWays, userKnownPackages, integerLibrary, buildHaddock, validating,
    ghciWithDebugger, ghcProfiled, ghcDebugged, dynamicGhcPrograms,
    turnWarningsIntoErrors, splitObjects, verboseCommands, putBuild, putSuccess
    ) where

import System.Console.ANSI

import Base
import CmdLineFlag
import GHC
import Predicate
import Settings.Default

-- See doc/user-settings.md for instructions.

-- | All build results are put into 'buildRootPath' directory.
buildRootPath :: FilePath
buildRootPath = "_build"

-- | Modify default build command line arguments.
userArgs :: Args
userArgs = builder Ghc ? remove ["-Wall", "-fwarn-tabs"]

-- | Modify the set of packages that are built by default in each stage.
userPackages :: Packages
userPackages = mempty

-- | Add user defined packages. Don't forget to add them to 'userPackages' too.
userKnownPackages :: [Package]
userKnownPackages = []

-- | Choose the integer library: 'integerGmp' or 'integerSimple'.
integerLibrary :: Package
integerLibrary = integerGmp

-- FIXME: We skip 'dynamic' since it's currently broken #4.
-- | Modify the set of ways in which library packages are built.
userLibraryWays :: Ways
userLibraryWays = remove [dynamic]

-- | Modify the set of ways in which the 'rts' package is built.
userRtsWays :: Ways
userRtsWays = mempty

-- | User defined flags. Note the following type semantics:
-- * @Bool@: a plain Boolean flag whose value is known at compile time.
-- * @Action Bool@: a flag whose value can depend on the build environment.
-- * @Predicate@: a flag whose value can depend on the build environment and
-- on the current build target.

-- TODO: Drop 'trackBuildSystem' as it brings negligible gains.
-- | Set this to True if you are making any changes in the build system and want
-- appropriate rebuilds to be initiated. Switching this to False speeds things
-- up a little (particularly zero builds).
-- WARNING: a complete rebuild is required when changing this setting.
trackBuildSystem :: Bool
trackBuildSystem = True

-- TODO: This should be set automatically when validating.
validating :: Bool
validating = False

-- | Control when split objects are generated. Note, due to the GHC bug #11315
-- it is necessary to do a full clean rebuild when changing this option.
splitObjects :: Predicate
splitObjects = return cmdSplitObjects &&^ defaultSplitObjects

-- | Control when to build Haddock documentation.
buildHaddock :: Predicate
buildHaddock = return cmdBuildHaddock

-- TODO: Do we need to be able to set these from command line?
-- TODO: Turn the flags below into a simple list @ghcWays :: [Way]@?
dynamicGhcPrograms :: Bool
dynamicGhcPrograms = True

ghciWithDebugger :: Bool
ghciWithDebugger = False

ghcProfiled :: Bool
ghcProfiled = False

ghcDebugged :: Bool
ghcDebugged = False

-- TODO: Replace with stage2 ? arg "-Werror"? Also see #251.
-- | To enable -Werror in Stage2 set turnWarningsIntoErrors = stage2.
turnWarningsIntoErrors :: Predicate
turnWarningsIntoErrors = return False

-- | Set to True to print full command lines during the build process. Note,
-- this is a Predicate, hence you can enable verbose output only for certain
-- targets, e.g.: @verboseCommands = package ghcPrim@.
verboseCommands :: Predicate
verboseCommands = return False

-- | Customise build progress messages (e.g. executing a build command).
putBuild :: String -> Action ()
putBuild = putColoured Vivid White

-- | Customise build success messages (e.g. a package is built successfully).
putSuccess :: String -> Action ()
putSuccess = putColoured Vivid Green
