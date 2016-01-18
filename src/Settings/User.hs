module Settings.User (
    buildRootPath, trackBuildSystem, compileInterfaceFilesSeparately,
    userArgs, userPackages, userLibraryWays, userRtsWays, userKnownPackages,
    integerLibrary, buildHaddock, validating, ghciWithDebugger, ghcProfiled,
    ghcDebugged, dynamicGhcPrograms, laxDependencies, buildSystemConfigFile,
    verboseCommands, turnWarningsIntoErrors, splitObjects
    ) where

import GHC
import Expression
import Predicates

-- | All build artefacts are stored in 'buildRootPath' directory.
buildRootPath :: FilePath
buildRootPath = ".build"

-- Control user-specific settings
userArgs :: Args
userArgs = builderGhc ? remove ["-Wall", "-fwarn-tabs"]

-- Control which packages get to be built
userPackages :: Packages
userPackages = mempty

-- Add new user-defined packages
userKnownPackages :: [Package]
userKnownPackages = []

-- | Control which ways library packages are built
-- FIXME: skip profiling for speed
-- FIXME: skip dynamic since it's currently broken #4
userLibraryWays :: Ways
userLibraryWays = remove [profiling, dynamic]

-- | Control which ways the 'rts' package is built
userRtsWays :: Ways
userRtsWays = mempty

-- | Choose the integer library: integerGmp or integerSimple
integerLibrary :: Package
integerLibrary = integerGmp

-- | User-defined flags. Note the following type semantics:
-- * Bool: a plain Boolean flag whose value is known at compile time
-- * Action Bool: a flag whose value can depend on the build environment
-- * Predicate: a flag depending on the build environment and the current target

-- | Set this to True if you are making any changes in the build system and want
-- appropriate rebuilds to be initiated. Switching this to False speeds things
-- up a little (particularly zero builds).
-- WARNING: a complete rebuild is required when changing this setting.
trackBuildSystem :: Bool
trackBuildSystem = True

validating :: Bool
validating = False

-- To switch on split objects use 'splitObjects = defaultSplitObjects', see #153
splitObjects :: Predicate
splitObjects = return False

dynamicGhcPrograms :: Bool
dynamicGhcPrograms = False

ghciWithDebugger :: Bool
ghciWithDebugger = False

ghcProfiled :: Bool
ghcProfiled = False

-- TODO: do we need to be able to set this from command line?
ghcDebugged :: Bool
ghcDebugged = False

-- | When laxDependencies is set to True, dependencies on the GHC executable
-- are turned into order-only dependencies to avoid needless recompilation when
-- making changes to GHC's sources. In certain situations this can lead to build
-- failures, in which case you should reset the flag (at least temporarily).
laxDependencies :: Bool
laxDependencies = False

buildHaddock :: Predicate
buildHaddock = return False -- FIXME: should be return True, see #98

buildSystemConfigFile :: Bool
buildSystemConfigFile = False

-- | Set to True to print full command lines during the build process. Note,
-- this is a Predicate, hence you can enable verbose output for a chosen package
-- only, e.g.: verboseCommands = package ghcPrim
verboseCommands :: Predicate
verboseCommands = return False

-- | To enable -Werror in Stage2 set turnWarningsIntoErrors = stage2.
turnWarningsIntoErrors :: Predicate
turnWarningsIntoErrors = return False

-- | Decouple the compilation of @*.hi@ and @*.o@ files by setting to True.
compileInterfaceFilesSeparately :: Bool
compileInterfaceFilesSeparately = False
