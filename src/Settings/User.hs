module Settings.User (
    module Settings.Default,
    userArgs, userPackages, userWays, userTargetDirectory,
    userKnownPackages, integerLibrary,
    buildHaddock, validating, dynamicGhcPrograms, laxDependencies
    ) where

import Stage
import Package
import Expression
import Settings.Default

-- No user-specific settings by default
-- TODO: rename to userArgs
userArgs :: Args
userArgs = mempty

-- Control which packages get to be built
userPackages :: Packages
userPackages = remove [compiler] -- TODO: fix compiler

-- Add new user-defined packages
userKnownPackages :: [Package]
userKnownPackages = []

-- Control which ways are built
userWays :: Ways
userWays = mempty

-- Control where build results go (see Settings.Default for an example)
userTargetDirectory :: Stage -> Package -> FilePath
userTargetDirectory = defaultTargetDirectory

-- Choose integer library: integerGmp, integerGmp2 or integerSimple
integerLibrary :: Package
integerLibrary = integerGmp2

-- User-defined flags. Note the following type semantics:
-- * Bool: a plain Boolean flag whose value is known at compile time
-- * Action Bool: a flag whose value can depend on the build environment
-- * Predicate: a flag depending on the build environment and the current target
validating :: Bool
validating = False

dynamicGhcPrograms :: Bool
dynamicGhcPrograms = False

laxDependencies :: Bool
laxDependencies = False

buildHaddock :: Predicate
buildHaddock = return True
