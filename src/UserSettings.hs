module UserSettings (
    userSettings, userPackages, userWays, userTargetDirectory,
    userKnownPackages, integerLibrary,
    buildHaddock, validating
    ) where

import Base hiding (arg, args, Args)
import Package
import Targets
import Expression

-- No user-specific settings by default
-- TODO: rename to userArgs
userSettings :: Settings
userSettings = mempty

-- Control which packages get to be built
userPackages :: Packages
userPackages = mempty

-- Add new user-defined packages
userKnownPackages :: [Package]
userKnownPackages = []

-- Control which ways are built
userWays :: Ways
userWays = mempty

-- Control where build results go
userTargetDirectory :: Stage -> Package -> FilePath
userTargetDirectory = defaultTargetDirectory

-- Choose integer library: integerGmp, integerGmp2 or integerSimple
integerLibrary :: Package
integerLibrary = integerGmp2

-- User-defined predicates
-- TODO: migrate more predicates here from configuration files
buildHaddock :: Predicate
buildHaddock = return True

validating :: Predicate
validating = return False
