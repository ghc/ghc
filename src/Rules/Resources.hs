module Rules.Resources (resourceRules, Resources (..), resPackageDbLimit) where

import Base

data Resources = Resources
    {
        resPackageDb :: Resource
    }

-- We cannot register multiple packages in parallel. Also we cannot run GHC
-- when the package database is being mutated by "ghc-pkg". This is a classic
-- concurrent read exclusive write (CREW) conflict.
resourceRules :: Rules Resources
resourceRules = Resources <$> newResource "package-db" resPackageDbLimit

resPackageDbLimit :: Int
resPackageDbLimit = 1000
