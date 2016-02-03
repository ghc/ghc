module Rules.Resources (resourceRules, Resources (..)) where

import Base

data Resources = Resources
    {
        resGhcPkg :: Resource
    }

-- We cannot register multiple packages in parallel:
resourceRules :: Rules Resources
resourceRules = Resources <$> newResource "ghc-pkg" 1
