module Rules.Resources (resourceRules, Resources (..)) where

import Base
import Util

data Resources = Resources
    {
        ghcCabal :: Resource,
        ghcPkg   :: Resource
    }

-- Unfortunately parallel invokations of ghc-cabal or ghc-pkg do not work:
-- * https://mail.haskell.org/pipermail/ghc-commits/2013-May/001712.html
-- * ghc.mk: see comment about parallel ghc-pkg invokations
resourceRules :: Rules Resources
resourceRules = liftM2 Resources (newResource "ghc-cabal" 1)
                                 (newResource "ghc-pkg"   1)
