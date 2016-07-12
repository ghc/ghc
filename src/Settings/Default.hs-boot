module Settings.Default (
    defaultBuilderArgs, defaultPackageArgs, defaultArgs, defaultPackages,
    defaultLibraryWays, defaultRtsWays, defaultFlavour, defaultSplitObjects
    ) where

import Flavour
import Predicate

defaultBuilderArgs, defaultPackageArgs, defaultArgs :: Args
defaultPackages :: Packages
defaultLibraryWays, defaultRtsWays :: Ways
defaultFlavour :: Flavour
defaultSplitObjects :: Predicate
