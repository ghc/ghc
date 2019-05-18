module Settings.Default (
    SourceArgs (..), sourceArgs, defaultBuilderArgs, defaultPackageArgs,
    defaultArgs, defaultLibraryWays, defaultRtsWays,
    defaultFlavour, defaultPackages
    ) where

import Flavour
import Expression

data SourceArgs = SourceArgs
    { hsDefault  :: Args
    , hsLibrary  :: Args
    , hsCompiler :: Args
    , hsGhc      :: Args }

sourceArgs :: SourceArgs -> Args

defaultBuilderArgs, defaultPackageArgs, defaultArgs :: Args
defaultLibraryWays, defaultRtsWays :: Ways
defaultFlavour :: Flavour
defaultPackages :: Stage -> Action [Package]
