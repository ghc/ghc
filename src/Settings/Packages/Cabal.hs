module Settings.Packages.Cabal where

import Expression

cabalPackageArgs :: Args
cabalPackageArgs = package cabal ?
    -- Cabal is a rather large library and quite slow to compile. Moreover, we
    -- build it for stage0 only so we can link ghc-pkg against it, so there is
    -- little reason to spend the effort to optimize it.
    stage0 ? builder Ghc ? arg "-O0"
