module Settings.Flavours.Quick (quickFlavour) where

import Flavour
import Predicate
import {-# SOURCE #-} Settings.Default
import Settings.Optimisation

quickFlavour :: Flavour
quickFlavour = defaultFlavour
    { name        = "quick"
    , args        = defaultBuilderArgs <> quickArgs <> defaultPackageArgs
    , libraryWays = append [vanilla] }

-- TODO: the hsLibrary setting seems wrong, but it matches mk/flavours/quick.mk
quickArgs :: Args
quickArgs = optimisationArgs $ Optimisation
    { hsDefault  = append ["-O0", "-H64m"]
    , hsLibrary  = notStage0 ? arg "-O"
    , hsCompiler =    stage0 ? arg "-O"
    , hsGhc      =    stage0 ? arg "-O" }
