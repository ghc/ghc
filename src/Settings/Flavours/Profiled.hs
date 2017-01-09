module Settings.Flavours.Profiled (profiledFlavour) where

import Flavour
import Predicate
import {-# SOURCE #-} Settings.Default
import Settings.Optimisation

profiledFlavour :: Flavour
profiledFlavour = defaultFlavour
    { name        = "prof"
    , args        = defaultBuilderArgs <> profiledArgs <> defaultPackageArgs
    , ghcProfiled = True }

profiledArgs :: Args
profiledArgs = optimisationArgs $ Optimisation
    { hsDefault  = append ["-O0", "-H64m"]
    , hsLibrary  = notStage0 ? arg "-O"
    , hsCompiler = arg "-O"
    , hsGhc      = arg "-O" }
