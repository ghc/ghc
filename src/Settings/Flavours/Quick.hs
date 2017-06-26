module Settings.Flavours.Quick (quickFlavour) where

import Flavour
import Predicate
import Oracles.Config.Flag (platformSupportsSharedLibs)
import {-# SOURCE #-} Settings.Default

quickFlavour :: Flavour
quickFlavour = defaultFlavour
    { name        = "quick"
    , args        = defaultBuilderArgs <> quickArgs <> defaultPackageArgs
    , libraryWays = mconcat
                    [ append [vanilla]
                    , notStage0 ? platformSupportsSharedLibs ? append [dynamic] ] }

quickArgs :: Args
quickArgs = sourceArgs $ SourceArgs
    { hsDefault  = append ["-O0", "-H32m"]
    , hsLibrary  = notStage0 ? arg "-O"
    , hsCompiler =    stage0 ? arg "-O"
    , hsGhc      =    stage0 ? arg "-O" }
