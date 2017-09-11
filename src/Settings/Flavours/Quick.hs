module Settings.Flavours.Quick (quickFlavour) where

import Flavour
import Expression
import Oracles.Flag
import {-# SOURCE #-} Settings.Default

quickFlavour :: Flavour
quickFlavour = defaultFlavour
    { name        = "quick"
    , args        = defaultBuilderArgs <> quickArgs <> defaultPackageArgs
    , libraryWays = mconcat
                    [ pure [vanilla]
                    , notStage0 ? platformSupportsSharedLibs ? pure [dynamic] ] }

quickArgs :: Args
quickArgs = sourceArgs $ SourceArgs
    { hsDefault  = pure ["-O0", "-H64m"]
    , hsLibrary  = notStage0 ? arg "-O"
    , hsCompiler =    stage0 ? arg "-O"
    , hsGhc      =    stage0 ? arg "-O" }
