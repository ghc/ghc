module Settings.Flavours.Performance (performanceFlavour) where

import Flavour
import Predicate
import {-# SOURCE #-} Settings.Default
import Settings.SourceArgs

performanceFlavour :: Flavour
performanceFlavour = defaultFlavour
    { name = "perf"
    , args = defaultBuilderArgs <> performanceArgs <> defaultPackageArgs }

performanceArgs :: Args
performanceArgs = sourceArgs $ SourceArgs
    { hsDefault  = append ["-O", "-H64m"]
    , hsLibrary  = notStage0 ? arg "-O2"
    , hsCompiler = mconcat [stage0 ? arg "-O", notStage0 ? arg "-O2"]
    , hsGhc      = mconcat [stage0 ? arg "-O", notStage0 ? arg "-O2"] }
