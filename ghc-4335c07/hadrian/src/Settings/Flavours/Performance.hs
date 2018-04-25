module Settings.Flavours.Performance (performanceFlavour) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
performanceFlavour :: Flavour
performanceFlavour = defaultFlavour
    { name = "perf"
    , args = defaultBuilderArgs <> performanceArgs <> defaultPackageArgs }

performanceArgs :: Args
performanceArgs = sourceArgs SourceArgs
    { hsDefault  = pure ["-O", "-H64m"]
    , hsLibrary  = notStage0 ? arg "-O2"
    , hsCompiler = mconcat [stage0 ? arg "-O", notStage0 ? arg "-O2"]
    , hsGhc      = mconcat [stage0 ? arg "-O", notStage0 ? arg "-O2"] }
