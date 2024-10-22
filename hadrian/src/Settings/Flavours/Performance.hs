module Settings.Flavours.Performance (performanceFlavour, performanceArgs) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
performanceFlavour :: Flavour
performanceFlavour = splitSections $ enableLateCCS $ defaultFlavour
    { name = "perf"
    , extraArgs = performanceArgs <> defaultHaddockExtraArgs }

performanceArgs :: Args
performanceArgs = sourceArgs SourceArgs
    { hsDefault  = pure ["-O", "-H64m"]
    , hsLibrary  = orM [notStage0, cross] ? arg "-O2"
    , hsCompiler = pure ["-O2"]
    , hsGhc      = mconcat
                    [ andM [stage0, notCross] ? arg "-O"
                    , orM  [notStage0, cross] ? arg "-O2"
                    ]
    }
