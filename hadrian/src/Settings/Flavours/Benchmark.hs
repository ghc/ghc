module Settings.Flavours.Benchmark (benchmarkFlavour) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
benchmarkFlavour :: Flavour
benchmarkFlavour = defaultFlavour
    { name = "bench"
    , args = defaultBuilderArgs <> benchmarkArgs <> defaultPackageArgs
    , libraryWays = pure [vanilla]
    , rtsWays = pure [vanilla, threaded] }

benchmarkArgs :: Args
benchmarkArgs = sourceArgs SourceArgs
    { hsDefault  = pure ["-O", "-H64m"]
    , hsLibrary  = pure ["-O2"]
    -- TODO: We should really pass -O2 when notStage0. Otherwise, we aren't
    -- really measuring the overhead of a potential new optimisation we want
    -- to benchmark. This has to happen in sync with the Makefile build, though.
    , hsCompiler = mconcat [stage0 ? arg "-O2", notStage0 ? arg "-O0"]
    , hsGhc      = pure ["-O2"] }

