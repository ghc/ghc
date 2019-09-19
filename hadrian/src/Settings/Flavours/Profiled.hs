module Settings.Flavours.Profiled (profiledFlavour) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
profiledFlavour :: Flavour
profiledFlavour = defaultFlavour
    { name        = "prof"
    , args        = defaultBuilderArgs <> profiledArgs <> defaultPackageArgs
    , ghcProfiled = True
    , dynamicGhcPrograms = pure False }

profiledArgs :: Args
profiledArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat
        [ pure ["-O0", "-H64m"]
        ]
    , hsLibrary  = notStage0 ? arg "-O"
    , hsCompiler = mconcat [stage0 ? arg "-O2", notStage0 ? arg "-O"]
    , hsGhc      = arg "-O" }
