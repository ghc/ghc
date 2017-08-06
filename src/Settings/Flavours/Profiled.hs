module Settings.Flavours.Profiled (profiledFlavour) where

import Flavour
import Expression
import {-# SOURCE #-} Settings.Default

profiledFlavour :: Flavour
profiledFlavour = defaultFlavour
    { name        = "prof"
    , args        = defaultBuilderArgs <> profiledArgs <> defaultPackageArgs
    , ghcProfiled = True }

profiledArgs :: Args
profiledArgs = sourceArgs $ SourceArgs
    { hsDefault  = append ["-O0", "-H32m"]
    , hsLibrary  = notStage0 ? arg "-O"
    , hsCompiler = arg "-O"
    , hsGhc      = arg "-O" }
