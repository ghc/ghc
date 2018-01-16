module Settings.Flavours.Profiled (profiledFlavour) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
profiledFlavour :: Flavour
profiledFlavour = defaultFlavour
    { name        = "prof"
    , args        = defaultBuilderArgs <> profiledArgs <> defaultPackageArgs
    , ghcProfiled = True }

profiledArgs :: Args
profiledArgs = sourceArgs SourceArgs
    { hsDefault  = pure ["-O0", "-H64m"]
    , hsLibrary  = notStage0 ? arg "-O"
    , hsCompiler = arg "-O"
    , hsGhc      = arg "-O" }
