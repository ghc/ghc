module Settings.Flavours.Quickest (quickestFlavour) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
quickestFlavour :: Flavour
quickestFlavour = defaultFlavour
    { name        = "quickest"
    , args        = defaultArgs <> quickestArgs
    , libraryWays = pure [vanilla]
    , rtsWays     = pure [vanilla, threaded, threadedLogging, logging]
    , dynamicGhcPrograms = return False }

quickestArgs :: Args
quickestArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat $
        [ pure ["-O0", "-H64m"]
        ]
    , hsLibrary  = mempty
    , hsCompiler = stage0 ? arg "-O"
    , hsGhc      = stage0 ? arg "-O" }
