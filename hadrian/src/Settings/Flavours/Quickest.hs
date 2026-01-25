module Settings.Flavours.Quickest (quickestFlavour) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
quickestFlavour :: Flavour
quickestFlavour = disableDynamicLibs $ disableProfiledLibs $ defaultFlavour
    { name        = "quickest"
    , extraArgs        = quickestArgs
    }

quickestArgs :: Args
quickestArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat $
        [ pure ["-O0", "+RTS", "-O64M", "-RTS"]
        ]
    , hsLibrary  = mempty
    , hsCompiler = stage0 ? arg "-O"
    , hsGhc      = stage0 ? arg "-O" }
