module Settings.Flavours.QuickCross (quickCrossFlavour) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
quickCrossFlavour :: Flavour
quickCrossFlavour = disableProfiledLibs $ defaultFlavour
    { name        = "quick-cross"
    , extraArgs        = quickCrossArgs
    , dynamicGhcPrograms = pure False
    }

quickCrossArgs :: Args
quickCrossArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat $
        [ pure ["-O0", "+RTS", "-O64M", "-RTS"]
        ]
    , hsLibrary  = notStage0 ? mconcat [ arg "-O", arg "-fllvm" ]
    , hsCompiler = stage0 ? arg "-O2"
    , hsGhc      = mconcat
                   [ stage0 ? arg "-O"
                   , stage1 ? mconcat [ arg "-O0", arg "-fllvm" ] ] }
