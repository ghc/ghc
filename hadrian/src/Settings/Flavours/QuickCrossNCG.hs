module Settings.Flavours.QuickCrossNCG (quickCrossNCGFlavour) where

import Expression
import Flavour
import Oracles.Flag
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
quickCrossNCGFlavour :: Flavour
quickCrossNCGFlavour = defaultFlavour
    { name        = "quick-cross-ncg"
    , args        = defaultBuilderArgs <> quickCrossNCGArgs <> defaultPackageArgs
    , dynamicGhcPrograms = pure False
    , libraryWays = mconcat
                    [ pure [vanilla]
                    , notStage0 ? platformSupportsSharedLibs ? pure [dynamic] ]
    , rtsWays     = mconcat
                    [ pure
                      [ vanilla, threaded, logging, debug
                      , threadedDebug, threadedLogging, threaded ]
                    , notStage0 ? platformSupportsSharedLibs ? pure
                      [ dynamic, debugDynamic, threadedDynamic, loggingDynamic
                      , threadedDebugDynamic, threadedLoggingDynamic ]
                    ] }

quickCrossNCGArgs :: Args
quickCrossNCGArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat $
        [ pure ["-O0", "-H64m"]
        ]
    , hsLibrary  = notStage0 ? mconcat [ arg "-O", arg "-fasm" ]
    , hsCompiler = stage0 ? arg "-O2"
    , hsGhc      = mconcat
                   [ stage0 ? arg "-O"
                   , stage1 ? mconcat [ arg "-O0", arg "-fasm" ] ] }
