module Settings.Flavours.QuickCross (quickCrossFlavour) where

import qualified Data.Set as Set

import Expression
import Flavour
import Oracles.Flag
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
quickCrossFlavour :: Flavour
quickCrossFlavour = defaultFlavour
    { name        = "quick-cross"
    , extraArgs        = quickCrossArgs
    , dynamicGhcPrograms = pure False
    , libraryWays = Set.fromList <$>
                    mconcat
                    [ pure [vanilla]
                    , notStage0 ? platformSupportsSharedLibs ? pure [dynamic] ]
    , rtsWays     = Set.fromList <$>
                    mconcat
                    [ pure
                      [ vanilla, debug ]
                    , targetSupportsThreadedRts ? pure [threaded, threadedDebug]
                    , notStage0 ? platformSupportsSharedLibs ? pure
                      [ dynamic, debugDynamic ]
                    , notStage0 ? platformSupportsSharedLibs ? targetSupportsThreadedRts ? pure [
                      threadedDynamic, threadedDebugDynamic
                    ]
                    ] }

quickCrossArgs :: Args
quickCrossArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat $
        [ pure ["-O0", "-H64m"]
        ]
    , hsLibrary  = notStage0 ? mconcat [ arg "-O", arg "-fllvm" ]
    , hsCompiler = stage0 ? arg "-O2"
    , hsGhc      = mconcat
                   [ stage0 ? arg "-O"
                   , stage1 ? mconcat [ arg "-O0", arg "-fllvm" ] ] }
