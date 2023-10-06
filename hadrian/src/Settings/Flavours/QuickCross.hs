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
    , dynamicGhcPrograms = const (pure False)
    , libraryWays = Set.fromList <$>
                    mconcat
                    [ pure [vanilla]
                    , notStage0 ? staged targetSupportsSharedLibs ? pure [dynamic] ]
    , rtsWays     = Set.fromList <$>
                    mconcat
                    [ pure
                      [ vanilla, debug ]
                    , staged targetSupportsThreadedRts ? pure [threaded, threadedDebug]
                    , notStage0 ? staged targetSupportsSharedLibs ? pure
                      [ dynamic, debugDynamic ]
                    , notStage0 ? staged targetSupportsSharedLibs ? staged targetSupportsThreadedRts ? pure [
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
