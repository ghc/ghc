module Settings.Flavours.Quick
   ( quickFlavour
   , quickDebugFlavour
   )
where

import qualified Data.Set as Set

import Expression
import Flavour
import Oracles.Flag
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
quickFlavour :: Flavour
quickFlavour = defaultFlavour
    { name        = "quick"
    , extraArgs        = quickArgs
    , libraryWays = Set.fromList <$>
                    mconcat
                    [ pure [vanilla]
                    , notStage0 ? platformSupportsSharedLibs ? pure [dynamic] ]
    , rtsWays     = Set.fromList <$>
                    mconcat
                    [ pure
                      [ vanilla, debug ]
                    , targetSupportsThreadedRts ? pure [ threaded, threadedDebug ]
                    , notStage0 ? platformSupportsSharedLibs ? pure
                      [ dynamic, debugDynamic ]
                    , notStage0 ? platformSupportsSharedLibs ? targetSupportsThreadedRts ? pure [
                      threadedDynamic, threadedDebugDynamic
                    ]
                    ] }

quickArgs :: Args
quickArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat [ pure ["-O0", "-H64m"] ]
    , hsLibrary  = notStage0 ? arg "-O"
    , hsCompiler = stage0 ? arg "-O2"
    , hsGhc      = stage0 ? arg "-O" }

quickDebugFlavour :: Flavour
quickDebugFlavour = quickFlavour
    { name = "quick-debug"
    , ghcDebugged = (>= Stage2)
    }
