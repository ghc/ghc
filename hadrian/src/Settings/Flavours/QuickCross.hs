module Settings.Flavours.QuickCross (quickCrossFlavour, quickJsFlavour) where

import qualified Data.Set as Set

import Expression
import Flavour
import Oracles.Flag
import {-# SOURCE #-} Settings.Default

quickJsFlavour :: Flavour
quickJsFlavour = defaultFlavour
    { name        = "quick-js"
    , args        = defaultBuilderArgs <> quickJsArgs <> defaultPackageArgs
    , dynamicGhcPrograms = pure False
    , libraryWays = pure $ Set.singleton vanilla
    , rtsWays     = pure $ Set.singleton vanilla
    }

-- Same as quickCrossArgs (until it bitrots) but don't enable -fllvm
quickJsArgs :: Args
quickJsArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat $
        [ pure ["-O0", "-H64m"]
        ]
    , hsLibrary  = notStage0 ? mconcat [ arg "-O" ]
    , hsCompiler = stage0 ? arg "-O2"
    , hsGhc      = mconcat
                   [ stage0 ? arg "-O"
                   , stage1 ? mconcat [ arg "-O0" ] ] }

-- Please update doc/flavours.md when changing this file.
quickCrossFlavour :: Flavour
quickCrossFlavour = defaultFlavour
    { name        = "quick-cross"
    , args        = defaultBuilderArgs <> quickCrossArgs <> defaultPackageArgs
    , dynamicGhcPrograms = pure False
    , libraryWays = Set.fromList <$>
                    mconcat
                    [ pure [vanilla]
                    , notStage0 ? platformSupportsSharedLibs ? pure [dynamic] ]
    , rtsWays     = Set.fromList <$>
                    mconcat
                    [ pure
                      [ vanilla, threaded, debug, threadedDebug, threaded ]
                    , notStage0 ? platformSupportsSharedLibs ? pure
                      [ dynamic, debugDynamic, threadedDynamic
                      , threadedDebugDynamic ]
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
