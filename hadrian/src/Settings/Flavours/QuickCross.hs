module Settings.Flavours.QuickCross (quickCrossFlavour, quickJsFlavour, perfJsFlavour) where

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

perfJsFlavour :: Flavour
perfJsFlavour = defaultFlavour
    { name        = "perf-js"
    , args        = defaultBuilderArgs <> perfJsArgs <> defaultPackageArgs
    , dynamicGhcPrograms = pure False
    , libraryWays = pure $ Set.singleton vanilla
    , rtsWays     = pure $ Set.singleton vanilla
    }

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

perfJsArgs :: Args
perfJsArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat [ arg "-O2", arg "-H64m"]
    , hsLibrary  = arg "-O2"
    , hsCompiler = arg "-O2"
    , hsGhc      = arg "-O2"
    }

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
