module Settings.Flavours.Validate (validateFlavour, slowValidateFlavour,
                                    quickValidateFlavour) where

import qualified Data.Set as Set

import Expression
import Flavour
import Oracles.Flag
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
validateFlavour :: Flavour
validateFlavour = enableLinting $ werror $ defaultFlavour
    { name = "validate"
    , extraArgs = validateArgs <> defaultHaddockExtraArgs
    , libraryWays = Set.fromList <$>
                    mconcat [ pure [vanilla]
                            , notStage0 ? staged targetSupportsSharedLibs ? pure [dynamic]
                            ]
    , rtsWays = Set.fromList <$>
                mconcat [ pure [vanilla, debug]
                        , staged targetSupportsThreadedRts ? pure [threaded, threadedDebug]
                        , notStage0 ? staged targetSupportsSharedLibs ? pure
                            [ dynamic, debugDynamic
                            ]
                        , notStage0 ? staged targetSupportsSharedLibs ? staged targetSupportsThreadedRts ? pure
                            [ threadedDynamic, threadedDebugDynamic ]
                        ]
    , ghcDebugAssertions = (<= Stage1)
    }

validateArgs :: Args
validateArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat [ stage0 ? pure ["-O0", "-H64m"]
                             -- See #11487
                           , notStage0 ? arg "-fllvm-fill-undef-with-garbage"
                           , notStage0 ? arg "-dno-debug-output"
                           , notStage0 ? arg "-fcheck-prim-bounds"
                           ]
    , hsLibrary  = pure ["-O"]
    , hsCompiler = mconcat [ stage0 ? pure ["-O2"]
                           , notStage0 ? pure ["-O" ]
                           ]
    , hsGhc      = pure ["-O"] }


slowValidateFlavour :: Flavour
slowValidateFlavour = validateFlavour
    { name = "slow-validate"
    , ghcDebugAssertions = const True
    }

quickValidateArgs :: Args
quickValidateArgs = sourceArgs SourceArgs
    { hsDefault  = mempty
    , hsLibrary  = pure [ "-O" ]
    , hsCompiler = mconcat [ stage0 ? arg "-O2", notStage0 ? arg "-O"]
    , hsGhc      = pure [ "-O", "-hide-all-packages" ]
    }

quickValidateFlavour :: Flavour
quickValidateFlavour = werror $ validateFlavour
    { name               = "quick-validate"
    , extraArgs               = quickValidateArgs }
