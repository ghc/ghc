module Settings.Flavours.Validate (validateFlavour, slowValidateFlavour,
                                    quickValidateFlavour) where

import Expression
import Flavour
import Oracles.Flag
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
validateFlavour :: Flavour
validateFlavour = werror $ defaultFlavour
    { name = "validate"
    , args = defaultBuilderArgs <> validateArgs <> defaultPackageArgs
    , libraryWays = mconcat [ pure [vanilla]
                            , notStage0 ? platformSupportsSharedLibs ? pure [dynamic]
                            ]
    , rtsWays = mconcat [ pure [vanilla, threaded, debug, logging, threadedDebug, threadedLogging]
                        , notStage0 ? platformSupportsSharedLibs ? pure
                            [ dynamic, threadedDynamic, debugDynamic, threadedDebugDynamic
                            , loggingDynamic, threadedLoggingDynamic
                            ]
                        ]
    }

validateArgs :: Args
validateArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat [ stage0 ? pure ["-O0", "-H64m"]
                             -- See #11487
                           , notStage0 ? arg "-fllvm-fill-undef-with-garbage"
                           ]
    , hsLibrary  = pure ["-O", "-dcore-lint", "-dno-debug-output"]
    , hsCompiler = mconcat [ stage0 ? pure ["-O2", "-DDEBUG"]
                           , notStage0 ? pure ["-O", "-dcore-lint", "-dno-debug-output"]
                           ]
    , hsGhc      = pure ["-O"] }

slowValidateFlavour :: Flavour
slowValidateFlavour = werror $ validateFlavour
    { name = "slow-validate"
    , args = defaultBuilderArgs <> validateArgs <> defaultPackageArgs
    , ghcDebugAssertions = True
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
    , args               = defaultBuilderArgs <> quickValidateArgs <> defaultPackageArgs }
