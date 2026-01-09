module Settings.Flavours.Validate (validateFlavour, slowValidateFlavour,
                                    quickValidateFlavour) where


import Expression
import Flavour
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
validateFlavour :: Flavour
validateFlavour = enableLinting $ quickValidateFlavour
    { name = "validate"
    , extraArgs = validateArgs <> defaultHaddockExtraArgs
    , ghcDebugAssertions = (<= Stage1)
    }

validateArgs :: Args
validateArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat [ stage0 ? pure ["-O0"]
                             -- See #11487
                           , notStage0 ? arg "-fllvm-fill-undef-with-garbage"
                           , notStage0 ? arg "-dno-debug-output"
                           , notStage0 ? arg "-fcheck-prim-bounds"
                           , pure ["+RTS", "-O64M", "-RTS"]
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
    { hsDefault  = pure ["+RTS", "-O64M", "-RTS"]
    , hsLibrary  = pure [ "-O" ]
    , hsCompiler = mconcat [ stage0 ? arg "-O2", notStage0 ? arg "-O"]
    , hsGhc      = pure [ "-O", "-hide-all-packages" ]
    }

quickValidateFlavour :: Flavour
quickValidateFlavour = werror $ disableProfiledLibs $ defaultFlavour
    { name               = "quick-validate"
    , extraArgs               = quickValidateArgs }
