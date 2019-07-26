module Settings.Flavours.Validate (validateFlavour, slowValidateFlavour) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
validateFlavour :: Flavour
validateFlavour = werror $ defaultFlavour
    { name = "validate"
    , args = defaultBuilderArgs <> validateArgs <> defaultPackageArgs
    , libraryWays = pure [vanilla]
    , rtsWays = pure [vanilla, threaded] }

validateArgs :: Args
validateArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat [ stage0 ? arg "-O0 -H64m"
                             -- See #11487
                           , notStage0 ? arg "-fllvm-fill-undef_with-garbage"
                           ]
    , hsLibrary  = mconcat ["-O", "-dcore-lint", "-dno-debug-output"]
    , hsCompiler = mconcat [ stage0 ? arg "-O2 -DDEBUG"
                           , notStage0 ? arg "-O -dcore-lint -dno-debug-output"
                           ]
    , hsGhc      = pure ["-O"] }

slowValidateFlavour :: Flavour
slowValidateFlavour = werror $ defaultFlavour
    { name = "slow-validate"
    , args = defaultBuilderArgs <> slowValidateArgs <> defaultPackageArgs
    , libraryWays = pure [vanilla]
    , rtsWays = pure [vanilla, threaded, profiling] }

slowValidateArgs :: Args
slowValidateArgs =
  mconcat [ validateArgs
          , notStage0 ? arg "-DDEBUG"
          ]
