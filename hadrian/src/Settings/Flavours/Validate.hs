module Settings.Flavours.Validate (validateFlavour, slowValidateFlavour) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
validateFlavour :: Flavour
validateFlavour = werror $ defaultFlavour
    { name = "validate"
    , args = defaultBuilderArgs <> benchmarkArgs <> coreLintArgs <> defaultPackageArgs
    , libraryWays = pure [vanilla]
    , rtsWays = pure [vanilla, threaded] }

validateArgs :: Args
validateArgs = sourceArgs SourceArgs
    { hsDefault  = pure ["-O0", "-H64m"]
    , hsLibrary  = pure ["-O"]
    , hsCompiler = mconcat [stage0 ? arg "-DDEBUG", notStage0 ? arg "-O0"]
    , hsGhc      = pure ["-O2"] }

slowValidateFlavour :: Flavour
slowValidateFlavour = werror $ defaultFlavour
    { name = "slow-validate"
    , args = defaultBuilderArgs <> slowValidateArgs <> coreLintArgs <> defaultPackageArgs
    , libraryWays = pure [vanilla]
    , rtsWays = pure [vanilla, threaded, profiling] }

slowValidateArgs :: Args
slowValidateArgs =
  mconcat [ validateArgs
          -- See #11487
          , builder Ghc ? stage Stage1 ? arg "-fllvm-fill-undef-with-garbage"
          ]

-- | Enable Core Linting in builds with stage1 compiler.
coreLintArgs :: Args
coreLintArgs =
  builder Ghc ? stage Stage1 ? arg "-dcore-lint -dno-debug-output"
