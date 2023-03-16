module Hadrian.Builder.Git (gitArgs) where

import Expression

-- | Default command line arguments for invoking the archiving utility @git@.
gitArgs :: Args
gitArgs = mconcat
  [ builder (Git ListFiles) ? mconcat
      [ arg "ls-files"
      , arg "--recurse-submodules"
      , arg "-z"
      , getInputs
      ]
  ]
