module Settings.Builders.Ar (arBuilderArgs) where

import Settings.Builders.Common

-- | Note that we do *not* emit arguments for the input paths here since we may
-- want to place these in a response file. This is handled in
-- 'Hadrian.Builder.Ar.runAr'.
arBuilderArgs :: Args
arBuilderArgs = mconcat
    [ builder (Ar Pack) ? mconcat
      [ arg "q"
      , arg =<< getOutput
      ]
    , builder (Ar Unpack) ? mconcat
      [ arg "x"
      , arg =<< getInput
      ]
    ]
