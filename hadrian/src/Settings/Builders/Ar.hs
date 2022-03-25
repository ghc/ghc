module Settings.Builders.Ar (arBuilderArgs) where

import Settings.Builders.Common

-- | Note that we do *not* emit arguments for the input paths here since we may
-- want to place these in a response file. This is handled in
-- 'Hadrian.Builder.Ar.runAr'.
arBuilderArgs :: Args
arBuilderArgs = mconcat
    [ builder (Ar Pack) ? mconcat
      [ -- When building on platforms which don't support object merging
        -- we must use the -L flag supported by llvm-ar, which ensures that
        -- .a files added to the archive are merged into the resulting archive,
        -- not added as a single file. This requires that we are using llvm-ar
        --
        -- See Note [Object merging] in GHC.Driver.Pipeline.Execute for details.
        ifM ((&&) <$> notStage0 <*> expr (flag ArSupportsDashL)) (arg "qL") (arg "q")
      , arg =<< getOutput
      ]
    , builder (Ar Unpack) ? mconcat
      [ arg "x"
      , arg =<< getInput
      ]
    ]
