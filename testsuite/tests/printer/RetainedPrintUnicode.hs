{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module RetainedPrintUnicode where

keep ∷ foreach -- Retain this Boolean argument.
    (b ∷ Bool)
    → Select b → Select b
keep b x = x
