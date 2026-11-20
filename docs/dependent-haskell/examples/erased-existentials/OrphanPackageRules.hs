{-# LANGUAGE ExperimentalDependentTypes #-}
module OrphanPackageRules () where

import PackageRules (open)

{-# RULES "open/int-package" forall (x :: Int) (k :: Int -> Int).
    open (x, k) = applyInt k x #-}

-- | Apply the callback carried with an integer witness.
{-# NOINLINE applyInt #-}
applyInt :: (Int -> Int) -> Int -> Int
applyInt k x = k x
