{-# LANGUAGE ImpredicativeTypes, LambdaCase, NoExperimentalDependentTypes #-}
module RankedPackageClient (runGuarded) where

import RankedPackage (Callback(..), GuardedBuilder, atTwoTypes, consumeIdentity, observeGuarded)

-- | Use the declared consumer to infer the witness for both guards.
builder :: GuardedBuilder
builder = \cases
    ready input | ready && input -> consumeIdentity id
                | otherwise -> Callback id atTwoTypes

-- | Skip the input when the first flag is false.
-- Run the typed branch when both flags are true.
runGuarded :: ((Int, Bool), (Int, Bool))
runGuarded = (observeGuarded builder False (error "unused guard input"),
              observeGuarded builder True True)
