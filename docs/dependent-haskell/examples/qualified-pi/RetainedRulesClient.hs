{-# LANGUAGE NoExperimentalDependentTypes #-}
module RetainedRulesClient (fromInput, boundPayload, atFalse, ignoredIndex) where

import RetainedRules

-- | Apply both functions to one runtime input.
fromInput :: Bool -> Int
fromInput b = readChoice (keep b (choose b))

-- | The rewrite rule's payload type refers to the preceding Boolean.
boundPayload :: Bool -> Int
boundPayload b = readKept b (choose b)

-- | Match the closed rule with a payload other than a call to choose.
atFalse :: Int -> Int
atFalse n = readChoice (keep False (Unused n))

-- | An indexed payload can be available before its index.
ignoredIndex :: Int
ignoredIndex = readChoice (keep b (Unused 19))
  where
    b :: Bool
    b = error "forced the index"
