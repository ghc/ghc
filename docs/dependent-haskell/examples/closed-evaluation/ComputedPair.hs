{-# LANGUAGE NoExperimentalDependentTypes #-}
module ComputedPair (bump, bumpRight) where

import Numeric.Natural (Natural)

-- | Add one and leave the second field unread.
bump :: Natural -> (Natural, Char)
bump n = (n + 1, error "unread")

-- | Compute the right field and leave the exceptional left field lazy.
bumpRight :: Natural -> (Char, Natural)
bumpRight n = (error "unread left field", n + 1)
