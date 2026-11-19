{-# LANGUAGE NoExperimentalDependentTypes #-}
module Signal (Signal (..), at, shouldStop) where

import Numeric.Natural (Natural)

-- | The two results that a client can use as indices.
data Signal = Stop | Go

-- | Select a signal for this instant.
at :: Natural -> Signal
at n = if even n then Stop else Go

-- | Decide whether to stop at the given signal.
shouldStop :: Signal -> Bool
shouldStop Stop = True
shouldStop Go = False
