{-# LANGUAGE NoExperimentalDependentTypes #-}

module OrdinaryOffer (offered) where

import Numeric.Natural (Natural)

-- | Offer two adjacent field labels starting at the requested label.
offered :: Natural -> [Natural]
offered start = [start, start + 1]
