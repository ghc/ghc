{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}

-- | Read indexed Natural values from an ordinary module.
module RetainedNaturalClient (observeNaturals, observeNamedNatural) where

import Numeric.Natural (Natural)
import RetainedNaturals

-- | The signatures require the same numerals at the type and term levels.
observeNaturals :: (Natural, Natural)
observeNaturals = (readNatural small, readNatural large)
  where
    small :: Reading 5
    small = rememberNatural 5

    large :: Reading 18446744073709551616
    large = rememberNatural 18446744073709551616

-- | Use a named runtime value as an opaque index.
observeNamedNatural :: Natural -> Natural
observeNamedNatural n = readNatural (rememberNatural n)
