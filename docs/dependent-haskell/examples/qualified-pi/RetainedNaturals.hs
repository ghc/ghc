{-# LANGUAGE DataKinds, ExperimentalDependentTypes, RoleAnnotations #-}

-- | Keep a built-in Natural alongside its index.
module RetainedNaturals (Reading, rememberNatural, readNatural) where

import qualified GHC.TypeNats as N
import Numeric.Natural (Natural)

-- | A reading at one type-level natural number.
type role Reading nominal
data Reading (n :: N.Nat) = Reading Natural

-- | Record the runtime input at the same index.
rememberNatural :: foreach (n :: Natural) -> Reading n
rememberNatural n = Reading n

-- | Read the value stored in the indexed result.
readNatural :: Reading n -> Natural
readNatural (Reading n) = n
