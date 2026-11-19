{-# LANGUAGE NoExperimentalDependentTypes #-}
module NativeClosedFusedRuleLib (Parcel, make, first) where

import Numeric.Natural (Natural)

data Parcel = Parcel Natural

{-# NOINLINE make #-}
make :: Natural -> Parcel
make n = Parcel n

{-# NOINLINE first #-}
first :: Parcel -> Natural
first (Parcel n) = n

-- Deliberately changes the unoptimized answer. The ordinary oracle
-- distinguishes a real rule firing from fallback through the objects.
{-# RULES "parcel/fused" forall n. first (make n) = 7 #-}
