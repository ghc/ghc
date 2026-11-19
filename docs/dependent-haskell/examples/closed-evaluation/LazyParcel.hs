{-# LANGUAGE NoExperimentalDependentTypes #-}

module LazyParcel (Parcel, make, first, second) where

import Numeric.Natural (Natural)

-- | A parcel whose representation is private to this module.
data Parcel = Parcel Natural Natural

-- | Build a parcel with an exceptional second field.
make :: Natural -> Parcel
make n = Parcel n (errorWithoutStackTrace "unused second parcel field")

-- | Read the first field.
first :: Parcel -> Natural
first (Parcel n _) = n

-- | Read the second field.
second :: Parcel -> Natural
second (Parcel _ n) = n
