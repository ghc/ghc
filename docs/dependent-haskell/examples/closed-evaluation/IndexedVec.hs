{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoExperimentalDependentTypes #-}

module IndexedVec (N (..), Vec (..), two, safeHead, readTwo) where

import Numeric.Natural (Natural)

-- | Natural numbers used in vector lengths.
data N = Z | S N

-- | An ordinary GADT indexed by the number of cells.
data Vec (n :: N) a where
    VNil :: Vec 'Z a
    VCons :: a -> Vec n a -> Vec ('S n) a

-- | Build two consecutive values.
two :: Natural -> Vec ('S ('S 'Z)) Natural
two x = VCons x (VCons (x + 1) VNil)

-- | Read the first cell of a nonempty vector.
safeHead :: Vec ('S n) a -> a
safeHead (VCons x _) = x

-- | Combine the two cells in their original order.
readTwo :: Vec ('S ('S 'Z)) Natural -> Natural
readTwo (VCons x (VCons y VNil)) = 10 * x + y
