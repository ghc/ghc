{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}

module WitnessedBatch (Batch(..), build, partial, partialTail, firstWidth) where

import ExistentialVec (SN(..))
import IndexedVec (Vec(..))
import Numeric.Natural (Natural)

-- | Each chunk carries its own vector length.
data Batch a where
    End :: Batch a
    Chunk :: SN n -> Vec n a -> Batch a -> Batch a

-- | Choose chunk lengths from the input.
build :: Natural -> Batch Natural
build n
    | even n = Chunk (SS SZ) (VCons n VNil)
                     (Chunk (SS (SS SZ)) (VCons (n + 1) (VCons (n + 2) VNil)) End)
    | otherwise = Chunk (SS (SS SZ)) (VCons n (VCons (n + 1) VNil)) End

-- | Leave both the first vector and the remaining chunks undefined.
partial :: Natural -> Batch Natural
partial _ = Chunk (SS SZ) (error "unavailable vector") (error "unavailable next chunk")

-- | Leave the remaining chunks undefined after a complete first vector.
partialTail :: Natural -> Batch Natural
partialTail n = Chunk (SS SZ) (VCons n VNil) (error "unavailable next chunk")

-- | Observe only the first length witness.
firstWidth :: Batch a -> Natural
firstWidth End = 0
firstWidth (Chunk size _ _) = width size
  where
    width :: SN n -> Natural
    width SZ = 0
    width (SS n) = 1 + width n
