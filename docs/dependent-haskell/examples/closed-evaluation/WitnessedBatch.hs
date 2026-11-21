{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}

module WitnessedBatch
    ( Batch(..), build, partial, partialTail, firstWidth
    , MaskedRow(..), maskedRow, partialRow, rowWidth
    ) where

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

-- | The values and their mask share one length.
data MaskedRow a where
    MaskedRow :: SN n -> Vec n a -> Char -> Vec n Bool -> MaskedRow a

-- | Make a marked row with one selected value.
maskedRow :: Natural -> MaskedRow Natural
maskedRow n = MaskedRow (SS SZ) (VCons n VNil) 'x' (VCons True VNil)

-- | Supply the length alone.
partialRow :: Natural -> MaskedRow Natural
partialRow _ = MaskedRow (SS SZ) (error "unused values") (error "unused label") (error "unused mask")

-- | Read the width without inspecting the other fields.
rowWidth :: MaskedRow a -> Natural
rowWidth (MaskedRow size _ _ _) = width size

-- | Count the successors of a length witness.
width :: SN n -> Natural
width SZ = 0
width (SS n) = 1 + width n
