{-# LANGUAGE DataKinds, ExplicitForAll, GADTs, NoExperimentalDependentTypes #-}

module WitnessedBatch
    ( Batch(..), build, partial, partialTail, firstWidth
    , MaskedRow(..), maskedRow, partialRow, rowWidth, selectedValues
    , KeyedRow(..), keyedRow, lookupColumn
    , TableMargins(..), tableMargins, lookupTotals
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
maskedRow n = MaskedRow (SS (SS SZ)) (VCons n (VCons (n + 1) VNil))
                        'x' (VCons False (VCons True VNil))

-- | Supply the length alone.
partialRow :: Natural -> MaskedRow Natural
partialRow _ = MaskedRow (SS SZ) (error "unused values") (error "unused label") (error "unused mask")

-- | Read the width without inspecting the other fields.
rowWidth :: MaskedRow a -> Natural
rowWidth (MaskedRow size _ _ _) = width size

-- | Return the values selected by the mask in their original order.
selectedValues :: MaskedRow a -> [a]
selectedValues (MaskedRow _ values _ mask) = select values mask
  where
    select :: Vec n a -> Vec n Bool -> [a]
    select VNil VNil = []
    select (VCons x xs) (VCons keep rest)
        | keep = x : select xs rest
        | otherwise = select xs rest

-- | The keys and values have the same hidden length.
data KeyedRow a where
    KeyedRow :: Char -> Vec n Char -> Vec n a -> KeyedRow a

-- | Construct an empty row at zero and a two-column row otherwise.
keyedRow :: Natural -> KeyedRow Natural
keyedRow 0 = KeyedRow 'e' VNil VNil
keyedRow n = KeyedRow 'r' (VCons 'x' (VCons 'y' VNil))
                          (VCons n (VCons (n + 1) VNil))

-- | Read the value at the first matching key.
lookupColumn :: Char -> KeyedRow a -> Maybe a
lookupColumn key (KeyedRow _ keys values) = lookupKey key keys values

-- | Row and column labels each index their own totals.
data TableMargins a where
    TableMargins :: forall cols rows a. Vec rows Char -> Vec cols Char
                 -> Vec rows a -> Vec cols a -> TableMargins a

-- | Use one row and two columns for a positive input.
tableMargins :: Natural -> TableMargins Natural
tableMargins 0 = TableMargins VNil (VCons 'x' VNil) VNil (VCons 0 VNil)
tableMargins n = TableMargins (VCons 'r' VNil) (VCons 'x' (VCons 'y' VNil))
                              (VCons (n + n) VNil) (VCons n (VCons n VNil))

-- | Look up a row total and a column total independently.
lookupTotals :: Char -> Char -> TableMargins a -> (Maybe a, Maybe a)
lookupTotals r c (TableMargins rows cols rowTotals colTotals) =
    (lookupKey r rows rowTotals, lookupKey c cols colTotals)

-- | Look up a key in two vectors of equal length.
lookupKey :: Char -> Vec n Char -> Vec n a -> Maybe a
lookupKey _ VNil VNil = Nothing
lookupKey key (VCons k ks) (VCons x xs)
    | key == k  = Just x
    | otherwise = lookupKey key ks xs

-- | Count the successors of a length witness.
width :: SN n -> Natural
width SZ = 0
width (SS n) = 1 + width n
