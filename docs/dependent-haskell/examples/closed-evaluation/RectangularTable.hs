{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module RectangularTable (Table(..), totals, sample, selectedTotals) where

import IndexedVec (Vec(..))
import Numeric.Natural (Natural)
import WitnessedBatch (TableMargins(..), lookupTotals)

-- | Each row has one cell for each column label.
data Table a where
    Table :: Vec rows Char -> Vec cols Char -> Vec rows (Vec cols a) -> Table a

-- | Sum each row and column with its original labels.
totals :: Table Natural -> TableMargins Natural
totals (Table rows cols cells) =
    TableMargins rows cols (mapVec (foldVec (+) 0) cells)
                           (foldVec addRows (mapVec (const 0) cols) cells)
  where
    addRows :: Vec n Natural -> Vec n Natural -> Vec n Natural
    addRows VNil VNil = VNil
    addRows (VCons x xs) (VCons y ys) = VCons (x + y) (addRows xs ys)

    mapVec :: (a -> b) -> Vec n a -> Vec n b
    mapVec _ VNil = VNil
    mapVec f (VCons x xs) = VCons (f x) (mapVec f xs)

    foldVec :: (a -> b -> b) -> b -> Vec n a -> b
    foldVec _ z VNil = z
    foldVec f z (VCons x xs) = f x (foldVec f z xs)

-- | Use the argument as the first entry of the second row.
sample :: Natural -> Table Natural
sample n = Table (VCons 'r' (VCons 's' VNil))
                 (VCons 'x' (VCons 'y' (VCons 'z' VNil)))
                 (VCons (VCons 1 (VCons 2 (VCons 3 VNil)))
                 (VCons (VCons n (VCons 5 (VCons 6 VNil))) VNil))

-- | Observe row s and column z.
selectedTotals :: Table Natural -> (Maybe Natural, Maybe Natural)
selectedTotals = lookupTotals 's' 'z' . totals
