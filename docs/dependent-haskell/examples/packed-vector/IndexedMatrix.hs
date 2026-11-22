{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}

module IndexedMatrix (Matrix(Rectangle, Square)) where

import PackedVecLibrary (Nat, Vec)

-- | Store the dimensions and cells of a rectangle or square.
data Matrix a (rows :: Nat) (columns :: Nat) where
    Rectangle :: foreach (rows :: Nat) (columns :: Nat)
              -> Vec (Vec a columns) rows -> Matrix a rows columns
    Square :: foreach (side :: Nat)
           -> Vec (Vec a side) side -> Matrix a side side
