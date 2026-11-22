{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}

module IndexedMatrix (Matrix(Rectangle, Square), TileShape(Rectangular), Tile(Tile), HiddenTile) where

import PackedVecLibrary (Nat, Vec)

-- | Store the dimensions and cells of a rectangle or square.
data Matrix a (rows :: Nat) (columns :: Nat) where
    Rectangle :: foreach (rows :: Nat) (columns :: Nat)
              -> Vec (Vec a columns) rows -> Matrix a rows columns
    Square :: foreach (side :: Nat)
           -> Vec (Vec a side) side -> Matrix a side side

-- | Combine the row and column lengths of a tile.
data TileShape = Rectangular Nat Nat

-- | Store a rectangular payload under one shape index.
data Tile a (shape :: TileShape) where
    Tile :: foreach (rows :: Nat) (columns :: Nat)
         -> Vec (Vec a columns) rows -> Tile a ('Rectangular rows columns)

-- | Hide the public shape of a runtime tile.
type HiddenTile a = exists (shape :: TileShape). Tile a shape
