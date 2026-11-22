{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}

module IndexedMatrixClient (observeDimensions, observeTile) where

import Data.IORef (newIORef, readIORef)
import Data.Type.Equality ((:~:)(Refl))
import IndexedMatrix (Matrix(Rectangle, Square), Tile(Tile), TileShape(Rectangular))
import PackedVecLibrary (Nat(S, Z), Vec(VCons, VNil), natToInt, toList)

-- | Read the rectangle's rows and prove the square's dimensions equal.
observeDimensions :: IO (Int, Bool)
observeDimensions = do
    unused <- newIORef (error "forced unused dimension" :: Nat) >>= readIORef
    let rectangle = Rectangle (S Z) unused (error "forced rectangle cells")
        square = Square unused (error "forced square cells")
        squareProof = case equalDimensions square of
            Just Refl -> True
            Nothing -> False
    pure (countRows rectangle, squareProof)
  where
    countRows :: Matrix a rows columns -> Int
    countRows (Rectangle n _ _) = natToInt n
    countRows (Square n _) = natToInt n

    equalDimensions :: Matrix a rows columns -> Maybe (rows :~: columns)
    equalDimensions (Square _ _) = Just Refl
    equalDimensions (Rectangle _ _ _) = Nothing

-- | Read the dimensions and the cells of a tile at its public shape.
observeTile :: ((Int, Int), [[Bool]])
observeTile = (dimensions, map toList (toList cells))
  where
    (dimensions, cells) =
        readTile (Tile (S Z) (S (S Z)) (VCons (VCons True (VCons False VNil)) VNil))

    readTile :: Tile a ('Rectangular rows columns) -> ((Int, Int), Vec (Vec a columns) rows)
    readTile (Tile rows columns payload) = ((natToInt rows, natToInt columns), payload)
