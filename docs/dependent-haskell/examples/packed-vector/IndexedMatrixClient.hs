{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}

module IndexedMatrixClient (observeDimensions, observeTile) where

import Data.IORef (newIORef, readIORef)
import Data.Type.Equality ((:~:)(Refl))
import IndexedMatrix (HiddenTile, Matrix(Rectangle, Square), Tile(Tile))
import PackedVecLibrary (Nat(S, Z), natToInt, replicateNative, toList)

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

-- | Open two runtime tiles and read columns and cells only for nonempty rows.
observeTile :: IO [((Int, Maybe Int), [[Bool]])]
observeTile = do
    rows <- newIORef (S Z) >>= readIORef
    columns <- newIORef (S (S Z)) >>= readIORef
    zero <- newIORef Z >>= readIORef
    unused <- newIORef (error "forced empty tile columns") >>= readIORef
    pure [readTile (fillTile rows columns True), readTile (emptyTile zero unused)]
  where
    fillTile :: Nat -> Nat -> a -> HiddenTile a
    fillTile rows columns cell =
        Tile rows columns (replicateNative rows (replicateNative columns cell))

    emptyTile :: Nat -> Nat -> HiddenTile Bool
    emptyTile rows columns = Tile rows columns (error "forced empty tile cells")

    readTile :: Tile a shape -> ((Int, Maybe Int), [[a]])
    readTile (Tile Z _ _) = ((0, Nothing), [])
    readTile (Tile rows columns payload) =
        ((natToInt rows, Just (natToInt columns)), map toList (toList payload))
