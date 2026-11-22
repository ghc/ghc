{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}

module IndexedMatrixClient (observeDimensions) where

import Data.IORef (newIORef, readIORef)
import Data.Type.Equality ((:~:)(Refl))
import IndexedMatrix (Matrix(Rectangle, Square))
import PackedVecLibrary (Nat(S, Z), natToInt)

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
