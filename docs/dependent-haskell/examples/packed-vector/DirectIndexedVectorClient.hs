{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}

module DirectIndexedVectorClient (firstCells, firstCellsByLength) where

import Data.IORef (newIORef, readIORef)
import DirectIndexedVector (SomeVec(SomeVec))
import PackedVecLibrary (Nat(Z, S), PackedVec(PackVec), Vec(VCons), packList, toList)

-- | Read the first cell of two vectors at their original runtime lengths.
firstCells :: IO ([Bool], [Char])
firstCells = do
    bits <- newIORef (True : error "forced indexed Boolean vector tail") >>= readIORef
    chars <- newIORef ('q' : error "forced indexed character vector tail") >>= readIORef
    pure (firstCell (packList bits), firstCell (packList chars))
  where
    firstCell :: PackedVec a -> [a]
    firstCell (PackVec n vector) =
        let indexed = SomeVec n vector
        in case rebuild indexed of
            SomeVec _ xs -> take 1 (toList xs)

    rebuild :: SomeVec a n -> SomeVec a n
    rebuild (SomeVec n vector) = SomeVec n vector

-- | Read a cell only when the stored length is a successor.
firstCellsByLength :: (Maybe Bool, Maybe Bool)
firstCellsByLength =
    ( firstDirect (SomeVec Z (error "forced empty vector"))
    , firstDirect (SomeVec (S Z) (VCons True (error "forced nonempty vector tail")))
    )
  where
    firstDirect :: SomeVec a n -> Maybe a
    firstDirect (SomeVec Z _) = Nothing
    firstDirect (SomeVec (S _) (VCons x _)) = Just x
