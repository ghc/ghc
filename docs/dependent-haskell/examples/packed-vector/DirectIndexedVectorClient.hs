{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}

module DirectIndexedVectorClient (firstCells) where

import Data.IORef (newIORef, readIORef)
import DirectIndexedVector (SomeVec(SomeVec))
import PackedVecLibrary (PackedVec(PackVec), packList, toList)

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
