{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.ST
import Data.Primitive

main :: IO ()
main = do
    let xs :: [Float] = runST $ do
        barr <- mutableByteArrayFromList [1..fromIntegral n::Float]
        peekByteArray n barr
    print xs
  where
    n = 13

mutableByteArrayFromList :: forall s a . (Prim a)
                         => [a]
                         -> ST s (MutableByteArray s)
mutableByteArrayFromList xs = do
    arr <- newByteArray (length xs*sizeOf (undefined :: a))
    loop arr 0 xs
    return arr
  where
    loop :: (Prim a) => MutableByteArray s -> Int -> [a] -> ST s ()
    loop _ _ [] = return ()

    loop arr i (x : xs) = do
        writeByteArray arr i x
        loop arr (i+1) xs

peekByteArray :: (Prim a)
              => Int
              -> MutableByteArray s
              -> ST s [a]
peekByteArray n arr =
    loop 0 arr
  where
    loop :: (Prim a)
         => Int
         -> MutableByteArray s
         -> ST s [a]
    loop i _ | i >= n = return []

    loop i arr = do
        x  <- readByteArray arr i
        xs <- loop (i+1) arr
        return (x : xs)
