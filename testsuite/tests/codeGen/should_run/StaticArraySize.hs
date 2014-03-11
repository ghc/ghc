{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}

-- Test allocation of statically sized arrays. There's an optimization
-- that targets these and we want to make sure that the code generated
-- in the optimized case is correct.
--
-- The tests proceeds by allocating a bunch of arrays of different
-- sizes and reading elements from them, to try to provoke GC crashes,
-- which would be a symptom of the optimization not generating correct
-- code.
module Main where

import GHC.Exts
import GHC.IO
import Prelude hiding (read)

main :: IO ()
main = do
    loop 1000
    putStrLn "success"
  where
    loop :: Int -> IO ()
    loop 0 = return ()
    loop i = do
        -- Sizes have been picked to match the triggering of the
        -- optimization and to match boundary conditions. Sizes are
        -- given explicitly as to not rely on other optimizations to
        -- make the static size known to the compiler.
        marr0 <- newArray 0
        marr1 <- newArray 1
        marr2 <- newArray 2
        marr3 <- newArray 3
        marr4 <- newArray 4
        marr5 <- newArray 5
        marr6 <- newArray 6
        marr7 <- newArray 7
        marr8 <- newArray 8
        marr9 <- newArray 9
        marr10 <- newArray 10
        marr11 <- newArray 11
        marr12 <- newArray 12
        marr13 <- newArray 13
        marr14 <- newArray 14
        marr15 <- newArray 15
        marr16 <- newArray 16
        marr17 <- newArray 17
        let marrs = [marr0, marr1, marr2, marr3, marr4, marr5, marr6, marr7,
                     marr8, marr9, marr10, marr11, marr12, marr13, marr14,
                     marr15, marr16, marr17]
        print `fmap` sumManyArrays marrs
        loop (i-1)

sumManyArrays :: [MArray] -> IO Int
sumManyArrays = go 0
  where
    go !acc [] = return acc
    go acc (marr:marrs) = do
        n <- sumArray marr
        go (acc+n) marrs

sumArray :: MArray -> IO Int
sumArray marr = go 0 0
  where
    go :: Int -> Int -> IO Int
    go !acc i
      | i < len = do
          k <- read marr i
          go (acc + k) (i+1)
      | otherwise = return acc
    len = lengthM marr

data MArray = MArray { unMArray :: !(MutableArray# RealWorld Int) }

newArray :: Int -> IO MArray
newArray (I# sz#) = IO $ \s -> case newArray# sz# 1 s of
    (# s', marr #) -> (# s', MArray marr #)
{-# INLINE newArray #-}  -- to make sure optimization triggers

lengthM :: MArray -> Int
lengthM marr = I# (sizeofMutableArray# (unMArray marr))

read :: MArray -> Int -> IO Int
read marr i@(I# i#)
  | i < 0 || i >= len =
      error $ "bounds error, offset " ++ show i ++ ", length " ++ show len
  | otherwise = IO $ \ s -> readArray# (unMArray marr) i# s
  where len = lengthM marr
