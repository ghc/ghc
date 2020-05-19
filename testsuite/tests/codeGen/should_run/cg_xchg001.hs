{-# LANGUAGE CPP, MagicHash, BlockArguments, UnboxedTuples #-}

-- Tests for the atomic exchange primop.

-- We initialize a value with 1, and then perform exchanges on it
-- with two different values. At the end all the values should still
-- be present.

module Main ( main ) where

import Data.Bits
import GHC.Int
import GHC.Prim
import GHC.Word
import Control.Monad
import Control.Concurrent
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.List

import GHC.Exts
import GHC.Types

#include "MachDeps.h"

main = do
   alloca $ \ptr_i -> do
      poke ptr_i (1 :: Int)
      w1 <- newEmptyMVar :: IO (MVar Int)
      forkIO $ do
         v <- swapN 50000 2 ptr_i
         putMVar w1 v

      v2 <- swapN 50000 3 ptr_i
      v1 <- takeMVar w1
      print $ sort $ [1,v1,v2]

swapN :: Int -> Int -> Ptr Int -> IO Int
swapN 0 val ptr = return val
swapN n val ptr = do
   val' <- swap ptr val
   swapN (n-1) val' ptr


swap :: Ptr Int -> Int -> IO Int
swap (Ptr ptr) (I# val) = do
   IO $ \s -> case (interlockedExchangeInt# ptr val s) of
            (# s2, old_val #) -> (# s2, I# old_val #)


-- imul2 :: Int -> Int -> (Int,Int,Int)
-- imul2 (I# x) (I# y) = case timesInt2# x y of
--    (# c, h, l #) -> (I# c, I# h, I# l)

-- checkImul2 :: Int -> Int -> IO ()
-- checkImul2 x y = do
--    -- First we compare against Integer result. Note that this test will become
--    -- moot when Integer implementation will use this primitive
--    let
--       w2 = fromIntegral x * (fromIntegral y :: Integer)
--       (c,h,l) = imul2 x y
--       w = case c of
--             0 -> fromIntegral l
--             _ -> int2ToInteger h l

--    unless (w == w2) do
--       putStrLn $ mconcat
--        [ "Failed: "
--        , show x
--        , " * "
--        , show y
--        , "\n    Got: "
--        , show w
--        , "\n    Expected: "
--        , show w2
--        ]

--    -- Now we compare with a generic version using unsigned multiply.
--    -- This reimplements the fallback generic version that the compiler uses when
--    -- the mach-op isn't available so it'd better be correct too.
--    let (c',h',l') = genericIMul2 x y

--    unless ((c,h,l) == (c',h',l')) do
--       putStrLn $ mconcat
--        [ "Failed: "
--        , show x
--        , " * "
--        , show y
--        , "\n    Got: "
--        , show (c,h,l)
--        , "\n    Expected: "
--        , show (c',h',l')
--        ]

-- addWordC :: Word -> Word -> (Word,Word)
-- addWordC (W# x) (W# y) = case addWordC# x y of
--    (# l,c #) -> (W# (int2Word# c), W# l)

-- int2ToInteger :: Int -> Int -> Integer
-- int2ToInteger h l
--   | h < 0     = case addWordC (complement (fromIntegral l)) 1 of
--                   (c,w) -> -1 * word2ToInteger (c + complement (fromIntegral h)) w
--   | otherwise = word2ToInteger (fromIntegral h) (fromIntegral l)
--   where
--    word2ToInteger :: Word -> Word -> Integer
--    word2ToInteger x y = (fromIntegral x) `shiftL` WORD_SIZE_IN_BITS + fromIntegral y

-- timesWord2 :: Word -> Word -> (Int,Int)
-- timesWord2 (W# x) (W# y) = case timesWord2# x y of
--    (# h, l #) -> (I# (word2Int# h), I# (word2Int# l))

-- genericIMul2 :: Int -> Int -> (Int,Int,Int)
-- genericIMul2 x y = (c,h,l)
--    where
--       (p,l) = timesWord2 (fromIntegral x) (fromIntegral y)
--       h = p - f x y - f y x
--       c = if h == carryFill l then 0 else 1
--       f u v = carryFill u .&. v

--       -- Return either 00..00 or FF..FF depending on the carry
--       carryFill :: Int -> Int
--       carryFill x = x `shiftR` (WORD_SIZE_IN_BITS - 1)


-- main = do
--    checkImul2 10 10
--    checkImul2 10 (-10)
--    checkImul2 minBound (-1)
--    checkImul2 maxBound (-1)
--    checkImul2 minBound 0
--    checkImul2 maxBound 0
--    checkImul2 minBound minBound
--    checkImul2 minBound maxBound
--    checkImul2 maxBound maxBound
