{-# LANGUAGE CPP, MagicHash, BlockArguments, UnboxedTuples #-}
{-# LANGUAGE CPP, MagicHash, BlockArguments, ScopedTypeVariables #-}

-- Test the atomic exchange primop.

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
import Data.List (sort)

import GHC.Exts
import GHC.Types
import GHC.Ptr

#include "MachDeps.h"

main = do
   alloca $ \(ptr_p :: Ptr (Ptr Int)) -> do
   alloca $ \(ptr_i :: Ptr Int) -> do
   alloca $ \(ptr_j :: Ptr Int) -> do
      poke ptr_i (1 :: Int)
      poke ptr_j (2 :: Int)

      --expected to swap
      res_i <- cas ptr_i 1 3 :: IO Int
      -- expected to fail
      res_j <- cas ptr_j 1 4 :: IO Int

      putStrLn "Returned results:"
      --(1,2)
      print (res_i, res_j)

      i <-peek ptr_i
      j <-peek ptr_j

      putStrLn "Stored results:"
      --(3,2)
      print (i,j)
      -- let x = 0
      -- exchangePtr ptr_p ptr_j

      -- p <- peek ptr_p
      -- poke p 99
      -- -- poke ptr_j 2

      -- p1 <- peek ptr_i
      -- p2 <- peek ptr_j
      -- print (x,p1,p2)


    --   w1 <- newEmptyMVar :: IO (MVar Int)
    --   forkIO $ do
    --      v <- swapN 50000 2 ptr_i
    --      putMVar w1 v

    --   v2 <- swapN 50000 3 ptr_i
    --   v1 <- takeMVar w1
    --   v0 <- peek ptr_i
    --   -- Should be [1,2,3]
    --   print $ sort [v0,v1,v2]

-- swapN :: Int -> Int -> Ptr Int -> IO Int
-- swapN 0 val ptr = return val
-- swapN n val ptr = do
--    val' <- swap ptr val
--    swapN (n-1) val' ptr


cas :: Ptr Int -> Int -> Int -> IO Int
cas (Ptr ptr) (I# expected) (I# desired)= do
   IO $ \s -> case (atomicCasInt# ptr expected desired s) of
            (# s2, old_val #) -> (# s2, I# old_val #)
