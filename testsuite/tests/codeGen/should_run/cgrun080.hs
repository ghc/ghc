{-# LANGUAGE CPP, MagicHash, BlockArguments, UnboxedTuples #-}

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
      v0 <- peek ptr_i
      -- Should be [1,2,3]
      print $ sort [v0,v1,v2]

swapN :: Int -> Int -> Ptr Int -> IO Int
swapN 0 val ptr = return val
swapN n val ptr = do
   val' <- swap ptr val
   swapN (n-1) val' ptr


swap :: Ptr Int -> Int -> IO Int
swap (Ptr ptr) (I# val) = do
   IO $ \s -> case (interlockedExchangeInt# ptr val s) of
            (# s2, old_val #) -> (# s2, I# old_val #)

