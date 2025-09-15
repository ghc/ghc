{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE CPP, MagicHash, BlockArguments, UnboxedTuples, ScopedTypeVariables #-}

-- Test the atomic exchange primop.

-- We initialize a value with 1, and then perform exchanges on it
-- with two different values. At the end all the values should still
-- be present.

module Main ( main ) where

import Data.Bits
import GHC.Int
import GHC.Exts
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
   alloca $ \(ptr_p :: Ptr (Ptr Word)) -> do
   alloca $ \(ptr_i :: Ptr Word) -> do
   alloca $ \(ptr_j :: Ptr Word) -> do
      poke ptr_i (1 :: Word)
      poke ptr_j (2 :: Word)

      --expected to swap
      res_i <- cas ptr_i 1 3 :: IO Word
      -- expected to fail
      res_j <- cas ptr_j 1 4 :: IO Word

      putStrLn "Returned results:"
      --(1,2)
      print (res_i, res_j)

      i <-peek ptr_i
      j <-peek ptr_j

      putStrLn "Stored results:"
      --(3,2)
      print (i,j)

cas :: Ptr Word -> Word -> Word -> IO Word
cas (Ptr ptr) (W# expected) (W# desired)= do
   IO $ \s -> case (atomicCasWordAddr# ptr expected desired s) of
            (# s2, old_val #) -> (# s2, W# old_val #)
