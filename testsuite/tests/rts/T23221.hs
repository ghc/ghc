{-# LANGUAGE MagicHash, UnboxedTuples, NumericUnderscores #-}

module Main where

import GHC.Exts
import GHC.IO
import System.Mem
import System.Environment
import Debug.Trace
import Control.Monad
import GHC.Stats
import Data.Word
import GHC.Stack (HasCallStack)

-- This test is for checking the memory return behaviour of blocks which will be
-- copied and blocks which are not copied (#23221)
main :: IO ()
main = do
  [sn] <- getArgs
  let n = read sn
  -- By checking that lower bound of unpinned is the upper bound of pinned then we
  -- check that unpinned has lower memory baseline than pinned.
  loop newByteArray 2 3 n
  loop newPinnedByteArray 1 2 n


-- The upper_bound is the upper bound on how much total memory should be live at the end
-- of the test as a factor of the expected live bytes.
loop :: HasCallStack => (Int -> IO a) -> Double -> Double -> Int -> IO ()
loop f lower_bound upper_bound n = do
  ba <- mapM (\_ -> f 128) [0..n]
  traceMarkerIO "Allocated_all"
  performGC
  let !ba' = take (n `div` 4) ba
  evaluate (length ba')
  traceMarkerIO "GC_4"
  performGC
  evaluate (length (reverse ba'))
  replicateM_ 20 performGC
  total_mem <- checkStats lower_bound upper_bound (n `div` 4)
  evaluate (length (reverse ba'))
  return total_mem

checkStats :: HasCallStack => Double -> Double -> Int -> IO ()
checkStats lower_bound upper_bound n = do
  stats <- getRTSStats
  let expected_live_memory = fromIntegral n -- How many objects
                             * (3     -- One list cons
                                + 2   -- One BA constructor
                                + 18) -- ByteArray# object (size 16 + 2 overhead)
                                  -- size of each object
                             * 8            -- word size
  let bytes_used = gcdetails_mem_in_use_bytes (gc stats)
      mblocks = bytes_used  `div` (2 ^ 20)
  when (truncate (expected_live_memory * upper_bound) < bytes_used) $
    error ("Upper Memory bound failed: " ++ show (truncate expected_live_memory, upper_bound, bytes_used))
  when (truncate (expected_live_memory * lower_bound) >= bytes_used) $
    error ("Lower Memory bound failed: " ++ show (truncate expected_live_memory, lower_bound, bytes_used))

data BA = BA ByteArray#

newByteArray :: Int -> IO BA
newByteArray (I# sz#) = IO $ \s -> case newByteArray# sz# s of
    (# s', k #) -> case unsafeFreezeByteArray# k s' of
                    (# s'', ba# #) -> (# s'', BA ba# #)

newPinnedByteArray :: Int -> IO BA
newPinnedByteArray (I# sz#) = IO $ \s -> case newPinnedByteArray# sz# s of
    (# s', k #) -> case unsafeFreezeByteArray# k s' of
                    (# s'', ba# #) -> (# s'', BA ba# #)


