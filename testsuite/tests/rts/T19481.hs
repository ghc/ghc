{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
module Main where

import GHC.Exts
import GHC.IO
import GHC.Stats
import System.Mem
import Control.Monad

data BA = BA ByteArray#

-- TODO: This shouldn't be hardcoded but MBLOCK_SIZE isn't exported by
-- any RTS header I could find.
mblockSize = 2 ^ 20

main = do
 -- Increasing this number increases the amount of fragmentation (but not
 -- linearly)
 ba <- replicateM 500 one
 replicateM 100 performMajorGC
 s <- getRTSStats
 let mblocks = (gcdetails_mem_in_use_bytes (gc s) `div` mblockSize)
 if mblocks < 15
  then return ()
  else error ("Heap is fragmented: " ++ show mblocks)
 return ()

one = do
  ba <- mkBlock
  bs <- mapM isP ba
  return ()


isP (BA ba) = IO $ \s0 -> (# s0, isTrue# (isByteArrayPinned# ba) #)

mkN 0 = return []
mkN k = (:) <$> mkBA <*> mkN (k - 1)

-- Mixture of pinned and unpinned allocation so that allocatePinned takes
-- some pinned blocks from the nursery.
mkBlock = (++) <$> replicateM 100 mkBAP <*> replicateM 10000 mkBA

mkBAP =
    IO $ \s0 ->
      -- 1024 is below large object threshold but fills up a block quickly
      case newPinnedByteArray# 1024# s0 of
        (# s1, mba #) -> case unsafeFreezeByteArray# mba s1 of
           (# s2, ba #) -> (# s2, BA ba #)

mkBA =
    IO $ \s0 ->
      -- 1024 is below large object threshold but fills up a block quickly
      case newByteArray# 1024# s0 of
        (# s1, mba #) -> case unsafeFreezeByteArray# mba s1 of
           (# s2, ba #) -> (# s2, BA ba #)
