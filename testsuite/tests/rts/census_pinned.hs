{-# LANGUAGE MagicHash, UnboxedTuples #-}
-- Many small pinned byte arrays share one block. The heap census used to
-- count only the first array of such a block (#28000). The Makefile runs
-- this program with +RTS -hT -i0 and prints the ARR_WORDS line of the
-- census taken at the major GC.
module Main (main) where

import GHC.Exts
import GHC.IO (IO(..))
import System.Mem (performMajorGC)

data PinnedBytes = PinnedBytes (MutableByteArray# RealWorld)

newPinned :: Int -> IO PinnedBytes
newPinned (I# n) = IO (\s -> case newPinnedByteArray# n s of
                                (# s', mba #) -> (# s', PinnedBytes mba #))

main :: IO ()
main = do
  -- 10000 arrays of 24 bytes: 40 bytes each with the two-word header on a
  -- 64-bit machine, 32 bytes on a 32-bit one. Nothing else has allocated a
  -- byte array when the major GC runs (on Windows, one array of 24 bytes),
  -- so the first ARR_WORDS line of the profile is exactly these arrays,
  -- including those in the pinned block that the capability is still
  -- filling, and the expected output depends on the word size and the
  -- platform. One major GC is enough: the full pinned blocks are moved to
  -- the large-object list at the start of the GC and the census runs at
  -- its end.
  arrs <- mapM (const (newPinned 24)) [1 .. 10000 :: Int]
  performMajorGC
  print (length arrs)
