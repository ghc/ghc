{-# language MagicHash, UnboxedTuples #-}
module Main where

import GHC.Stats
import GHC.Exts
import GHC.IO
import System.Mem

data MBA = MBA (MutableByteArray# RealWorld)

main :: IO ()
main = do
  MBA mba <- IO $ \s0 -> case newPinnedByteArray# 4096# s0 of
    (# s1, mba #) -> (# s1, MBA mba #)

  IO $ \s0 -> case writeIntArray# mba 0# 1# s0 of
    s1 -> (# s1, () #)

  performMajorGC

  stats <- getRTSStats

  if max_pinned_objects_bytes stats > 0
    then putStrLn "OK"
    else putStrLn "expected max_pinned_objects_bytes to be >0"

  if gcdetails_pinned_objects_bytes (gc stats) > 0
    then putStrLn "OK"
    else putStrLn "expected gcdetails_pinned_objects_bytes to be >0"

  i <- IO $ \s0 -> case readIntArray# mba 0# s0 of
    (# s1, val #) -> (# s1, I# val #)

  print i
  putStrLn "OK"
