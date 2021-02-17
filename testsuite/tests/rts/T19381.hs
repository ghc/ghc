{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import GHC.Exts
import GHC.IO
import GHC.Stats
import System.Mem
import Control.Monad

data BA = BA ByteArray#

mblockSize = 2 ^ 20

main = do
 -- Allocate 1000 byte arrays, to get a high watermark before only keeping
 -- 100 of them.
 ba <- take 100 <$> replicateM 1000 mkBA
 let !n = (length ba)
 -- Each major GC should free some amount of memory, 100 is just a large
 -- number
 replicateM 100 performMajorGC
 s <- getRTSStats
 let mblocks = (gcdetails_mem_in_use_bytes (gc s) `div` mblockSize)
     live = (gcdetails_live_bytes (gc s) `div` mblockSize)
 if fromIntegral mblocks < (2.2 * fromIntegral live)
  then return ()
  else error ("Additional memory is retained: "
              ++ show live ++ "/"
              ++ show mblocks)
 -- Here to retain the ba
 (length ba) `seq` return ()

mkBA =
    let (I# siz) = 2^19  -- ~0.1MB
    in IO $ \s0 ->
      case newByteArray# siz s0 of
        (# s1, mba #) -> case unsafeFreezeByteArray# mba s1 of
           (# s2, ba #) -> (# s2, BA ba #)
