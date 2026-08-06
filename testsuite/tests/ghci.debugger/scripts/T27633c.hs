{-# LANGUAGE UnboxedTuples, MagicHash #-}

-- Stopping at a breakpoint in the case continuation of a large unboxed
-- tuple return (generic stg_ctoi_t frame) and resuming must re-enter the
-- frame with the correct tso->ctoi_tuple_spill_words. The continuation
-- breakpoint is activated via :stepout.
-- See Note [GHCi unboxed tuples stack spills] in rts/StgMiscClosures.cmm.

module Main where

import GHC.Exts
import GHC.IO (IO(..))
import T27633c_obj

unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO f) = f

poke :: Int -> IO Int
poke x =
  return (x + 1)          -- line 20: initial breakpoint

scrut :: State# RealWorld -> (# State# RealWorld, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int# #)
scrut s0 = case unIO (poke 41) s0 of (# s1, _ #) -> mkT s1
{-# NOINLINE scrut #-}

main :: IO ()
main = IO (\s0 -> case scrut s0 of
             (# s1, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24 #) ->
               unIO (print (I# (x1 +# x2 +# x3 +# x4 +# x5 +# x6 +# x7 +# x8 +# x9 +# x10 +# x11 +# x12 +# x13 +# x14 +# x15 +# x16 +# x17 +# x18 +# x19 +# x20 +# x21 +# x22 +# x23 +# x24))) s1)
