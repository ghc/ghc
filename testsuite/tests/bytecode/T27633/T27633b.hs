{-# LANGUAGE UnboxedTuples, MagicHash #-}
{-# OPTIONS_GHC -fbyte-code #-}

-- Regression test for #27633 (bug 2): STM retry unwinding
-- (findRetryFrameHelper/findAtomicallyFrameHelper) must restore
-- tso->ctoi_tuple_spill_words when it discards a generic stg_ctoi_t frame.
-- See Note [GHCi unboxed tuples stack spills] in rts/StgMiscClosures.cmm.

module Main where

import GHC.Conc (STM(..), atomically, orElse)
import GHC.Exts
import GHC.IO (IO(..))
import Obj

unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO f) = f

-- Inside the transaction: case on a 16-field tuple (different spill count
-- than the outer 24-field one), then retry. The generic stg_ctoi_t frame
-- pushed for this case is discarded by findRetryFrameHelper.
innerScrut :: State# RealWorld -> (# State# RealWorld, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int# #)
innerScrut s0 = case retry# s0 of (# s1, () #) -> (# s1, 1#, 2#, 3#, 4#, 5#, 6#, 7#, 8#, 9#, 10#, 11#, 12#, 13#, 14#, 15#, 16# #)
{-# NOINLINE innerScrut #-}

stmBody :: STM ()
stmBody = STM (\s0 -> case innerScrut s0 of
                (# s1, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16 #) -> (# s1, () #))

outerScrut :: State# RealWorld -> (# State# RealWorld, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int# #)
outerScrut s0 =
  case unIO (atomically (stmBody `orElse` return ())) s0 of
    (# s1, _ #) -> mkT s1
{-# NOINLINE outerScrut #-}

main :: IO ()
main = IO (\s0 -> case outerScrut s0 of
             (# s1, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24 #) ->
               unIO (print (I# (x1 +# x2 +# x3 +# x4 +# x5 +# x6 +# x7 +# x8 +# x9 +# x10 +# x11 +# x12 +# x13 +# x14 +# x15 +# x16 +# x17 +# x18 +# x19 +# x20 +# x21 +# x22 +# x23 +# x24))) s1)
