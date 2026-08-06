{-# LANGUAGE UnboxedTuples, MagicHash #-}
{-# OPTIONS_GHC -fbyte-code #-}

-- Regression test for #27633 (bug 1): a failed control0# capture must not
-- clobber tso->ctoi_tuple_spill_words while a generic stg_ctoi_t frame is
-- still live on the stack.
-- See Note [GHCi unboxed tuples stack spills] in rts/StgMiscClosures.cmm.

module Main where

import Control.Exception
import GHC.Exts
import GHC.IO (IO(..))
import Obj

unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO f) = f

-- control0# with a tag that has no matching prompt anywhere on the stack;
-- the resulting exception is caught right here, above the stg_ctoi_t frame,
-- so the unwind stops before reaching the frame.
failing :: IO ()
failing = IO (\s0 -> case newPromptTag# s0 of
                (# s1, tag #) -> control0# tag (\_ s -> (# s, () #)) s1)
          `catch` \(SomeException _) -> return ()

scrut :: State# RealWorld -> (# State# RealWorld, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int# #)
scrut s0 = case unIO failing s0 of (# s1, _ #) -> mkT s1
{-# NOINLINE scrut #-}

main :: IO ()
main = IO (\s0 -> case scrut s0 of
             (# s1, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24 #) ->
               unIO (print (I# (x1 +# x2 +# x3 +# x4 +# x5 +# x6 +# x7 +# x8 +# x9 +# x10 +# x11 +# x12 +# x13 +# x14 +# x15 +# x16 +# x17 +# x18 +# x19 +# x20 +# x21 +# x22 +# x23 +# x24))) s1)
