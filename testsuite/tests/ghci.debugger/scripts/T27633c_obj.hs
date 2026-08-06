{-# OPTIONS_GHC -fobject-code #-}
{-# LANGUAGE UnboxedTuples, MagicHash #-}
module T27633c_obj where

import GHC.Exts

-- Compiled to object code; returns a large unboxed tuple, so the return
-- into an interpreted caller goes through the native stg_ctoi_t entry code,
-- which reads tso->ctoi_tuple_spill_words.
mkT :: State# RealWorld -> (# State# RealWorld, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int# #)
mkT s = (# s, 1#, 2#, 3#, 4#, 5#, 6#, 7#, 8#, 9#, 10#, 11#, 12#, 13#, 14#, 15#, 16#, 17#, 18#, 19#, 20#, 21#, 22#, 23#, 24# #)
{-# NOINLINE mkT #-}
