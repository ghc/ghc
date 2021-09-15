{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NegativeLiterals #-}

module T20374 where

import GHC.Exts

foo0 (# #) = timesInt2# 100005# 6832#
foo1 (# #) = timesInt2# -100005# 6832#
foo2 (# #) = timesInt2# -100005# -6832#
foo3 (# #) = timesInt2# 100005# -6832#

foo4 (# #) = timesInt2# 100005# 0#
foo5 (# #) = timesInt2# 0# 6832#

foo6 other = case timesInt2# 1# other of
              (# 0#, _ , l #) -> l
              _               -> error "Unexpected foo6 result"
foo7 other = case timesInt2# other 1# of
              (# 0#, _ , l #) -> l
              _               -> error "Unexpected foo7 result"

foo8 (# #) = timesInt2# 1# 128#

foo9 (# #) = timesInt2# 1# -128#

foo10 (# #) = let (I# m) = maxBound
             in case timesInt2# m m of
                  (# 1#, _ , 1# #) -> 1#
                  _                -> error "Unexpected foo10 result"
