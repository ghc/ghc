{-# LANGUAGE UnboxedTuples, MagicHash #-}
module T23142 where

import GHC.IO
import GHC.Exts

main :: IO ()
main = IO (\s -> case newArray# 10# (2 :: Int) s of
                    (# s', a #) -> case unsafeFreezeArray# a s' of
                       (# s'', _ #) -> (# s'', () #))
        >>
       IO (\s -> case newSmallArray# 10# (2 :: Int) s of
                    (# s', a #) -> case unsafeFreezeSmallArray# a s' of
                       (# s'', _ #) -> (# s'', () #))
        >>
       IO (atomically# (\s -> catchSTM# (\s -> (# s, () #)) (\_ s -> (# s, () #)) s))
        >>
       IO (atomically# (\s -> catchRetry# (\s -> (# s, () #)) (\s -> (# s, () #)) s))
