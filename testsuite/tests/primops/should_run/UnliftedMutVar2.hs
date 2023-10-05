{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}

module Main where

import GHC.Exts
import GHC.IO

type U :: UnliftedType
data U = U Int#

main :: IO ()
main = do
  res <- IO \ s0 ->
    case newMutVar# (U 0#) s0 of
      (# s1, var #) ->
        sum_squares var s1
  print res

sum_squares :: MutVar# s U -> State# s -> (# State# s, Int #)
sum_squares var s = case go s of { (# s', i #) -> (# s', I# i #) }
  where
    go s0 = case readMutVar# var s0 of
      (# s1, U val #)
        | I# val >= 1000000
        -> (# s1, val #)
        | otherwise
        -> let nxt = val +# 1#
           in case writeMutVar# var (U (val +# nxt *# nxt)) s1 of
               s2 -> go s2
