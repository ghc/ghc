{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import GHC.Exts

main :: IO ()
main =
  case foo ( \ x y -> plusDoubleX2# x y ) of
    v -> case unpackDoubleX2# v of
      (# d1, d2 #) ->
        print [ D# d1, D# d2 ]

{-# NOINLINE foo #-}
foo :: ( DoubleX2# -> DoubleX2# -> DoubleX2# ) -> DoubleX2#
foo f =
  case packDoubleX2# (# 10.01##, 20.02## #) of
    !x -> f x x
