{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import GHC.Exts

main :: IO ()
main =
  case foo ( \ x y -> plusDoubleX4# x y ) of
    v -> case unpackDoubleX4# v of
      (# d1, d2, d3, d4 #) ->
        print [ D# d1, D# d2, D# d3, D# d4 ]

{-# NOINLINE foo #-}
foo :: ( DoubleX4# -> DoubleX4# -> DoubleX4# ) -> DoubleX4#
foo f =
  case packDoubleX4# (# 10.01##, 20.02##, 30.03##, 40.04## #) of
    !x -> f x x
