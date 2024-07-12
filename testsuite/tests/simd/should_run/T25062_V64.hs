{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import GHC.Exts

main :: IO ()
main =
  case foo ( \ x y -> plusDoubleX8# x y ) of
    v -> case unpackDoubleX8# v of
      (# d1, d2, d3, d4, d5, d6, d7, d8 #) ->
        print [ D# d1, D# d2, D# d3, D# d4, D# d5, D# d6, D# d7, D# d8s ]

{-# NOINLINE foo #-}
foo :: ( DoubleX8# -> DoubleX8# -> DoubleX8# ) -> DoubleX8#
foo f =
  case packDoubleX8# (# 10.01##, 20.02##, 30.03##, 40.04##, 50.05##, 60.06##, 70.07##, 80.08## #) of
    !x -> f x x
