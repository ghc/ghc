{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- tests for vector FMA instructions

import GHC.Exts
import GHC.Prim


main :: IO ()
main = do

    -- FloatX4#
    let
      !f1 = packFloatX4# (# 1.1#, 2.2#, 3.3#, 4.4# #)
      !f2 = packFloatX4# (# 10.1#, 20.2#, 30.3#, 40.4# #)
      !f3 = packFloatX4# (# 1000.0#, 2000.0#, 3000.0#, 4000.0# #)

    case unpackFloatX4# (fmaddFloatX4# f1 f2 f3) of
        (# a, b, c, d #) -> print (F# a, F# b, F# c, F# d)
    case unpackFloatX4# (fmsubFloatX4# f1 f2 f3) of
        (# a, b, c, d #) -> print (F# a, F# b, F# c, F# d)
    case unpackFloatX4# (fnmaddFloatX4# f1 f2 f3) of
        (# a, b, c, d #) -> print (F# a, F# b, F# c, F# d)
    case unpackFloatX4# (fnmsubFloatX4# f1 f2 f3) of
        (# a, b, c, d #) -> print (F# a, F# b, F# c, F# d)

    -- DoubleX2#
    let
      !d1 = packDoubleX2# (# 1.1##, 2.2## #)
      !d2 = packDoubleX2# (# 10.1##, 20.2## #)
      !d3 = packDoubleX2# (# 1000.0##, 2000.0## #)

    case unpackDoubleX2# (fmaddDoubleX2# d1 d2 d3) of
        (# a, b #) -> print (D# a, D# b)
    case unpackDoubleX2# (fmsubDoubleX2# d1 d2 d3) of
        (# a, b #) -> print (D# a, D# b)
    case unpackDoubleX2# (fnmaddDoubleX2# d1 d2 d3) of
        (# a, b #) -> print (D# a, D# b)
    case unpackDoubleX2# (fnmsubDoubleX2# d1 d2 d3) of
        (# a, b #) -> print (D# a, D# b)
