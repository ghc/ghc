{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- simple test for vector min/max instructions

import GHC.Exts
import GHC.Prim


main :: IO ()
main = do

    -- FloatX4#
    let
      !f1 = packFloatX4# (# 1.1#, 20.1#, 3.1#, 40.1# #)
      !f2 = packFloatX4# (# 10.2#, 2.2#, 30.2#, 4.2# #)

    case unpackFloatX4# (minFloatX4# f1 f2) of
        (# a, b, c, d #) -> print (F# a, F# b, F# c, F# d)
    case unpackFloatX4# (maxFloatX4# f1 f2) of
        (# a, b, c, d #) -> print (F# a, F# b, F# c, F# d)

    -- DoubleX2#
    let
      !d1 = packDoubleX2# (# 1.1##, 20.1## #)
      !d2 = packDoubleX2# (# 10.2##, 2.2## #)

    case unpackDoubleX2# (minDoubleX2# d1 d2) of
        (# a, b #) -> print (D# a, D# b)
    case unpackDoubleX2# (maxDoubleX2# d1 d2) of
        (# a, b #) -> print (D# a, D# b)
