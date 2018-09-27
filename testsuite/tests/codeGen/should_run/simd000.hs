{-# OPTIONS_GHC -mavx #-}
{-# OPTIONS_GHC -msse4 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- !!! test broadcasting, packing and unpacking for vector types

import GHC.Exts

main :: IO ()
main = do
    -- FloatX4#
    case unpackFloatX4# (broadcastFloatX4# 1.5#) of
        (# a, b, c, d #) -> print (F# a, F# b, F# c, F# d)
    case unpackFloatX4# (packFloatX4# (# 4.5#,7.8#, 2.3#, 6.5# #)) of
        (# a, b, c, d #) -> print (F# a, F# b, F# c, F# d)

    -- DoubleX2#
    case unpackDoubleX2# (broadcastDoubleX2# 6.5##) of
        (# a, b #) -> print (D# a, D# b)
    case unpackDoubleX2# (packDoubleX2# (# 8.9##,7.2## #)) of
        (# a, b #) -> print (D# a, D# b)
