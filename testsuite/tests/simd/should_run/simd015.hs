{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ExtendedLiterals #-}

-- bitwise instructions on floating point vectors

import GHC.Exts
import GHC.Int
import GHC.Prim


main :: IO ()
main = do
    putStrLn "DoubleX2#"
    let
      !d1 = packDoubleX2# (# 1.1##, 2.2## #)
      !d2 = packDoubleX2# (# 0.0##, 2.2## #)
      !d3 = packDoubleX2# (# -5.5##, 32.0## #)
      !d4 = packDoubleX2# (# 5.5##, 128.0## #)

    case unpackDoubleX2# (andDoubleX2# d1 d2) of
        (# a, b #) -> print (D# a, D# b)
    case unpackDoubleX2# (andDoubleX2# d3 d4) of
        (# c, d #) -> print (D# c, D# d)
    case unpackDoubleX2# (orDoubleX2# d1 d2) of
        (# a, b #) -> print (D# a, D# b)
    case unpackDoubleX2# (orDoubleX2# d3 d4) of
        (# c, d #) -> print (D# c, D# d)
    case unpackDoubleX2# (xorDoubleX2# d1 d2) of
        (# a, b #) -> print (D# a, D# b)
    case unpackDoubleX2# (xorDoubleX2# d3 d4) of
        (# c, d #) -> print (D# c, D# d)

    putStrLn ""
    putStrLn "FloatX4#"
    let
      !f1 = packFloatX4# (# 1.1#, 2.2#, -5.5#, 128.0# #)
      !f2 = packFloatX4# (# 0.0#, 2.2#, 5.5#, 32.0# #)

    case unpackFloatX4# (andFloatX4# f1 f2) of
        (# a, b, c, d #) -> print (F# a, F# b, F# c, F# d)
    case unpackFloatX4# (orFloatX4# f1 f2) of
        (# a, b, c, d #) -> print (F# a, F# b, F# c, F# d)
    case unpackFloatX4# (xorFloatX4# f1 f2) of
        (# a, b, c, d #) -> print (F# a, F# b, F# c, F# d)
