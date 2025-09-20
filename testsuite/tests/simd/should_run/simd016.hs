{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ExtendedLiterals #-}

-- bitwise instructions on signed integer vectors

import GHC.Exts
import GHC.Int
import GHC.Prim


main :: IO ()
main = do
    putStrLn "Int64X2#"
    let
      !i64_1 = packInt64X2# (# 1#Int64, 2#Int64 #)
      !i64_2 = packInt64X2# (# 0#Int64, 2#Int64 #)
      !i64_3 = packInt64X2# (# -5#Int64, 128#Int64 #)
      !i64_4 = packInt64X2# (# 5#Int64, 32#Int64 #)

    case unpackInt64X2# (andInt64X2# i64_1 i64_2) of
        (# a, b #) -> print (I64# a, I64# b)
    case unpackInt64X2# (andInt64X2# i64_3 i64_4) of
        (# c, d #) -> print (I64# c, I64# d)
    case unpackInt64X2# (orInt64X2# i64_1 i64_2) of
        (# a, b #) -> print (I64# a, I64# b)
    case unpackInt64X2# (orInt64X2# i64_3 i64_4) of
        (# c, d #) -> print (I64# c, I64# d)
    case unpackInt64X2# (xorInt64X2# i64_1 i64_2) of
        (# a, b #) -> print (I64# a, I64# b)
    case unpackInt64X2# (xorInt64X2# i64_3 i64_4) of
        (# c, d #) -> print (I64# c, I64# d)

    putStrLn ""
    putStrLn "Int32X4#"
    let
      !i32_1 = packInt32X4# (# 1#Int32, 2#Int32, -5#Int32, 128#Int32 #)
      !i32_2 = packInt32X4# (# 0#Int32, 2#Int32, 5#Int32, 32#Int32 #)

    case unpackInt32X4# (andInt32X4# i32_1 i32_2) of
        (# a, b, c, d #) -> print (I32# a, I32# b, I32# c, I32# d)
    case unpackInt32X4# (orInt32X4# i32_1 i32_2) of
        (# a, b, c, d #) -> print (I32# a, I32# b, I32# c, I32# d)
    case unpackInt32X4# (xorInt32X4# i32_1 i32_2) of
        (# a, b, c, d #) -> print (I32# a, I32# b, I32# c, I32# d)

    putStrLn ""
    putStrLn "Int16X8#"
    let
      !i16_1 = packInt16X8#
        (# 1#Int16, 2#Int16, -5#Int16, 128#Int16
        ,  1#Int16, 2#Int16, -5#Int16, 128#Int16
        #)
      !i16_2 = packInt16X8#
        (# 0#Int16, 2#Int16, 5#Int16, 32#Int16
        ,  0#Int16, 2#Int16, 5#Int16, 32#Int16
        #)
    case unpackInt16X8# (andInt16X8# i16_1 i16_2) of
        (# a, b, c, d, e, f, g, h #) ->
          print
            ( (I16# a, I16# b, I16# c, I16# d)
            , (I16# e, I16# f, I16# g, I16# h)
            )
    case unpackInt16X8# (orInt16X8# i16_1 i16_2) of
        (# a, b, c, d, e, f, g, h #) ->
          print
            ( (I16# a, I16# b, I16# c, I16# d)
            , (I16# e, I16# f, I16# g, I16# h)
            )
    case unpackInt16X8# (xorInt16X8# i16_1 i16_2) of
        (# a, b, c, d, e, f, g, h #) ->
          print
            ( (I16# a, I16# b, I16# c, I16# d)
            , (I16# e, I16# f, I16# g, I16# h)
            )

    putStrLn ""
    putStrLn "Int8X16#"
    let
      !i8_1 = packInt8X16#
        (# 1#Int8, 2#Int8, -5#Int8, 128#Int8
        ,  1#Int8, 2#Int8, -5#Int8, 128#Int8
        ,  1#Int8, 2#Int8, -5#Int8, 128#Int8
        ,  1#Int8, 2#Int8, -5#Int8, 128#Int8
        #)
      !i8_2 = packInt8X16#
        (# 0#Int8, 2#Int8, 5#Int8, 32#Int8
        ,  0#Int8, 2#Int8, 5#Int8, 32#Int8
        ,  0#Int8, 2#Int8, 5#Int8, 32#Int8
        ,  0#Int8, 2#Int8, 5#Int8, 32#Int8
        #)
    case unpackInt8X16# (andInt8X16# i8_1 i8_2) of
        (# a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p #) ->
          print
            ( (I8# a, I8# b, I8# c, I8# d)
            , (I8# e, I8# f, I8# g, I8# h)
            , (I8# i, I8# j, I8# k, I8# l)
            , (I8# m, I8# n, I8# o, I8# p)
            )
    case unpackInt8X16# (orInt8X16# i8_1 i8_2) of
        (# a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p #) ->
          print
            ( (I8# a, I8# b, I8# c, I8# d)
            , (I8# e, I8# f, I8# g, I8# h)
            , (I8# i, I8# j, I8# k, I8# l)
            , (I8# m, I8# n, I8# o, I8# p)
            )
    case unpackInt8X16# (xorInt8X16# i8_1 i8_2) of
        (# a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p #) ->
          print
            ( (I8# a, I8# b, I8# c, I8# d)
            , (I8# e, I8# f, I8# g, I8# h)
            , (I8# i, I8# j, I8# k, I8# l)
            , (I8# m, I8# n, I8# o, I8# p)
            )
