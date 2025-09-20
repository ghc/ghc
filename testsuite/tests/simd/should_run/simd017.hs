{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ExtendedLiterals #-}

-- bitwise instructions on unsigned integer vectors

import GHC.Exts
import GHC.Word
import GHC.Prim


main :: IO ()
main = do
    putStrLn "Word64X2#"
    let
      !w64_1 = packWord64X2# (# 1#Word64, 2#Word64 #)
      !w64_2 = packWord64X2# (# 0#Word64, 2#Word64 #)
      !w64_3 = packWord64X2# (# 18446744073709551615#Word64, 128#Word64 #)
      !w64_4 = packWord64X2# (# 5#Word64, 32#Word64 #)

    case unpackWord64X2# (andWord64X2# w64_1 w64_2) of
        (# a, b #) -> print (W64# a, W64# b)
    case unpackWord64X2# (andWord64X2# w64_3 w64_4) of
        (# c, d #) -> print (W64# c, W64# d)
    case unpackWord64X2# (orWord64X2# w64_1 w64_2) of
        (# a, b #) -> print (W64# a, W64# b)
    case unpackWord64X2# (orWord64X2# w64_3 w64_4) of
        (# c, d #) -> print (W64# c, W64# d)
    case unpackWord64X2# (xorWord64X2# w64_1 w64_2) of
        (# a, b #) -> print (W64# a, W64# b)
    case unpackWord64X2# (xorWord64X2# w64_3 w64_4) of
        (# c, d #) -> print (W64# c, W64# d)

    putStrLn ""
    putStrLn "Word32X4#"
    let
      !w32_1 = packWord32X4# (# 1#Word32, 2#Word32, 4294967295#Word32, 128#Word32 #)
      !w32_2 = packWord32X4# (# 0#Word32, 2#Word32, 5#Word32, 32#Word32 #)

    case unpackWord32X4# (andWord32X4# w32_1 w32_2) of
        (# a, b, c, d #) -> print (W32# a, W32# b, W32# c, W32# d)
    case unpackWord32X4# (orWord32X4# w32_1 w32_2) of
        (# a, b, c, d #) -> print (W32# a, W32# b, W32# c, W32# d)
    case unpackWord32X4# (xorWord32X4# w32_1 w32_2) of
        (# a, b, c, d #) -> print (W32# a, W32# b, W32# c, W32# d)

    putStrLn ""
    putStrLn "Word16X8#"
    let
      !w16_1 = packWord16X8#
        (# 1#Word16, 2#Word16, 65535#Word16, 128#Word16
        ,  1#Word16, 2#Word16, 65535#Word16, 128#Word16
        #)
      !w16_2 = packWord16X8#
        (# 0#Word16, 2#Word16, 5#Word16, 32#Word16
        ,  0#Word16, 2#Word16, 5#Word16, 32#Word16
        #)
    case unpackWord16X8# (andWord16X8# w16_1 w16_2) of
        (# a, b, c, d, e, f, g, h #) ->
          print
            ( (W16# a, W16# b, W16# c, W16# d)
            , (W16# e, W16# f, W16# g, W16# h)
            )
    case unpackWord16X8# (orWord16X8# w16_1 w16_2) of
        (# a, b, c, d, e, f, g, h #) ->
          print
            ( (W16# a, W16# b, W16# c, W16# d)
            , (W16# e, W16# f, W16# g, W16# h)
            )
    case unpackWord16X8# (xorWord16X8# w16_1 w16_2) of
        (# a, b, c, d, e, f, g, h #) ->
          print
            ( (W16# a, W16# b, W16# c, W16# d)
            , (W16# e, W16# f, W16# g, W16# h)
            )

    putStrLn ""
    putStrLn "Word8X16#"
    let
      !w8_1 = packWord8X16#
        (# 1#Word8, 2#Word8, 255#Word8, 128#Word8
        ,  1#Word8, 2#Word8, 255#Word8, 128#Word8
        ,  1#Word8, 2#Word8, 255#Word8, 128#Word8
        ,  1#Word8, 2#Word8, 255#Word8, 128#Word8
        #)
      !w8_2 = packWord8X16#
        (# 0#Word8, 2#Word8, 5#Word8, 32#Word8
        ,  0#Word8, 2#Word8, 5#Word8, 32#Word8
        ,  0#Word8, 2#Word8, 5#Word8, 32#Word8
        ,  0#Word8, 2#Word8, 5#Word8, 32#Word8
        #)
    case unpackWord8X16# (andWord8X16# w8_1 w8_2) of
        (# a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p #) ->
          print
            ( (W8# a, W8# b, W8# c, W8# d)
            , (W8# e, W8# f, W8# g, W8# h)
            , (W8# i, W8# j, W8# k, W8# l)
            , (W8# m, W8# n, W8# o, W8# p)
            )
    case unpackWord8X16# (orWord8X16# w8_1 w8_2) of
        (# a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p #) ->
          print
            ( (W8# a, W8# b, W8# c, W8# d)
            , (W8# e, W8# f, W8# g, W8# h)
            , (W8# i, W8# j, W8# k, W8# l)
            , (W8# m, W8# n, W8# o, W8# p)
            )
    case unpackWord8X16# (xorWord8X16# w8_1 w8_2) of
        (# a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p #) ->
          print
            ( (W8# a, W8# b, W8# c, W8# d)
            , (W8# e, W8# f, W8# g, W8# h)
            , (W8# i, W8# j, W8# k, W8# l)
            , (W8# m, W8# n, W8# o, W8# p)
            )
