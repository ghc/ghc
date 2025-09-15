{-# LANGUAGE MagicHash, ExtendedLiterals #-}
import GHC.Word
import GHC.Int
import GHC.Exts

main = do
    print (W8#  (fibw8 6#Word8),
           W16# (fibw16 6#Word16),
           W32# (fibw32 6#Word32),
           W64# (fibw64 6#Word64))
    print (I8# (fibi8 6#Int8),
           I16# (fibi16 6#Int16),
           I32# (fibi32 6#Int32),
           I64# (fibi64 6#Int64))

    print (W64# 0xFFFFFFFFFFFFFFFF#Word64)
    print (I64# 0x7FFFFFFFFFFFFFFF#Int64)
    print (I64# -0x8000000000000000#Int64)
    print (W64# (x () `timesWord64#` y ()))
    print (case x () `timesWord64#` y () of
       276447232#Word64 -> False
       276447233#Word64 -> False
       276447234#Word64 -> False
       276047234#Word64 -> False
       5000000004#Word64 -> False
       100000000000000#Word64 -> True
       _ -> False)
    print (case x () `timesWord64#` y () of
       276447232#Word64 -> True
       _ -> False)

    print [ W8# (branchi8 0#Int8)
          , W8# (branchi8 1#Int8)
          , W8# (branchi8 -1#Int8)
          , W8# (branchi8 126#Int8)
          , W8# (branchi8 127#Int8)
          , W8# (branchi8 -127#Int8)
          , W8# (branchi8 -128#Int8)
          , W8# (branchi8 2#Int8)
          ]

    print [ W16# (branchi16 0#Int16)
          , W16# (branchi16 1#Int16)
          , W16# (branchi16 (-1#Int16))
          , W16# (branchi16 32767#Int16)
          , W16# (branchi16 32766#Int16)
          , W16# (branchi16 (-32768#Int16))
          , W16# (branchi16 (-32767#Int16))
          , W16# (branchi16 2#Int16)
          ]

    print [ W32# (branchi32 0#Int32)
          , W32# (branchi32 1#Int32)
          , W32# (branchi32 (-1#Int32))
          , W32# (branchi32 2147483646#Int32)
          , W32# (branchi32 2147483647#Int32)
          , W32# (branchi32 (-2147483648#Int32))
          , W32# (branchi32 (-2147483647#Int32))
          , W32# (branchi32 2#Int32)
          ]

    print [ W64# (branchi64 0#Int64)
          , W64# (branchi64 1#Int64)
          , W64# (branchi64 (-1#Int64))
          , W64# (branchi64 2147483647#Int64)
          , W64# (branchi64 2147483648#Int64)
          , W64# (branchi64 4294967297#Int64)
          , W64# (branchi64 (-2147483648#Int64))
          , W64# (branchi64 (-2147483649#Int64))
          , W64# (branchi64 (-4294967295#Int64))
          , W64# (branchi64 9223372036854775807#Int64)
          , W64# (branchi64 9223372036854775806#Int64)
          , W64# (branchi64 (-9223372036854775808#Int64))
          , W64# (branchi64 (-9223372036854775807#Int64))
          , W64# (branchi64 2#Int64)
          ]

    print [ I8# (branchw8 0#Word8)
          , I8# (branchw8 1#Word8)
          , I8# (branchw8 254#Word8)
          , I8# (branchw8 255#Word8)
          , I8# (branchw8 2#Word8)
          ]

    print [ I16# (branchw16 0#Word16)
          , I16# (branchw16 1#Word16)
          , I16# (branchw16 255#Word16)
          , I16# (branchw16 256#Word16)
          , I16# (branchw16 65534#Word16)
          , I16# (branchw16 65535#Word16)
          , I16# (branchw16 2#Word16)
          ]

    print [ I32# (branchw32 0#Word32)
          , I32# (branchw32 1#Word32)
          , I32# (branchw32 65534#Word32)
          , I32# (branchw32 65535#Word32)
          , I32# (branchw32 65536#Word32)
          , I32# (branchw32 4294967295#Word32)
          , I32# (branchw32 4294967294#Word32)
          , I32# (branchw32 4294967293#Word32)
          , I32# (branchw32 2#Word32)
          ]

    print [ I64# (branchw64 0#Word64)
          , I64# (branchw64 1#Word64)
          , I64# (branchw64 65536#Word64)
          , I64# (branchw64 4294967295#Word64)
          , I64# (branchw64 4294967296#Word64)
          , I64# (branchw64 4294967297#Word64)
          , I64# (branchw64 18446744073709551615#Word64)
          , I64# (branchw64 18446744073709551614#Word64)
          , I64# (branchw64 18446744073709551613#Word64)
          , I64# (branchw64 2#Word64)
          ]

fibw8 :: Word8# -> Word8#
fibw8 0#Word8 = 0#Word8
fibw8 1#Word8 = 1#Word8
fibw8 n = fibw8 (n `subWord8#` 1#Word8) `plusWord8#` fibw8 (n `subWord8#` 2#Word8)

fibw16 :: Word16# -> Word16#
fibw16 0#Word16 = 0#Word16
fibw16 1#Word16 = 1#Word16
fibw16 n = fibw16 (n `subWord16#` 1#Word16) `plusWord16#` fibw16 (n `subWord16#` 2#Word16)

fibw32 :: Word32# -> Word32#
fibw32 0#Word32 = 0#Word32
fibw32 1#Word32 = 1#Word32
fibw32 n = fibw32 (n `subWord32#` 1#Word32) `plusWord32#` fibw32 (n `subWord32#` 2#Word32)

fibw64 :: Word64# -> Word64#
fibw64 0#Word64 = 0#Word64
fibw64 1#Word64 = 1#Word64
fibw64 n = fibw64 (n `subWord64#` 1#Word64) `plusWord64#` fibw64 (n `subWord64#` 2#Word64)

--

fibi8 :: Int8# -> Int8#
fibi8 0#Int8 = 0#Int8
fibi8 1#Int8 = 1#Int8
fibi8 n = fibi8 (n `subInt8#` 1#Int8) `plusInt8#` fibi8 (n `subInt8#` 2#Int8)

fibi16 :: Int16# -> Int16#
fibi16 0#Int16 = 0#Int16
fibi16 1#Int16 = 1#Int16
fibi16 n = fibi16 (n `subInt16#` 1#Int16) `plusInt16#` fibi16 (n `subInt16#` 2#Int16)

fibi32 :: Int32# -> Int32#
fibi32 0#Int32 = 0#Int32
fibi32 1#Int32 = 1#Int32
fibi32 n = fibi32 (n `subInt32#` 1#Int32) `plusInt32#` fibi32 (n `subInt32#` 2#Int32)

fibi64 :: Int64# -> Int64#
fibi64 0#Int64 = 0#Int64
fibi64 1#Int64 = 1#Int64
fibi64 n = fibi64 (n `subInt64#` 1#Int64) `plusInt64#` fibi64 (n `subInt64#` 2#Int64)

--

branchi8 :: Int8# -> Word8#
branchi8 0#Int8      = 1#Word8
branchi8 1#Int8      = 2#Word8
branchi8 (-1#Int8)   = 3#Word8
branchi8 126#Int8    = 4#Word8
branchi8 127#Int8    = 5#Word8
branchi8 (-127#Int8) = 6#Word8
branchi8 (-128#Int8) = 7#Word8
branchi8 _           = 0#Word8
{-# NOINLINE branchi8 #-}

branchi16 :: Int16# -> Word16#
branchi16 0#Int16        = 1#Word16
branchi16 1#Int16        = 2#Word16
branchi16 (-1#Int16)     = 3#Word16
branchi16 32767#Int16    = 255#Word16
branchi16 32766#Int16    = 256#Word16
branchi16 (-32768#Int16) = 65534#Word16
branchi16 (-32767#Int16) = 65535#Word16
branchi16 _              = 0#Word16
{-# NOINLINE branchi16 #-}

branchi32 :: Int32# -> Word32#
branchi32 0#Int32             = 1#Word32
branchi32 1#Int32             = 2#Word32
branchi32 (-1#Int32)          = 3#Word32
branchi32 2147483646#Int32    = 65535#Word32
branchi32 2147483647#Int32    = 65536#Word32
branchi32 (-2147483648#Int32) = 4294967294#Word32
branchi32 (-2147483647#Int32) = 4294967295#Word32
branchi32 _                   = 0#Word32
{-# NOINLINE branchi32 #-}

branchi64 :: Int64# -> Word64#
branchi64 0#Int64                      = 18446744073709551615#Word64
branchi64 1#Int64                      = 2147483648#Word64
branchi64 (-1#Int64)                   = 4294967296#Word64
branchi64 2147483647#Int64             = 4294967297#Word64
branchi64 2147483648#Int64             = 9#Word64
branchi64 4294967297#Int64             = 1#Word64
branchi64 (-2147483648#Int64)          = 18446744073709551614#Word64
branchi64 (-2147483649#Int64)          = 3#Word64
branchi64 (-4294967295#Int64)          = 4#Word64
branchi64 9223372036854775807#Int64    = 5#Word64
branchi64 9223372036854775806#Int64    = 6#Word64
branchi64 (-9223372036854775808#Int64) = 7#Word64
branchi64 (-9223372036854775807#Int64) = 8#Word64
branchi64 _                            = 0#Word64
{-# NOINLINE branchi64 #-}

branchw8 :: Word8# -> Int8#
branchw8 0#Word8   = 1#Int8
branchw8 1#Word8   = (-1#Int8)
branchw8 254#Word8 = 2#Int8
branchw8 255#Word8 = (-2#Int8)
branchw8 _         = 0#Int8
{-# NOINLINE branchw8 #-}

branchw16 :: Word16# -> Int16#
branchw16 0#Word16      = 256#Int16
branchw16 1#Word16      = (-256#Int16)
branchw16 255#Word16    = 32767#Int16
branchw16 256#Word16    = (-32768#Int16)
branchw16 65534#Word16  = (-1#Int16)
branchw16 65535#Word16  = 1#Int16
branchw16 _             = 0#Int16
{-# NOINLINE branchw16 #-}

branchw32 :: Word32# -> Int32#
branchw32 0#Word32          = 2147483647#Int32
branchw32 1#Word32          = (-2147483648#Int32)
branchw32 65534#Word32      = 65535#Int32
branchw32 65535#Word32      = 65536#Int32
branchw32 65536#Word32      = (-1#Int32)
branchw32 4294967295#Word32 = (-65536#Int32)
branchw32 4294967294#Word32 = (-65537#Int32)
branchw32 4294967293#Word32 = 1#Int32
branchw32 _                 = 0#Int32
{-# NOINLINE branchw32 #-}

branchw64 :: Word64# -> Int64#
branchw64 0#Word64                    = 9223372036854775807#Int64
branchw64 1#Word64                    = 2147483648#Int64
branchw64 65536#Word64                = 4294967296#Int64
branchw64 4294967295#Word64           = 4294967297#Int64
branchw64 4294967296#Word64           = (-1#Int64)
branchw64 4294967297#Word64           = 9223372036854775806#Int64
branchw64 18446744073709551615#Word64 = (-9223372036854775808#Int64)
branchw64 18446744073709551614#Word64 = (-9223372036854775807#Int64)
branchw64 18446744073709551613#Word64 = 1#Int64
branchw64 _                           = 0#Int64
{-# NOINLINE branchw64 #-}

x :: () -> Word64#
x () = 2000000000#Word64
{-# NOINLINE x #-}

y :: () -> Word64#
y () = 50000#Word64
{-# NOINLINE y #-}
