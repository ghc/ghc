module SizedLiteralsA where

import GHC.Word
import GHC.Int
import Language.Haskell.TH.Syntax

fibw8 :: Word8 -> Word8
fibw8 0 = 0
fibw8 1 = 1
fibw8 n = fibw8 (n-1) + fibw8 (n-2)

fibw16 :: Word16 -> Word16
fibw16 0 = 0
fibw16 1 = 1
fibw16 n = fibw16 (n-1) + fibw16 (n-2)

fibw32 :: Word32 -> Word32
fibw32 0 = 0
fibw32 1 = 1
fibw32 n = fibw32 (n-1) + fibw32 (n-2)

fibw64 :: Word64 -> Word64
fibw64 0 = 0
fibw64 1 = 1
fibw64 n = fibw64 (n-1) + fibw64 (n-2)

--

fibi8 :: Int8 -> Int8
fibi8 0 = 0
fibi8 1 = 1
fibi8 n = fibi8 (n-1) + fibi8 (n-2)

fibi16 :: Int16 -> Int16
fibi16 0 = 0
fibi16 1 = 1
fibi16 n = fibi16 (n-1) + fibi16 (n-2)

fibi32 :: Int32 -> Int32
fibi32 0 = 0
fibi32 1 = 1
fibi32 n = fibi32 (n-1) + fibi32 (n-2)

fibi64 :: Int64 -> Int64
fibi64 0 = 0
fibi64 1 = 1
fibi64 n = fibi64 (n-1) + fibi64 (n-2)

--

branchi8 :: Int8 -> Word8
branchi8 0      = 1
branchi8 1      = 2
branchi8 (-1)   = 3
branchi8 126    = 4
branchi8 127    = 5
branchi8 (-127) = 6
branchi8 (-128) = 7
branchi8 _      = 0

branchi16 :: Int16 -> Word16
branchi16 0        = 1
branchi16 1        = 2
branchi16 (-1)     = 3
branchi16 32767    = 255
branchi16 32766    = 256
branchi16 (-32768) = 65534
branchi16 (-32767) = 65535
branchi16 _        = 0

branchi32 :: Int32 -> Word32
branchi32 0             = 1
branchi32 1             = 2
branchi32 (-1)          = 3
branchi32 2147483646    = 65535
branchi32 2147483647    = 65536
branchi32 (-2147483648) = 4294967294
branchi32 (-2147483647) = 4294967295
branchi32 _             = 0

branchi64 :: Int64 -> Word64
branchi64 0                      = 18446744073709551615
branchi64 1                      = 2147483648
branchi64 (-1)                   = 4294967296
branchi64 2147483647             = 4294967297
branchi64 2147483648             = 9
branchi64 4294967297             = 1
branchi64 (-2147483648)          = 18446744073709551614
branchi64 (-2147483649)          = 3
branchi64 (-4294967295)          = 4
branchi64 9223372036854775807    = 5
branchi64 9223372036854775806    = 6
branchi64 (-9223372036854775808) = 7
branchi64 (-9223372036854775807) = 8
branchi64 _                      = 0

branchw8 :: Word8 -> Int8
branchw8 0   = 1
branchw8 1   = (-1)
branchw8 254 = 2
branchw8 255 = (-2)
branchw8 _   = 0

branchw16 :: Word16 -> Int16
branchw16 0      = 256
branchw16 1      = (-256)
branchw16 255    = 32767
branchw16 256    = (-32768)
branchw16 65534  = (-1)
branchw16 65535  = 1
branchw16 _      = 0

branchw32 :: Word32 -> Int32
branchw32 0          = 2147483647
branchw32 1          = (-2147483648)
branchw32 65534      = 65535
branchw32 65535      = 65536
branchw32 65536      = (-1)
branchw32 4294967295 = (-65536)
branchw32 4294967294 = (-65537)
branchw32 4294967293 = 1
branchw32 _          = 0

branchw64 :: Word64 -> Int64
branchw64 0                    = 9223372036854775807
branchw64 1                    = 2147483648
branchw64 65536                = 4294967296
branchw64 4294967295           = 4294967297
branchw64 4294967296           = (-1)
branchw64 4294967297           = 9223372036854775806
branchw64 18446744073709551615 = (-9223372036854775808)
branchw64 18446744073709551614 = (-9223372036854775807)
branchw64 18446744073709551613 = 1
branchw64 _                    = 0

--

ie :: Integral a => a -> Exp
ie x = LitE (IntegerL (toInteger x))
