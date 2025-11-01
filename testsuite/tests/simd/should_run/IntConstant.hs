{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ExtendedLiterals #-}
import GHC.Exts
import GHC.Int
import GHC.Word
import Control.Monad

-- The MOVI/MVNI instruction of AArch64 can encode some integer constants.
-- This file checks that the encoding works correctly.

data Int8X16 = I8X16 Int8X16#
data Int16X8 = I16X8 Int16X8#
data Int32X4 = I32X4 Int32X4#
data Int64X2 = I64X2 Int64X2#
data Word8X16 = W8X16 Word8X16#
data Word16X8 = W16X8 Word16X8#
data Word32X4 = W32X4 Word32X4#
data Word64X2 = W64X2 Word64X2#

int8Constants :: [Int8X16]
int8Constants =
  [ I8X16 (broadcastInt8X16# -0x69#Int8)
  , I8X16 (broadcastInt8X16# -0x1#Int8)
  , I8X16 (broadcastInt8X16# 0x0#Int8)
  , I8X16 (broadcastInt8X16# 0x30#Int8)
  , I8X16 (broadcastInt8X16# 0x5f#Int8)
  ]
{-# OPAQUE int8Constants #-}

int16Constants :: [Int16X8]
int16Constants =
  [ I16X8 (broadcastInt16X8# -0x7a01#Int16)
  , I16X8 (broadcastInt16X8# -0x7500#Int16)
  , I16X8 (broadcastInt16X8# -0x7101#Int16)
  , I16X8 (broadcastInt16X8# -0x6f00#Int16)
  , I16X8 (broadcastInt16X8# -0x4401#Int16)
  , I16X8 (broadcastInt16X8# -0x708#Int16)
  , I16X8 (broadcastInt16X8# -0xce#Int16)
  , I16X8 (broadcastInt16X8# -0xa1#Int16)
  , I16X8 (broadcastInt16X8# -0xc#Int16)
  , I16X8 (broadcastInt16X8# -0x1#Int16)
  , I16X8 (broadcastInt16X8# 0x0#Int16)
  , I16X8 (broadcastInt16X8# 0xf#Int16)
  , I16X8 (broadcastInt16X8# 0xd0#Int16)
  , I16X8 (broadcastInt16X8# 0xd2#Int16)
  , I16X8 (broadcastInt16X8# 0x1700#Int16)
  , I16X8 (broadcastInt16X8# 0x2929#Int16)
  , I16X8 (broadcastInt16X8# 0x5e5e#Int16)
  ]
{-# OPAQUE int16Constants #-}

int32Constants :: [Int32X4]
int32Constants =
  [ I32X4 (broadcastInt32X4# -0x7cff7d00#Int32)
  , I32X4 (broadcastInt32X4# -0x7b007b01#Int32)
  , I32X4 (broadcastInt32X4# -0x6f000000#Int32)
  , I32X4 (broadcastInt32X4# -0x60000001#Int32)
  , I32X4 (broadcastInt32X4# -0x59ff5a00#Int32)
  , I32X4 (broadcastInt32X4# -0x4d4d4d4e#Int32)
  , I32X4 (broadcastInt32X4# -0x47000001#Int32)
  , I32X4 (broadcastInt32X4# -0x24242425#Int32)
  , I32X4 (broadcastInt32X4# -0x20000000#Int32)
  , I32X4 (broadcastInt32X4# -0x1d1d1d1e#Int32)
  , I32X4 (broadcastInt32X4# -0x13131314#Int32)
  , I32X4 (broadcastInt32X4# -0xfd0000#Int32)
  , I32X4 (broadcastInt32X4# -0xaa0000#Int32)
  , I32X4 (broadcastInt32X4# -0x9f0000#Int32)
  , I32X4 (broadcastInt32X4# -0x820001#Int32)
  , I32X4 (broadcastInt32X4# -0x780079#Int32)
  , I32X4 (broadcastInt32X4# -0x690001#Int32)
  , I32X4 (broadcastInt32X4# -0x440045#Int32)
  , I32X4 (broadcastInt32X4# -0xf0001#Int32)
  , I32X4 (broadcastInt32X4# -0x50006#Int32)
  , I32X4 (broadcastInt32X4# -0xda00#Int32)
  , I32X4 (broadcastInt32X4# -0xb701#Int32)
  , I32X4 (broadcastInt32X4# -0x9401#Int32)
  , I32X4 (broadcastInt32X4# -0x7200#Int32)
  , I32X4 (broadcastInt32X4# -0x4201#Int32)
  , I32X4 (broadcastInt32X4# -0xb00#Int32)
  , I32X4 (broadcastInt32X4# -0xde#Int32)
  , I32X4 (broadcastInt32X4# -0x2e#Int32)
  , I32X4 (broadcastInt32X4# -0x25#Int32)
  , I32X4 (broadcastInt32X4# -0x1#Int32)
  , I32X4 (broadcastInt32X4# 0x0#Int32)
  , I32X4 (broadcastInt32X4# 0x5b#Int32)
  , I32X4 (broadcastInt32X4# 0x5c#Int32)
  , I32X4 (broadcastInt32X4# 0xed#Int32)
  , I32X4 (broadcastInt32X4# 0x900#Int32)
  , I32X4 (broadcastInt32X4# 0x4200#Int32)
  , I32X4 (broadcastInt32X4# 0x4aff#Int32)
  , I32X4 (broadcastInt32X4# 0x7f00#Int32)
  , I32X4 (broadcastInt32X4# 0xa2ff#Int32)
  , I32X4 (broadcastInt32X4# 0xe2ff#Int32)
  , I32X4 (broadcastInt32X4# 0x1fffff#Int32)
  , I32X4 (broadcastInt32X4# 0x410041#Int32)
  , I32X4 (broadcastInt32X4# 0x4b0000#Int32)
  , I32X4 (broadcastInt32X4# 0x4d004d#Int32)
  , I32X4 (broadcastInt32X4# 0x7a0000#Int32)
  , I32X4 (broadcastInt32X4# 0x9b0000#Int32)
  , I32X4 (broadcastInt32X4# 0xa8ffff#Int32)
  , I32X4 (broadcastInt32X4# 0xcb00cb#Int32)
  , I32X4 (broadcastInt32X4# 0xebffff#Int32)
  , I32X4 (broadcastInt32X4# 0xd0d0d0d#Int32)
  , I32X4 (broadcastInt32X4# 0x18000000#Int32)
  , I32X4 (broadcastInt32X4# 0x1dff1dff#Int32)
  , I32X4 (broadcastInt32X4# 0x3a003a00#Int32)
  , I32X4 (broadcastInt32X4# 0x50505050#Int32)
  , I32X4 (broadcastInt32X4# 0x69ff69ff#Int32)
  , I32X4 (broadcastInt32X4# 0x77ffffff#Int32)
  ]
{-# OPAQUE int32Constants #-}

int64Constants :: [Int64X2]
int64Constants =
  [ I64X2 (broadcastInt64X2# -0x7b0000007b000001#Int64)
  , I64X2 (broadcastInt64X2# -0x7474747474747475#Int64)
  , I64X2 (broadcastInt64X2# -0x5300530053005301#Int64)
  , I64X2 (broadcastInt64X2# -0x4e004e004e004e01#Int64)
  , I64X2 (broadcastInt64X2# -0x3100000031000001#Int64)
  , I64X2 (broadcastInt64X2# -0x1f1f1f1f1f1f1f20#Int64)
  , I64X2 (broadcastInt64X2# -0x1e001e001e001e01#Int64)
  , I64X2 (broadcastInt64X2# -0xf0000000f00001#Int64)
  , I64X2 (broadcastInt64X2# -0xc1000000c10001#Int64)
  , I64X2 (broadcastInt64X2# -0xa9000000a90001#Int64)
  , I64X2 (broadcastInt64X2# -0xa800a800a800a9#Int64)
  , I64X2 (broadcastInt64X2# -0x7e007e007e007f#Int64)
  , I64X2 (broadcastInt64X2# -0x5f005f005f0060#Int64)
  , I64X2 (broadcastInt64X2# -0x4dffff004e0000#Int64)
  , I64X2 (broadcastInt64X2# -0xeffff000f0000#Int64)
  , I64X2 (broadcastInt64X2# -0x8ffff00090000#Int64)
  , I64X2 (broadcastInt64X2# -0xcfff0000d000#Int64)
  , I64X2 (broadcastInt64X2# -0xad000000ad01#Int64)
  , I64X2 (broadcastInt64X2# -0x95ff00009600#Int64)
  , I64X2 (broadcastInt64X2# -0x85ff00008600#Int64)
  , I64X2 (broadcastInt64X2# -0x770000007701#Int64)
  , I64X2 (broadcastInt64X2# -0x300000003001#Int64)
  , I64X2 (broadcastInt64X2# -0x5200000053#Int64)
  , I64X2 (broadcastInt64X2# -0x2600000027#Int64)
  , I64X2 (broadcastInt64X2# -0x1000000011#Int64)
  , I64X2 (broadcastInt64X2# -0x1#Int64)
  , I64X2 (broadcastInt64X2# 0x0#Int64)
  , I64X2 (broadcastInt64X2# 0x1b0000001b#Int64)
  , I64X2 (broadcastInt64X2# 0xb7000000b7#Int64)
  , I64X2 (broadcastInt64X2# 0xb9000000b9#Int64)
  , I64X2 (broadcastInt64X2# 0x120000001200#Int64)
  , I64X2 (broadcastInt64X2# 0x73ff000073ff#Int64)
  , I64X2 (broadcastInt64X2# 0x92ff000092ff#Int64)
  , I64X2 (broadcastInt64X2# 0xb2ff0000b2ff#Int64)
  , I64X2 (broadcastInt64X2# 0xc0000000c000#Int64)
  , I64X2 (broadcastInt64X2# 0xc3000000c300#Int64)
  , I64X2 (broadcastInt64X2# 0x1c0000001c0000#Int64)
  , I64X2 (broadcastInt64X2# 0x28ffff0028ffff#Int64)
  , I64X2 (broadcastInt64X2# 0x2c0000002c0000#Int64)
  , I64X2 (broadcastInt64X2# 0x33003300330033#Int64)
  , I64X2 (broadcastInt64X2# 0x53ffff0053ffff#Int64)
  , I64X2 (broadcastInt64X2# 0x69006900690069#Int64)
  , I64X2 (broadcastInt64X2# 0x81008100810081#Int64)
  , I64X2 (broadcastInt64X2# 0xbdffff00bdffff#Int64)
  , I64X2 (broadcastInt64X2# 0xc8000000c80000#Int64)
  , I64X2 (broadcastInt64X2# 0x1fe000fffffff80#Int64)
  , I64X2 (broadcastInt64X2# 0x1fffc0fffffc000#Int64)
  , I64X2 (broadcastInt64X2# 0x1fffff81fffc000#Int64)
  , I64X2 (broadcastInt64X2# 0x2900000029000000#Int64)
  , I64X2 (broadcastInt64X2# 0x2dffffff2dffffff#Int64)
  , I64X2 (broadcastInt64X2# 0x2e2e2e2e2e2e2e2e#Int64)
  , I64X2 (broadcastInt64X2# 0x2f002f002f002f00#Int64)
  , I64X2 (broadcastInt64X2# 0x3700370037003700#Int64)
  , I64X2 (broadcastInt64X2# 0x3f003f003f003f00#Int64)
  , I64X2 (broadcastInt64X2# 0x4500000045000000#Int64)
  , I64X2 (broadcastInt64X2# 0x5b5b5b5b5b5b5b5b#Int64)
  , I64X2 (broadcastInt64X2# 0x5e5e5e5e5e5e5e5e#Int64)
  , I64X2 (broadcastInt64X2# 0x6200000062000000#Int64)
  , I64X2 (broadcastInt64X2# 0x7575757575757575#Int64)
  ]
{-# OPAQUE int64Constants #-}

word8Constants :: [Word8X16]
word8Constants =
  [ W8X16 (broadcastWord8X16# 0x0#Word8)
  , W8X16 (broadcastWord8X16# 0x1#Word8)
  , W8X16 (broadcastWord8X16# 0x3e#Word8)
  , W8X16 (broadcastWord8X16# 0xac#Word8)
  , W8X16 (broadcastWord8X16# 0xff#Word8)
  ]
{-# OPAQUE word8Constants #-}

word16Constants :: [Word16X8]
word16Constants =
  [ W16X8 (broadcastWord16X8# 0x0#Word16)
  , W16X8 (broadcastWord16X8# 0x3#Word16)
  , W16X8 (broadcastWord16X8# 0x34#Word16)
  , W16X8 (broadcastWord16X8# 0xaa#Word16)
  , W16X8 (broadcastWord16X8# 0x1cff#Word16)
  , W16X8 (broadcastWord16X8# 0x3e00#Word16)
  , W16X8 (broadcastWord16X8# 0x7878#Word16)
  , W16X8 (broadcastWord16X8# 0x8600#Word16)
  , W16X8 (broadcastWord16X8# 0x8700#Word16)
  , W16X8 (broadcastWord16X8# 0xa5ff#Word16)
  , W16X8 (broadcastWord16X8# 0xafff#Word16)
  , W16X8 (broadcastWord16X8# 0xcbcb#Word16)
  , W16X8 (broadcastWord16X8# 0xefef#Word16)
  , W16X8 (broadcastWord16X8# 0xff59#Word16)
  , W16X8 (broadcastWord16X8# 0xff67#Word16)
  , W16X8 (broadcastWord16X8# 0xffa6#Word16)
  , W16X8 (broadcastWord16X8# 0xffff#Word16)
  ]
{-# OPAQUE word16Constants #-}

word32Constants :: [Word32X4]
word32Constants =
  [ W32X4 (broadcastWord32X4# 0x0#Word32)
  , W32X4 (broadcastWord32X4# 0x58#Word32)
  , W32X4 (broadcastWord32X4# 0x7d#Word32)
  , W32X4 (broadcastWord32X4# 0xc3#Word32)
  , W32X4 (broadcastWord32X4# 0x1c00#Word32)
  , W32X4 (broadcastWord32X4# 0x45ff#Word32)
  , W32X4 (broadcastWord32X4# 0xcb00#Word32)
  , W32X4 (broadcastWord32X4# 0xd1ff#Word32)
  , W32X4 (broadcastWord32X4# 0xd400#Word32)
  , W32X4 (broadcastWord32X4# 0xe2ff#Word32)
  , W32X4 (broadcastWord32X4# 0x37ffff#Word32)
  , W32X4 (broadcastWord32X4# 0x3a0000#Word32)
  , W32X4 (broadcastWord32X4# 0x4f0000#Word32)
  , W32X4 (broadcastWord32X4# 0x6d006d#Word32)
  , W32X4 (broadcastWord32X4# 0x7b007b#Word32)
  , W32X4 (broadcastWord32X4# 0x92ffff#Word32)
  , W32X4 (broadcastWord32X4# 0xca00ca#Word32)
  , W32X4 (broadcastWord32X4# 0xed0000#Word32)
  , W32X4 (broadcastWord32X4# 0xfdffff#Word32)
  , W32X4 (broadcastWord32X4# 0x9090909#Word32)
  , W32X4 (broadcastWord32X4# 0x19001900#Word32)
  , W32X4 (broadcastWord32X4# 0x23000000#Word32)
  , W32X4 (broadcastWord32X4# 0x2b000000#Word32)
  , W32X4 (broadcastWord32X4# 0x44444444#Word32)
  , W32X4 (broadcastWord32X4# 0x4cffffff#Word32)
  , W32X4 (broadcastWord32X4# 0x55005500#Word32)
  , W32X4 (broadcastWord32X4# 0x5f5f5f5f#Word32)
  , W32X4 (broadcastWord32X4# 0x7bff7bff#Word32)
  , W32X4 (broadcastWord32X4# 0x8a8a8a8a#Word32)
  , W32X4 (broadcastWord32X4# 0x91000000#Word32)
  , W32X4 (broadcastWord32X4# 0x9cffffff#Word32)
  , W32X4 (broadcastWord32X4# 0xa3ffa3ff#Word32)
  , W32X4 (broadcastWord32X4# 0xbf00bf00#Word32)
  , W32X4 (broadcastWord32X4# 0xc1c1c1c1#Word32)
  , W32X4 (broadcastWord32X4# 0xd1ffd1ff#Word32)
  , W32X4 (broadcastWord32X4# 0xd6d6d6d6#Word32)
  , W32X4 (broadcastWord32X4# 0xf0ffffff#Word32)
  , W32X4 (broadcastWord32X4# 0xff1e0000#Word32)
  , W32X4 (broadcastWord32X4# 0xff2cff2c#Word32)
  , W32X4 (broadcastWord32X4# 0xff6bffff#Word32)
  , W32X4 (broadcastWord32X4# 0xffc00000#Word32)
  , W32X4 (broadcastWord32X4# 0xffc9ffff#Word32)
  , W32X4 (broadcastWord32X4# 0xffcfffcf#Word32)
  , W32X4 (broadcastWord32X4# 0xffd1ffd1#Word32)
  , W32X4 (broadcastWord32X4# 0xfff9ffff#Word32)
  , W32X4 (broadcastWord32X4# 0xfffd0000#Word32)
  , W32X4 (broadcastWord32X4# 0xffff16ff#Word32)
  , W32X4 (broadcastWord32X4# 0xffff2300#Word32)
  , W32X4 (broadcastWord32X4# 0xffff5c00#Word32)
  , W32X4 (broadcastWord32X4# 0xffffb300#Word32)
  , W32X4 (broadcastWord32X4# 0xffffd9ff#Word32)
  , W32X4 (broadcastWord32X4# 0xfffff8ff#Word32)
  , W32X4 (broadcastWord32X4# 0xffffff12#Word32)
  , W32X4 (broadcastWord32X4# 0xffffff99#Word32)
  , W32X4 (broadcastWord32X4# 0xffffffd2#Word32)
  , W32X4 (broadcastWord32X4# 0xffffffff#Word32)
  ]
{-# OPAQUE word32Constants #-}

word64Constants :: [Word64X2]
word64Constants =
  [ W64X2 (broadcastWord64X2# 0x0#Word64)
  , W64X2 (broadcastWord64X2# 0x5400000054#Word64)
  , W64X2 (broadcastWord64X2# 0xea000000ea#Word64)
  , W64X2 (broadcastWord64X2# 0xef000000ef#Word64)
  , W64X2 (broadcastWord64X2# 0x70000000700#Word64)
  , W64X2 (broadcastWord64X2# 0x7ffffe00000#Word64)
  , W64X2 (broadcastWord64X2# 0x3c0000003c00#Word64)
  , W64X2 (broadcastWord64X2# 0x76ff000076ff#Word64)
  , W64X2 (broadcastWord64X2# 0x910000009100#Word64)
  , W64X2 (broadcastWord64X2# 0xd8ff0000d8ff#Word64)
  , W64X2 (broadcastWord64X2# 0xe3ff0000e3ff#Word64)
  , W64X2 (broadcastWord64X2# 0x26000000260000#Word64)
  , W64X2 (broadcastWord64X2# 0x2f0000002f0000#Word64)
  , W64X2 (broadcastWord64X2# 0x4f0000004f0000#Word64)
  , W64X2 (broadcastWord64X2# 0x77007700770077#Word64)
  , W64X2 (broadcastWord64X2# 0x7effff007effff#Word64)
  , W64X2 (broadcastWord64X2# 0x8fffff008fffff#Word64)
  , W64X2 (broadcastWord64X2# 0x99009900990099#Word64)
  , W64X2 (broadcastWord64X2# 0xa500a500a500a5#Word64)
  , W64X2 (broadcastWord64X2# 0xfeffff00feffff#Word64)
  , W64X2 (broadcastWord64X2# 0x100010001000100#Word64)
  , W64X2 (broadcastWord64X2# 0x1fffff8003fff80#Word64)
  , W64X2 (broadcastWord64X2# 0x1fffffff03fc0ff#Word64)
  , W64X2 (broadcastWord64X2# 0x300000003000000#Word64)
  , W64X2 (broadcastWord64X2# 0x900090009000900#Word64)
  , W64X2 (broadcastWord64X2# 0x2323232323232323#Word64)
  , W64X2 (broadcastWord64X2# 0x32ff32ff32ff32ff#Word64)
  , W64X2 (broadcastWord64X2# 0x33ff33ff33ff33ff#Word64)
  , W64X2 (broadcastWord64X2# 0x4f4f4f4f4f4f4f4f#Word64)
  , W64X2 (broadcastWord64X2# 0x5a005a005a005a00#Word64)
  , W64X2 (broadcastWord64X2# 0x7e7e7e7e7e7e7e7e#Word64)
  , W64X2 (broadcastWord64X2# 0x86ffffff86ffffff#Word64)
  , W64X2 (broadcastWord64X2# 0x8a0000008a000000#Word64)
  , W64X2 (broadcastWord64X2# 0x8d8d8d8d8d8d8d8d#Word64)
  , W64X2 (broadcastWord64X2# 0x9b0000009b000000#Word64)
  , W64X2 (broadcastWord64X2# 0x9fffffff9fffffff#Word64)
  , W64X2 (broadcastWord64X2# 0xaaffaaffaaffaaff#Word64)
  , W64X2 (broadcastWord64X2# 0xdfffffffdfffffff#Word64)
  , W64X2 (broadcastWord64X2# 0xeeeeeeeeeeeeeeee#Word64)
  , W64X2 (broadcastWord64X2# 0xff01ff01ff01ff01#Word64)
  , W64X2 (broadcastWord64X2# 0xff36ffffff36ffff#Word64)
  , W64X2 (broadcastWord64X2# 0xff4b0000ff4b0000#Word64)
  , W64X2 (broadcastWord64X2# 0xff560000ff560000#Word64)
  , W64X2 (broadcastWord64X2# 0xff67ffffff67ffff#Word64)
  , W64X2 (broadcastWord64X2# 0xff6a0000ff6a0000#Word64)
  , W64X2 (broadcastWord64X2# 0xffceffffffceffff#Word64)
  , W64X2 (broadcastWord64X2# 0xffd8ffd8ffd8ffd8#Word64)
  , W64X2 (broadcastWord64X2# 0xffe9ffe9ffe9ffe9#Word64)
  , W64X2 (broadcastWord64X2# 0xffff0000ffff0000#Word64)
  , W64X2 (broadcastWord64X2# 0xffff0d00ffff0d00#Word64)
  , W64X2 (broadcastWord64X2# 0xffff18ffffff18ff#Word64)
  , W64X2 (broadcastWord64X2# 0xffff2bffffff2bff#Word64)
  , W64X2 (broadcastWord64X2# 0xffff3300ffff3300#Word64)
  , W64X2 (broadcastWord64X2# 0xffffa0ffffffa0ff#Word64)
  , W64X2 (broadcastWord64X2# 0xffffff5bffffff5b#Word64)
  , W64X2 (broadcastWord64X2# 0xffffff8dffffff8d#Word64)
  , W64X2 (broadcastWord64X2# 0xffffffa4ffffffa4#Word64)
  , W64X2 (broadcastWord64X2# 0xffffffffffffffff#Word64)
  ]
{-# OPAQUE word64Constants #-}

main :: IO ()
main = do
  putStrLn "Int8X16:"
  forM_ int8Constants $ \(I8X16 v) ->
    case unpackInt8X16# v of
      (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #) ->
        print [I8# x0, I8# x1, I8# x2, I8# x3, I8# x4, I8# x5, I8# x6, I8# x7, I8# x8, I8# x9, I8# x10, I8# x11, I8# x12, I8# x13, I8# x14, I8# x15]
  putStrLn "Int16X8:"
  forM_ int16Constants $ \(I16X8 v) ->
    case unpackInt16X8# v of
      (# x0, x1, x2, x3, x4, x5, x6, x7 #) ->
        print [I16# x0, I16# x1, I16# x2, I16# x3, I16# x4, I16# x5, I16# x6, I16# x7]
  putStrLn "Int32X4:"
  forM_ int32Constants $ \(I32X4 v) ->
    case unpackInt32X4# v of
      (# x0, x1, x2, x3 #) ->
        print [I32# x0, I32# x1, I32# x2, I32# x3]
  putStrLn "Int64X2:"
  forM_ int64Constants $ \(I64X2 v) ->
    case unpackInt64X2# v of
      (# x0, x1 #) ->
        print [I64# x0, I64# x1]
  putStrLn "Word8X16:"
  forM_ word8Constants $ \(W8X16 v) ->
    case unpackWord8X16# v of
      (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #) ->
        print [W8# x0, W8# x1, W8# x2, W8# x3, W8# x4, W8# x5, W8# x6, W8# x7, W8# x8, W8# x9, W8# x10, W8# x11, W8# x12, W8# x13, W8# x14, W8# x15]
  putStrLn "Word16X8:"
  forM_ word16Constants $ \(W16X8 v) ->
    case unpackWord16X8# v of
      (# x0, x1, x2, x3, x4, x5, x6, x7 #) ->
        print [W16# x0, W16# x1, W16# x2, W16# x3, W16# x4, W16# x5, W16# x6, W16# x7]
  putStrLn "Word32X4:"
  forM_ word32Constants $ \(W32X4 v) ->
    case unpackWord32X4# v of
      (# x0, x1, x2, x3 #) ->
        print [W32# x0, W32# x1, W32# x2, W32# x3]
  putStrLn "Word64X2:"
  forM_ word64Constants $ \(W64X2 v) ->
    case unpackWord64X2# v of
      (# x0, x1 #) ->
        print [W64# x0, W64# x1]

{- Generated by:
{- cabal:
build-depends: base, random
-}
{-# LANGUAGE NumericUnderscores #-}
import System.Random.Stateful
import qualified Data.List as List
import Control.Monad
import Data.Int
import Data.Word
import Data.Bits
import Numeric

gen8 :: IOGenM StdGen -> IO Word8
gen8 = uniformWord8

gen16 :: IOGenM StdGen -> [IO Word16]
gen16 g = [ fromIntegral <$> gen8 g
          , (\w -> fromIntegral w `shiftL` 8) <$> gen8 g
          , (\w -> fromIntegral w .|. 0xFF00) <$> gen8 g
          , (\w -> (fromIntegral w `shiftL` 8) .|. 0x00FF) <$> gen8 g
          , (\w -> fromIntegral w * 0x0101) <$> gen8 g
          ]

gen32 :: IOGenM StdGen -> [IO Word32]
gen32 g = [ fromIntegral <$> gen8 g
          , (\w -> fromIntegral w `shiftL` 8) <$> gen8 g
          , (\w -> fromIntegral w `shiftL` 16) <$> gen8 g
          , (\w -> fromIntegral w `shiftL` 24) <$> gen8 g
          , (\w -> (fromIntegral w `shiftL` 8) .|. 0xFF) <$> gen8 g
          , (\w -> (fromIntegral w `shiftL` 16) .|. 0xFFFF) <$> gen8 g
          , (\w -> fromIntegral w .|. 0xFFFF_FF00) <$> gen8 g
          , (\w -> (fromIntegral w `shiftL` 8) .|. 0xFFFF_00FF) <$> gen8 g
          , (\w -> (fromIntegral w `shiftL` 16) .|. 0xFF00_FFFF) <$> gen8 g
          , (\w -> (fromIntegral w `shiftL` 24) .|. 0x00FF_FFFF) <$> gen8 g
          , (\w -> (fromIntegral w `shiftL` 8) .|. 0xFFFF_0000) <$> gen8 g
          , (\w -> (fromIntegral w `shiftL` 16) .|. 0xFF00_0000) <$> gen8 g
          , (\w -> fromIntegral w * 0x0101_0101) <$> gen8 g
          ] ++ map ((\w -> fromIntegral w * 0x10001) <$>) (gen16 g)

gen64 :: IOGenM StdGen -> [IO Word64]
gen64 g = [expand <$> gen8 g] ++ map ((\w -> fromIntegral w * 0x1_0000_0001) <$>) (gen32 g)
  where
    expand w = foldl (.|.) 0 [if testBit w i then 0xff `shiftL` (7 * i) else 0 | i <- [0..7]]

genCases :: (Integral w, Integral i, Ord i, Show i) => String -> String -> String -> String -> [IO w] -> [i] -> IO ()
genCases listName tyName ctrName suffix gen fixedCases = do
  let bcst = "broadcast" ++ tyName ++ "#"
  randomCases <- fmap concat $ forM gen $ \g -> map fromIntegral <$> replicateM 3 g
  let xs = map (\x -> ctrName ++ " (" ++ bcst ++ " " ++ showSigned (\x s -> "0x" ++ showHex x s) 0 x suffix ++ ")") $ List.nub $ List.sort $ fixedCases ++ randomCases
  putStrLn $ listName ++ " :: [" ++ tyName ++ "]"
  putStrLn $ listName ++ " ="
  putStrLn $ "  [ " ++ List.intercalate "\n  , " xs ++ "\n  ]"
  putStrLn $ "{-# OPAQUE " ++ listName ++ " #-}"

main :: IO ()
main = do
  g <- newIOGenM (mkStdGen 42)
  genCases "int8Constants" "Int8X16" "I8X16" "#Int8" [gen8 g] ([0, -1] :: [Int8])
  putStrLn ""
  genCases "int16Constants" "Int16X8" "I16X8" "#Int16" (gen16 g) ([0, -1] :: [Int16])
  putStrLn ""
  genCases "int32Constants" "Int32X4" "I32X4" "#Int32" (gen32 g) ([0, -1] :: [Int32])
  putStrLn ""
  genCases "int64Constants" "Int64X2" "I64X2" "#Int64" (gen64 g) ([0, -1] :: [Int64])
  putStrLn ""
  genCases "word8Constants" "Word8X16" "W8X16" "#Word8" [gen8 g] ([0, 0xff] :: [Word8])
  putStrLn ""
  genCases "word16Constants" "Word16X8" "W16X8" "#Word16" (gen16 g) ([0, 0xffff] :: [Word16])
  putStrLn ""
  genCases "word32Constants" "Word32X4" "W32X4" "#Word32" (gen32 g) ([0, 0xffff_ffff] :: [Word32])
  putStrLn ""
  genCases "word64Constants" "Word64X2" "W64X2" "#Word64" (gen64 g) ([0, 0xffff_ffff_ffff_ffff] :: [Word64])
-}
