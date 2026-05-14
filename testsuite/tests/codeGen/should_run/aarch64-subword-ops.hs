{-# LANGUAGE MagicHash #-}
module Main where

import GHC.Exts
import GHC.Word
import GHC.Int

-- Uses sub-word primops directly so that the NCG sees MO_Shl W8,
-- MO_U_Shr W8, MO_S_Shr W8 etc. (the Bits class widens to Word#/Int#).

-- NOINLINE to prevent constant folding.

-- MO_U_Shr W8/W16 variable shift
{-# NOINLINE ushrW8 #-}
ushrW8 :: Word8 -> Int -> Word8
ushrW8 (W8# w) (I# i) = W8# (uncheckedShiftRLWord8# w i)

{-# NOINLINE ushrW16 #-}
ushrW16 :: Word16 -> Int -> Word16
ushrW16 (W16# w) (I# i) = W16# (uncheckedShiftRLWord16# w i)

-- MO_S_Shr W8/W16 variable shift
{-# NOINLINE sshrI8 #-}
sshrI8 :: Int8 -> Int -> Int8
sshrI8 (I8# x) (I# i) = I8# (uncheckedShiftRAInt8# x i)

{-# NOINLINE sshrI16 #-}
sshrI16 :: Int16 -> Int -> Int16
sshrI16 (I16# x) (I# i) = I16# (uncheckedShiftRAInt16# x i)

-- MO_Shl W8/W16 variable shift
{-# NOINLINE shlW8 #-}
shlW8 :: Word8 -> Int -> Word8
shlW8 (W8# w) (I# i) = W8# (uncheckedShiftLWord8# w i)

{-# NOINLINE shlW16 #-}
shlW16 :: Word16 -> Int -> Word16
shlW16 (W16# w) (I# i) = W16# (uncheckedShiftLWord16# w i)

-- quot exercising MO_U_Quot W8/W16
{-# NOINLINE quotW8 #-}
quotW8 :: Word8 -> Word8 -> Word8
quotW8 (W8# x) (W8# y) = W8# (quotWord8# x y)

{-# NOINLINE quotW16 #-}
quotW16 :: Word16 -> Word16 -> Word16
quotW16 (W16# x) (W16# y) = W16# (quotWord16# x y)

-- Register clobbering: use a value both in a shift/quot and afterward.
-- If the sign/zero extension clobbers the source register, the second
-- use sees the wrong value.

{-# NOINLINE sshrAndAdd8 #-}
sshrAndAdd8 :: Int8 -> Int -> Int8
sshrAndAdd8 a n = sshrI8 a n + a

{-# NOINLINE sshrAndAdd16 #-}
sshrAndAdd16 :: Int16 -> Int -> Int16
sshrAndAdd16 a n = sshrI16 a n + a

{-# NOINLINE quotAndAdd8 #-}
quotAndAdd8 :: Word8 -> Word8 -> Word8
quotAndAdd8 a b = quotW8 a b + a + b

{-# NOINLINE quotAndAdd16 #-}
quotAndAdd16 :: Word16 -> Word16 -> Word16
quotAndAdd16 a b = quotW16 a b + a + b

main :: IO ()
main = do
    putStrLn "-- MO_U_Shr variable shift"
    print (ushrW8 0x80 1)     -- 64
    print (ushrW8 0xFF 4)     -- 15
    print (ushrW8 0x42 0)     -- 66
    print (ushrW16 0x8000 1)  -- 16384
    print (ushrW16 0xFFFF 8)  -- 255
    print (ushrW16 0x1234 0)  -- 4660

    putStrLn "-- MO_S_Shr variable shift"
    print (sshrI8 (-1) 1)      -- -1
    print (sshrI8 (-128) 1)    -- -64
    print (sshrI8 127 1)       -- 63
    print (sshrI8 0x42 3)      -- 8
    print (sshrI16 (-1) 1)     -- -1
    print (sshrI16 (-32768) 1) -- -16384
    print (sshrI16 32767 8)    -- 127

    putStrLn "-- MO_Shl variable shift"
    print (shlW8 0x01 0)    -- 1
    print (shlW8 0x01 4)    -- 16
    print (shlW8 0xFF 1)    -- 254
    print (shlW8 0x42 3)    -- 16
    print (shlW16 0x0001 0) -- 1
    print (shlW16 0x0001 8) -- 256
    print (shlW16 0xFFFF 1) -- 65534
    print (shlW16 0x1234 4) -- 9024

    putStrLn "-- MO_U_Quot"
    print (quotW8 255 10)      -- 25
    print (quotW8 200 7)       -- 28
    print (quotW8 1 1)         -- 1
    print (quotW16 65535 256)  -- 255
    print (quotW16 1000 3)     -- 333

    putStrLn "-- register clobbering: shift + reuse"
    print (sshrAndAdd8 (-128) 1)    -- 64  (wraps: -64 + -128 = -192 = 64 as Int8)
    print (sshrAndAdd8 0x42 1)      -- 99
    print (sshrAndAdd16 (-32768) 1) -- 16384  (wraps)
    print (sshrAndAdd16 0x1234 4)   -- 4951

    putStrLn "-- register clobbering: quot + reuse"
    print (quotAndAdd8 200 7)      -- 235
    print (quotAndAdd8 255 10)     -- 34  (wraps: 290 mod 256)
    print (quotAndAdd16 1000 3)    -- 1336
    print (quotAndAdd16 65535 256) -- 510  (wraps: 66046 mod 65536)
