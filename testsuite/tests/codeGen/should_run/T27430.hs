{-# LANGUAGE MagicHash #-}

import GHC.Exts
import Data.Bits
import GHC.Word

foreign import ccall unsafe "u64_to_u8" u64_to_u8 :: Word64 -> Word8
foreign import ccall unsafe "u64_to_u16" u64_to_u16 :: Word64 -> Word16
foreign import ccall unsafe "u64_to_u32" u64_to_u32 :: Word64 -> Word32

x :: Word64
x = 5

-- Those should give just x when truncated.
y8,y16,y32 :: Word64
y8 = setBit x 8
y16 = setBit x 16
y32 = setBit x 32

eq8 :: Word8 -> Word8 -> Int
eq8 (W8# a) (W8# b) = I# (eqWord8# a b)

eq16 :: Word16 -> Word16 -> Int
eq16 (W16# a) (W16# b) = I# (eqWord16# a b)

eq32 :: Word32 -> Word32 -> Int
eq32 (W32# a) (W32# b) = I# (eqWord32# a b)

{-# NOINLINE outline_eq8 #-}
outline_eq8 = eq8
{-# NOINLINE outline_eq16 #-}
outline_eq16 = eq16
{-# NOINLINE outline_eq32 #-}
outline_eq32 = eq32

main :: IO ()
main = do
  print (eq8 (u64_to_u8 x) (u64_to_u8 y8))
  print (eq16 (u64_to_u16 x) (u64_to_u16 y16))
  print (eq32 (u64_to_u32 x) (u64_to_u32 y32))

  print (outline_eq8 (u64_to_u8 x) (u64_to_u8 y8))
  print (outline_eq16 (u64_to_u16 x) (u64_to_u16 y16))
  print (outline_eq32 (u64_to_u32 x) (u64_to_u32 y32))
