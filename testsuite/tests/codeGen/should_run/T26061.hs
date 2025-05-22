{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ExtendedLiterals #-}
import GHC.Word
import GHC.Exts

f :: Int16# -> Word16#
f x = let !w = int16ToWord16# (x `uncheckedShiftRAInt16#` 1#)
      in w `remWord16#` 13#Word16
{-# NOINLINE f #-}

g :: Int8# -> Word8#
g x = let !w = int8ToWord8# (x `uncheckedShiftRAInt8#` 1#)
      in w `remWord8#` 19#Word8
{-# NOINLINE g #-}

h :: Int16# -> Int# -> Word16#
h x y = let !w = int16ToWord16# (x `uncheckedShiftRAInt16#` y)
      in w `remWord16#` 13#Word16
{-# NOINLINE h #-}

i :: Int8# -> Int# -> Word8#
i x y = let !w = int8ToWord8# (x `uncheckedShiftRAInt8#` y)
        in w `remWord8#` 19#Word8
{-# NOINLINE i #-}

main :: IO ()
main = do
  print (W16# (f (-100#Int16)))
  print (W8# (g (-100#Int8)))
  print (W16# (h (-100#Int16) 1#))
  print (W8# (i (-100#Int8) 1#))

-- int16ToWord16 (-100 `shiftR` 1) `rem` 13
--   = int16ToWord16 (-50) `rem` 13
--   = 65486 `rem` 13
--   = 5

-- int8ToWord8 (-100 `shiftR` 1) `rem` 19
--   = int8ToWord8 (-50) `rem` 19
--   = 206 `rem` 19
--   = 16
