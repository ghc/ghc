{-# LANGUAGE MagicHash, GHCForeignImportPrim, UnliftedFFITypes #-}

-- Each procedure of cmm_sinkoff_cmm.cmm sinks a local whose value is a sum
-- or difference with a constant into a register offset of it, compared
-- here with the same formula computed in Haskell over boundary values.

import Control.Monad
import Data.Bits
import Data.Word
import GHC.Exts
import Text.Printf

foreign import prim "s1_64_c1" s1_64_c1# :: Word# -> Word# -> Word#
foreign import prim "s1_64_c2" s1_64_c2# :: Word# -> Word# -> Word#
foreign import prim "s1_64_c3" s1_64_c3# :: Word# -> Word# -> Word#
foreign import prim "s1_64_c4" s1_64_c4# :: Word# -> Word# -> Word#
foreign import prim "s2_64_c1" s2_64_c1# :: Word# -> Word# -> Word#
foreign import prim "s2_64_c2" s2_64_c2# :: Word# -> Word# -> Word#
foreign import prim "s2_64_c3" s2_64_c3# :: Word# -> Word# -> Word#
foreign import prim "s2_64_c4" s2_64_c4# :: Word# -> Word# -> Word#
foreign import prim "s3_64_c1" s3_64_c1# :: Word# -> Word# -> Word#
foreign import prim "s3_64_c2" s3_64_c2# :: Word# -> Word# -> Word#
foreign import prim "s3_64_c3" s3_64_c3# :: Word# -> Word# -> Word#
foreign import prim "s3_64_c4" s3_64_c4# :: Word# -> Word# -> Word#
foreign import prim "s4_64_c1" s4_64_c1# :: Word# -> Word# -> Word#
foreign import prim "s4_64_c2" s4_64_c2# :: Word# -> Word# -> Word#
foreign import prim "s4_64_c3" s4_64_c3# :: Word# -> Word# -> Word#
foreign import prim "s4_64_c4" s4_64_c4# :: Word# -> Word# -> Word#
foreign import prim "s5_64_c1" s5_64_c1# :: Word# -> Word# -> Word#
foreign import prim "s5_64_c2" s5_64_c2# :: Word# -> Word# -> Word#
foreign import prim "s5_64_c3" s5_64_c3# :: Word# -> Word# -> Word#
foreign import prim "s5_64_c4" s5_64_c4# :: Word# -> Word# -> Word#
foreign import prim "s6_64_c1" s6_64_c1# :: Word# -> Word# -> Word#
foreign import prim "s6_64_c3" s6_64_c3# :: Word# -> Word# -> Word#
foreign import prim "s6_64_c4" s6_64_c4# :: Word# -> Word# -> Word#
foreign import prim "s1_32_c1" s1_32_c1# :: Word# -> Word# -> Word#
foreign import prim "s1_32_c2" s1_32_c2# :: Word# -> Word# -> Word#
foreign import prim "s1_32_c3" s1_32_c3# :: Word# -> Word# -> Word#
foreign import prim "s1_32_c4" s1_32_c4# :: Word# -> Word# -> Word#
foreign import prim "s2_32_c1" s2_32_c1# :: Word# -> Word# -> Word#
foreign import prim "s2_32_c2" s2_32_c2# :: Word# -> Word# -> Word#
foreign import prim "s2_32_c3" s2_32_c3# :: Word# -> Word# -> Word#
foreign import prim "s2_32_c4" s2_32_c4# :: Word# -> Word# -> Word#
foreign import prim "s3_32_c1" s3_32_c1# :: Word# -> Word# -> Word#
foreign import prim "s3_32_c2" s3_32_c2# :: Word# -> Word# -> Word#
foreign import prim "s3_32_c3" s3_32_c3# :: Word# -> Word# -> Word#
foreign import prim "s3_32_c4" s3_32_c4# :: Word# -> Word# -> Word#
foreign import prim "s4_32_c1" s4_32_c1# :: Word# -> Word# -> Word#
foreign import prim "s4_32_c2" s4_32_c2# :: Word# -> Word# -> Word#
foreign import prim "s4_32_c3" s4_32_c3# :: Word# -> Word# -> Word#
foreign import prim "s4_32_c4" s4_32_c4# :: Word# -> Word# -> Word#
foreign import prim "s5_32_c1" s5_32_c1# :: Word# -> Word# -> Word#
foreign import prim "s5_32_c2" s5_32_c2# :: Word# -> Word# -> Word#
foreign import prim "s5_32_c3" s5_32_c3# :: Word# -> Word# -> Word#
foreign import prim "s5_32_c4" s5_32_c4# :: Word# -> Word# -> Word#
foreign import prim "s6_32_c1" s6_32_c1# :: Word# -> Word# -> Word#
foreign import prim "s6_32_c3" s6_32_c3# :: Word# -> Word# -> Word#
foreign import prim "s6_32_c4" s6_32_c4# :: Word# -> Word# -> Word#
foreign import prim "s1_8_c1" s1_8_c1# :: Word# -> Word# -> Word#
foreign import prim "s1_8_c2" s1_8_c2# :: Word# -> Word# -> Word#
foreign import prim "s1_8_c3" s1_8_c3# :: Word# -> Word# -> Word#
foreign import prim "s1_8_c4" s1_8_c4# :: Word# -> Word# -> Word#
foreign import prim "s2_8_c1" s2_8_c1# :: Word# -> Word# -> Word#
foreign import prim "s2_8_c2" s2_8_c2# :: Word# -> Word# -> Word#
foreign import prim "s2_8_c3" s2_8_c3# :: Word# -> Word# -> Word#
foreign import prim "s2_8_c4" s2_8_c4# :: Word# -> Word# -> Word#
foreign import prim "s3_8_c1" s3_8_c1# :: Word# -> Word# -> Word#
foreign import prim "s3_8_c2" s3_8_c2# :: Word# -> Word# -> Word#
foreign import prim "s3_8_c3" s3_8_c3# :: Word# -> Word# -> Word#
foreign import prim "s3_8_c4" s3_8_c4# :: Word# -> Word# -> Word#
foreign import prim "s4_8_c1" s4_8_c1# :: Word# -> Word# -> Word#
foreign import prim "s4_8_c2" s4_8_c2# :: Word# -> Word# -> Word#
foreign import prim "s4_8_c3" s4_8_c3# :: Word# -> Word# -> Word#
foreign import prim "s4_8_c4" s4_8_c4# :: Word# -> Word# -> Word#
foreign import prim "s5_8_c1" s5_8_c1# :: Word# -> Word# -> Word#
foreign import prim "s5_8_c2" s5_8_c2# :: Word# -> Word# -> Word#
foreign import prim "s5_8_c3" s5_8_c3# :: Word# -> Word# -> Word#
foreign import prim "s5_8_c4" s5_8_c4# :: Word# -> Word# -> Word#
foreign import prim "s6_8_c1" s6_8_c1# :: Word# -> Word# -> Word#
foreign import prim "s6_8_c3" s6_8_c3# :: Word# -> Word# -> Word#
foreign import prim "s6_8_c4" s6_8_c4# :: Word# -> Word# -> Word#

un :: Word64 -> Word#
un w = case fromIntegral w of W# x -> x

wd :: Word# -> Word64
wd x = fromIntegral (W# x)

f2 :: (Word# -> Word# -> Word#) -> [Word64] -> Word64
f2 f [x, y] = wd (f (un x) (un y))
f2 _ _ = error "arity"

check :: String -> [[Word64]] -> ([Word64] -> Word64) -> ([Word64] -> Word64) -> IO Int
check name argss f r = do
  forM_ argss $ \a -> let { x = f a; y = r a } in
    when (x /= y) $ printf "%s %s: %016x, expected %016x\n" name (show a) x y
  return (length argss)

main :: IO ()
main = do
  ns <- sequence
    [ check "s1_64_c1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s1_64_c1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 1 :: Word64; k2 = 1 :: Word64 } in fromIntegral (((x `xor` y) - k1) + k2))
    , check "s1_64_c2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s1_64_c2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 9223372036854775807 :: Word64; k2 = 1 :: Word64 } in fromIntegral (((x `xor` y) - k1) + k2))
    , check "s1_64_c3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s1_64_c3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 9223372036854775808 :: Word64; k2 = 9223372036854775808 :: Word64 } in fromIntegral (((x `xor` y) - k1) + k2))
    , check "s1_64_c4" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s1_64_c4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 18446744073709551615 :: Word64; k2 = 2 :: Word64 } in fromIntegral (((x `xor` y) - k1) + k2))
    , check "s2_64_c1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s2_64_c1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 1 :: Word64; k2 = 1 :: Word64 } in fromIntegral (((x `xor` y) + k1) + k2))
    , check "s2_64_c2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s2_64_c2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 9223372036854775807 :: Word64; k2 = 1 :: Word64 } in fromIntegral (((x `xor` y) + k1) + k2))
    , check "s2_64_c3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s2_64_c3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 9223372036854775808 :: Word64; k2 = 9223372036854775808 :: Word64 } in fromIntegral (((x `xor` y) + k1) + k2))
    , check "s2_64_c4" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s2_64_c4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 18446744073709551615 :: Word64; k2 = 2 :: Word64 } in fromIntegral (((x `xor` y) + k1) + k2))
    , check "s3_64_c1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s3_64_c1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 1 :: Word64; k2 = 1 :: Word64 } in fromIntegral ((k1 - (x `xor` y)) + k2))
    , check "s3_64_c2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s3_64_c2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 9223372036854775807 :: Word64; k2 = 1 :: Word64 } in fromIntegral ((k1 - (x `xor` y)) + k2))
    , check "s3_64_c3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s3_64_c3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 9223372036854775808 :: Word64; k2 = 9223372036854775808 :: Word64 } in fromIntegral ((k1 - (x `xor` y)) + k2))
    , check "s3_64_c4" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s3_64_c4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 18446744073709551615 :: Word64; k2 = 2 :: Word64 } in fromIntegral ((k1 - (x `xor` y)) + k2))
    , check "s4_64_c1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s4_64_c1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 1 :: Word64; k2 = 1 :: Word64 } in fromIntegral (((x `xor` y) - k1) - k2))
    , check "s4_64_c2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s4_64_c2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 9223372036854775807 :: Word64; k2 = 1 :: Word64 } in fromIntegral (((x `xor` y) - k1) - k2))
    , check "s4_64_c3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s4_64_c3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 9223372036854775808 :: Word64; k2 = 9223372036854775808 :: Word64 } in fromIntegral (((x `xor` y) - k1) - k2))
    , check "s4_64_c4" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s4_64_c4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 18446744073709551615 :: Word64; k2 = 2 :: Word64 } in fromIntegral (((x `xor` y) - k1) - k2))
    , check "s5_64_c1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s5_64_c1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 1 :: Word64; k2 = 1 :: Word64 } in fromIntegral (((x - y) + k1) + k2))
    , check "s5_64_c2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s5_64_c2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 9223372036854775807 :: Word64; k2 = 1 :: Word64 } in fromIntegral (((x - y) + k1) + k2))
    , check "s5_64_c3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s5_64_c3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 9223372036854775808 :: Word64; k2 = 9223372036854775808 :: Word64 } in fromIntegral (((x - y) + k1) + k2))
    , check "s5_64_c4" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s5_64_c4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 18446744073709551615 :: Word64; k2 = 2 :: Word64 } in fromIntegral (((x - y) + k1) + k2))
    , check "s6_64_c1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s6_64_c1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 1 :: Word64; k2 = 1 :: Word64 } in fromIntegral ((x `xor` y) + k2))
    , check "s6_64_c3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s6_64_c3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 9223372036854775808 :: Word64; k2 = 9223372036854775808 :: Word64 } in fromIntegral ((x `xor` y) + k2))
    , check "s6_64_c4" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 s6_64_c4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; k1 = 18446744073709551615 :: Word64; k2 = 2 :: Word64 } in fromIntegral ((x `xor` y) + k2))
    , check "s1_32_c1" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s1_32_c1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 1 :: Word32; k2 = 1 :: Word32 } in fromIntegral (((x `xor` y) - k1) + k2))
    , check "s1_32_c2" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s1_32_c2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 2147483647 :: Word32; k2 = 1 :: Word32 } in fromIntegral (((x `xor` y) - k1) + k2))
    , check "s1_32_c3" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s1_32_c3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 2147483648 :: Word32; k2 = 2147483648 :: Word32 } in fromIntegral (((x `xor` y) - k1) + k2))
    , check "s1_32_c4" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s1_32_c4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 4294967295 :: Word32; k2 = 2 :: Word32 } in fromIntegral (((x `xor` y) - k1) + k2))
    , check "s2_32_c1" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s2_32_c1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 1 :: Word32; k2 = 1 :: Word32 } in fromIntegral (((x `xor` y) + k1) + k2))
    , check "s2_32_c2" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s2_32_c2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 2147483647 :: Word32; k2 = 1 :: Word32 } in fromIntegral (((x `xor` y) + k1) + k2))
    , check "s2_32_c3" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s2_32_c3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 2147483648 :: Word32; k2 = 2147483648 :: Word32 } in fromIntegral (((x `xor` y) + k1) + k2))
    , check "s2_32_c4" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s2_32_c4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 4294967295 :: Word32; k2 = 2 :: Word32 } in fromIntegral (((x `xor` y) + k1) + k2))
    , check "s3_32_c1" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s3_32_c1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 1 :: Word32; k2 = 1 :: Word32 } in fromIntegral ((k1 - (x `xor` y)) + k2))
    , check "s3_32_c2" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s3_32_c2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 2147483647 :: Word32; k2 = 1 :: Word32 } in fromIntegral ((k1 - (x `xor` y)) + k2))
    , check "s3_32_c3" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s3_32_c3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 2147483648 :: Word32; k2 = 2147483648 :: Word32 } in fromIntegral ((k1 - (x `xor` y)) + k2))
    , check "s3_32_c4" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s3_32_c4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 4294967295 :: Word32; k2 = 2 :: Word32 } in fromIntegral ((k1 - (x `xor` y)) + k2))
    , check "s4_32_c1" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s4_32_c1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 1 :: Word32; k2 = 1 :: Word32 } in fromIntegral (((x `xor` y) - k1) - k2))
    , check "s4_32_c2" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s4_32_c2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 2147483647 :: Word32; k2 = 1 :: Word32 } in fromIntegral (((x `xor` y) - k1) - k2))
    , check "s4_32_c3" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s4_32_c3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 2147483648 :: Word32; k2 = 2147483648 :: Word32 } in fromIntegral (((x `xor` y) - k1) - k2))
    , check "s4_32_c4" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s4_32_c4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 4294967295 :: Word32; k2 = 2 :: Word32 } in fromIntegral (((x `xor` y) - k1) - k2))
    , check "s5_32_c1" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s5_32_c1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 1 :: Word32; k2 = 1 :: Word32 } in fromIntegral (((x - y) + k1) + k2))
    , check "s5_32_c2" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s5_32_c2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 2147483647 :: Word32; k2 = 1 :: Word32 } in fromIntegral (((x - y) + k1) + k2))
    , check "s5_32_c3" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s5_32_c3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 2147483648 :: Word32; k2 = 2147483648 :: Word32 } in fromIntegral (((x - y) + k1) + k2))
    , check "s5_32_c4" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s5_32_c4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 4294967295 :: Word32; k2 = 2 :: Word32 } in fromIntegral (((x - y) + k1) + k2))
    , check "s6_32_c1" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s6_32_c1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 1 :: Word32; k2 = 1 :: Word32 } in fromIntegral ((x `xor` y) + k2))
    , check "s6_32_c3" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s6_32_c3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 2147483648 :: Word32; k2 = 2147483648 :: Word32 } in fromIntegral ((x `xor` y) + k2))
    , check "s6_32_c4" (replicateM 2 [0, 1, 2147483647, 2147483648, 4294967295]) (f2 s6_32_c4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word32; y = fromIntegral a1 :: Word32; k1 = 4294967295 :: Word32; k2 = 2 :: Word32 } in fromIntegral ((x `xor` y) + k2))
    , check "s1_8_c1" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s1_8_c1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 1 :: Word8; k2 = 1 :: Word8 } in fromIntegral (((x `xor` y) - k1) + k2))
    , check "s1_8_c2" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s1_8_c2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 127 :: Word8; k2 = 1 :: Word8 } in fromIntegral (((x `xor` y) - k1) + k2))
    , check "s1_8_c3" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s1_8_c3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 128 :: Word8; k2 = 128 :: Word8 } in fromIntegral (((x `xor` y) - k1) + k2))
    , check "s1_8_c4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s1_8_c4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 255 :: Word8; k2 = 2 :: Word8 } in fromIntegral (((x `xor` y) - k1) + k2))
    , check "s2_8_c1" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s2_8_c1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 1 :: Word8; k2 = 1 :: Word8 } in fromIntegral (((x `xor` y) + k1) + k2))
    , check "s2_8_c2" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s2_8_c2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 127 :: Word8; k2 = 1 :: Word8 } in fromIntegral (((x `xor` y) + k1) + k2))
    , check "s2_8_c3" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s2_8_c3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 128 :: Word8; k2 = 128 :: Word8 } in fromIntegral (((x `xor` y) + k1) + k2))
    , check "s2_8_c4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s2_8_c4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 255 :: Word8; k2 = 2 :: Word8 } in fromIntegral (((x `xor` y) + k1) + k2))
    , check "s3_8_c1" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s3_8_c1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 1 :: Word8; k2 = 1 :: Word8 } in fromIntegral ((k1 - (x `xor` y)) + k2))
    , check "s3_8_c2" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s3_8_c2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 127 :: Word8; k2 = 1 :: Word8 } in fromIntegral ((k1 - (x `xor` y)) + k2))
    , check "s3_8_c3" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s3_8_c3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 128 :: Word8; k2 = 128 :: Word8 } in fromIntegral ((k1 - (x `xor` y)) + k2))
    , check "s3_8_c4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s3_8_c4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 255 :: Word8; k2 = 2 :: Word8 } in fromIntegral ((k1 - (x `xor` y)) + k2))
    , check "s4_8_c1" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s4_8_c1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 1 :: Word8; k2 = 1 :: Word8 } in fromIntegral (((x `xor` y) - k1) - k2))
    , check "s4_8_c2" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s4_8_c2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 127 :: Word8; k2 = 1 :: Word8 } in fromIntegral (((x `xor` y) - k1) - k2))
    , check "s4_8_c3" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s4_8_c3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 128 :: Word8; k2 = 128 :: Word8 } in fromIntegral (((x `xor` y) - k1) - k2))
    , check "s4_8_c4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s4_8_c4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 255 :: Word8; k2 = 2 :: Word8 } in fromIntegral (((x `xor` y) - k1) - k2))
    , check "s5_8_c1" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s5_8_c1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 1 :: Word8; k2 = 1 :: Word8 } in fromIntegral (((x - y) + k1) + k2))
    , check "s5_8_c2" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s5_8_c2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 127 :: Word8; k2 = 1 :: Word8 } in fromIntegral (((x - y) + k1) + k2))
    , check "s5_8_c3" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s5_8_c3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 128 :: Word8; k2 = 128 :: Word8 } in fromIntegral (((x - y) + k1) + k2))
    , check "s5_8_c4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s5_8_c4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 255 :: Word8; k2 = 2 :: Word8 } in fromIntegral (((x - y) + k1) + k2))
    , check "s6_8_c1" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s6_8_c1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 1 :: Word8; k2 = 1 :: Word8 } in fromIntegral ((x `xor` y) + k2))
    , check "s6_8_c3" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s6_8_c3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 128 :: Word8; k2 = 128 :: Word8 } in fromIntegral ((x `xor` y) + k2))
    , check "s6_8_c4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 s6_8_c4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; k1 = 255 :: Word8; k2 = 2 :: Word8 } in fromIntegral ((x `xor` y) + k2))
    ]
  printf "%d procedures, %d calls\n" (length ns) (sum ns)
