{-# LANGUAGE MagicHash, GHCForeignImportPrim, UnliftedFFITypes #-}

-- Each procedure of cmm_scaledlit_cmm.cmm but ta, which returns lbl, loads or
-- stores at (i << s) + lbl, or computes (x << s) + k, with no base; compared
-- here with the same formula computed in Haskell.

import Control.Monad
import Data.Bits
import Data.Word
import GHC.Exts
import Text.Printf

foreign import prim "t64" t64# :: Word# -> Word#
foreign import prim "t64p" t64p# :: Word# -> Word#
foreign import prim "t64m" t64m# :: Word# -> Word#
foreign import prim "t32" t32# :: Word# -> Word#
foreign import prim "t16" t16# :: Word# -> Word#
foreign import prim "t8" t8# :: Word# -> Word#
foreign import prim "t32b" t32b# :: Word# -> Word#
foreign import prim "ts64" ts64# :: Word# -> Word# -> Word#
foreign import prim "ts32" ts32# :: Word# -> Word# -> Word#
foreign import prim "tl" tl# :: Word# -> Word#
foreign import prim "ta" ta# :: Word# -> Word#
foreign import prim "v64_s0_k0" v64_s0_k0# :: Word# -> Word#
foreign import prim "v64_s0_k1" v64_s0_k1# :: Word# -> Word#
foreign import prim "v64_s0_k2" v64_s0_k2# :: Word# -> Word#
foreign import prim "v64_s0_k3" v64_s0_k3# :: Word# -> Word#
foreign import prim "v64_s0_k4" v64_s0_k4# :: Word# -> Word#
foreign import prim "v64_s0_k5" v64_s0_k5# :: Word# -> Word#
foreign import prim "v64_s0_k6" v64_s0_k6# :: Word# -> Word#
foreign import prim "v64_s0_k7" v64_s0_k7# :: Word# -> Word#
foreign import prim "v64_s1_k0" v64_s1_k0# :: Word# -> Word#
foreign import prim "v64_s1_k1" v64_s1_k1# :: Word# -> Word#
foreign import prim "v64_s1_k2" v64_s1_k2# :: Word# -> Word#
foreign import prim "v64_s1_k3" v64_s1_k3# :: Word# -> Word#
foreign import prim "v64_s1_k4" v64_s1_k4# :: Word# -> Word#
foreign import prim "v64_s1_k5" v64_s1_k5# :: Word# -> Word#
foreign import prim "v64_s1_k6" v64_s1_k6# :: Word# -> Word#
foreign import prim "v64_s1_k7" v64_s1_k7# :: Word# -> Word#
foreign import prim "v64_s2_k0" v64_s2_k0# :: Word# -> Word#
foreign import prim "v64_s2_k1" v64_s2_k1# :: Word# -> Word#
foreign import prim "v64_s2_k2" v64_s2_k2# :: Word# -> Word#
foreign import prim "v64_s2_k3" v64_s2_k3# :: Word# -> Word#
foreign import prim "v64_s2_k4" v64_s2_k4# :: Word# -> Word#
foreign import prim "v64_s2_k5" v64_s2_k5# :: Word# -> Word#
foreign import prim "v64_s2_k6" v64_s2_k6# :: Word# -> Word#
foreign import prim "v64_s2_k7" v64_s2_k7# :: Word# -> Word#
foreign import prim "v64_s3_k0" v64_s3_k0# :: Word# -> Word#
foreign import prim "v64_s3_k1" v64_s3_k1# :: Word# -> Word#
foreign import prim "v64_s3_k2" v64_s3_k2# :: Word# -> Word#
foreign import prim "v64_s3_k3" v64_s3_k3# :: Word# -> Word#
foreign import prim "v64_s3_k4" v64_s3_k4# :: Word# -> Word#
foreign import prim "v64_s3_k5" v64_s3_k5# :: Word# -> Word#
foreign import prim "v64_s3_k6" v64_s3_k6# :: Word# -> Word#
foreign import prim "v64_s3_k7" v64_s3_k7# :: Word# -> Word#
foreign import prim "v64_s4_k0" v64_s4_k0# :: Word# -> Word#
foreign import prim "v64_s4_k1" v64_s4_k1# :: Word# -> Word#
foreign import prim "v64_s4_k2" v64_s4_k2# :: Word# -> Word#
foreign import prim "v64_s4_k3" v64_s4_k3# :: Word# -> Word#
foreign import prim "v64_s4_k4" v64_s4_k4# :: Word# -> Word#
foreign import prim "v64_s4_k5" v64_s4_k5# :: Word# -> Word#
foreign import prim "v64_s4_k6" v64_s4_k6# :: Word# -> Word#
foreign import prim "v64_s4_k7" v64_s4_k7# :: Word# -> Word#
foreign import prim "v32_s0_k0" v32_s0_k0# :: Word# -> Word#
foreign import prim "v32_s0_k1" v32_s0_k1# :: Word# -> Word#
foreign import prim "v32_s0_k2" v32_s0_k2# :: Word# -> Word#
foreign import prim "v32_s0_k3" v32_s0_k3# :: Word# -> Word#
foreign import prim "v32_s1_k0" v32_s1_k0# :: Word# -> Word#
foreign import prim "v32_s1_k1" v32_s1_k1# :: Word# -> Word#
foreign import prim "v32_s1_k2" v32_s1_k2# :: Word# -> Word#
foreign import prim "v32_s1_k3" v32_s1_k3# :: Word# -> Word#
foreign import prim "v32_s2_k0" v32_s2_k0# :: Word# -> Word#
foreign import prim "v32_s2_k1" v32_s2_k1# :: Word# -> Word#
foreign import prim "v32_s2_k2" v32_s2_k2# :: Word# -> Word#
foreign import prim "v32_s2_k3" v32_s2_k3# :: Word# -> Word#
foreign import prim "v32_s3_k0" v32_s3_k0# :: Word# -> Word#
foreign import prim "v32_s3_k1" v32_s3_k1# :: Word# -> Word#
foreign import prim "v32_s3_k2" v32_s3_k2# :: Word# -> Word#
foreign import prim "v32_s3_k3" v32_s3_k3# :: Word# -> Word#
foreign import prim "v32_s4_k0" v32_s4_k0# :: Word# -> Word#
foreign import prim "v32_s4_k1" v32_s4_k1# :: Word# -> Word#
foreign import prim "v32_s4_k2" v32_s4_k2# :: Word# -> Word#
foreign import prim "v32_s4_k3" v32_s4_k3# :: Word# -> Word#
foreign import prim "v16_s0_k0" v16_s0_k0# :: Word# -> Word#
foreign import prim "v16_s0_k1" v16_s0_k1# :: Word# -> Word#
foreign import prim "v16_s0_k2" v16_s0_k2# :: Word# -> Word#
foreign import prim "v16_s0_k3" v16_s0_k3# :: Word# -> Word#
foreign import prim "v16_s1_k0" v16_s1_k0# :: Word# -> Word#
foreign import prim "v16_s1_k1" v16_s1_k1# :: Word# -> Word#
foreign import prim "v16_s1_k2" v16_s1_k2# :: Word# -> Word#
foreign import prim "v16_s1_k3" v16_s1_k3# :: Word# -> Word#
foreign import prim "v16_s2_k0" v16_s2_k0# :: Word# -> Word#
foreign import prim "v16_s2_k1" v16_s2_k1# :: Word# -> Word#
foreign import prim "v16_s2_k2" v16_s2_k2# :: Word# -> Word#
foreign import prim "v16_s2_k3" v16_s2_k3# :: Word# -> Word#
foreign import prim "v16_s3_k0" v16_s3_k0# :: Word# -> Word#
foreign import prim "v16_s3_k1" v16_s3_k1# :: Word# -> Word#
foreign import prim "v16_s3_k2" v16_s3_k2# :: Word# -> Word#
foreign import prim "v16_s3_k3" v16_s3_k3# :: Word# -> Word#
foreign import prim "v16_s4_k0" v16_s4_k0# :: Word# -> Word#
foreign import prim "v16_s4_k1" v16_s4_k1# :: Word# -> Word#
foreign import prim "v16_s4_k2" v16_s4_k2# :: Word# -> Word#
foreign import prim "v16_s4_k3" v16_s4_k3# :: Word# -> Word#
foreign import prim "v8_s0_k0" v8_s0_k0# :: Word# -> Word#
foreign import prim "v8_s0_k1" v8_s0_k1# :: Word# -> Word#
foreign import prim "v8_s0_k2" v8_s0_k2# :: Word# -> Word#
foreign import prim "v8_s1_k0" v8_s1_k0# :: Word# -> Word#
foreign import prim "v8_s1_k1" v8_s1_k1# :: Word# -> Word#
foreign import prim "v8_s1_k2" v8_s1_k2# :: Word# -> Word#
foreign import prim "v8_s2_k0" v8_s2_k0# :: Word# -> Word#
foreign import prim "v8_s2_k1" v8_s2_k1# :: Word# -> Word#
foreign import prim "v8_s2_k2" v8_s2_k2# :: Word# -> Word#
foreign import prim "v8_s3_k0" v8_s3_k0# :: Word# -> Word#
foreign import prim "v8_s3_k1" v8_s3_k1# :: Word# -> Word#
foreign import prim "v8_s3_k2" v8_s3_k2# :: Word# -> Word#
foreign import prim "v8_s4_k0" v8_s4_k0# :: Word# -> Word#
foreign import prim "v8_s4_k1" v8_s4_k1# :: Word# -> Word#
foreign import prim "v8_s4_k2" v8_s4_k2# :: Word# -> Word#

un :: Word64 -> Word#
un w = case fromIntegral w of W# x -> x

wd :: Word# -> Word64
wd x = fromIntegral (W# x)

f1 :: (Word# -> Word#) -> [Word64] -> Word64
f1 f [x] = wd (f (un x))
f1 _ _ = error "arity"

f2 :: (Word# -> Word# -> Word#) -> [Word64] -> Word64
f2 f [x, y] = wd (f (un x) (un y))
f2 _ _ = error "arity"

-- the little-endian n-byte word at byte offset b of the table
tblAt :: Int -> Word64 -> Word64
tblAt n b = foldr (\j acc -> acc `shiftL` 8 .|. byte (fromIntegral b + j)) 0 [0 .. n - 1]
  where byte o = (elems !! (o `div` 8) `shiftR` (8 * (o `mod` 8))) .&. 0xff
        elems = [0x0102030405060708 * (e + 1) | e <- [0 .. 15]] :: [Word64]

-- the number of calls, each mismatch printed
check :: String -> [[Word64]] -> ([Word64] -> Word64) -> ([Word64] -> Word64) -> IO Int
check name argss f r = do
  forM_ argss $ \a -> let { x = f a; y = r a } in
    when (x /= y) $ printf "%s %s: %016x, expected %016x\n" name (show a) x y
  return (length argss)

main :: IO ()
main = do
  ns <- sequence
    [ check "t64" [[i] | i <- [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]] (f1 t64#) (\[i] -> tblAt 8 (i * 8 + (0)))
    , check "t64p" [[i] | i <- [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]] (f1 t64p#) (\[i] -> tblAt 8 (i * 8 + (8)))
    , check "t64m" [[i] | i <- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]] (f1 t64m#) (\[i] -> tblAt 8 (i * 8 + (-8)))
    , check "t32" [[i] | i <- [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]] (f1 t32#) (\[i] -> tblAt 4 (i * 4 + (0)))
    , check "t16" [[i] | i <- [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63]] (f1 t16#) (\[i] -> tblAt 2 (i * 2 + (0)))
    , check "t8" [[i] | i <- [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127]] (f1 t8#) (\[i] -> tblAt 1 (i * 1 + (0)))
    , check "t32b" [[i] | i <- [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]] (f1 t32b#) (\[i] -> tblAt 4 (i * 8 + (4)))
    , check "ts64" [[i, 0xdead000000000000 + i] | i <- [0 .. 15]] (f2 ts64#) (\[_, v] -> v)
    , check "ts32" [[i, 0xffffffff00000000 + 0xbeef0000 + i] | i <- [0 .. 14]] (f2 ts32#) (\[_, v] -> v .&. 0xffffffff)
    , check "tl" [[i] | i <- [0 .. 15]] (f1 tl#) (\[i] -> wd (ta# (un 0)) + 8 * i)
    , check "v64_s0_k0" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s0_k0#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 0) + 1))
    , check "v64_s0_k1" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s0_k1#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 0) - 1))
    , check "v64_s0_k2" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s0_k2#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 0) + 4))
    , check "v64_s0_k3" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s0_k3#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 0) - 3))
    , check "v64_s0_k4" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s0_k4#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 0) + 2147483647))
    , check "v64_s0_k5" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s0_k5#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 0) - 2147483648))
    , check "v64_s0_k6" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s0_k6#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 0) + 2147483648))
    , check "v64_s0_k7" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s0_k7#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 0) - 2147483649))
    , check "v64_s1_k0" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s1_k0#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 1) + 1))
    , check "v64_s1_k1" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s1_k1#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 1) - 1))
    , check "v64_s1_k2" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s1_k2#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 1) + 4))
    , check "v64_s1_k3" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s1_k3#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 1) - 3))
    , check "v64_s1_k4" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s1_k4#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 1) + 2147483647))
    , check "v64_s1_k5" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s1_k5#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 1) - 2147483648))
    , check "v64_s1_k6" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s1_k6#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 1) + 2147483648))
    , check "v64_s1_k7" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s1_k7#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 1) - 2147483649))
    , check "v64_s2_k0" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s2_k0#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 2) + 1))
    , check "v64_s2_k1" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s2_k1#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 2) - 1))
    , check "v64_s2_k2" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s2_k2#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 2) + 4))
    , check "v64_s2_k3" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s2_k3#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 2) - 3))
    , check "v64_s2_k4" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s2_k4#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 2) + 2147483647))
    , check "v64_s2_k5" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s2_k5#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 2) - 2147483648))
    , check "v64_s2_k6" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s2_k6#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 2) + 2147483648))
    , check "v64_s2_k7" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s2_k7#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 2) - 2147483649))
    , check "v64_s3_k0" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s3_k0#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 3) + 1))
    , check "v64_s3_k1" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s3_k1#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 3) - 1))
    , check "v64_s3_k2" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s3_k2#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 3) + 4))
    , check "v64_s3_k3" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s3_k3#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 3) - 3))
    , check "v64_s3_k4" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s3_k4#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 3) + 2147483647))
    , check "v64_s3_k5" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s3_k5#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 3) - 2147483648))
    , check "v64_s3_k6" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s3_k6#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 3) + 2147483648))
    , check "v64_s3_k7" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s3_k7#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 3) - 2147483649))
    , check "v64_s4_k0" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s4_k0#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 4) + 1))
    , check "v64_s4_k1" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s4_k1#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 4) - 1))
    , check "v64_s4_k2" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s4_k2#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 4) + 4))
    , check "v64_s4_k3" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s4_k3#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 4) - 3))
    , check "v64_s4_k4" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s4_k4#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 4) + 2147483647))
    , check "v64_s4_k5" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s4_k5#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 4) - 2147483648))
    , check "v64_s4_k6" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s4_k6#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 4) + 2147483648))
    , check "v64_s4_k7" (map pure [0, 1, 2, 1152921504606846975, 1152921504606846976, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 v64_s4_k7#)
      (\[a] -> let x = fromIntegral a :: Word64 in fromIntegral ((x `shiftL` 4) - 2147483649))
    , check "v32_s0_k0" (map pure [0, 1, 2, 268435455, 268435456, 2147483647, 2147483648, 4294967295]) (f1 v32_s0_k0#)
      (\[a] -> let x = fromIntegral a :: Word32 in fromIntegral ((x `shiftL` 0) + 1))
    , check "v32_s0_k1" (map pure [0, 1, 2, 268435455, 268435456, 2147483647, 2147483648, 4294967295]) (f1 v32_s0_k1#)
      (\[a] -> let x = fromIntegral a :: Word32 in fromIntegral ((x `shiftL` 0) - 1))
    , check "v32_s0_k2" (map pure [0, 1, 2, 268435455, 268435456, 2147483647, 2147483648, 4294967295]) (f1 v32_s0_k2#)
      (\[a] -> let x = fromIntegral a :: Word32 in fromIntegral ((x `shiftL` 0) + 2147483647))
    , check "v32_s0_k3" (map pure [0, 1, 2, 268435455, 268435456, 2147483647, 2147483648, 4294967295]) (f1 v32_s0_k3#)
      (\[a] -> let x = fromIntegral a :: Word32 in fromIntegral ((x `shiftL` 0) - 2147483648))
    , check "v32_s1_k0" (map pure [0, 1, 2, 268435455, 268435456, 2147483647, 2147483648, 4294967295]) (f1 v32_s1_k0#)
      (\[a] -> let x = fromIntegral a :: Word32 in fromIntegral ((x `shiftL` 1) + 1))
    , check "v32_s1_k1" (map pure [0, 1, 2, 268435455, 268435456, 2147483647, 2147483648, 4294967295]) (f1 v32_s1_k1#)
      (\[a] -> let x = fromIntegral a :: Word32 in fromIntegral ((x `shiftL` 1) - 1))
    , check "v32_s1_k2" (map pure [0, 1, 2, 268435455, 268435456, 2147483647, 2147483648, 4294967295]) (f1 v32_s1_k2#)
      (\[a] -> let x = fromIntegral a :: Word32 in fromIntegral ((x `shiftL` 1) + 2147483647))
    , check "v32_s1_k3" (map pure [0, 1, 2, 268435455, 268435456, 2147483647, 2147483648, 4294967295]) (f1 v32_s1_k3#)
      (\[a] -> let x = fromIntegral a :: Word32 in fromIntegral ((x `shiftL` 1) - 2147483648))
    , check "v32_s2_k0" (map pure [0, 1, 2, 268435455, 268435456, 2147483647, 2147483648, 4294967295]) (f1 v32_s2_k0#)
      (\[a] -> let x = fromIntegral a :: Word32 in fromIntegral ((x `shiftL` 2) + 1))
    , check "v32_s2_k1" (map pure [0, 1, 2, 268435455, 268435456, 2147483647, 2147483648, 4294967295]) (f1 v32_s2_k1#)
      (\[a] -> let x = fromIntegral a :: Word32 in fromIntegral ((x `shiftL` 2) - 1))
    , check "v32_s2_k2" (map pure [0, 1, 2, 268435455, 268435456, 2147483647, 2147483648, 4294967295]) (f1 v32_s2_k2#)
      (\[a] -> let x = fromIntegral a :: Word32 in fromIntegral ((x `shiftL` 2) + 2147483647))
    , check "v32_s2_k3" (map pure [0, 1, 2, 268435455, 268435456, 2147483647, 2147483648, 4294967295]) (f1 v32_s2_k3#)
      (\[a] -> let x = fromIntegral a :: Word32 in fromIntegral ((x `shiftL` 2) - 2147483648))
    , check "v32_s3_k0" (map pure [0, 1, 2, 268435455, 268435456, 2147483647, 2147483648, 4294967295]) (f1 v32_s3_k0#)
      (\[a] -> let x = fromIntegral a :: Word32 in fromIntegral ((x `shiftL` 3) + 1))
    , check "v32_s3_k1" (map pure [0, 1, 2, 268435455, 268435456, 2147483647, 2147483648, 4294967295]) (f1 v32_s3_k1#)
      (\[a] -> let x = fromIntegral a :: Word32 in fromIntegral ((x `shiftL` 3) - 1))
    , check "v32_s3_k2" (map pure [0, 1, 2, 268435455, 268435456, 2147483647, 2147483648, 4294967295]) (f1 v32_s3_k2#)
      (\[a] -> let x = fromIntegral a :: Word32 in fromIntegral ((x `shiftL` 3) + 2147483647))
    , check "v32_s3_k3" (map pure [0, 1, 2, 268435455, 268435456, 2147483647, 2147483648, 4294967295]) (f1 v32_s3_k3#)
      (\[a] -> let x = fromIntegral a :: Word32 in fromIntegral ((x `shiftL` 3) - 2147483648))
    , check "v32_s4_k0" (map pure [0, 1, 2, 268435455, 268435456, 2147483647, 2147483648, 4294967295]) (f1 v32_s4_k0#)
      (\[a] -> let x = fromIntegral a :: Word32 in fromIntegral ((x `shiftL` 4) + 1))
    , check "v32_s4_k1" (map pure [0, 1, 2, 268435455, 268435456, 2147483647, 2147483648, 4294967295]) (f1 v32_s4_k1#)
      (\[a] -> let x = fromIntegral a :: Word32 in fromIntegral ((x `shiftL` 4) - 1))
    , check "v32_s4_k2" (map pure [0, 1, 2, 268435455, 268435456, 2147483647, 2147483648, 4294967295]) (f1 v32_s4_k2#)
      (\[a] -> let x = fromIntegral a :: Word32 in fromIntegral ((x `shiftL` 4) + 2147483647))
    , check "v32_s4_k3" (map pure [0, 1, 2, 268435455, 268435456, 2147483647, 2147483648, 4294967295]) (f1 v32_s4_k3#)
      (\[a] -> let x = fromIntegral a :: Word32 in fromIntegral ((x `shiftL` 4) - 2147483648))
    , check "v16_s0_k0" (map pure [0, 1, 2, 4095, 4096, 32767, 32768, 65535]) (f1 v16_s0_k0#)
      (\[a] -> let x = fromIntegral a :: Word16 in fromIntegral ((x `shiftL` 0) + 1))
    , check "v16_s0_k1" (map pure [0, 1, 2, 4095, 4096, 32767, 32768, 65535]) (f1 v16_s0_k1#)
      (\[a] -> let x = fromIntegral a :: Word16 in fromIntegral ((x `shiftL` 0) - 1))
    , check "v16_s0_k2" (map pure [0, 1, 2, 4095, 4096, 32767, 32768, 65535]) (f1 v16_s0_k2#)
      (\[a] -> let x = fromIntegral a :: Word16 in fromIntegral ((x `shiftL` 0) + 32767))
    , check "v16_s0_k3" (map pure [0, 1, 2, 4095, 4096, 32767, 32768, 65535]) (f1 v16_s0_k3#)
      (\[a] -> let x = fromIntegral a :: Word16 in fromIntegral ((x `shiftL` 0) - 32768))
    , check "v16_s1_k0" (map pure [0, 1, 2, 4095, 4096, 32767, 32768, 65535]) (f1 v16_s1_k0#)
      (\[a] -> let x = fromIntegral a :: Word16 in fromIntegral ((x `shiftL` 1) + 1))
    , check "v16_s1_k1" (map pure [0, 1, 2, 4095, 4096, 32767, 32768, 65535]) (f1 v16_s1_k1#)
      (\[a] -> let x = fromIntegral a :: Word16 in fromIntegral ((x `shiftL` 1) - 1))
    , check "v16_s1_k2" (map pure [0, 1, 2, 4095, 4096, 32767, 32768, 65535]) (f1 v16_s1_k2#)
      (\[a] -> let x = fromIntegral a :: Word16 in fromIntegral ((x `shiftL` 1) + 32767))
    , check "v16_s1_k3" (map pure [0, 1, 2, 4095, 4096, 32767, 32768, 65535]) (f1 v16_s1_k3#)
      (\[a] -> let x = fromIntegral a :: Word16 in fromIntegral ((x `shiftL` 1) - 32768))
    , check "v16_s2_k0" (map pure [0, 1, 2, 4095, 4096, 32767, 32768, 65535]) (f1 v16_s2_k0#)
      (\[a] -> let x = fromIntegral a :: Word16 in fromIntegral ((x `shiftL` 2) + 1))
    , check "v16_s2_k1" (map pure [0, 1, 2, 4095, 4096, 32767, 32768, 65535]) (f1 v16_s2_k1#)
      (\[a] -> let x = fromIntegral a :: Word16 in fromIntegral ((x `shiftL` 2) - 1))
    , check "v16_s2_k2" (map pure [0, 1, 2, 4095, 4096, 32767, 32768, 65535]) (f1 v16_s2_k2#)
      (\[a] -> let x = fromIntegral a :: Word16 in fromIntegral ((x `shiftL` 2) + 32767))
    , check "v16_s2_k3" (map pure [0, 1, 2, 4095, 4096, 32767, 32768, 65535]) (f1 v16_s2_k3#)
      (\[a] -> let x = fromIntegral a :: Word16 in fromIntegral ((x `shiftL` 2) - 32768))
    , check "v16_s3_k0" (map pure [0, 1, 2, 4095, 4096, 32767, 32768, 65535]) (f1 v16_s3_k0#)
      (\[a] -> let x = fromIntegral a :: Word16 in fromIntegral ((x `shiftL` 3) + 1))
    , check "v16_s3_k1" (map pure [0, 1, 2, 4095, 4096, 32767, 32768, 65535]) (f1 v16_s3_k1#)
      (\[a] -> let x = fromIntegral a :: Word16 in fromIntegral ((x `shiftL` 3) - 1))
    , check "v16_s3_k2" (map pure [0, 1, 2, 4095, 4096, 32767, 32768, 65535]) (f1 v16_s3_k2#)
      (\[a] -> let x = fromIntegral a :: Word16 in fromIntegral ((x `shiftL` 3) + 32767))
    , check "v16_s3_k3" (map pure [0, 1, 2, 4095, 4096, 32767, 32768, 65535]) (f1 v16_s3_k3#)
      (\[a] -> let x = fromIntegral a :: Word16 in fromIntegral ((x `shiftL` 3) - 32768))
    , check "v16_s4_k0" (map pure [0, 1, 2, 4095, 4096, 32767, 32768, 65535]) (f1 v16_s4_k0#)
      (\[a] -> let x = fromIntegral a :: Word16 in fromIntegral ((x `shiftL` 4) + 1))
    , check "v16_s4_k1" (map pure [0, 1, 2, 4095, 4096, 32767, 32768, 65535]) (f1 v16_s4_k1#)
      (\[a] -> let x = fromIntegral a :: Word16 in fromIntegral ((x `shiftL` 4) - 1))
    , check "v16_s4_k2" (map pure [0, 1, 2, 4095, 4096, 32767, 32768, 65535]) (f1 v16_s4_k2#)
      (\[a] -> let x = fromIntegral a :: Word16 in fromIntegral ((x `shiftL` 4) + 32767))
    , check "v16_s4_k3" (map pure [0, 1, 2, 4095, 4096, 32767, 32768, 65535]) (f1 v16_s4_k3#)
      (\[a] -> let x = fromIntegral a :: Word16 in fromIntegral ((x `shiftL` 4) - 32768))
    , check "v8_s0_k0" (map pure [0, 1, 2, 15, 16, 127, 128, 255]) (f1 v8_s0_k0#)
      (\[a] -> let x = fromIntegral a :: Word8 in fromIntegral ((x `shiftL` 0) + 1))
    , check "v8_s0_k1" (map pure [0, 1, 2, 15, 16, 127, 128, 255]) (f1 v8_s0_k1#)
      (\[a] -> let x = fromIntegral a :: Word8 in fromIntegral ((x `shiftL` 0) - 1))
    , check "v8_s0_k2" (map pure [0, 1, 2, 15, 16, 127, 128, 255]) (f1 v8_s0_k2#)
      (\[a] -> let x = fromIntegral a :: Word8 in fromIntegral ((x `shiftL` 0) + 127))
    , check "v8_s1_k0" (map pure [0, 1, 2, 15, 16, 127, 128, 255]) (f1 v8_s1_k0#)
      (\[a] -> let x = fromIntegral a :: Word8 in fromIntegral ((x `shiftL` 1) + 1))
    , check "v8_s1_k1" (map pure [0, 1, 2, 15, 16, 127, 128, 255]) (f1 v8_s1_k1#)
      (\[a] -> let x = fromIntegral a :: Word8 in fromIntegral ((x `shiftL` 1) - 1))
    , check "v8_s1_k2" (map pure [0, 1, 2, 15, 16, 127, 128, 255]) (f1 v8_s1_k2#)
      (\[a] -> let x = fromIntegral a :: Word8 in fromIntegral ((x `shiftL` 1) + 127))
    , check "v8_s2_k0" (map pure [0, 1, 2, 15, 16, 127, 128, 255]) (f1 v8_s2_k0#)
      (\[a] -> let x = fromIntegral a :: Word8 in fromIntegral ((x `shiftL` 2) + 1))
    , check "v8_s2_k1" (map pure [0, 1, 2, 15, 16, 127, 128, 255]) (f1 v8_s2_k1#)
      (\[a] -> let x = fromIntegral a :: Word8 in fromIntegral ((x `shiftL` 2) - 1))
    , check "v8_s2_k2" (map pure [0, 1, 2, 15, 16, 127, 128, 255]) (f1 v8_s2_k2#)
      (\[a] -> let x = fromIntegral a :: Word8 in fromIntegral ((x `shiftL` 2) + 127))
    , check "v8_s3_k0" (map pure [0, 1, 2, 15, 16, 127, 128, 255]) (f1 v8_s3_k0#)
      (\[a] -> let x = fromIntegral a :: Word8 in fromIntegral ((x `shiftL` 3) + 1))
    , check "v8_s3_k1" (map pure [0, 1, 2, 15, 16, 127, 128, 255]) (f1 v8_s3_k1#)
      (\[a] -> let x = fromIntegral a :: Word8 in fromIntegral ((x `shiftL` 3) - 1))
    , check "v8_s3_k2" (map pure [0, 1, 2, 15, 16, 127, 128, 255]) (f1 v8_s3_k2#)
      (\[a] -> let x = fromIntegral a :: Word8 in fromIntegral ((x `shiftL` 3) + 127))
    , check "v8_s4_k0" (map pure [0, 1, 2, 15, 16, 127, 128, 255]) (f1 v8_s4_k0#)
      (\[a] -> let x = fromIntegral a :: Word8 in fromIntegral ((x `shiftL` 4) + 1))
    , check "v8_s4_k1" (map pure [0, 1, 2, 15, 16, 127, 128, 255]) (f1 v8_s4_k1#)
      (\[a] -> let x = fromIntegral a :: Word8 in fromIntegral ((x `shiftL` 4) - 1))
    , check "v8_s4_k2" (map pure [0, 1, 2, 15, 16, 127, 128, 255]) (f1 v8_s4_k2#)
      (\[a] -> let x = fromIntegral a :: Word8 in fromIntegral ((x `shiftL` 4) + 127))
    ]
  printf "%d procedures, %d calls\n" (length ns) (sum ns)
