{-# LANGUAGE MagicHash, GHCForeignImportPrim, UnliftedFFITypes #-}

-- Note [Additive normal form] in GHC.Cmm.Opt: each procedure of
-- cmm_addnf_cmm.cmm computes one shape of addition and subtraction, compared
-- here with the same formula computed in Haskell over boundary values.

import Control.Monad
import Data.Bits
import Data.Int
import Data.Word
import Foreign.Marshal.Array
import Foreign.Ptr
import GHC.Exts
import Text.Printf

foreign import prim "c01_64_s2" c01_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c01_64_s3" c01_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c01_8_s4" c01_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c02_64_s2" c02_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c02_64_s3" c02_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c02_8_s4" c02_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c03_64_s2" c03_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c03_64_s3" c03_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c03_8_s4" c03_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c04_64_s2" c04_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c04_64_s3" c04_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c04_8_s4" c04_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c05_64_s2" c05_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c05_64_s3" c05_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c05_8_s4" c05_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c06_64_s2" c06_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c06_64_s3" c06_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c06_8_s4" c06_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c07_64_s2" c07_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c07_64_s3" c07_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c07_8_s4" c07_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c08_64_s2" c08_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c08_64_s3" c08_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c08_8_s4" c08_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c09_64_s2" c09_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c09_64_s3" c09_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c09_8_s4" c09_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c10_64_s2" c10_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c10_64_s3" c10_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c10_8_s4" c10_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c11_64_s2" c11_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c11_64_s3" c11_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c11_8_s4" c11_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c12_64_s2" c12_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c12_64_s3" c12_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c12_8_s4" c12_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c13_64_s2" c13_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c13_64_s3" c13_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c13_8_s4" c13_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c14_64_s2" c14_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c14_64_s3" c14_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c14_8_s4" c14_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c15_64_s2" c15_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c15_64_s3" c15_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c15_8_s4" c15_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c16_64_s2" c16_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c16_64_s3" c16_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c16_8_s4" c16_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c17_64_s2" c17_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c17_64_s3" c17_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c17_8_s4" c17_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c18_64_s2" c18_64_s2# :: Word# -> Word# -> Word# -> Word#
foreign import prim "c18_64_s3" c18_64_s3# :: Word# -> Word# -> Word# -> Word#
foreign import prim "c18_8_s4" c18_8_s4# :: Word# -> Word# -> Word# -> Word#
foreign import prim "c19_64_s2" c19_64_s2# :: Word# -> Word# -> Word# -> Word#
foreign import prim "c19_64_s3" c19_64_s3# :: Word# -> Word# -> Word# -> Word#
foreign import prim "c19_8_s4" c19_8_s4# :: Word# -> Word# -> Word# -> Word#
foreign import prim "c20_64_s2" c20_64_s2# :: Word# -> Word# -> Word# -> Word#
foreign import prim "c20_64_s3" c20_64_s3# :: Word# -> Word# -> Word# -> Word#
foreign import prim "c20_8_s4" c20_8_s4# :: Word# -> Word# -> Word# -> Word#
foreign import prim "c21_64_s2" c21_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c21_64_s3" c21_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c21_8_s4" c21_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c22_64_s2" c22_64_s2# :: Word# -> Word# -> Word# -> Word#
foreign import prim "c22_64_s3" c22_64_s3# :: Word# -> Word# -> Word# -> Word#
foreign import prim "c22_8_s4" c22_8_s4# :: Word# -> Word# -> Word# -> Word#
foreign import prim "c23_64_s2" c23_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c23_64_s3" c23_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c23_8_s4" c23_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c24_64_s2" c24_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c24_64_s3" c24_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c24_8_s4" c24_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c25_64_s2" c25_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c25_64_s3" c25_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c25_8_s4" c25_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c26_64_s2" c26_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c26_64_s3" c26_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c26_8_s4" c26_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c27_64_s2" c27_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c27_64_s3" c27_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c27_8_s4" c27_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c28_64_s2" c28_64_s2# :: Word# -> Word# -> Word#
foreign import prim "c28_64_s3" c28_64_s3# :: Word# -> Word# -> Word#
foreign import prim "c28_8_s4" c28_8_s4# :: Word# -> Word# -> Word#
foreign import prim "c29_64_s2" c29_64_s2# :: Word# -> Word# -> Word# -> Word#
foreign import prim "c29_64_s3" c29_64_s3# :: Word# -> Word# -> Word# -> Word#
foreign import prim "c29_8_s4" c29_8_s4# :: Word# -> Word# -> Word# -> Word#
foreign import prim "c30_64_s2" c30_64_s2# :: Word# -> Word# -> Word# -> Word#
foreign import prim "c30_64_s3" c30_64_s3# :: Word# -> Word# -> Word# -> Word#
foreign import prim "c30_8_s4" c30_8_s4# :: Word# -> Word# -> Word# -> Word#
foreign import prim "conv1" conv1# :: Word# -> Word#
foreign import prim "a01" a01# :: Word# -> Word# -> Word#
foreign import prim "a02" a02# :: Word# -> Word# -> Word#
foreign import prim "a03" a03# :: Word# -> Word# -> Word#
foreign import prim "a04" a04# :: Word# -> Word# -> Word#
foreign import prim "a05" a05# :: Word# -> Word# -> Word#
foreign import prim "a06" a06# :: Word# -> Word# -> Word#
foreign import prim "a07" a07# :: Word# -> Word# -> Word#
foreign import prim "a09" a09# :: Word# -> Word# -> Word#
foreign import prim "a10" a10# :: Word# -> Word# -> Word#
foreign import prim "a11" a11# :: Word# -> Word# -> Word#
foreign import prim "a12" a12# :: Word# -> Word# -> Word#
foreign import prim "a13" a13# :: Word# -> Word# -> Word#
foreign import prim "a14" a14# :: Word# -> Word# -> Word#
foreign import prim "s01" s01# :: Word# -> Word# -> Word# -> Word#
foreign import prim "l01" l01# :: Word# -> Word#
foreign import prim "l02" l02# :: Word# -> Word#
foreign import prim "l03" l03# :: Word# -> Word#
foreign import prim "l04" l04# :: Word# -> Word#
foreign import prim "l05" l05# :: Word# -> Word#
foreign import prim "l06" l06# :: Word# -> Word#
foreign import prim "k01" k01# :: Word# -> Word# -> Word#
foreign import prim "d01_64_t1" d01_64_t1# :: Word# -> Word# -> Word#
foreign import prim "d01_64_s3" d01_64_s3# :: Word# -> Word# -> Word#
foreign import prim "d01_8_s4" d01_8_s4# :: Word# -> Word# -> Word#
foreign import prim "d02_64_t1" d02_64_t1# :: Word# -> Word# -> Word#
foreign import prim "d02_64_s3" d02_64_s3# :: Word# -> Word# -> Word#
foreign import prim "d02_8_s4" d02_8_s4# :: Word# -> Word# -> Word#
foreign import prim "d03_64_t1" d03_64_t1# :: Word# -> Word# -> Word#
foreign import prim "d03_64_s3" d03_64_s3# :: Word# -> Word# -> Word#
foreign import prim "d03_8_s4" d03_8_s4# :: Word# -> Word# -> Word#
foreign import prim "d04_64_t1" d04_64_t1# :: Word# -> Word# -> Word#
foreign import prim "d04_64_s3" d04_64_s3# :: Word# -> Word# -> Word#
foreign import prim "d04_8_s4" d04_8_s4# :: Word# -> Word# -> Word#
foreign import prim "d05_64_t1" d05_64_t1# :: Word# -> Word# -> Word#
foreign import prim "d05_64_s3" d05_64_s3# :: Word# -> Word# -> Word#
foreign import prim "d05_8_s4" d05_8_s4# :: Word# -> Word# -> Word#
foreign import prim "d06_64_t1" d06_64_t1# :: Word# -> Word# -> Word#
foreign import prim "d06_64_s3" d06_64_s3# :: Word# -> Word# -> Word#
foreign import prim "d06_8_s4" d06_8_s4# :: Word# -> Word# -> Word#
foreign import prim "d07_64_t1" d07_64_t1# :: Word# -> Word# -> Word#
foreign import prim "d07_64_s3" d07_64_s3# :: Word# -> Word# -> Word#
foreign import prim "d07_8_s4" d07_8_s4# :: Word# -> Word# -> Word#
foreign import prim "d08_64_t1" d08_64_t1# :: Word# -> Word# -> Word#
foreign import prim "d08_64_s3" d08_64_s3# :: Word# -> Word# -> Word#
foreign import prim "d08_8_s4" d08_8_s4# :: Word# -> Word# -> Word#
foreign import prim "d09_64_t1" d09_64_t1# :: Word# -> Word# -> Word#
foreign import prim "d09_64_s3" d09_64_s3# :: Word# -> Word# -> Word#
foreign import prim "d09_8_s4" d09_8_s4# :: Word# -> Word# -> Word#
foreign import prim "d10_64_t1" d10_64_t1# :: Word# -> Word# -> Word#
foreign import prim "d10_64_s3" d10_64_s3# :: Word# -> Word# -> Word#
foreign import prim "d10_8_s4" d10_8_s4# :: Word# -> Word# -> Word#
foreign import prim "d11_64_t1" d11_64_t1# :: Word# -> Word# -> Word#
foreign import prim "d11_64_s3" d11_64_s3# :: Word# -> Word# -> Word#
foreign import prim "d11_8_s4" d11_8_s4# :: Word# -> Word# -> Word#
foreign import prim "d12_64_t1" d12_64_t1# :: Word# -> Word# -> Word#
foreign import prim "d12_64_s3" d12_64_s3# :: Word# -> Word# -> Word#
foreign import prim "d12_8_s4" d12_8_s4# :: Word# -> Word# -> Word#
foreign import prim "d13_64_t1" d13_64_t1# :: Word# -> Word# -> Word#
foreign import prim "d13_64_s3" d13_64_s3# :: Word# -> Word# -> Word#
foreign import prim "d13_8_s4" d13_8_s4# :: Word# -> Word# -> Word#
foreign import prim "d14_64_t1" d14_64_t1# :: Word# -> Word# -> Word#
foreign import prim "d14_64_s3" d14_64_s3# :: Word# -> Word# -> Word#
foreign import prim "d14_8_s4" d14_8_s4# :: Word# -> Word# -> Word#
foreign import prim "d15_64_t1" d15_64_t1# :: Word# -> Word# -> Word#
foreign import prim "d15_64_s3" d15_64_s3# :: Word# -> Word# -> Word#
foreign import prim "d15_8_s4" d15_8_s4# :: Word# -> Word# -> Word#
foreign import prim "d16_64_t1" d16_64_t1# :: Word# -> Word# -> Word#
foreign import prim "d16_64_s3" d16_64_s3# :: Word# -> Word# -> Word#
foreign import prim "d16_8_s4" d16_8_s4# :: Word# -> Word# -> Word#
foreign import prim "d17_64_t1" d17_64_t1# :: Word# -> Word# -> Word#
foreign import prim "d17_64_s3" d17_64_s3# :: Word# -> Word# -> Word#
foreign import prim "d17_8_s4" d17_8_s4# :: Word# -> Word# -> Word#
foreign import prim "d18_64_t1" d18_64_t1# :: Word# -> Word# -> Word#
foreign import prim "d18_64_s3" d18_64_s3# :: Word# -> Word# -> Word#
foreign import prim "d18_8_s4" d18_8_s4# :: Word# -> Word# -> Word#
foreign import prim "d19_64_t1" d19_64_t1# :: Word# -> Word# -> Word# -> Word#
foreign import prim "d19_64_s3" d19_64_s3# :: Word# -> Word# -> Word# -> Word#
foreign import prim "d19_8_s4" d19_8_s4# :: Word# -> Word# -> Word# -> Word#
foreign import prim "d20_64_t1" d20_64_t1# :: Word# -> Word# -> Word#
foreign import prim "d20_64_s3" d20_64_s3# :: Word# -> Word# -> Word#
foreign import prim "d20_8_s4" d20_8_s4# :: Word# -> Word# -> Word#
foreign import prim "e01" e01# :: Word# -> Word# -> Word#
foreign import prim "e02" e02# :: Word# -> Word# -> Word#
foreign import prim "e03" e03# :: Word# -> Word# -> Word#
foreign import prim "e04" e04# :: Word# -> Word# -> Word#
foreign import prim "e05" e05# :: Word# -> Word# -> Word#
foreign import prim "e06" e06# :: Word# -> Word# -> Word#
foreign import prim "e07" e07# :: Word# -> Word# -> Word#
foreign import prim "e08" e08# :: Word# -> Word# -> Word#
foreign import prim "e09" e09# :: Word# -> Word# -> Word#
foreign import prim "e10" e10# :: Word# -> Word# -> Word#
foreign import prim "e11" e11# :: Word# -> Word# -> Word#
foreign import prim "e12" e12# :: Word# -> Word# -> Word#
foreign import prim "s02" s02# :: Word# -> Word# -> Word# -> Word#
foreign import prim "m01" m01# :: Word# -> Word#
foreign import prim "m02" m02# :: Word# -> Word#
foreign import prim "m03" m03# :: Word# -> Word#
foreign import prim "m04" m04# :: Word# -> Word#
foreign import prim "m05" m05# :: Word# -> Word#
foreign import prim "m06" m06# :: Word# -> Word#

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

f3 :: (Word# -> Word# -> Word# -> Word#) -> [Word64] -> Word64
f3 f [x, y, z] = wd (f (un x) (un y) (un z))
f3 _ _ = error "arity"

-- the number of calls, each mismatch printed
check :: String -> [[Word64]] -> ([Word64] -> Word64) -> ([Word64] -> Word64) -> IO Int
check name argss f r = do
  forM_ argss $ \a -> let { x = f a; y = r a } in
    when (x /= y) $ printf "%s %s: %016x, expected %016x\n" name (show a) x y
  return (length argss)

main :: IO ()
main = allocaArray 64 $ \buf -> do
  pokeArray buf [0x1000 + n | n <- [0 .. 63 :: Word64]]
  -- p is the buffer's element 8
  let pw = fromIntegral (ptrToWordPtr buf) + 64 :: Word64
  ns <- sequence
    [ check "c01_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c01_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((x - y) + (9223372036854775807 :: Word64)))
    , check "c01_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c01_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((x - y) + (9223372036854775808 :: Word64)))
    , check "c01_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c01_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral ((x - y) + (255 :: Word8)))
    , check "c02_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c02_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((x + (9223372036854775807 :: Word64)) - y))
    , check "c02_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c02_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((x + (9223372036854775808 :: Word64)) - y))
    , check "c02_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c02_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral ((x + (255 :: Word8)) - y))
    , check "c03_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c03_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (x - (y - (9223372036854775807 :: Word64))))
    , check "c03_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c03_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (x - (y - (9223372036854775808 :: Word64))))
    , check "c03_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c03_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (x - (y - (255 :: Word8))))
    , check "c04_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c04_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (x + (y + (9223372036854775807 :: Word64))))
    , check "c04_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c04_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (x + (y + (9223372036854775808 :: Word64))))
    , check "c04_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c04_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (x + (y + (255 :: Word8))))
    , check "c05_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c05_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((x + (9223372036854775807 :: Word64)) + (y + (9223372036854775807 :: Word64))))
    , check "c05_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c05_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((x + (9223372036854775808 :: Word64)) + (y + (1 :: Word64))))
    , check "c05_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c05_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral ((x + (255 :: Word8)) + (y + (255 :: Word8))))
    , check "c06_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c06_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((x - (9223372036854775807 :: Word64)) - (y - (9223372036854775807 :: Word64))))
    , check "c06_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c06_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((x - (9223372036854775808 :: Word64)) - (y - (1 :: Word64))))
    , check "c06_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c06_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral ((x - (255 :: Word8)) - (y - (255 :: Word8))))
    , check "c07_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c07_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((9223372036854775807 :: Word64) - x) + ((9223372036854775807 :: Word64) - y)))
    , check "c07_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c07_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((9223372036854775808 :: Word64) - x) + ((1 :: Word64) - y)))
    , check "c07_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c07_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (((255 :: Word8) - x) + ((255 :: Word8) - y)))
    , check "c08_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c08_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((9223372036854775807 :: Word64) - x) - ((9223372036854775807 :: Word64) - y)))
    , check "c08_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c08_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((9223372036854775808 :: Word64) - x) - ((1 :: Word64) - y)))
    , check "c08_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c08_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (((255 :: Word8) - x) - ((255 :: Word8) - y)))
    , check "c09_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c09_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((9223372036854775807 :: Word64) - x) + y))
    , check "c09_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c09_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((9223372036854775808 :: Word64) - x) + y))
    , check "c09_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c09_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (((255 :: Word8) - x) + y))
    , check "c10_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c10_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((9223372036854775807 :: Word64) - x) - y))
    , check "c10_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c10_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((9223372036854775808 :: Word64) - x) - y))
    , check "c10_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c10_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (((255 :: Word8) - x) - y))
    , check "c11_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c11_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (x - ((9223372036854775807 :: Word64) - y)))
    , check "c11_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c11_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (x - ((9223372036854775808 :: Word64) - y)))
    , check "c11_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c11_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (x - ((255 :: Word8) - y)))
    , check "c12_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c12_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (x + ((9223372036854775807 :: Word64) - y)))
    , check "c12_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c12_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (x + ((9223372036854775808 :: Word64) - y)))
    , check "c12_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c12_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (x + ((255 :: Word8) - y)))
    , check "c13_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c13_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((9223372036854775807 :: Word64) - (x + (9223372036854775807 :: Word64))))
    , check "c13_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c13_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((9223372036854775808 :: Word64) - (x + (1 :: Word64))))
    , check "c13_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c13_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral ((255 :: Word8) - (x + (255 :: Word8))))
    , check "c14_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c14_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((9223372036854775807 :: Word64) - ((9223372036854775807 :: Word64) - x)))
    , check "c14_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c14_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((9223372036854775808 :: Word64) - ((1 :: Word64) - x)))
    , check "c14_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c14_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral ((255 :: Word8) - ((255 :: Word8) - x)))
    , check "c15_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c15_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((x + (9223372036854775807 :: Word64)) - (9223372036854775807 :: Word64)))
    , check "c15_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c15_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((x + (9223372036854775808 :: Word64)) - (9223372036854775808 :: Word64)))
    , check "c15_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c15_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral ((x + (255 :: Word8)) - (255 :: Word8)))
    , check "c16_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c16_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (x - (9223372036854775807 :: Word64)))
    , check "c16_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c16_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (x - (9223372036854775808 :: Word64)))
    , check "c16_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c16_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (x - (255 :: Word8)))
    , check "c17_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c17_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((x + (9223372036854775807 :: Word64)) + (9223372036854775807 :: Word64)))
    , check "c17_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c17_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((x + (9223372036854775808 :: Word64)) + (1 :: Word64)))
    , check "c17_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c17_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral ((x + (255 :: Word8)) + (255 :: Word8)))
    , check "c18_64_s2" (replicateM 3 [0, 9223372036854775807, 18446744073709551615]) (f3 c18_64_s2#)
      (\[a0, a1, a2] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; z = fromIntegral a2 :: Word64 } in fromIntegral (((((x + (9223372036854775807 :: Word64)) - y) + (9223372036854775807 :: Word64)) - z) + (9223372036854775807 :: Word64)))
    , check "c18_64_s3" (replicateM 3 [0, 9223372036854775807, 18446744073709551615]) (f3 c18_64_s3#)
      (\[a0, a1, a2] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; z = fromIntegral a2 :: Word64 } in fromIntegral (((((x + (9223372036854775808 :: Word64)) - y) + (1 :: Word64)) - z) + (9223372036854775808 :: Word64)))
    , check "c18_8_s4" (replicateM 3 [0, 127, 255]) (f3 c18_8_s4#)
      (\[a0, a1, a2] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; z = fromIntegral a2 :: Word8 } in fromIntegral (((((x + (255 :: Word8)) - y) + (255 :: Word8)) - z) + (255 :: Word8)))
    , check "c19_64_s2" (replicateM 3 [0, 9223372036854775807, 18446744073709551615]) (f3 c19_64_s2#)
      (\[a0, a1, a2] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; z = fromIntegral a2 :: Word64 } in fromIntegral (((x - (9223372036854775807 :: Word64)) - (y + (9223372036854775807 :: Word64))) - (z - (9223372036854775807 :: Word64))))
    , check "c19_64_s3" (replicateM 3 [0, 9223372036854775807, 18446744073709551615]) (f3 c19_64_s3#)
      (\[a0, a1, a2] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; z = fromIntegral a2 :: Word64 } in fromIntegral (((x - (9223372036854775808 :: Word64)) - (y + (1 :: Word64))) - (z - (9223372036854775808 :: Word64))))
    , check "c19_8_s4" (replicateM 3 [0, 127, 255]) (f3 c19_8_s4#)
      (\[a0, a1, a2] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; z = fromIntegral a2 :: Word8 } in fromIntegral (((x - (255 :: Word8)) - (y + (255 :: Word8))) - (z - (255 :: Word8))))
    , check "c20_64_s2" (replicateM 3 [0, 9223372036854775807, 18446744073709551615]) (f3 c20_64_s2#)
      (\[a0, a1, a2] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; z = fromIntegral a2 :: Word64 } in fromIntegral ((x + (9223372036854775807 :: Word64)) - ((y - (9223372036854775807 :: Word64)) - (z + (9223372036854775807 :: Word64)))))
    , check "c20_64_s3" (replicateM 3 [0, 9223372036854775807, 18446744073709551615]) (f3 c20_64_s3#)
      (\[a0, a1, a2] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; z = fromIntegral a2 :: Word64 } in fromIntegral ((x + (9223372036854775808 :: Word64)) - ((y - (1 :: Word64)) - (z + (9223372036854775808 :: Word64)))))
    , check "c20_8_s4" (replicateM 3 [0, 127, 255]) (f3 c20_8_s4#)
      (\[a0, a1, a2] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; z = fromIntegral a2 :: Word8 } in fromIntegral ((x + (255 :: Word8)) - ((y - (255 :: Word8)) - (z + (255 :: Word8)))))
    , check "c21_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c21_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((x * y) + (9223372036854775807 :: Word64)) - y))
    , check "c21_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c21_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((x * y) + (9223372036854775808 :: Word64)) - y))
    , check "c21_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c21_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (((x * y) + (255 :: Word8)) - y))
    , check "c22_64_s2" (replicateM 3 [0, 9223372036854775807, 18446744073709551615]) (f3 c22_64_s2#)
      (\[a0, a1, a2] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; z = fromIntegral a2 :: Word64 } in fromIntegral ((((9223372036854775807 :: Word64) - x) - ((9223372036854775807 :: Word64) - y)) + (z + (9223372036854775807 :: Word64))))
    , check "c22_64_s3" (replicateM 3 [0, 9223372036854775807, 18446744073709551615]) (f3 c22_64_s3#)
      (\[a0, a1, a2] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; z = fromIntegral a2 :: Word64 } in fromIntegral ((((9223372036854775808 :: Word64) - x) - ((1 :: Word64) - y)) + (z + (9223372036854775808 :: Word64))))
    , check "c22_8_s4" (replicateM 3 [0, 127, 255]) (f3 c22_8_s4#)
      (\[a0, a1, a2] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; z = fromIntegral a2 :: Word8 } in fromIntegral ((((255 :: Word8) - x) - ((255 :: Word8) - y)) + (z + (255 :: Word8))))
    , check "c23_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c23_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((0 :: Word64) - (x + (9223372036854775807 :: Word64))))
    , check "c23_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c23_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((0 :: Word64) - (x + (9223372036854775808 :: Word64))))
    , check "c23_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c23_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral ((0 :: Word8) - (x + (255 :: Word8))))
    , check "c24_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c24_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((0 :: Word64) - x) + ((0 :: Word64) - y)))
    , check "c24_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c24_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((0 :: Word64) - x) + ((0 :: Word64) - y)))
    , check "c24_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c24_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (((0 :: Word8) - x) + ((0 :: Word8) - y)))
    , check "c25_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c25_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((9223372036854775807 :: Word64) - x) + (9223372036854775807 :: Word64)))
    , check "c25_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c25_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((9223372036854775808 :: Word64) - x) + (1 :: Word64)))
    , check "c25_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c25_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (((255 :: Word8) - x) + (255 :: Word8)))
    , check "c26_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c26_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((9223372036854775807 :: Word64) + x))
    , check "c26_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c26_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((9223372036854775808 :: Word64) + x))
    , check "c26_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c26_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral ((255 :: Word8) + x))
    , check "c27_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c27_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((x + (9223372036854775807 :: Word64)) - (x + (9223372036854775807 :: Word64))))
    , check "c27_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c27_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((x + (9223372036854775808 :: Word64)) - (x + (9223372036854775808 :: Word64))))
    , check "c27_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c27_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral ((x + (255 :: Word8)) - (x + (255 :: Word8))))
    , check "c28_64_s2" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c28_64_s2#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((x - y) + (y - (9223372036854775807 :: Word64))))
    , check "c28_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 c28_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((x - y) + (y - (9223372036854775808 :: Word64))))
    , check "c28_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 c28_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral ((x - y) + (y - (255 :: Word8))))
    , check "c29_64_s2" (replicateM 3 [0, 9223372036854775807, 18446744073709551615]) (f3 c29_64_s2#)
      (\[a0, a1, a2] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; z = fromIntegral a2 :: Word64 } in fromIntegral ((x + y) + (z + (9223372036854775807 :: Word64))))
    , check "c29_64_s3" (replicateM 3 [0, 9223372036854775807, 18446744073709551615]) (f3 c29_64_s3#)
      (\[a0, a1, a2] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; z = fromIntegral a2 :: Word64 } in fromIntegral ((x + y) + (z + (9223372036854775808 :: Word64))))
    , check "c29_8_s4" (replicateM 3 [0, 127, 255]) (f3 c29_8_s4#)
      (\[a0, a1, a2] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; z = fromIntegral a2 :: Word8 } in fromIntegral ((x + y) + (z + (255 :: Word8))))
    , check "c30_64_s2" (replicateM 3 [0, 9223372036854775807, 18446744073709551615]) (f3 c30_64_s2#)
      (\[a0, a1, a2] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; z = fromIntegral a2 :: Word64 } in fromIntegral ((x - (9223372036854775807 :: Word64)) + ((y + z) - (9223372036854775807 :: Word64))))
    , check "c30_64_s3" (replicateM 3 [0, 9223372036854775807, 18446744073709551615]) (f3 c30_64_s3#)
      (\[a0, a1, a2] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; z = fromIntegral a2 :: Word64 } in fromIntegral ((x - (1 :: Word64)) + ((y + z) - (9223372036854775808 :: Word64))))
    , check "c30_8_s4" (replicateM 3 [0, 127, 255]) (f3 c30_8_s4#)
      (\[a0, a1, a2] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; z = fromIntegral a2 :: Word8 } in fromIntegral ((x - (255 :: Word8)) + ((y + z) - (255 :: Word8))))
    , check "conv1" (map pure [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 conv1#)
      (\[x] -> fromIntegral (fromIntegral (fromIntegral (fromIntegral x + 2147483647 + 1 :: Word32) :: Int32) :: Int64) + 1)
    , check "a01" [[pw, i] | i <- [0 .. 7]] (f2 a01#) (\[_, i] -> 0x1000 + (8 + 2 + i))
    , check "a02" [[pw, i] | i <- [0 .. 7]] (f2 a02#) (\[_, i] -> 0x1000 + (8 + 2 + i))
    , check "a03" [[pw, i] | i <- [0 .. 7]] (f2 a03#) (\[_, i] -> 0x1000 + (8 + 2 + i))
    , check "a04" [[pw, i] | i <- [0 .. 7]] (f2 a04#) (\[_, i] -> 0x1000 + (8 + i - 1))
    , check "a05" [[pw, i] | i <- [0 .. 7]] (f2 a05#) (\[_, i] -> 0x1000 + (8 + 2 + i))
    , check "a06" [[pw, i] | i <- [0 .. 7]] (f2 a06#) (\[_, i] -> 0x1000 + (8 + 2 + 2 * i))
    , check "a07" [[pw, i] | i <- [0 .. 7]] (f2 a07#) (\[_, i] -> 0x1000 + (8 + 3 + i))
    , check "a09" [[pw, i] | i <- [0 .. 7]] (f2 a09#) (\[_, i] -> 0x1000 + (8 + 2 + i))
    , check "a10" [[pw, i] | i <- [0 .. 7]] (f2 a10#) (\[_, i] -> 0x1000 + (8 + 16 - i))
    , check "a11" [[pw, i] | i <- [0 .. 7]] (f2 a11#) (\[_, i] -> 0x1000 + (8 + 1 + i))
    , check "a12" [[pw, i] | i <- [0 .. 7]] (f2 a12#) (\[_, i] -> 0x1000 + (8 + i))
    , check "a13" [[pw, i] | i <- [0 .. 7]] (f2 a13#) (\[_, i] -> 0x1000 + (8 + i))
    , check "a14" [[pw, i] | i <- [0 .. 7]] (f2 a14#) (\[_, i] -> 0x1000 + (8 + 2 * i + 1))
    , check "s01" [[pw, i, 0xdead0000 + i] | i <- [0 .. 7]] (f3 s01#) (\[_, _, v] -> v)
    , check "l01" [[i] | i <- [8, 16 .. 64]] (f1 l01#) (\[i] -> 11 * (1 + (1 + i `div` 8)))
    , check "l02" [[i] | i <- [8, 16 .. 64]] (f1 l02#) (\[i] -> 11 * (1 + (i `div` 8 - 1)))
    , check "l03" [[i] | i <- [8, 16 .. 64]] (f1 l03#) (\[i] -> 11 * (1 + (1 + i `div` 8)))
    , check "l04" [[i] | i <- [8, 16 .. 64]] (f1 l04#) (\[i] -> 11 * (1 + (1 + i `div` 8)))
    , check "l05" [[i] | i <- [8, 16 .. 64]] (f1 l05#) (\[i] -> 11 * (1 + (1 + i `div` 8)))
    , check "l06" (map pure [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f1 l06#) (\[i] -> i)
    , check "k01" (replicateM 2 [0, 1, 127, 128, 255]) (f2 k01#)
      (\[x, y] -> if (fromIntegral x + 1 - fromIntegral y :: Word8) == 3 then 1 else 0)
    , check "d01_64_t1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d01_64_t1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((x + (1 :: Word64)) `shiftL` 3) + y))
    , check "d01_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d01_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((x + (9223372036854775808 :: Word64)) `shiftL` 3) + y))
    , check "d01_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 d01_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (((x + (255 :: Word8)) `shiftL` 3) + y))
    , check "d02_64_t1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d02_64_t1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (y + ((x + (1 :: Word64)) `shiftL` 3)))
    , check "d02_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d02_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (y + ((x + (9223372036854775808 :: Word64)) `shiftL` 3)))
    , check "d02_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 d02_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (y + ((x + (255 :: Word8)) `shiftL` 3)))
    , check "d03_64_t1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d03_64_t1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (y - ((x + (1 :: Word64)) `shiftL` 3)))
    , check "d03_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d03_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (y - ((x + (9223372036854775808 :: Word64)) `shiftL` 3)))
    , check "d03_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 d03_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (y - ((x + (255 :: Word8)) `shiftL` 3)))
    , check "d04_64_t1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d04_64_t1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((x - (1 :: Word64)) `shiftL` 3) - y))
    , check "d04_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d04_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((x - (9223372036854775808 :: Word64)) `shiftL` 3) - y))
    , check "d04_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 d04_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (((x - (255 :: Word8)) `shiftL` 3) - y))
    , check "d05_64_t1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d05_64_t1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((((1 :: Word64) - x) `shiftL` 3) + y))
    , check "d05_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d05_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((((9223372036854775808 :: Word64) - x) `shiftL` 3) + y))
    , check "d05_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 d05_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral ((((255 :: Word8) - x) `shiftL` 3) + y))
    , check "d06_64_t1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d06_64_t1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (y - (((1 :: Word64) - x) `shiftL` 1)))
    , check "d06_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d06_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (y - (((9223372036854775808 :: Word64) - x) `shiftL` 1)))
    , check "d06_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 d06_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (y - (((255 :: Word8) - x) `shiftL` 1)))
    , check "d07_64_t1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d07_64_t1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((x + (1 :: Word64)) `shiftL` 63) + y))
    , check "d07_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d07_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((x + (9223372036854775808 :: Word64)) `shiftL` 63) + y))
    , check "d07_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 d07_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (((x + (255 :: Word8)) `shiftL` 7) + y))
    , check "d08_64_t1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d08_64_t1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((x + (1 :: Word64)) `shiftL` 40) - y))
    , check "d08_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d08_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((x + (9223372036854775808 :: Word64)) `shiftL` 40) - y))
    , check "d08_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 d08_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (((x + (255 :: Word8)) `shiftL` 7) - y))
    , check "d09_64_t1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d09_64_t1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((x + (1 :: Word64)) * (24 :: Word64)) + y))
    , check "d09_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d09_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((x + (9223372036854775808 :: Word64)) * (1 :: Word64)) + y))
    , check "d09_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 d09_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (((x + (255 :: Word8)) * (255 :: Word8)) + y))
    , check "d10_64_t1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d10_64_t1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (y - ((x - (1 :: Word64)) * (24 :: Word64))))
    , check "d10_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d10_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (y - ((x - (9223372036854775808 :: Word64)) * (1 :: Word64))))
    , check "d10_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 d10_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (y - ((x - (255 :: Word8)) * (255 :: Word8))))
    , check "d11_64_t1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d11_64_t1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((((1 :: Word64) - x) * (24 :: Word64)) - y))
    , check "d11_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d11_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((((9223372036854775808 :: Word64) - x) * (1 :: Word64)) - y))
    , check "d11_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 d11_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral ((((255 :: Word8) - x) * (255 :: Word8)) - y))
    , check "d12_64_t1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d12_64_t1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((24 :: Word64) * (x + (1 :: Word64))) + y))
    , check "d12_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d12_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((1 :: Word64) * (x + (9223372036854775808 :: Word64))) + y))
    , check "d12_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 d12_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (((255 :: Word8) * (x + (255 :: Word8))) + y))
    , check "d13_64_t1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d13_64_t1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((x + (1 :: Word64)) `shiftL` 3) + ((y + (24 :: Word64)) `shiftL` 1)))
    , check "d13_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d13_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((x + (9223372036854775808 :: Word64)) `shiftL` 3) + ((y + (1 :: Word64)) `shiftL` 1)))
    , check "d13_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 d13_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (((x + (255 :: Word8)) `shiftL` 3) + ((y + (255 :: Word8)) `shiftL` 1)))
    , check "d14_64_t1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d14_64_t1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((((x `shiftL` 2) + (1 :: Word64)) `shiftL` 3) + y))
    , check "d14_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d14_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((((x `shiftL` 2) + (9223372036854775808 :: Word64)) `shiftL` 3) + y))
    , check "d14_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 d14_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral ((((x `shiftL` 2) + (255 :: Word8)) `shiftL` 3) + y))
    , check "d15_64_t1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d15_64_t1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((x + (1 :: Word64)) `shiftL` 3) + (24 :: Word64)))
    , check "d15_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d15_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((x + (9223372036854775808 :: Word64)) `shiftL` 3) + (1 :: Word64)))
    , check "d15_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 d15_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (((x + (255 :: Word8)) `shiftL` 3) + (255 :: Word8)))
    , check "d16_64_t1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d16_64_t1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((24 :: Word64) - ((x + (1 :: Word64)) `shiftL` 2)))
    , check "d16_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d16_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((1 :: Word64) - ((x + (9223372036854775808 :: Word64)) `shiftL` 2)))
    , check "d16_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 d16_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral ((255 :: Word8) - ((x + (255 :: Word8)) `shiftL` 2)))
    , check "d17_64_t1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d17_64_t1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((x + (1 :: Word64)) `shiftL` 3) .&. y))
    , check "d17_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d17_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((x + (9223372036854775808 :: Word64)) `shiftL` 3) .&. y))
    , check "d17_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 d17_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (((x + (255 :: Word8)) `shiftL` 3) .&. y))
    , check "d18_64_t1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d18_64_t1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((((x + y) + (1 :: Word64)) `shiftL` 3) - x))
    , check "d18_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d18_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral ((((x + y) + (9223372036854775808 :: Word64)) `shiftL` 3) - x))
    , check "d18_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 d18_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral ((((x + y) + (255 :: Word8)) `shiftL` 3) - x))
    , check "d19_64_t1" (replicateM 3 [0, 9223372036854775807, 18446744073709551615]) (f3 d19_64_t1#)
      (\[a0, a1, a2] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; z = fromIntegral a2 :: Word64 } in fromIntegral ((((x + (1 :: Word64)) `shiftL` 3) + y) - ((z - (24 :: Word64)) * (1 :: Word64))))
    , check "d19_64_s3" (replicateM 3 [0, 9223372036854775807, 18446744073709551615]) (f3 d19_64_s3#)
      (\[a0, a1, a2] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64; z = fromIntegral a2 :: Word64 } in fromIntegral ((((x + (9223372036854775808 :: Word64)) `shiftL` 3) + y) - ((z - (1 :: Word64)) * (9223372036854775808 :: Word64))))
    , check "d19_8_s4" (replicateM 3 [0, 127, 255]) (f3 d19_8_s4#)
      (\[a0, a1, a2] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8; z = fromIntegral a2 :: Word8 } in fromIntegral ((((x + (255 :: Word8)) `shiftL` 3) + y) - ((z - (255 :: Word8)) * (255 :: Word8))))
    , check "d20_64_t1" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d20_64_t1#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((((x + (1 :: Word64)) `shiftL` 1) + (24 :: Word64)) `shiftL` 2) + y))
    , check "d20_64_s3" (replicateM 2 [0, 1, 9223372036854775807, 9223372036854775808, 18446744073709551615]) (f2 d20_64_s3#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word64; y = fromIntegral a1 :: Word64 } in fromIntegral (((((x + (9223372036854775808 :: Word64)) `shiftL` 1) + (1 :: Word64)) `shiftL` 2) + y))
    , check "d20_8_s4" (replicateM 2 [0, 1, 127, 128, 255]) (f2 d20_8_s4#)
      (\[a0, a1] -> let { x = fromIntegral a0 :: Word8; y = fromIntegral a1 :: Word8 } in fromIntegral (((((x + (255 :: Word8)) `shiftL` 1) + (255 :: Word8)) `shiftL` 2) + y))
    , check "e01" [[pw + 192, i] | i <- [0 .. 7]] (f2 e01#) (\[_, i] -> 0x1000 + 24 + (8 + i + 1))
    , check "e02" [[pw + 192, i] | i <- [0 .. 7]] (f2 e02#) (\[_, i] -> 0x1000 + 24 + (8 + i - 1))
    , check "e03" [[pw + 192, i] | i <- [0 .. 7]] (f2 e03#) (\[_, i] -> 0x1000 + 24 + (8 + 2 + i - 1))
    , check "e04" [[pw + 192, i] | i <- [0 .. 7]] (f2 e04#) (\[_, i] -> 0x1000 + 24 + (8 + i + 2))
    , check "e05" [[pw + 192, i] | i <- [0 .. 7]] (f2 e05#) (\[_, i] -> 0x1000 + 24 + (8 + i - 1))
    , check "e06" [[pw + 192, i] | i <- [0 .. 7]] (f2 e06#) (\[_, i] -> 0x1000 + 24 + (8 + 2 * i))
    , check "e07" [[pw + 192, i] | i <- [0 .. 7]] (f2 e07#) (\[_, i] -> 0x1000 + 24 + (8 + 3 * (i + 1)))
    , check "e08" [[pw + 192, i] | i <- [0 .. 7]] (f2 e08#) (\[_, i] -> 0x1000 + 24 + (8 - i - 1))
    , check "e09" [[pw + 192, i] | i <- [0 .. 7]] (f2 e09#) (\[_, i] -> 0x1000 + 24 + (8 + 2 * (i + 1)))
    , check "e10" [[pw + 192, i] | i <- [0 .. 7]] (f2 e10#) (\[_, i] -> 0x1000 + 24 + (8 + i))
    , check "e11" [[pw + 192, i] | i <- [0 .. 7]] (f2 e11#) (\[_, i] -> 0x1000 + 24 + (8 + i))
    , check "e12" [[pw + 192, i] | i <- [0 .. 7]] (f2 e12#) (\[_, i] -> 0x1000 + 24 + (8 + i))
    , check "s02" [[pw, i, 0xbeef0000 + i] | i <- [0 .. 7]] (f3 s02#) (\[_, _, v] -> v)
    , check "m01" [[i] | i <- [8, 16 .. 64]] (f1 m01#) (\[i] -> 11 * (1 + (i `div` 8 - 1)))
    , check "m02" [[i] | i <- [8, 16 .. 64]] (f1 m02#) (\[i] -> 11 * (1 + (i `div` 8 + 1)))
    , check "m03" [[i] | i <- [8, 16 .. 64]] (f1 m03#) (\[i] -> 11 * (1 + (i `div` 8)))
    , check "m04" [[i] | i <- [8, 16 .. 64]] (f1 m04#) (\[i] -> 11 * (1 + (i `div` 8 + 1)))
    , check "m05" [[i] | i <- [8, 16 .. 64]] (f1 m05#) (\[i] -> 11 * (1 + (9 - i `div` 8)))
    , check "m06" [[i] | i <- [8, 16 .. 64]] (f1 m06#) (\[i] -> 11 * (1 + (i `div` 8 - 1)))
    ]
  printf "%d procedures, %d calls\n" (length ns) (sum ns)
