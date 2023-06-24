{-

  This module produced a panic when compiled with -fbyte-code-and-object-code
  and optimization because it required stack offsets greater than 65535

  See #22888

 -}

module Main (main, processSHA256Block) where

import Data.Binary.Get (Get, getWord32be)
import Data.Bits (Bits(..))
import Data.Word (Word32)

main :: IO ()
main = pure ()

data SHA256Sched = SHA256Sched !Word32 !Word32 !Word32 !Word32 !Word32 -- 00-04
                               !Word32 !Word32 !Word32 !Word32 !Word32 -- 05-09
                               !Word32 !Word32 !Word32 !Word32 !Word32 -- 10-04
                               !Word32 !Word32 !Word32 !Word32 !Word32 -- 15-09
                               !Word32 !Word32 !Word32 !Word32 !Word32 -- 20-04
                               !Word32 !Word32 !Word32 !Word32 !Word32 -- 25-09
                               !Word32 !Word32 !Word32 !Word32 !Word32 -- 30-04
                               !Word32 !Word32 !Word32 !Word32 !Word32 -- 35-09
                               !Word32 !Word32 !Word32 !Word32 !Word32 -- 40-04
                               !Word32 !Word32 !Word32 !Word32 !Word32 -- 45-09
                               !Word32 !Word32 !Word32 !Word32 !Word32 -- 50-04
                               !Word32 !Word32 !Word32 !Word32 !Word32 -- 55-09
                               !Word32 !Word32 !Word32 !Word32         -- 60-63

data SHA256State = SHA256S !Word32 !Word32 !Word32 !Word32
                           !Word32 !Word32 !Word32 !Word32

{-# SPECIALIZE ch :: Word32 -> Word32 -> Word32 -> Word32 #-}
ch :: Bits a => a -> a -> a -> a
ch x y z = (x .&. y) `xor` (complement x .&. z)

{-# SPECIALIZE maj :: Word32 -> Word32 -> Word32 -> Word32 #-}
maj :: Bits a => a -> a -> a -> a
maj x y z = (x .&. (y .|. z)) .|. (y .&. z)

bsig256_0 :: Word32 -> Word32
bsig256_0 x = rotateR x 2 `xor` rotateR x 13 `xor` rotateR x 22

bsig256_1 :: Word32 -> Word32
bsig256_1 x = rotateR x 6 `xor` rotateR x 11 `xor` rotateR x 25

lsig256_0 :: Word32 -> Word32
lsig256_0 x = rotateR x 7 `xor` rotateR x 18 `xor` shiftR x 3

lsig256_1 :: Word32 -> Word32
lsig256_1 x = rotateR x 17 `xor` rotateR x 19 `xor` shiftR x 10

getSHA256Sched :: Get SHA256Sched
getSHA256Sched = do
  w00 <- getWord32be
  w01 <- getWord32be
  w02 <- getWord32be
  w03 <- getWord32be
  w04 <- getWord32be
  w05 <- getWord32be
  w06 <- getWord32be
  w07 <- getWord32be
  w08 <- getWord32be
  w09 <- getWord32be
  w10 <- getWord32be
  w11 <- getWord32be
  w12 <- getWord32be
  w13 <- getWord32be
  w14 <- getWord32be
  w15 <- getWord32be
  let w16 = lsig256_1 w14 + w09 + lsig256_0 w01 + w00
      w17 = lsig256_1 w15 + w10 + lsig256_0 w02 + w01
      w18 = lsig256_1 w16 + w11 + lsig256_0 w03 + w02
      w19 = lsig256_1 w17 + w12 + lsig256_0 w04 + w03
      w20 = lsig256_1 w18 + w13 + lsig256_0 w05 + w04
      w21 = lsig256_1 w19 + w14 + lsig256_0 w06 + w05
      w22 = lsig256_1 w20 + w15 + lsig256_0 w07 + w06
      w23 = lsig256_1 w21 + w16 + lsig256_0 w08 + w07
      w24 = lsig256_1 w22 + w17 + lsig256_0 w09 + w08
      w25 = lsig256_1 w23 + w18 + lsig256_0 w10 + w09
      w26 = lsig256_1 w24 + w19 + lsig256_0 w11 + w10
      w27 = lsig256_1 w25 + w20 + lsig256_0 w12 + w11
      w28 = lsig256_1 w26 + w21 + lsig256_0 w13 + w12
      w29 = lsig256_1 w27 + w22 + lsig256_0 w14 + w13
      w30 = lsig256_1 w28 + w23 + lsig256_0 w15 + w14
      w31 = lsig256_1 w29 + w24 + lsig256_0 w16 + w15
      w32 = lsig256_1 w30 + w25 + lsig256_0 w17 + w16
      w33 = lsig256_1 w31 + w26 + lsig256_0 w18 + w17
      w34 = lsig256_1 w32 + w27 + lsig256_0 w19 + w18
      w35 = lsig256_1 w33 + w28 + lsig256_0 w20 + w19
      w36 = lsig256_1 w34 + w29 + lsig256_0 w21 + w20
      w37 = lsig256_1 w35 + w30 + lsig256_0 w22 + w21
      w38 = lsig256_1 w36 + w31 + lsig256_0 w23 + w22
      w39 = lsig256_1 w37 + w32 + lsig256_0 w24 + w23
      w40 = lsig256_1 w38 + w33 + lsig256_0 w25 + w24
      w41 = lsig256_1 w39 + w34 + lsig256_0 w26 + w25
      w42 = lsig256_1 w40 + w35 + lsig256_0 w27 + w26
      w43 = lsig256_1 w41 + w36 + lsig256_0 w28 + w27
      w44 = lsig256_1 w42 + w37 + lsig256_0 w29 + w28
      w45 = lsig256_1 w43 + w38 + lsig256_0 w30 + w29
      w46 = lsig256_1 w44 + w39 + lsig256_0 w31 + w30
      w47 = lsig256_1 w45 + w40 + lsig256_0 w32 + w31
      w48 = lsig256_1 w46 + w41 + lsig256_0 w33 + w32
      w49 = lsig256_1 w47 + w42 + lsig256_0 w34 + w33
      w50 = lsig256_1 w48 + w43 + lsig256_0 w35 + w34
      w51 = lsig256_1 w49 + w44 + lsig256_0 w36 + w35
      w52 = lsig256_1 w50 + w45 + lsig256_0 w37 + w36
      w53 = lsig256_1 w51 + w46 + lsig256_0 w38 + w37
      w54 = lsig256_1 w52 + w47 + lsig256_0 w39 + w38
      w55 = lsig256_1 w53 + w48 + lsig256_0 w40 + w39
      w56 = lsig256_1 w54 + w49 + lsig256_0 w41 + w40
      w57 = lsig256_1 w55 + w50 + lsig256_0 w42 + w41
      w58 = lsig256_1 w56 + w51 + lsig256_0 w43 + w42
      w59 = lsig256_1 w57 + w52 + lsig256_0 w44 + w43
      w60 = lsig256_1 w58 + w53 + lsig256_0 w45 + w44
      w61 = lsig256_1 w59 + w54 + lsig256_0 w46 + w45
      w62 = lsig256_1 w60 + w55 + lsig256_0 w47 + w46
      w63 = lsig256_1 w61 + w56 + lsig256_0 w48 + w47
  return $! SHA256Sched w00 w01 w02 w03 w04 w05 w06 w07 w08 w09
                        w10 w11 w12 w13 w14 w15 w16 w17 w18 w19
                        w20 w21 w22 w23 w24 w25 w26 w27 w28 w29
                        w30 w31 w32 w33 w34 w35 w36 w37 w38 w39
                        w40 w41 w42 w43 w44 w45 w46 w47 w48 w49
                        w50 w51 w52 w53 w54 w55 w56 w57 w58 w59
                        w60 w61 w62 w63

processSHA256Block :: SHA256State -> Get SHA256State
processSHA256Block !s00@(SHA256S a00 b00 c00 d00 e00 f00 g00 h00) = do
  (SHA256Sched w00 w01 w02 w03 w04 w05 w06 w07 w08 w09
               w10 w11 w12 w13 w14 w15 w16 w17 w18 w19
               w20 w21 w22 w23 w24 w25 w26 w27 w28 w29
               w30 w31 w32 w33 w34 w35 w36 w37 w38 w39
               w40 w41 w42 w43 w44 w45 w46 w47 w48 w49
               w50 w51 w52 w53 w54 w55 w56 w57 w58 w59
               w60 w61 w62 w63) <- getSHA256Sched
  let s01 = step256 s00 0x428a2f98 w00
      s02 = step256 s01 0x71374491 w01
      s03 = step256 s02 0xb5c0fbcf w02
      s04 = step256 s03 0xe9b5dba5 w03
      s05 = step256 s04 0x3956c25b w04
      s06 = step256 s05 0x59f111f1 w05
      s07 = step256 s06 0x923f82a4 w06
      s08 = step256 s07 0xab1c5ed5 w07
      s09 = step256 s08 0xd807aa98 w08
      s10 = step256 s09 0x12835b01 w09
      s11 = step256 s10 0x243185be w10
      s12 = step256 s11 0x550c7dc3 w11
      s13 = step256 s12 0x72be5d74 w12
      s14 = step256 s13 0x80deb1fe w13
      s15 = step256 s14 0x9bdc06a7 w14
      s16 = step256 s15 0xc19bf174 w15
      s17 = step256 s16 0xe49b69c1 w16
      s18 = step256 s17 0xefbe4786 w17
      s19 = step256 s18 0x0fc19dc6 w18
      s20 = step256 s19 0x240ca1cc w19
      s21 = step256 s20 0x2de92c6f w20
      s22 = step256 s21 0x4a7484aa w21
      s23 = step256 s22 0x5cb0a9dc w22
      s24 = step256 s23 0x76f988da w23
      s25 = step256 s24 0x983e5152 w24
      s26 = step256 s25 0xa831c66d w25
      s27 = step256 s26 0xb00327c8 w26
      s28 = step256 s27 0xbf597fc7 w27
      s29 = step256 s28 0xc6e00bf3 w28
      s30 = step256 s29 0xd5a79147 w29
      s31 = step256 s30 0x06ca6351 w30
      s32 = step256 s31 0x14292967 w31
      s33 = step256 s32 0x27b70a85 w32
      s34 = step256 s33 0x2e1b2138 w33
      s35 = step256 s34 0x4d2c6dfc w34
      s36 = step256 s35 0x53380d13 w35
      s37 = step256 s36 0x650a7354 w36
      s38 = step256 s37 0x766a0abb w37
      s39 = step256 s38 0x81c2c92e w38
      s40 = step256 s39 0x92722c85 w39
      s41 = step256 s40 0xa2bfe8a1 w40
      s42 = step256 s41 0xa81a664b w41
      s43 = step256 s42 0xc24b8b70 w42
      s44 = step256 s43 0xc76c51a3 w43
      s45 = step256 s44 0xd192e819 w44
      s46 = step256 s45 0xd6990624 w45
      s47 = step256 s46 0xf40e3585 w46
      s48 = step256 s47 0x106aa070 w47
      s49 = step256 s48 0x19a4c116 w48
      s50 = step256 s49 0x1e376c08 w49
      s51 = step256 s50 0x2748774c w50
      s52 = step256 s51 0x34b0bcb5 w51
      s53 = step256 s52 0x391c0cb3 w52
      s54 = step256 s53 0x4ed8aa4a w53
      s55 = step256 s54 0x5b9cca4f w54
      s56 = step256 s55 0x682e6ff3 w55
      s57 = step256 s56 0x748f82ee w56
      s58 = step256 s57 0x78a5636f w57
      s59 = step256 s58 0x84c87814 w58
      s60 = step256 s59 0x8cc70208 w59
      s61 = step256 s60 0x90befffa w60
      s62 = step256 s61 0xa4506ceb w61
      s63 = step256 s62 0xbef9a3f7 w62
      s64 = step256 s63 0xc67178f2 w63
      SHA256S a64 b64 c64 d64 e64 f64 g64 h64 = s64
  return $! SHA256S (a00 + a64) (b00 + b64) (c00 + c64) (d00 + d64)
                    (e00 + e64) (f00 + f64) (g00 + g64) (h00 + h64)

{-# INLINE step256 #-}
step256 :: SHA256State -> Word32 -> Word32 -> SHA256State
step256 !(SHA256S a b c d e f g h) k w = SHA256S a' b' c' d' e' f' g' h'
 where
  t1 = h + bsig256_1 e + ch e f g + k + w
  t2 = bsig256_0 a + maj a b c
  h' = g
  g' = f
  f' = e
  e' = d + t1
  d' = c
  c' = b
  b' = a
  a' = t1 + t2

