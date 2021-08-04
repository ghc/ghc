
-- |
-- Module      : Crypto.Cipher.Camellia.Primitive
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
-- This only cover Camellia 128 bits for now. The API will change once
-- 192 and 256 mode are implemented too.
{-# LANGUAGE MagicHash #-}
module Crypto.Cipher.Camellia.Primitive
    ( Camellia
    , initCamellia
    , encrypt
    , decrypt
    ) where

import           Data.Word
import           Data.Bits

import           Crypto.Error
import           Crypto.Internal.ByteArray (ByteArrayAccess, ByteArray)
import qualified Crypto.Internal.ByteArray as B
import           Crypto.Internal.Words
import           Crypto.Internal.WordArray
import           Data.Memory.Endian

data Mode = Decrypt | Encrypt

w64tow128 :: (Word64, Word64) -> Word128
w64tow128 (x1, x2) = Word128 x1 x2

w64tow8 :: Word64 -> (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
w64tow8 x = (t1, t2, t3, t4, t5, t6, t7, t8)
    where
        t1 = fromIntegral (x `shiftR` 56)
        t2 = fromIntegral (x `shiftR` 48)
        t3 = fromIntegral (x `shiftR` 40)
        t4 = fromIntegral (x `shiftR` 32)
        t5 = fromIntegral (x `shiftR` 24)
        t6 = fromIntegral (x `shiftR` 16)
        t7 = fromIntegral (x `shiftR` 8)
        t8 = fromIntegral (x)

w8tow64 :: (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8) -> Word64
w8tow64 (t1,t2,t3,t4,t5,t6,t7,t8) =
    (fromIntegral t1 `shiftL` 56) .|.
    (fromIntegral t2 `shiftL` 48) .|.
    (fromIntegral t3 `shiftL` 40) .|.
    (fromIntegral t4 `shiftL` 32) .|.
    (fromIntegral t5 `shiftL` 24) .|.
    (fromIntegral t6 `shiftL` 16) .|.
    (fromIntegral t7 `shiftL` 8)  .|.
    (fromIntegral t8)

sbox :: Int -> Word8
sbox = arrayRead8 t
  where t = array8
            "\x70\x82\x2c\xec\xb3\x27\xc0\xe5\xe4\x85\x57\x35\xea\x0c\xae\x41\
            \\x23\xef\x6b\x93\x45\x19\xa5\x21\xed\x0e\x4f\x4e\x1d\x65\x92\xbd\
            \\x86\xb8\xaf\x8f\x7c\xeb\x1f\xce\x3e\x30\xdc\x5f\x5e\xc5\x0b\x1a\
            \\xa6\xe1\x39\xca\xd5\x47\x5d\x3d\xd9\x01\x5a\xd6\x51\x56\x6c\x4d\
            \\x8b\x0d\x9a\x66\xfb\xcc\xb0\x2d\x74\x12\x2b\x20\xf0\xb1\x84\x99\
            \\xdf\x4c\xcb\xc2\x34\x7e\x76\x05\x6d\xb7\xa9\x31\xd1\x17\x04\xd7\
            \\x14\x58\x3a\x61\xde\x1b\x11\x1c\x32\x0f\x9c\x16\x53\x18\xf2\x22\
            \\xfe\x44\xcf\xb2\xc3\xb5\x7a\x91\x24\x08\xe8\xa8\x60\xfc\x69\x50\
            \\xaa\xd0\xa0\x7d\xa1\x89\x62\x97\x54\x5b\x1e\x95\xe0\xff\x64\xd2\
            \\x10\xc4\x00\x48\xa3\xf7\x75\xdb\x8a\x03\xe6\xda\x09\x3f\xdd\x94\
            \\x87\x5c\x83\x02\xcd\x4a\x90\x33\x73\x67\xf6\xf3\x9d\x7f\xbf\xe2\
            \\x52\x9b\xd8\x26\xc8\x37\xc6\x3b\x81\x96\x6f\x4b\x13\xbe\x63\x2e\
            \\xe9\x79\xa7\x8c\x9f\x6e\xbc\x8e\x29\xf5\xf9\xb6\x2f\xfd\xb4\x59\
            \\x78\x98\x06\x6a\xe7\x46\x71\xba\xd4\x25\xab\x42\x88\xa2\x8d\xfa\
            \\x72\x07\xb9\x55\xf8\xee\xac\x0a\x36\x49\x2a\x68\x3c\x38\xf1\xa4\
            \\x40\x28\xd3\x7b\xbb\xc9\x43\xc1\x15\xe3\xad\xf4\x77\xc7\x80\x9e"#

sbox1 :: Word8 -> Word8
sbox1 x = sbox (fromIntegral x)

sbox2 :: Word8 -> Word8
sbox2 x = sbox1 x `rotateL` 1

sbox3 :: Word8 -> Word8
sbox3 x = sbox1 x `rotateL` 7

sbox4 :: Word8 -> Word8
sbox4 x = sbox1 (x `rotateL` 1)

sigma1, sigma2, sigma3, sigma4, sigma5, sigma6 :: Word64
sigma1 = 0xA09E667F3BCC908B
sigma2 = 0xB67AE8584CAA73B2
sigma3 = 0xC6EF372FE94F82BE
sigma4 = 0x54FF53A5F1D36F1C
sigma5 = 0x10E527FADE682D1D
sigma6 = 0xB05688C2B3E6C1FD

rotl128 :: Word128 -> Int -> Word128
rotl128 v               0  = v
rotl128 (Word128 x1 x2) 64 = Word128 x2 x1

rotl128 v@(Word128 x1 x2) w
    | w > 64    = (v `rotl128` 64) `rotl128` (w - 64)
    | otherwise = Word128 (x1high .|. x2low) (x2high .|. x1low)
        where
            splitBits i = (i .&. complement x, i .&. x)
                where x = 2 ^ w - 1
            (x1high, x1low) = splitBits (x1 `rotateL` w)
            (x2high, x2low) = splitBits (x2 `rotateL` w)

-- | Camellia context
data Camellia = Camellia
    { k  :: Array64
    , kw :: Array64
    , ke :: Array64
    }

setKeyInterim :: ByteArrayAccess key => key -> (Word128, Word128, Word128, Word128)
setKeyInterim keyseed = (w64tow128 kL, w64tow128 kR, w64tow128 kA, w64tow128 kB)
  where kL = (fromBE $ B.toW64BE keyseed 0, fromBE $ B.toW64BE keyseed 8)
        kR = (0, 0)

        kA = let d1 = (fst kL `xor` fst kR)
                 d2 = (snd kL `xor` snd kR)
                 d3 = d2 `xor` feistel d1 sigma1
                 d4 = d1 `xor` feistel d3 sigma2
                 d5 = d4 `xor` (fst kL)
                 d6 = d3 `xor` (snd kL)
                 d7 = d6 `xor` feistel d5 sigma3
                 d8 = d5 `xor` feistel d7 sigma4
              in (d8, d7)

        kB = let d1 = (fst kA `xor` fst kR)
                 d2 = (snd kA `xor` snd kR)
                 d3 = d2 `xor` feistel d1 sigma5
                 d4 = d1 `xor` feistel d3 sigma6
              in (d4, d3)

-- | Initialize a 128-bit key
--
-- Return the initialized key or a error message if the given 
-- keyseed was not 16-bytes in length.
initCamellia :: ByteArray key
             => key -- ^ The key to create the camellia context
             -> CryptoFailable Camellia
initCamellia key
    | B.length key /= 16 = CryptoFailed $ CryptoError_KeySizeInvalid
    | otherwise          =
        let (kL, _, kA, _) = setKeyInterim key in

        let (Word128 kw1 kw2) = (kL `rotl128` 0) in
        let (Word128 k1 k2)   = (kA `rotl128` 0) in
        let (Word128 k3 k4)   = (kL `rotl128` 15) in
        let (Word128 k5 k6)   = (kA `rotl128` 15) in
        let (Word128 ke1 ke2) = (kA `rotl128` 30) in --ke1 = (KA <<<  30) >> 64; ke2 = (KA <<<  30) & MASK64;
        let (Word128 k7 k8)   = (kL `rotl128` 45) in --k7  = (KL <<<  45) >> 64; k8  = (KL <<<  45) & MASK64;
        let (Word128 k9 _)    = (kA `rotl128` 45) in --k9  = (KA <<<  45) >> 64;
        let (Word128 _ k10)   = (kL `rotl128` 60) in
        let (Word128 k11 k12) = (kA `rotl128` 60) in
        let (Word128 ke3 ke4) = (kL `rotl128` 77) in
        let (Word128 k13 k14) = (kL `rotl128` 94) in
        let (Word128 k15 k16) = (kA `rotl128` 94) in
        let (Word128 k17 k18) = (kL `rotl128` 111) in
        let (Word128 kw3 kw4) = (kA `rotl128` 111) in

        CryptoPassed $ Camellia
            { kw = array64 4 [ kw1, kw2, kw3, kw4 ]
            , ke = array64 4 [ ke1, ke2, ke3, ke4 ]
            , k  = array64 18 [ k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18 ]
            }

feistel :: Word64 -> Word64 -> Word64
feistel fin sk = 
    let x = fin `xor` sk in
    let (t1, t2, t3, t4, t5, t6, t7, t8) = w64tow8 x in
    let t1' = sbox1 t1 in
    let t2' = sbox2 t2 in
    let t3' = sbox3 t3 in
    let t4' = sbox4 t4 in
    let t5' = sbox2 t5 in
    let t6' = sbox3 t6 in
    let t7' = sbox4 t7 in
    let t8' = sbox1 t8 in
    let y1 = t1' `xor` t3' `xor` t4' `xor` t6' `xor` t7' `xor` t8' in
    let y2 = t1' `xor` t2' `xor` t4' `xor` t5' `xor` t7' `xor` t8' in
    let y3 = t1' `xor` t2' `xor` t3' `xor` t5' `xor` t6' `xor` t8' in
    let y4 = t2' `xor` t3' `xor` t4' `xor` t5' `xor` t6' `xor` t7' in
    let y5 = t1' `xor` t2' `xor` t6' `xor` t7' `xor` t8' in
    let y6 = t2' `xor` t3' `xor` t5' `xor` t7' `xor` t8' in
    let y7 = t3' `xor` t4' `xor` t5' `xor` t6' `xor` t8' in
    let y8 = t1' `xor` t4' `xor` t5' `xor` t6' `xor` t7' in
    w8tow64 (y1, y2, y3, y4, y5, y6, y7, y8)

fl :: Word64 -> Word64 -> Word64
fl fin sk =
    let (x1, x2) = w64to32 fin in
    let (k1, k2) = w64to32 sk in
    let y2 = x2 `xor` ((x1 .&. k1) `rotateL` 1) in
    let y1 = x1 `xor` (y2 .|. k2) in
    w32to64 (y1, y2)

flinv :: Word64 -> Word64 -> Word64
flinv fin sk =
    let (y1, y2) = w64to32 fin in
    let (k1, k2) = w64to32 sk in
    let x1 = y1 `xor` (y2 .|. k2) in
    let x2 = y2 `xor` ((x1 .&. k1) `rotateL` 1) in
    w32to64 (x1, x2)

{- in decrypt mode 0->17 1->16 ... -}
getKeyK :: Mode -> Camellia -> Int -> Word64
getKeyK Encrypt key i = k key `arrayRead64` i
getKeyK Decrypt key i = k key `arrayRead64` (17 - i)

{- in decrypt mode 0->3 1->2 2->1 3->0 -}
getKeyKe :: Mode -> Camellia -> Int -> Word64
getKeyKe Encrypt key i = ke key `arrayRead64` i
getKeyKe Decrypt key i = ke key `arrayRead64` (3 - i)

{- in decrypt mode 0->2 1->3 2->0 3->1 -}
getKeyKw :: Mode -> Camellia -> Int -> Word64
getKeyKw Encrypt key i = (kw key) `arrayRead64` i
getKeyKw Decrypt key i = (kw key) `arrayRead64` ((i + 2) `mod` 4)

{- perform the following
    D2 = D2 ^ F(D1, k1);     // Round 1
    D1 = D1 ^ F(D2, k2);     // Round 2
    D2 = D2 ^ F(D1, k3);     // Round 3
    D1 = D1 ^ F(D2, k4);     // Round 4
    D2 = D2 ^ F(D1, k5);     // Round 5
    D1 = D1 ^ F(D2, k6);     // Round 6
 -}
doBlockRound :: Mode -> Camellia -> Word64 -> Word64 -> Int -> (Word64, Word64)
doBlockRound mode key d1 d2 i =
    let r1 = d2 `xor` feistel d1 (getKeyK mode key (0+i)) in     {- Round 1+i -}
    let r2 = d1 `xor` feistel r1 (getKeyK mode key (1+i)) in     {- Round 2+i -}
    let r3 = r1 `xor` feistel r2 (getKeyK mode key (2+i)) in     {- Round 3+i -}
    let r4 = r2 `xor` feistel r3 (getKeyK mode key (3+i)) in     {- Round 4+i -}
    let r5 = r3 `xor` feistel r4 (getKeyK mode key (4+i)) in     {- Round 5+i -}
    let r6 = r4 `xor` feistel r5 (getKeyK mode key (5+i)) in     {- Round 6+i -}
    (r6, r5)

doBlock :: Mode -> Camellia -> Word128 -> Word128
doBlock mode key (Word128 d1 d2) =
    let d1a = d1 `xor` (getKeyKw mode key 0) in {- Prewhitening -}
    let d2a = d2 `xor` (getKeyKw mode key 1) in

    let (d1b, d2b) = doBlockRound mode key d1a d2a 0 in

    let d1c = fl    d1b (getKeyKe mode key 0) in {- FL -}
    let d2c = flinv d2b (getKeyKe mode key 1) in {- FLINV -}

    let (d1d, d2d) = doBlockRound mode key d1c d2c 6 in

    let d1e = fl    d1d (getKeyKe mode key 2) in {- FL -}
    let d2e = flinv d2d (getKeyKe mode key 3) in {- FLINV -}

    let (d1f, d2f) = doBlockRound mode key d1e d2e 12 in

    let d2g = d2f `xor` (getKeyKw mode key 2) in {- Postwhitening -}
    let d1g = d1f `xor` (getKeyKw mode key 3) in
    w64tow128 (d2g, d1g)

{- encryption for 128 bits blocks -}
encryptBlock :: Camellia -> Word128 -> Word128
encryptBlock = doBlock Encrypt

{- decryption for 128 bits blocks -}
decryptBlock :: Camellia -> Word128 -> Word128
decryptBlock = doBlock Decrypt

-- | Encrypts the given ByteString using the given Key
encrypt :: ByteArray ba
        => Camellia     -- ^ The key to use
        -> ba           -- ^ The data to encrypt
        -> ba
encrypt key = B.mapAsWord128 (encryptBlock key)

-- | Decrypts the given ByteString using the given Key
decrypt :: ByteArray ba
        => Camellia     -- ^ The key to use
        -> ba           -- ^ The data to decrypt
        -> ba
decrypt key = B.mapAsWord128 (decryptBlock key)
