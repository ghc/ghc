{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
module Crypto.Cipher.Twofish.Primitive
    ( Twofish
    , initTwofish
    , encrypt
    , decrypt
    ) where

import           Crypto.Error
import           Crypto.Internal.ByteArray (ByteArray)
import qualified Crypto.Internal.ByteArray as B
import           Crypto.Internal.WordArray
import           Data.Word
import           Data.Bits
import           Data.List

-- Based on the Golang referance implementation
-- https://github.com/golang/crypto/blob/master/twofish/twofish.go


-- BlockSize is the constant block size of Twofish.
blockSize :: Int
blockSize = 16

mdsPolynomial, rsPolynomial :: Word32
mdsPolynomial = 0x169 -- x^8 + x^6 + x^5 + x^3 + 1, see [TWOFISH] 4.2
rsPolynomial = 0x14d  -- x^8 + x^6 + x^3 + x^2 + 1, see [TWOFISH] 4.3

data Twofish = Twofish { s :: (Array32, Array32, Array32, Array32)
                       , k :: Array32 }

data ByteSize = Bytes16 | Bytes24 | Bytes32 deriving (Eq)

data KeyPackage ba = KeyPackage { rawKeyBytes :: ba
                                , byteSize :: ByteSize }

buildPackage :: ByteArray ba => ba -> Maybe (KeyPackage ba)
buildPackage key
    | B.length key == 16 = return $ KeyPackage key Bytes16
    | B.length key == 24 = return $ KeyPackage key Bytes24
    | B.length key == 32 = return $ KeyPackage key Bytes32
    | otherwise = Nothing

-- | Initialize a 128-bit, 192-bit, or 256-bit key
--
-- Return the initialized key or a error message if the given
-- keyseed was not 16-bytes in length.
initTwofish :: ByteArray key
            => key -- ^ The key to create the twofish context
            -> CryptoFailable Twofish
initTwofish key =
    case buildPackage key of Nothing -> CryptoFailed CryptoError_KeySizeInvalid
                             Just keyPackage -> CryptoPassed Twofish { k = generatedK, s = generatedS }
                                  where generatedK = array32 40 $ genK keyPackage
                                        generatedS = genSboxes keyPackage $ sWords key

mapBlocks :: ByteArray ba => (ba -> ba) -> ba -> ba
mapBlocks operation input
    | B.null rest = blockOutput
    | otherwise = blockOutput `B.append` mapBlocks operation rest
        where (block, rest) = B.splitAt blockSize input
              blockOutput = operation block

-- | Encrypts the given ByteString using the given Key
encrypt :: ByteArray ba
        => Twofish     -- ^ The key to use
        -> ba           -- ^ The data to encrypt
        -> ba
encrypt cipher = mapBlocks (encryptBlock cipher)

encryptBlock :: ByteArray ba => Twofish -> ba -> ba
encryptBlock Twofish { s = (s1, s2, s3, s4), k = ks } message = store32ls ts
    where (a, b, c, d) = load32ls message
          a' = a `xor` arrayRead32 ks 0
          b' = b `xor` arrayRead32 ks 1
          c' = c `xor` arrayRead32 ks 2
          d' = d `xor` arrayRead32 ks 3
          (!a'', !b'', !c'', !d'') = foldl' shuffle (a', b', c', d') [0..7]
          ts = (c'' `xor` arrayRead32 ks 4, d'' `xor` arrayRead32 ks 5, a'' `xor` arrayRead32 ks 6, b'' `xor` arrayRead32 ks 7)

          shuffle :: (Word32, Word32, Word32, Word32) -> Int -> (Word32, Word32, Word32, Word32)
          shuffle (!retA, !retB, !retC, !retD) ind = (retA', retB', retC', retD')
            where [k0, k1, k2, k3] = fmap (\offset -> arrayRead32 ks $ (8 + 4 * ind) + offset) [0..3]
                  t2 = byteIndex s2 retB `xor` byteIndex s3 (shiftR retB 8) `xor` byteIndex s4 (shiftR retB 16) `xor` byteIndex s1 (shiftR retB 24)
                  t1 = (byteIndex s1 retA `xor` byteIndex s2 (shiftR retA 8) `xor` byteIndex s3 (shiftR retA 16) `xor` byteIndex s4 (shiftR retA 24)) + t2
                  retC' = rotateR (retC `xor` (t1 + k0)) 1
                  retD' = rotateL retD 1 `xor` (t1 + t2 + k1)
                  t2' = byteIndex s2 retD' `xor` byteIndex s3 (shiftR retD' 8) `xor` byteIndex s4 (shiftR retD' 16) `xor` byteIndex s1 (shiftR retD' 24)
                  t1' = (byteIndex s1 retC' `xor` byteIndex s2 (shiftR retC' 8) `xor` byteIndex s3 (shiftR retC' 16) `xor` byteIndex s4 (shiftR retC' 24)) + t2'
                  retA' = rotateR (retA `xor` (t1' + k2)) 1
                  retB' = rotateL retB 1 `xor` (t1' + t2' + k3)

-- Unsafe, no bounds checking
byteIndex :: Array32 -> Word32 -> Word32
byteIndex xs ind  = arrayRead32 xs $ fromIntegral byte
    where byte = ind `mod` 256

-- | Decrypts the given ByteString using the given Key
decrypt :: ByteArray ba
        => Twofish     -- ^ The key to use
        -> ba           -- ^ The data to decrypt
        -> ba
decrypt cipher = mapBlocks (decryptBlock cipher)

{- decryption for 128 bits blocks -}
decryptBlock :: ByteArray ba => Twofish -> ba -> ba
decryptBlock Twofish { s = (s1, s2, s3, s4), k = ks } message = store32ls ixs
    where (a, b, c, d) = load32ls message
          a' = c `xor` arrayRead32 ks 6
          b' = d `xor` arrayRead32 ks 7
          c' = a `xor` arrayRead32 ks 4
          d' = b `xor` arrayRead32 ks 5
          (!a'', !b'', !c'', !d'') = foldl' unshuffle (a', b', c', d') [8, 7..1]
          ixs = (a'' `xor` arrayRead32 ks 0, b'' `xor` arrayRead32 ks 1, c'' `xor` arrayRead32 ks 2, d'' `xor` arrayRead32 ks 3)

          unshuffle :: (Word32, Word32, Word32, Word32) -> Int -> (Word32, Word32, Word32, Word32)
          unshuffle (!retA, !retB, !retC, !retD) ind = (retA', retB', retC', retD')
            where [k0, k1, k2, k3] = fmap (\offset -> arrayRead32 ks $ (4 + 4 * ind) + offset) [0..3]
                  t2 = byteIndex s2 retD `xor` byteIndex s3 (shiftR retD 8) `xor` byteIndex s4 (shiftR retD 16) `xor` byteIndex s1 (shiftR retD 24)
                  t1 = (byteIndex s1 retC `xor` byteIndex s2 (shiftR retC 8) `xor` byteIndex s3 (shiftR retC 16) `xor` byteIndex s4 (shiftR retC 24)) + t2
                  retA' = rotateL retA 1 `xor` (t1 + k2)
                  retB' = rotateR (retB `xor` (t2 + t1 + k3)) 1
                  t2' = byteIndex s2 retB' `xor` byteIndex s3 (shiftR retB' 8) `xor` byteIndex s4 (shiftR retB' 16) `xor` byteIndex s1 (shiftR retB' 24)
                  t1' = (byteIndex s1 retA' `xor` byteIndex s2 (shiftR retA' 8) `xor` byteIndex s3 (shiftR retA' 16) `xor` byteIndex s4 (shiftR retA' 24)) + t2'
                  retC' = rotateL retC 1 `xor` (t1' + k0)
                  retD' = rotateR (retD `xor` (t2' + t1' + k1)) 1

sbox0 :: Int -> Word8
sbox0 = arrayRead8 t
    where t = array8
            "\xa9\x67\xb3\xe8\x04\xfd\xa3\x76\x9a\x92\x80\x78\xe4\xdd\xd1\x38\
            \\x0d\xc6\x35\x98\x18\xf7\xec\x6c\x43\x75\x37\x26\xfa\x13\x94\x48\
            \\xf2\xd0\x8b\x30\x84\x54\xdf\x23\x19\x5b\x3d\x59\xf3\xae\xa2\x82\
            \\x63\x01\x83\x2e\xd9\x51\x9b\x7c\xa6\xeb\xa5\xbe\x16\x0c\xe3\x61\
            \\xc0\x8c\x3a\xf5\x73\x2c\x25\x0b\xbb\x4e\x89\x6b\x53\x6a\xb4\xf1\
            \\xe1\xe6\xbd\x45\xe2\xf4\xb6\x66\xcc\x95\x03\x56\xd4\x1c\x1e\xd7\
            \\xfb\xc3\x8e\xb5\xe9\xcf\xbf\xba\xea\x77\x39\xaf\x33\xc9\x62\x71\
            \\x81\x79\x09\xad\x24\xcd\xf9\xd8\xe5\xc5\xb9\x4d\x44\x08\x86\xe7\
            \\xa1\x1d\xaa\xed\x06\x70\xb2\xd2\x41\x7b\xa0\x11\x31\xc2\x27\x90\
            \\x20\xf6\x60\xff\x96\x5c\xb1\xab\x9e\x9c\x52\x1b\x5f\x93\x0a\xef\
            \\x91\x85\x49\xee\x2d\x4f\x8f\x3b\x47\x87\x6d\x46\xd6\x3e\x69\x64\
            \\x2a\xce\xcb\x2f\xfc\x97\x05\x7a\xac\x7f\xd5\x1a\x4b\x0e\xa7\x5a\
            \\x28\x14\x3f\x29\x88\x3c\x4c\x02\xb8\xda\xb0\x17\x55\x1f\x8a\x7d\
            \\x57\xc7\x8d\x74\xb7\xc4\x9f\x72\x7e\x15\x22\x12\x58\x07\x99\x34\
            \\x6e\x50\xde\x68\x65\xbc\xdb\xf8\xc8\xa8\x2b\x40\xdc\xfe\x32\xa4\
            \\xca\x10\x21\xf0\xd3\x5d\x0f\x00\x6f\x9d\x36\x42\x4a\x5e\xc1\xe0"#

sbox1 :: Int -> Word8
sbox1 = arrayRead8 t
    where t = array8
            "\x75\xf3\xc6\xf4\xdb\x7b\xfb\xc8\x4a\xd3\xe6\x6b\x45\x7d\xe8\x4b\
            \\xd6\x32\xd8\xfd\x37\x71\xf1\xe1\x30\x0f\xf8\x1b\x87\xfa\x06\x3f\
            \\x5e\xba\xae\x5b\x8a\x00\xbc\x9d\x6d\xc1\xb1\x0e\x80\x5d\xd2\xd5\
            \\xa0\x84\x07\x14\xb5\x90\x2c\xa3\xb2\x73\x4c\x54\x92\x74\x36\x51\
            \\x38\xb0\xbd\x5a\xfc\x60\x62\x96\x6c\x42\xf7\x10\x7c\x28\x27\x8c\
            \\x13\x95\x9c\xc7\x24\x46\x3b\x70\xca\xe3\x85\xcb\x11\xd0\x93\xb8\
            \\xa6\x83\x20\xff\x9f\x77\xc3\xcc\x03\x6f\x08\xbf\x40\xe7\x2b\xe2\
            \\x79\x0c\xaa\x82\x41\x3a\xea\xb9\xe4\x9a\xa4\x97\x7e\xda\x7a\x17\
            \\x66\x94\xa1\x1d\x3d\xf0\xde\xb3\x0b\x72\xa7\x1c\xef\xd1\x53\x3e\
            \\x8f\x33\x26\x5f\xec\x76\x2a\x49\x81\x88\xee\x21\xc4\x1a\xeb\xd9\
            \\xc5\x39\x99\xcd\xad\x31\x8b\x01\x18\x23\xdd\x1f\x4e\x2d\xf9\x48\
            \\x4f\xf2\x65\x8e\x78\x5c\x58\x19\x8d\xe5\x98\x57\x67\x7f\x05\x64\
            \\xaf\x63\xb6\xfe\xf5\xb7\x3c\xa5\xce\xe9\x68\x44\xe0\x4d\x43\x69\
            \\x29\x2e\xac\x15\x59\xa8\x0a\x9e\x6e\x47\xdf\x34\x35\x6a\xcf\xdc\
            \\x22\xc9\xc0\x9b\x89\xd4\xed\xab\x12\xa2\x0d\x52\xbb\x02\x2f\xa9\
            \\xd7\x61\x1e\xb4\x50\x04\xf6\xc2\x16\x25\x86\x56\x55\x09\xbe\x91"#

rs :: [[Word8]]
rs = [ [0x01, 0xA4, 0x55, 0x87, 0x5A, 0x58, 0xDB, 0x9E]
     , [0xA4, 0x56, 0x82, 0xF3, 0x1E, 0xC6, 0x68, 0xE5]
     , [0x02, 0xA1, 0xFC, 0xC1, 0x47, 0xAE, 0x3D, 0x19]
     , [0xA4, 0x55, 0x87, 0x5A, 0x58, 0xDB, 0x9E, 0x03] ]



load32ls :: ByteArray ba => ba -> (Word32, Word32, Word32, Word32)
load32ls message = (intify q1, intify q2, intify q3, intify q4)
    where (half1, half2) = B.splitAt 8 message
          (q1, q2) = B.splitAt 4 half1
          (q3, q4) = B.splitAt 4 half2

          intify :: ByteArray ba => ba -> Word32
          intify bytes = foldl' (\int (!word, !ind) -> int .|. shiftL (fromIntegral word) (ind * 8) ) 0 (zip (B.unpack bytes) [0..])

store32ls :: ByteArray ba => (Word32, Word32, Word32, Word32) -> ba
store32ls (a, b, c, d) = B.pack $ concatMap splitWordl [a, b, c, d]
    where splitWordl :: Word32 -> [Word8]
          splitWordl w = fmap (\ind -> fromIntegral $ shiftR w (8 * ind)) [0..3]


-- Create S words
sWords :: ByteArray ba => ba -> [Word8]
sWords key = sWord
    where word64Count = B.length key `div` 2
          sWord = concatMap (\wordIndex ->
                        map (\rsRow ->
                            foldl' (\acc (!rsVal, !colIndex) ->
                                acc `xor` gfMult rsPolynomial (B.index key $ 8 * wordIndex + colIndex) rsVal
                                ) 0 (zip rsRow [0..])
                            ) rs
                    ) [0..word64Count - 1]

data Column = Zero | One | Two | Three deriving (Show, Eq, Enum, Bounded)

genSboxes :: KeyPackage ba -> [Word8] -> (Array32, Array32, Array32, Array32)
genSboxes keyPackage ws = (mkArray b0', mkArray b1', mkArray b2', mkArray b3')
    where range = [0..255]
          mkArray = array32 256
          [w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15] = take 16 ws
          (b0', b1', b2', b3') = sboxBySize $ byteSize keyPackage

          sboxBySize :: ByteSize -> ([Word32], [Word32], [Word32], [Word32])
          sboxBySize Bytes16 = (b0, b1, b2, b3)
            where !b0 = fmap mapper range
                    where mapper :: Int -> Word32
                          mapper byte = mdsColumnMult ((sbox1 . fromIntegral) ((sbox0 . fromIntegral $ sbox0 byte `xor` w0) `xor` w4)) Zero
                  !b1 = fmap mapper range
                    where mapper byte = mdsColumnMult ((sbox0 . fromIntegral) ((sbox0 . fromIntegral $ sbox1 byte `xor` w1) `xor` w5)) One
                  !b2 = fmap mapper range
                    where mapper byte = mdsColumnMult ((sbox1 . fromIntegral) ((sbox1 . fromIntegral $ sbox0 byte `xor` w2) `xor` w6)) Two
                  !b3 = fmap mapper range
                    where mapper byte = mdsColumnMult ((sbox0 . fromIntegral) ((sbox1 . fromIntegral $ sbox1 byte `xor` w3) `xor` w7)) Three

          sboxBySize Bytes24 = (b0, b1, b2, b3)
            where !b0 = fmap mapper range
                    where mapper byte = mdsColumnMult ((sbox1 . fromIntegral) ((sbox0 . fromIntegral) ((sbox0 . fromIntegral $ sbox1 byte `xor` w0) `xor` w4) `xor` w8)) Zero
                  !b1 = fmap mapper range
                    where mapper byte = mdsColumnMult ((sbox0 . fromIntegral) ((sbox0 . fromIntegral) ((sbox1 . fromIntegral $ sbox1 byte `xor` w1) `xor` w5) `xor` w9)) One
                  !b2 = fmap mapper range
                    where mapper byte = mdsColumnMult ((sbox1 . fromIntegral) ((sbox1 . fromIntegral) ((sbox0 . fromIntegral $ sbox0 byte `xor` w2) `xor` w6) `xor` w10)) Two
                  !b3 = fmap mapper range
                    where mapper byte = mdsColumnMult ((sbox0 . fromIntegral) ((sbox1 . fromIntegral) ((sbox1 . fromIntegral $ sbox0 byte `xor` w3) `xor` w7) `xor` w11)) Three

          sboxBySize Bytes32 = (b0, b1, b2, b3)
            where !b0 = fmap mapper range
                    where mapper byte = mdsColumnMult ((sbox1 . fromIntegral) ((sbox0 . fromIntegral) ((sbox0 . fromIntegral) ((sbox1 . fromIntegral $ sbox1 byte `xor` w0) `xor` w4) `xor` w8) `xor` w12)) Zero
                  !b1 = fmap mapper range
                    where mapper byte = mdsColumnMult ((sbox0 . fromIntegral) ((sbox0 . fromIntegral) ((sbox1 . fromIntegral) ((sbox1 . fromIntegral $ sbox0 byte `xor` w1) `xor` w5) `xor` w9) `xor` w13)) One
                  !b2 = fmap mapper range
                    where mapper byte = mdsColumnMult ((sbox1 . fromIntegral) ((sbox1 . fromIntegral) ((sbox0 . fromIntegral) ((sbox0 . fromIntegral $ sbox0 byte `xor` w2) `xor` w6) `xor` w10) `xor` w14)) Two
                  !b3 = fmap mapper range
                    where mapper byte = mdsColumnMult ((sbox0 . fromIntegral) ((sbox1 . fromIntegral) ((sbox1 . fromIntegral) ((sbox0 . fromIntegral $ sbox1 byte `xor` w3) `xor` w7) `xor` w11) `xor` w15)) Three

genK :: (ByteArray ba) => KeyPackage ba -> [Word32]
genK keyPackage = concatMap makeTuple [0..19]
    where makeTuple :: Word8 -> [Word32]
          makeTuple idx = [a + b', rotateL (2 * b' + a) 9]
            where tmp1 = replicate 4 $ 2 * idx
                  tmp2 = fmap (+1) tmp1
                  a = h tmp1 keyPackage 0
                  b = h tmp2 keyPackage 1
                  b' = rotateL b 8

h :: (ByteArray ba) => [Word8] -> KeyPackage ba -> Int -> Word32
h input keyPackage offset =  foldl' xorMdsColMult 0 $ zip [y0f, y1f, y2f, y3f] $ enumFrom Zero
    where key = rawKeyBytes keyPackage
          [y0, y1, y2, y3] = take 4 input
          (!y0f, !y1f, !y2f, !y3f) = run (y0, y1, y2, y3) $ byteSize keyPackage

          run :: (Word8, Word8, Word8, Word8) -> ByteSize -> (Word8, Word8, Word8, Word8)
          run (!y0'', !y1'', !y2'', !y3'') Bytes32 = run (y0', y1', y2', y3') Bytes24
            where y0' = sbox1 (fromIntegral y0'') `xor` B.index key (4 * (6 + offset) + 0)
                  y1' = sbox0 (fromIntegral y1'') `xor` B.index key (4 * (6 + offset) + 1)
                  y2' = sbox0 (fromIntegral y2'') `xor` B.index key (4 * (6 + offset) + 2)
                  y3' = sbox1 (fromIntegral y3'') `xor` B.index key (4 * (6 + offset) + 3)

          run (!y0'', !y1'', !y2'', !y3'') Bytes24 = run (y0', y1', y2', y3') Bytes16
            where y0' = sbox1 (fromIntegral y0'') `xor` B.index key (4 * (4 + offset) + 0)
                  y1' = sbox1 (fromIntegral y1'') `xor` B.index key (4 * (4 + offset) + 1)
                  y2' = sbox0 (fromIntegral y2'') `xor` B.index key (4 * (4 + offset) + 2)
                  y3' = sbox0 (fromIntegral y3'') `xor` B.index key (4 * (4 + offset) + 3)

          run (!y0'', !y1'', !y2'', !y3'') Bytes16 = (y0', y1', y2', y3')
            where y0' = sbox1 . fromIntegral $ (sbox0 . fromIntegral $ (sbox0 (fromIntegral y0'') `xor` B.index key (4 * (2 + offset) + 0))) `xor` B.index key (4 * (0 + offset) + 0)
                  y1' = sbox0 . fromIntegral $ (sbox0 . fromIntegral $ (sbox1 (fromIntegral y1'') `xor` B.index key (4 * (2 + offset) + 1))) `xor` B.index key (4 * (0 + offset) + 1)
                  y2' = sbox1 . fromIntegral $ (sbox1 . fromIntegral $ (sbox0 (fromIntegral y2'') `xor` B.index key (4 * (2 + offset) + 2))) `xor` B.index key (4 * (0 + offset) + 2)
                  y3' = sbox0 . fromIntegral $ (sbox1 . fromIntegral $ (sbox1 (fromIntegral y3'') `xor` B.index key (4 * (2 + offset) + 3))) `xor` B.index key (4 * (0 + offset) + 3)

          xorMdsColMult :: Word32 -> (Word8, Column) -> Word32
          xorMdsColMult acc wordAndIndex = acc `xor` uncurry mdsColumnMult wordAndIndex

mdsColumnMult :: Word8 -> Column -> Word32
mdsColumnMult !byte !col =
    case col of Zero  -> input .|. rotateL mul5B 8 .|. rotateL mulEF 16 .|. rotateL mulEF 24
                One   -> mulEF .|. rotateL mulEF 8 .|. rotateL mul5B 16 .|. rotateL input 24
                Two   -> mul5B .|. rotateL mulEF 8 .|. rotateL input 16 .|. rotateL mulEF 24
                Three -> mul5B .|. rotateL input 8 .|. rotateL mulEF 16 .|. rotateL mul5B 24
        where input = fromIntegral byte
              mul5B = fromIntegral $ gfMult mdsPolynomial byte 0x5B
              mulEF = fromIntegral $ gfMult mdsPolynomial byte 0xEF

tupInd :: (Bits b) => b -> (a, a) -> a
tupInd b
    | testBit b 0 = snd
    | otherwise = fst

gfMult :: Word32 -> Word8 -> Word8 -> Word8
gfMult p a b = fromIntegral $ run a b' p' result 0
    where b' = (0, fromIntegral b)
          p' = (0, p)
          result = 0

          run :: Word8 -> (Word32, Word32) -> (Word32, Word32) -> Word32 -> Int -> Word32
          run a' b'' p'' result' count =
            if count == 7
            then result''
            else run a'' b''' p'' result'' (count + 1)
                where result'' = result' `xor` tupInd (a' .&. 1) b''
                      a'' = shiftR a' 1
                      b''' = (fst b'', tupInd (shiftR (snd b'') 7) p'' `xor` shiftL (snd b'') 1)
