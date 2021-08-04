
module KAT_CMAC (tests) where

import qualified Crypto.MAC.CMAC as CMAC
import           Crypto.Cipher.Types (Cipher, cipherInit, BlockCipher, ecbEncrypt, blockSize)
import           Crypto.Error (eitherCryptoError)
import           Crypto.Cipher.AES (AES128, AES192, AES256)
import           Crypto.Cipher.TripleDES (DES_EDE3, DES_EDE2)

import           Imports

import           Data.Char (digitToInt)
import qualified Data.ByteString as BS
import qualified Data.ByteArray as B


hxs :: String -> ByteString
hxs = BS.pack . rec' where
    dtoW8 = fromIntegral . digitToInt
    rec' (' ':xs)  =  rec' xs
    rec' (x:y:xs)  =  dtoW8 x * 16 + dtoW8 y : rec' xs
    rec' [_]       =  error "hxs: invalid hex pattern."
    rec' []        =  []

unsafeCipher :: Cipher k => ByteString -> k
unsafeCipher = either (error . show) id . eitherCryptoError . cipherInit

ecb0 :: BlockCipher k => k -> ByteString
ecb0 k = ecbEncrypt k $ BS.replicate (blockSize k) 0

{- Test vectors from NIST data-sheet
   (AES128-CMAC, AES192-CMAC, AES256-CMAC, Three Key TDEA, Two Key TDEA)
   http://csrc.nist.gov/publications/nistpubs/800-38B/Updated_CMAC_Examples.pdf
   The data of AES128-CMAC is same as them in RFC4493.
 -}

msg512 :: ByteString
msg512 =
  hxs $
  "6bc1bee2 2e409f96 e93d7e11 7393172a" ++
  "ae2d8a57 1e03ac9c 9eb76fac 45af8e51" ++
  "30c81c46 a35ce411 e5fbc119 1a0a52ef" ++
  "f69f2445 df4f9b17 ad2b417b e66c3710"

msg320 :: ByteString
msg320 = BS.take 40 msg512

msg256 :: ByteString
msg256 = BS.take 32 msg512

msg160 :: ByteString
msg160 = BS.take 20 msg512

msg128 :: ByteString
msg128 = BS.take 16 msg512

msg64 :: ByteString
msg64 = BS.take 8 msg512

msg0 :: ByteString
msg0 = BS.empty

bsCMAC :: BlockCipher k => k -> ByteString -> ByteString
bsCMAC k = B.convert . CMAC.cmac k

gAES128 :: TestTree
gAES128 =
    igroup "aes128"
    [ ecb0 aes128key @?=  hxs "7df76b0c 1ab899b3 3e42f047 b91b546f"
    , aes128k1 @?=        hxs "fbeed618 35713366 7c85e08f 7236a8de"
    , aes128k2 @?=        hxs "f7ddac30 6ae266cc f90bc11e e46d513b"

    , bsCMAC aes128key msg0
      @?=                 hxs "bb1d6929 e9593728 7fa37d12 9b756746"
    , bsCMAC aes128key msg128
      @?=                 hxs "070a16b4 6b4d4144 f79bdd9d d04a287c"
    , bsCMAC aes128key msg320
      @?=                 hxs "dfa66747 de9ae630 30ca3261 1497c827"
    , bsCMAC aes128key msg512
      @?=                 hxs "51f0bebf 7e3b9d92 fc497417 79363cfe"
    ]
  where
    aes128key :: AES128
    aes128key =
        unsafeCipher $ hxs
        "2b7e1516 28aed2a6 abf71588 09cf4f3c"

    aes128k1, aes128k2 :: ByteString
    (aes128k1, aes128k2) = CMAC.subKeys aes128key


gAES192 :: TestTree
gAES192 =
    igroup "aes192"
    [ ecb0 aes192key @?=  hxs "22452d8e 49a8a593 9f7321ce ea6d514b"
    , aes192k1 @?=        hxs "448a5b1c 93514b27 3ee6439d d4daa296"
    , aes192k2 @?=        hxs "8914b639 26a2964e 7dcc873b a9b5452c"

    , bsCMAC aes192key msg0
      @?=                 hxs "d17ddf46 adaacde5 31cac483 de7a9367"
    , bsCMAC aes192key msg128
      @?=                 hxs "9e99a7bf 31e71090 0662f65e 617c5184"
    , bsCMAC aes192key msg320
      @?=                 hxs "8a1de5be 2eb31aad 089a82e6 ee908b0e"
    , bsCMAC aes192key msg512
      @?=                 hxs "a1d5df0e ed790f79 4d775896 59f39a11"
    ]
  where
    aes192key :: AES192
    aes192key =
        unsafeCipher . hxs $
        "8e73b0f7 da0e6452 c810f32b 809079e5" ++
        "62f8ead2 522c6b7b"

    aes192k1, aes192k2 :: ByteString
    (aes192k1, aes192k2) = CMAC.subKeys aes192key

gAES256 :: TestTree
gAES256 =
    igroup "aes256"
    [ ecb0 aes256key @?=  hxs "e568f681 94cf76d6 174d4cc0 4310a854"
    , aes256k1 @?=        hxs "cad1ed03 299eedac 2e9a9980 8621502f"
    , aes256k2 @?=        hxs "95a3da06 533ddb58 5d353301 0c42a0d9"

    , bsCMAC aes256key msg0
      @?=                 hxs "028962f6 1b7bf89e fc6b551f 4667d983"
    , bsCMAC aes256key msg128
      @?=                 hxs "28a7023f 452e8f82 bd4bf28d 8c37c35c"
    , bsCMAC aes256key msg320
      @?=                 hxs "aaf3d8f1 de5640c2 32f5b169 b9c911e6"
    , bsCMAC aes256key msg512
      @?=                 hxs "e1992190 549f6ed5 696a2c05 6c315410"
    ]
  where
    aes256key :: AES256
    aes256key =
        unsafeCipher . hxs $
        "603deb10 15ca71be 2b73aef0 857d7781" ++
        "1f352c07 3b6108d7 2d9810a3 0914dff4"

    aes256k1, aes256k2 :: ByteString
    (aes256k1, aes256k2) = CMAC.subKeys aes256key

gTDEA3 :: TestTree
gTDEA3 =
    igroup "Three Key TDEA"
    [ ecb0 tdea3key @?=  hxs "c8cc74e9 8a7329a2"
    , tdea3k1 @?=        hxs "9198e9d3 14e6535f"
    , tdea3k2 @?=        hxs "2331d3a6 29cca6a5"

    , bsCMAC tdea3key msg0
      @?=                hxs "b7a688e1 22ffaf95"
    , bsCMAC tdea3key msg64
      @?=                hxs "8e8f2931 36283797"
    , bsCMAC tdea3key msg160
      @?=                hxs "743ddbe0 ce2dc2ed"
    , bsCMAC tdea3key msg256
      @?=                hxs "33e6b109 2400eae5"
    ]
  where
    tdea3key :: DES_EDE3
    tdea3key =
        unsafeCipher . hxs $
        "8aa83bf8 cbda1062" ++
        "0bc1bf19 fbb6cd58" ++
        "bc313d4a 371ca8b5"

    tdea3k1, tdea3k2 :: ByteString
    (tdea3k1, tdea3k2) = CMAC.subKeys tdea3key

gTDEA2 :: TestTree
gTDEA2 =
    igroup "Two Key TDEA"
    [ ecb0 tdea2key @?=  hxs "c7679b9f 6b8d7d7a"
    , tdea2k1 @?=        hxs "8ecf373e d71afaef"
    , tdea2k2 @?=        hxs "1d9e6e7d ae35f5c5"

    , bsCMAC tdea2key msg0
      @?=                hxs "bd2ebf9a 3ba00361"
    , bsCMAC tdea2key msg64
      @?=                hxs "4ff2ab81 3c53ce83"
    , bsCMAC tdea2key msg160
      @?=                hxs "62dd1b47 1902bd4e"
    , bsCMAC tdea2key msg256
      @?=                hxs "31b1e431 dabc4eb8"
    ]
  where
    tdea2key :: DES_EDE2
    tdea2key =
          unsafeCipher . hxs $
          "4cf15134 a2850dd5" ++
          "8a3d10ba 80570d38"

    tdea2k1, tdea2k2 :: ByteString
    (tdea2k1, tdea2k2) = CMAC.subKeys tdea2key

igroup :: TestName -> [Assertion] -> TestTree
igroup nm = testGroup nm . zipWith (flip ($)) [1..] . map icase
  where
    icase c i = testCase (show (i :: Int)) c

nistVectors :: TestTree
nistVectors =
    testGroup "KAT - NIST test vectors"
    [ gAES128, gAES192, gAES256, gTDEA3, gTDEA2 ]

tests :: TestTree
tests =
    testGroup "CMAC"
    [ nistVectors ]
