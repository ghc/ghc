{-# LANGUAGE OverloadedStrings #-}
module KAT_AES (tests) where

import Imports
import BlockCipher
import Data.Maybe
import Crypto.Cipher.Types
import qualified Crypto.Cipher.AES as AES
import qualified Data.ByteString as B

import qualified KAT_AES.KATECB as KATECB
import qualified KAT_AES.KATCBC as KATCBC
import qualified KAT_AES.KATXTS as KATXTS
import qualified KAT_AES.KATGCM as KATGCM
import qualified KAT_AES.KATCCM as KATCCM
import qualified KAT_AES.KATOCB3 as KATOCB3

{-
instance Show AES.AES where
    show _ = "AES"
instance Arbitrary AES.AESIV where
    arbitrary = AES.aesIV_ . B.pack <$> replicateM 16 arbitrary
instance Arbitrary AES.AES where
    arbitrary = AES.initAES . B.pack <$> replicateM 16 arbitrary
-}

toKatECB (k,p,c) = KAT_ECB { ecbKey = k, ecbPlaintext = p, ecbCiphertext = c }
toKatCBC (k,iv,p,c) = KAT_CBC { cbcKey = k, cbcIV = iv, cbcPlaintext = p, cbcCiphertext = c }
toKatXTS (k1,k2,iv,p,_,c) = KAT_XTS { xtsKey1 = k1, xtsKey2 = k2, xtsIV = iv, xtsPlaintext = p, xtsCiphertext = c }
toKatAEAD mode (k,iv,h,p,c,taglen,tag) =
    KAT_AEAD { aeadMode       = mode
             , aeadKey        = k
             , aeadIV         = iv
             , aeadHeader     = h
             , aeadPlaintext  = p
             , aeadCiphertext = c
             , aeadTaglen     = taglen
             , aeadTag        = tag
             }
toKatGCM = toKatAEAD AEAD_GCM
toKatOCB = toKatAEAD AEAD_OCB

toKatCCM (k,iv,h,i,o,m) =
  KAT_AEAD { aeadMode = AEAD_CCM (B.length i) (ccmMVal m) CCM_L2
           , aeadKey  = k
           , aeadIV   = iv
           , aeadHeader = h
           , aeadPlaintext = i
           , aeadCiphertext = ct
           , aeadTaglen = m
           , aeadTag = at
           }
  where ccmMVal x = fromMaybe (error $ "unsupported CCM tag length: " ++ show x) $
                        lookup x [ (4, CCM_M4), (6, CCM_M6), (8, CCM_M8), (10, CCM_M10)
                                 , (12, CCM_M12), (14, CCM_M14), (16, CCM_M16)
                                 ]
        ctWithTag = B.drop (B.length h) o
        (ct, at)  = B.splitAt (B.length ctWithTag - m) ctWithTag

kats128 = defaultKATs
    { kat_ECB  = map toKatECB KATECB.vectors_aes128_enc
    , kat_CBC  = map toKatCBC KATCBC.vectors_aes128_enc
    , kat_CFB  = [ KAT_CFB { cfbKey        = "\x2b\x7e\x15\x16\x28\xae\xd2\xa6\xab\xf7\x15\x88\x09\xcf\x4f\x3c"
                           , cfbIV         = "\xC8\xA6\x45\x37\xA0\xB3\xA9\x3F\xCD\xE3\xCD\xAD\x9F\x1C\xE5\x8B"
                           , cfbPlaintext  = "\x30\xc8\x1c\x46\xa3\x5c\xe4\x11\xe5\xfb\xc1\x19\x1a\x0a\x52\xef"
                           , cfbCiphertext = "\x26\x75\x1f\x67\xa3\xcb\xb1\x40\xb1\x80\x8c\xf1\x87\xa4\xf4\xdf"
                           }
                 ]
    , kat_XTS  = map toKatXTS KATXTS.vectors_aes128_enc
    , kat_AEAD = map toKatGCM KATGCM.vectors_aes128_enc ++
                 map toKatOCB KATOCB3.vectors_aes128_enc ++
                 map toKatCCM KATCCM.vectors_aes128_enc
    }

kats192 = defaultKATs
    { kat_ECB  = map toKatECB KATECB.vectors_aes192_enc
    , kat_CBC  = map toKatCBC KATCBC.vectors_aes192_enc
    }

kats256 = defaultKATs
    { kat_ECB  = map toKatECB KATECB.vectors_aes256_enc
    , kat_CBC  = map toKatCBC KATCBC.vectors_aes256_enc
    , kat_XTS  = map toKatXTS KATXTS.vectors_aes256_enc
    , kat_AEAD = map toKatGCM KATGCM.vectors_aes256_enc
    }

tests = testGroup "AES"
    [ testBlockCipher kats128 (undefined :: AES.AES128)
    , testBlockCipher kats192 (undefined :: AES.AES192)
    , testBlockCipher kats256 (undefined :: AES.AES256)
{-
    , testProperty "genCtr" $ \(key, iv1) ->
        let (bs1, iv2)    = AES.genCounter key iv1 32
            (bs2, iv3)    = AES.genCounter key iv2 32
            (bsAll, iv3') = AES.genCounter key iv1 64
         in (B.concat [bs1,bs2] == bsAll && iv3 == iv3')
-}
    ]
