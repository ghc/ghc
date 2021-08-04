{-# LANGUAGE OverloadedStrings #-}
module KAT_Curve25519 ( tests ) where

import           Crypto.Error
import qualified Crypto.PubKey.Curve25519 as Curve25519
import           Data.ByteArray as B
import           Imports

alicePrivate = throwCryptoError $ Curve25519.secretKey ("\x77\x07\x6d\x0a\x73\x18\xa5\x7d\x3c\x16\xc1\x72\x51\xb2\x66\x45\xdf\x4c\x2f\x87\xeb\xc0\x99\x2a\xb1\x77\xfb\xa5\x1d\xb9\x2c\x2a" :: ByteString)
alicePublic  = throwCryptoError $ Curve25519.publicKey ("\x85\x20\xf0\x09\x89\x30\xa7\x54\x74\x8b\x7d\xdc\xb4\x3e\xf7\x5a\x0d\xbf\x3a\x0d\x26\x38\x1a\xf4\xeb\xa4\xa9\x8e\xaa\x9b\x4e\x6a" :: ByteString)
bobPrivate   = throwCryptoError $ Curve25519.secretKey ("\x5d\xab\x08\x7e\x62\x4a\x8a\x4b\x79\xe1\x7f\x8b\x83\x80\x0e\xe6\x6f\x3b\xb1\x29\x26\x18\xb6\xfd\x1c\x2f\x8b\x27\xff\x88\xe0\xeb" :: ByteString)
bobPublic    = throwCryptoError $ Curve25519.publicKey ("\xde\x9e\xdb\x7d\x7b\x7d\xc1\xb4\xd3\x5b\x61\xc2\xec\xe4\x35\x37\x3f\x83\x43\xc8\x5b\x78\x67\x4d\xad\xfc\x7e\x14\x6f\x88\x2b\x4f" :: ByteString)
aliceMultBob = "\x4a\x5d\x9d\x5b\xa4\xce\x2d\xe1\x72\x8e\x3b\xf4\x80\x35\x0f\x25\xe0\x7e\x21\xc9\x47\xd1\x9e\x33\x76\xf0\x9b\x3c\x1e\x16\x17\x42" :: ByteString

katTests :: [TestTree]
katTests =
    [ testCase "0" (aliceMultBob @=? B.convert (Curve25519.dh alicePublic bobPrivate))
    , testCase "1" (aliceMultBob @=? B.convert (Curve25519.dh bobPublic alicePrivate))
    , testCase "2" (alicePublic  @=? Curve25519.toPublic alicePrivate)
    , testCase "3" (bobPublic    @=? Curve25519.toPublic bobPrivate)
    ]

tests = testGroup "Curve25519"
    [ testGroup "KATs" katTests
    ]
