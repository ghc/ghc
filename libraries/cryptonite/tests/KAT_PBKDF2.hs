{-# LANGUAGE OverloadedStrings #-}

-- from <http://www.ietf.org/rfc/rfc6070.txt>
module KAT_PBKDF2 (tests) where

import Crypto.Hash (SHA1(..), SHA256(..), SHA512(..))
import qualified Crypto.KDF.PBKDF2 as PBKDF2

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()

import Test.Tasty
import Test.Tasty.HUnit

type VectParams = (ByteString, ByteString, Int, Int)

vectors_hmac_sha1 :: [ (VectParams, ByteString) ]
vectors_hmac_sha1 =
    [
        ( ("password","salt",2,20)
        , "\xea\x6c\x01\x4d\xc7\x2d\x6f\x8c\xcd\x1e\xd9\x2a\xce\x1d\x41\xf0\xd8\xde\x89\x57"
        )
    ,   ( ("password","salt",4096,20)
        , "\x4b\x00\x79\x01\xb7\x65\x48\x9a\xbe\xad\x49\xd9\x26\xf7\x21\xd0\x65\xa4\x29\xc1"
        )

    ,   ( ("passwordPASSWORDpassword", "saltSALTsaltSALTsaltSALTsaltSALTsalt", 4096, 25)
        , "\x3d\x2e\xec\x4f\xe4\x1c\x84\x9b\x80\xc8\xd8\x36\x62\xc0\xe4\x4a\x8b\x29\x1a\x96\x4c\xf2\xf0\x70\x38"
        )
    ,   ( ("pass\0word", "sa\0lt", 4096, 16)
        , "\x56\xfa\x6a\xa7\x55\x48\x09\x9d\xcc\x37\xd7\xf0\x34\x25\xe0\xc3"
        )
    ]

vectors_hmac_sha256 :: [ (VectParams, ByteString) ]
vectors_hmac_sha256 =
    [   ( ("password", "salt", 2, 32)
        , "\xae\x4d\x0c\x95\xaf\x6b\x46\xd3\x2d\x0a\xdf\xf9\x28\xf0\x6d\xd0\x2a\x30\x3f\x8e\xf3\xc2\x51\xdf\xd6\xe2\xd8\x5a\x95\x47\x4c\x43"
        )
    ,   ( ("passwordPASSWORDpassword", "saltSALTsaltSALTsaltSALTsaltSALTsalt", 4096, 40)
        , "\x34\x8c\x89\xdb\xcb\xd3\x2b\x2f\x32\xd8\x14\xb8\x11\x6e\x84\xcf\x2b\x17\x34\x7e\xbc\x18\x00\x18\x1c\x4e\x2a\x1f\xb8\xdd\x53\xe1\xc6\x35\x51\x8c\x7d\xac\x47\xe9"
        )
    ]

vectors_hmac_sha512 :: [ (VectParams, ByteString) ]
vectors_hmac_sha512 =
    [   ( ("password", "salt", 1, 32)
        , "\x86\x7f\x70\xcf\x1a\xde\x02\xcf\xf3\x75\x25\x99\xa3\xa5\x3d\xc4\xaf\x34\xc7\xa6\x69\x81\x5a\xe5\xd5\x13\x55\x4e\x1c\x8c\xf2\x52"
        )
    ,   ( ("password", "salt", 2, 32)
        ,  "\xe1\xd9\xc1\x6a\xa6\x81\x70\x8a\x45\xf5\xc7\xc4\xe2\x15\xce\xb6\x6e\x01\x1a\x2e\x9f\x00\x40\x71\x3f\x18\xae\xfd\xb8\x66\xd5\x3c"
        )
    ,   ( ("password", "salt", 4096, 32)
        ,  "\xd1\x97\xb1\xb3\x3d\xb0\x14\x3e\x01\x8b\x12\xf3\xd1\xd1\x47\x9e\x6c\xde\xbd\xcc\x97\xc5\xc0\xf8\x7f\x69\x02\xe0\x72\xf4\x57\xb5"
        )
    ,   ( ("passwordPASSWORDpassword", "saltSALTsaltSALTsaltSALTsaltSALTsalt", 1, 72)
        , "n\x23\xf2\x76\x38\x08\x4b\x0f\x7e\xa1\x73\x4e\x0d\x98\x41\xf5\x5d\xd2\x9e\xa6\x0a\x83\x44\x66\xf3\x39\x6b\xac\x80\x1f\xac\x1e\xeb\x63\x80\x2f\x03\xa0\xb4\xac\xd7\x60\x3e\x36\x99\xc8\xb7\x44\x37\xbe\x83\xff\x01\xad\x7f\x55\xda\xc1\xef\x60\xf4\xd5\x64\x80\xc3\x5e\xe6\x8f\xd5\x2c\x69\x36"
        )
    ]


tests = testGroup "PBKDF2"
    [ testGroup "KATs-HMAC-SHA1" (katTests (PBKDF2.prfHMAC SHA1) vectors_hmac_sha1)
    , testGroup "KATs-HMAC-SHA1 (fast)" (katTestFastPBKDF2_SHA1 vectors_hmac_sha1)
    , testGroup "KATs-HMAC-SHA256" (katTests (PBKDF2.prfHMAC SHA256) vectors_hmac_sha256)
    , testGroup "KATs-HMAC-SHA256 (fast)" (katTestFastPBKDF2_SHA256 vectors_hmac_sha256)
    , testGroup "KATs-HMAC-SHA512" (katTests (PBKDF2.prfHMAC SHA512) vectors_hmac_sha512)
    , testGroup "KATs-HMAC-SHA512 (fast)" (katTestFastPBKDF2_SHA512 vectors_hmac_sha512)
    ]
  where katTests prf = zipWith (toKatTest prf) is

        toKatTest prf i ((pass, salt, iter, dkLen), output) =
            testCase (show i) (output @=? PBKDF2.generate prf (PBKDF2.Parameters iter dkLen) pass salt)

        katTestFastPBKDF2_SHA1 = zipWith toKatTestFastPBKDF2_SHA1 is
        toKatTestFastPBKDF2_SHA1 i ((pass, salt, iter, dkLen), output) =
            testCase (show i) (output @=? PBKDF2.fastPBKDF2_SHA1 (PBKDF2.Parameters iter dkLen) pass salt)

        katTestFastPBKDF2_SHA256 = zipWith toKatTestFastPBKDF2_SHA256 is
        toKatTestFastPBKDF2_SHA256 i ((pass, salt, iter, dkLen), output) =
            testCase (show i) (output @=? PBKDF2.fastPBKDF2_SHA256 (PBKDF2.Parameters iter dkLen) pass salt)

        katTestFastPBKDF2_SHA512 = zipWith toKatTestFastPBKDF2_SHA512 is
        toKatTestFastPBKDF2_SHA512 i ((pass, salt, iter, dkLen), output) =
            testCase (show i) (output @=? PBKDF2.fastPBKDF2_SHA512 (PBKDF2.Parameters iter dkLen) pass salt)


        is :: [Int]
        is = [1..]
