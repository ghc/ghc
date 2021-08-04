{-# LANGUAGE OverloadedStrings #-}
module Main where

import Imports

import Crypto.System.CPU

import qualified Number
import qualified Number.F2m
import qualified BCrypt
import qualified BCryptPBKDF
import qualified ECC
import qualified ECC.Edwards25519
import qualified ECDSA
import qualified Hash
import qualified Poly1305
import qualified Salsa
import qualified XSalsa
import qualified ChaCha
import qualified ChaChaPoly1305
import qualified KAT_MiyaguchiPreneel
import qualified KAT_CMAC
import qualified KAT_HMAC
import qualified KAT_KMAC
import qualified KAT_HKDF
import qualified KAT_Argon2
import qualified KAT_PBKDF2
import qualified KAT_Curve25519
import qualified KAT_Curve448
import qualified KAT_Ed25519
import qualified KAT_Ed448
import qualified KAT_EdDSA
import qualified KAT_OTP
import qualified KAT_PubKey
import qualified KAT_Scrypt
-- symmetric cipher --------------------
import qualified KAT_AES
import qualified KAT_AESGCMSIV
import qualified KAT_Blowfish
import qualified KAT_CAST5
import qualified KAT_Camellia
import qualified KAT_DES
import qualified KAT_RC4
import qualified KAT_TripleDES
import qualified KAT_Twofish
-- misc --------------------------------
import qualified KAT_AFIS
import qualified Padding

tests = testGroup "cryptonite"
    [ testGroup "runtime"
        [ testCaseInfo "CPU" (return $ show processorOptions)
        ]
    , Number.tests
    , Number.F2m.tests
    , Hash.tests
    , Padding.tests
    , testGroup "ConstructHash"
        [ KAT_MiyaguchiPreneel.tests
        ]
    , testGroup "MAC"
        [ Poly1305.tests
        , KAT_CMAC.tests
        , KAT_HMAC.tests
        , KAT_KMAC.tests
        ]
    , KAT_Curve25519.tests
    , KAT_Curve448.tests
    , KAT_Ed25519.tests
    , KAT_Ed448.tests
    , KAT_EdDSA.tests
    , KAT_PubKey.tests
    , KAT_OTP.tests
    , testGroup "KDF"
        [ KAT_PBKDF2.tests
        , KAT_Scrypt.tests
        , BCrypt.tests
        , BCryptPBKDF.tests
        , KAT_HKDF.tests
        , KAT_Argon2.tests
        ]
    , testGroup "block-cipher"
        [ KAT_AES.tests
        , KAT_AESGCMSIV.tests
        , KAT_Blowfish.tests
        , KAT_CAST5.tests
        , KAT_Camellia.tests
        , KAT_DES.tests
        , KAT_TripleDES.tests
        , KAT_Twofish.tests
        ]
    , testGroup "stream-cipher"
        [ KAT_RC4.tests
        , ChaCha.tests
        , ChaChaPoly1305.tests
        , Salsa.tests
        , XSalsa.tests
        ]
    , KAT_AFIS.tests
    , ECC.tests
    , ECC.Edwards25519.tests
    , ECDSA.tests
    ]

main = defaultMain tests
