{-# LANGUAGE OverloadedStrings #-}

module BCryptPBKDF (tests) where

import qualified Data.ByteString        as B

import           Test.Tasty
import           Test.Tasty.HUnit

import           Crypto.KDF.BCryptPBKDF (Parameters (..), generate,
                                         hashInternal)

tests :: TestTree
tests = testGroup "BCryptPBKDF"
    [ testGroup "generate"
        [ testCase "1" generate1
        , testCase "2" generate2
        , testCase "3" generate3
        ]
    , testGroup "hashInternal"
        [ testCase "1" hashInternal1
        ]
    ]
  where
    -- test vector taken from the go implementation by @dchest
    generate1 = expected @=? generate params pass salt
        where
            params   = Parameters 12 32
            pass     = "password" :: B.ByteString
            salt     = "salt"     :: B.ByteString
            expected = B.pack
                [ 0x1a, 0xe4, 0x2c, 0x05, 0xd4, 0x87, 0xbc, 0x02
                , 0xf6, 0x49, 0x21, 0xa4, 0xeb, 0xe4, 0xea, 0x93
                , 0xbc, 0xac, 0xfe, 0x13, 0x5f, 0xda, 0x99, 0x97
                , 0x4c, 0x06, 0xb7, 0xb0, 0x1f, 0xae, 0x14, 0x9a
                ] :: B.ByteString

    -- test vector generated with the go implemenation by @dchest
    generate2 = expected @=? generate params pass salt
        where
            params   = Parameters 7 71
            pass     = "DieWuerdeDesMenschenIstUnantastbar" :: B.ByteString
            salt     = "Tafelsalz"                          :: B.ByteString
            expected = B.pack
                [ 0x17, 0xb4, 0x76, 0xaa, 0xd7, 0x42, 0x33, 0x49
                , 0x5c, 0xe8, 0x79, 0x49, 0x15, 0x74, 0x4c, 0x71
                , 0xf9, 0x99, 0x66, 0x89, 0x7a, 0x60, 0xc3, 0x70
                , 0xb4, 0x3c, 0xa8, 0x83, 0x80, 0x5a, 0x56, 0xde
                , 0x38, 0xbc, 0x51, 0x8c, 0xd4, 0xeb, 0xd1, 0xcf
                , 0x46, 0x0a, 0x68, 0x3d, 0xc8, 0x12, 0xcf, 0xf8
                , 0x43, 0xce, 0x21, 0x9d, 0x98, 0x81, 0x20, 0x26
                , 0x6e, 0x42, 0x0f, 0xaa, 0x75, 0x5d, 0x09, 0x8d
                , 0x45, 0xda, 0xd5, 0x15, 0x6e, 0x65, 0x1d
                ] :: B.ByteString

    -- test vector generated with the go implemenation by @dchest
    generate3 = expected @=? generate params pass salt
        where
            params    = Parameters 5 5
            pass      = "ABC" :: B.ByteString
            salt      = "DEF" :: B.ByteString
            expected  = B.pack
                [ 0xdd, 0x6e, 0xa0, 0x69, 0x29
                ] :: B.ByteString

    hashInternal1 = expected @=? hashInternal passHash saltHash
        where
            passHash = B.pack [ 0  ..  63 ] :: B.ByteString
            saltHash = B.pack [ 64 .. 127 ] :: B.ByteString
            expected = B.pack
                [ 0x87, 0x90, 0x48, 0x70, 0xee, 0xf9, 0xde, 0xdd
                , 0xf8, 0xe7, 0x61, 0x1a, 0x14, 0x01, 0x06, 0xe6
                , 0xaa, 0xf1, 0xa3, 0x63, 0xd9, 0xa2, 0xc5, 0x04
                , 0xdb, 0x35, 0x64, 0x43, 0x72, 0x1e, 0xb5, 0x55
                ] :: B.ByteString
