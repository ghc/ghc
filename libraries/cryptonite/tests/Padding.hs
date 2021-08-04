{-# LANGUAGE OverloadedStrings #-}
module Padding (tests) where

import qualified Data.ByteString as B
import Imports

import Crypto.Data.Padding

cases =
    [ ("abcdef", 8, "abcdef\x02\x02")
    , ("abcd", 4, "abcd\x04\x04\x04\x04")
    , ("xyze", 5, "xyze\x01")
    ]

zeroCases =
    [ ("", 4, "\NUL\NUL\NUL\NUL", Nothing)
    , ("abcdef", 8, "abcdef\NUL\NUL", Nothing)
    , ("0123456789abcdef", 16, "0123456789abcdef", Just "0123456789abcdef")
    ]

--instance Arbitrary where

testPad :: Int -> (B.ByteString, Int, B.ByteString) -> TestTree
testPad n (inp, sz, padded) =
    testCase (show n) $ propertyHoldCase [ eqTest "padded" padded (pad (PKCS7 sz) inp)
                                         , eqTest "unpadded" (Just inp) (unpad (PKCS7 sz) padded)
                                         ]

testZeroPad :: Int -> (B.ByteString, Int, B.ByteString, Maybe B.ByteString) -> TestTree
testZeroPad n (inp, sz, padded, unpadded) =
    testCase (show n) $ propertyHoldCase [ eqTest "padded" padded (pad (ZERO sz) inp)
                                         , eqTest "unpadded" unpadded (unpad (ZERO sz) padded)
                                         ]

tests = testGroup "Padding"
    [ testGroup "Cases" $ zipWith testPad [1..] cases
    , testGroup "ZeroCases" $ zipWith testZeroPad [1..] zeroCases
    ]
