{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module KAT_RC4 where

import Test.Tasty
import Test.Tasty.HUnit

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import qualified Crypto.Cipher.RC4 as RC4

-- taken from wikipedia pages
vectors :: [(ByteString, ByteString, ByteString)]
vectors =
    [   ("Key"
        ,"Plaintext"
        ,"\xBB\xF3\x16\xE8\xD9\x40\xAF\x0A\xD3"
        )
    ,   ("Wiki"
        ,"pedia"
        ,"\x10\x21\xBF\x04\x20"
        )
    ,   ("Secret"
        ,"Attack at dawn"
        ,"\x45\xA0\x1F\x64\x5F\xC3\x5B\x38\x35\x52\x54\x4B\x9B\xF5"
        )
    ]

tests = testGroup "RC4"
    $ zipWith toKatTest is vectors
  where toKatTest i (key, plainText, cipherText) =
            testCase (show i) (cipherText @=? snd (RC4.combine (RC4.initialize key) plainText))
        is :: [Int]
        is = [1..]
