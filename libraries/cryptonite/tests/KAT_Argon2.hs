{-# LANGUAGE OverloadedStrings #-}
module KAT_Argon2 (tests) where

import           Crypto.Error
import qualified Crypto.KDF.Argon2 as Argon2
import qualified Data.ByteString as B
import           Imports

data KDFVector = KDFVector
    { kdfPass      :: ByteString
    , kdfSalt      :: ByteString
    , kdfOptions   :: Argon2.Options
    , kdfResult    :: ByteString
    }

argon2i_13 :: Argon2.TimeCost -> Argon2.MemoryCost -> Argon2.Options
argon2i_13 iters memory = Argon2.Options
    { Argon2.iterations  = iters
    , Argon2.memory      = memory
    , Argon2.parallelism = 1
    , Argon2.variant     = Argon2.Argon2i
    , Argon2.version     = Argon2.Version13
    }

vectors =
    [ KDFVector "password" "somesalt" (argon2i_13 2 65536)
        "\xc1\x62\x88\x32\x14\x7d\x97\x20\xc5\xbd\x1c\xfd\x61\x36\x70\x78\x72\x9f\x6d\xfb\x6f\x8f\xea\x9f\xf9\x81\x58\xe0\xd7\x81\x6e\xd0"
    ]

kdfTests :: [TestTree]
kdfTests = zipWith toKDFTest is vectors
  where
    toKDFTest i v =
        testCase (show i)
            (CryptoPassed (kdfResult v) @=? Argon2.hash (kdfOptions v) (kdfPass v) (kdfSalt v) (B.length $ kdfResult v))

    is :: [Int]
    is = [1..]

tests = testGroup "Argon2"
    [ testGroup "KATs" kdfTests
    ]
