{-# LANGUAGE OverloadedStrings #-}
module KAT_HMAC (tests) where

import qualified Crypto.MAC.HMAC as HMAC
import Crypto.Hash (MD5(..), SHA1(..), SHA256(..)
                   , Keccak_224(..), Keccak_256(..), Keccak_384(..), Keccak_512(..)
                   , SHA3_224(..), SHA3_256(..), SHA3_384(..), SHA3_512(..)
                   , HashAlgorithm, digestFromByteString)
import qualified Data.ByteString as B

import Imports

data MACVector hash = MACVector
    { macKey :: ByteString
    , macSecret :: ByteString
    , macResult :: HMAC.HMAC hash
    }

instance Show (HMAC.HMAC a) where
    show (HMAC.HMAC d) = show d

digest :: HashAlgorithm hash => ByteString -> HMAC.HMAC hash
digest = maybe (error "cannot get digest") HMAC.HMAC . digestFromByteString

v1 :: ByteString
v1 = "The quick brown fox jumps over the lazy dog"

md5MACVectors :: [MACVector MD5]
md5MACVectors =
    [ MACVector B.empty B.empty $ digest "\x74\xe6\xf7\x29\x8a\x9c\x2d\x16\x89\x35\xf5\x8c\x00\x1b\xad\x88"
    , MACVector "key"   v1      $ digest "\x80\x07\x07\x13\x46\x3e\x77\x49\xb9\x0c\x2d\xc2\x49\x11\xe2\x75"
    ]

sha1MACVectors :: [MACVector SHA1]
sha1MACVectors =
    [ MACVector B.empty B.empty $ digest "\xfb\xdb\x1d\x1b\x18\xaa\x6c\x08\x32\x4b\x7d\x64\xb7\x1f\xb7\x63\x70\x69\x0e\x1d"
    , MACVector "key"   v1      $ digest "\xde\x7c\x9b\x85\xb8\xb7\x8a\xa6\xbc\x8a\x7a\x36\xf7\x0a\x90\x70\x1c\x9d\xb4\xd9"
    ]

sha256MACVectors :: [MACVector SHA256]
sha256MACVectors =
    [ MACVector B.empty B.empty $ digest "\xb6\x13\x67\x9a\x08\x14\xd9\xec\x77\x2f\x95\xd7\x78\xc3\x5f\xc5\xff\x16\x97\xc4\x93\x71\x56\x53\xc6\xc7\x12\x14\x42\x92\xc5\xad"
    , MACVector "key"   v1      $ digest "\xf7\xbc\x83\xf4\x30\x53\x84\x24\xb1\x32\x98\xe6\xaa\x6f\xb1\x43\xef\x4d\x59\xa1\x49\x46\x17\x59\x97\x47\x9d\xbc\x2d\x1a\x3c\xd8"
    ]

keccak_key1 = "\x4a\x65\x66\x65"
keccak_data1 = "\x77\x68\x61\x74\x20\x64\x6f\x20\x79\x61\x20\x77\x61\x6e\x74\x20\x66\x6f\x72\x20\x6e\x6f\x74\x68\x69\x6e\x67\x3f"

keccak_224_MAC_Vectors :: [MACVector Keccak_224]
keccak_224_MAC_Vectors =
    [ MACVector keccak_key1 keccak_data1 $ digest "\xe8\x24\xfe\xc9\x6c\x07\x4f\x22\xf9\x92\x35\xbb\x94\x2d\xa1\x98\x26\x64\xab\x69\x2c\xa8\x50\x10\x53\xcb\xd4\x14"
    ]

keccak_256_MAC_Vectors :: [MACVector Keccak_256]
keccak_256_MAC_Vectors =
    [  MACVector keccak_key1 keccak_data1 $ digest "\xaa\x9a\xed\x44\x8c\x7a\xbc\x8b\x5e\x32\x6f\xfa\x6a\x01\xcd\xed\xf7\xb4\xb8\x31\x88\x14\x68\xc0\x44\xba\x8d\xd4\x56\x63\x69\xa1"
    ]

keccak_384_MAC_Vectors :: [MACVector Keccak_384]
keccak_384_MAC_Vectors =
    [ MACVector keccak_key1 keccak_data1 $ digest "\x5a\xf5\xc9\xa7\x7a\x23\xa6\xa9\x3d\x80\x64\x9e\x56\x2a\xb7\x7f\x4f\x35\x52\xe3\xc5\xca\xff\xd9\x3b\xdf\x8b\x3c\xfc\x69\x20\xe3\x02\x3f\xc2\x67\x75\xd9\xdf\x1f\x3c\x94\x61\x31\x46\xad\x2c\x9d"
    ]

keccak_512_MAC_Vectors :: [MACVector Keccak_512]
keccak_512_MAC_Vectors =
    [ MACVector keccak_key1 keccak_data1 $ digest "\xc2\x96\x2e\x5b\xbe\x12\x38\x00\x78\x52\xf7\x9d\x81\x4d\xbb\xec\xd4\x68\x2e\x6f\x09\x7d\x37\xa3\x63\x58\x7c\x03\xbf\xa2\xeb\x08\x59\xd8\xd9\xc7\x01\xe0\x4c\xec\xec\xfd\x3d\xd7\xbf\xd4\x38\xf2\x0b\x8b\x64\x8e\x01\xbf\x8c\x11\xd2\x68\x24\xb9\x6c\xeb\xbd\xcb"
    ]

sha3_key1 = "\x4a\x65\x66\x65"
sha3_data1 = "\x77\x68\x61\x74\x20\x64\x6f\x20\x79\x61\x20\x77\x61\x6e\x74\x20\x66\x6f\x72\x20\x6e\x6f\x74\x68\x69\x6e\x67\x3f"

sha3_224_MAC_Vectors :: [MACVector SHA3_224]
sha3_224_MAC_Vectors =
    [ MACVector sha3_key1 sha3_data1 $ digest "\x7f\xdb\x8d\xd8\x8b\xd2\xf6\x0d\x1b\x79\x86\x34\xad\x38\x68\x11\xc2\xcf\xc8\x5b\xfa\xf5\xd5\x2b\xba\xce\x5e\x66"
    ]

sha3_256_MAC_Vectors :: [MACVector SHA3_256]
sha3_256_MAC_Vectors =
    [  MACVector sha3_key1 sha3_data1 $ digest "\xc7\xd4\x07\x2e\x78\x88\x77\xae\x35\x96\xbb\xb0\xda\x73\xb8\x87\xc9\x17\x1f\x93\x09\x5b\x29\x4a\xe8\x57\xfb\xe2\x64\x5e\x1b\xa5"
    ]

sha3_384_MAC_Vectors :: [MACVector SHA3_384]
sha3_384_MAC_Vectors =
    [ MACVector sha3_key1 sha3_data1 $ digest "\xf1\x10\x1f\x8c\xbf\x97\x66\xfd\x67\x64\xd2\xed\x61\x90\x3f\x21\xca\x9b\x18\xf5\x7c\xf3\xe1\xa2\x3c\xa1\x35\x08\xa9\x32\x43\xce\x48\xc0\x45\xdc\x00\x7f\x26\xa2\x1b\x3f\x5e\x0e\x9d\xf4\xc2\x0a"
    ]

sha3_512_MAC_Vectors :: [MACVector SHA3_512]
sha3_512_MAC_Vectors =
    [ MACVector sha3_key1 sha3_data1 $ digest "\x5a\x4b\xfe\xab\x61\x66\x42\x7c\x7a\x36\x47\xb7\x47\x29\x2b\x83\x84\x53\x7c\xdb\x89\xaf\xb3\xbf\x56\x65\xe4\xc5\xe7\x09\x35\x0b\x28\x7b\xae\xc9\x21\xfd\x7c\xa0\xee\x7a\x0c\x31\xd0\x22\xa9\x5e\x1f\xc9\x2b\xa9\xd7\x7d\xf8\x83\x96\x02\x75\xbe\xb4\xe6\x20\x24"
    ]


macTests :: [TestTree]
macTests =
    [ testGroup "md5" $ concatMap toMACTest $ zip is md5MACVectors
    , testGroup "sha1" $ concatMap toMACTest $ zip is sha1MACVectors
    , testGroup "sha256" $ concatMap toMACTest $ zip is sha256MACVectors
    , testGroup "keccak-224" $ concatMap toMACTest $ zip is keccak_224_MAC_Vectors
    , testGroup "keccak-256" $ concatMap toMACTest $ zip is keccak_256_MAC_Vectors
    , testGroup "keccak-384" $ concatMap toMACTest $ zip is keccak_384_MAC_Vectors
    , testGroup "keccak-512" $ concatMap toMACTest $ zip is keccak_512_MAC_Vectors
    , testGroup "sha3-224" $ concatMap toMACTest $ zip is sha3_224_MAC_Vectors
    , testGroup "sha3-256" $ concatMap toMACTest $ zip is sha3_256_MAC_Vectors
    , testGroup "sha3-384" $ concatMap toMACTest $ zip is sha3_384_MAC_Vectors
    , testGroup "sha3-512" $ concatMap toMACTest $ zip is sha3_512_MAC_Vectors
    ]
    where toMACTest (i, macVector) =
            [ testCase (show i) (macResult macVector @=? HMAC.hmac (macKey macVector) (macSecret macVector))
            , testCase ("incr-" ++ show i) (macResult macVector @=?
                        HMAC.finalize (HMAC.update (HMAC.initialize (macKey macVector)) (macSecret macVector)))
            ]
          is :: [Int]
          is = [1..]

data MacIncremental a = MacIncremental ByteString ByteString (HMAC.HMAC a)
    deriving (Show,Eq)

instance HashAlgorithm a => Arbitrary (MacIncremental a) where
    arbitrary = do
        key <- arbitraryBSof 1 89
        msg <- arbitraryBSof 1 99
        return $ MacIncremental key msg (HMAC.hmac key msg)

data MacIncrementalList a = MacIncrementalList ByteString [ByteString] (HMAC.HMAC a)
    deriving (Show,Eq)

instance HashAlgorithm a => Arbitrary (MacIncrementalList a) where
    arbitrary = do
        key  <- arbitraryBSof 1 89
        msgs <- choose (1,20) >>= \n -> replicateM n (arbitraryBSof 1 99)
        return $ MacIncrementalList key msgs (HMAC.hmac key (B.concat msgs))

macIncrementalTests :: [TestTree]
macIncrementalTests =
    [ testIncrProperties MD5
    , testIncrProperties SHA1
    , testIncrProperties SHA256
    , testIncrProperties SHA3_224
    , testIncrProperties SHA3_256
    , testIncrProperties SHA3_384
    , testIncrProperties SHA3_512
    ]
  where
        --testIncrProperties :: HashAlgorithm a => a -> [Property]
        testIncrProperties a = testGroup (show a)
            [ testProperty "list-one" (prop_inc0 a)
            , testProperty "list-multi" (prop_inc1 a)
            ]

        prop_inc0 :: HashAlgorithm a => a -> MacIncremental a -> Bool
        prop_inc0 _ (MacIncremental secret msg result) =
            result `assertEq` HMAC.finalize (HMAC.update (HMAC.initialize secret) msg)

        prop_inc1 :: HashAlgorithm a => a -> MacIncrementalList a -> Bool
        prop_inc1 _ (MacIncrementalList secret msgs result) =
            result `assertEq` HMAC.finalize (foldl' HMAC.update (HMAC.initialize secret) msgs)

tests = testGroup "HMAC"
    [ testGroup "KATs" macTests
    , testGroup "properties" macIncrementalTests
    ]
