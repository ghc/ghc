{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module KAT_KMAC (tests) where

import           Crypto.Hash (SHAKE128(..), SHAKE256(..),
                              HashAlgorithm, digestFromByteString)
import qualified Crypto.MAC.KMAC as KMAC

import qualified Data.ByteString as B

import Imports

data MACVector hash = MACVector
    { macString :: ByteString
    , macKey    :: ByteString
    , macSecret :: ByteString
    , macResult :: KMAC.KMAC hash
    }

instance Show (KMAC.KMAC a) where
    show (KMAC.KMAC d) = show d

digest :: HashAlgorithm hash => ByteString -> KMAC.KMAC hash
digest = maybe (error "cannot get digest") KMAC.KMAC . digestFromByteString

vectors128 :: [MACVector (SHAKE128 256)]
vectors128 =
    [ MACVector
        { macString = ""
        , macKey    = B.pack [ 0x40 .. 0x5f ]
        , macSecret = B.pack [ 0x00 .. 0x03 ]
        , macResult = digest "\xe5\x78\x0b\x0d\x3e\xa6\xf7\xd3\xa4\x29\xc5\x70\x6a\xa4\x3a\x00\xfa\xdb\xd7\xd4\x96\x28\x83\x9e\x31\x87\x24\x3f\x45\x6e\xe1\x4e"
        }
    , MACVector
        { macString = "My Tagged Application"
        , macKey    = B.pack [ 0x40 .. 0x5f ]
        , macSecret = B.pack [ 0x00 .. 0x03 ]
        , macResult = digest "\x3b\x1f\xba\x96\x3c\xd8\xb0\xb5\x9e\x8c\x1a\x6d\x71\x88\x8b\x71\x43\x65\x1a\xf8\xba\x0a\x70\x70\xc0\x97\x9e\x28\x11\x32\x4a\xa5"
        }
    , MACVector
        { macString = "My Tagged Application"
        , macKey    = B.pack [ 0x40 .. 0x5f ]
        , macSecret = B.pack [ 0x00 .. 0xc7 ]
        , macResult = digest "\x1f\x5b\x4e\x6c\xca\x02\x20\x9e\x0d\xcb\x5c\xa6\x35\xb8\x9a\x15\xe2\x71\xec\xc7\x60\x07\x1d\xfd\x80\x5f\xaa\x38\xf9\x72\x92\x30"
        }
    ]

vectors256 :: [MACVector (SHAKE256 512)]
vectors256 =
    [ MACVector
        { macString = "My Tagged Application"
        , macKey    = B.pack [ 0x40 .. 0x5f ]
        , macSecret = B.pack [ 0x00 .. 0x03 ]
        , macResult = digest "\x20\xc5\x70\xc3\x13\x46\xf7\x03\xc9\xac\x36\xc6\x1c\x03\xcb\x64\xc3\x97\x0d\x0c\xfc\x78\x7e\x9b\x79\x59\x9d\x27\x3a\x68\xd2\xf7\xf6\x9d\x4c\xc3\xde\x9d\x10\x4a\x35\x16\x89\xf2\x7c\xf6\xf5\x95\x1f\x01\x03\xf3\x3f\x4f\x24\x87\x10\x24\xd9\xc2\x77\x73\xa8\xdd"
        }
    , MACVector
        { macString = ""
        , macKey    = B.pack [ 0x40 .. 0x5f ]
        , macSecret = B.pack [ 0x00 .. 0xc7 ]
        , macResult = digest "\x75\x35\x8c\xf3\x9e\x41\x49\x4e\x94\x97\x07\x92\x7c\xee\x0a\xf2\x0a\x3f\xf5\x53\x90\x4c\x86\xb0\x8f\x21\xcc\x41\x4b\xcf\xd6\x91\x58\x9d\x27\xcf\x5e\x15\x36\x9c\xbb\xff\x8b\x9a\x4c\x2e\xb1\x78\x00\x85\x5d\x02\x35\xff\x63\x5d\xa8\x25\x33\xec\x6b\x75\x9b\x69"
        }
    , MACVector
        { macString = "My Tagged Application"
        , macKey    = B.pack [ 0x40 .. 0x5f ]
        , macSecret = B.pack [ 0x00 .. 0xc7 ]
        , macResult = digest "\xb5\x86\x18\xf7\x1f\x92\xe1\xd5\x6c\x1b\x8c\x55\xdd\xd7\xcd\x18\x8b\x97\xb4\xca\x4d\x99\x83\x1e\xb2\x69\x9a\x83\x7d\xa2\xe4\xd9\x70\xfb\xac\xfd\xe5\x00\x33\xae\xa5\x85\xf1\xa2\x70\x85\x10\xc3\x2d\x07\x88\x08\x01\xbd\x18\x28\x98\xfe\x47\x68\x76\xfc\x89\x65"
        }
    ]

macTests :: [TestTree]
macTests =
    [ testGroup "SHAKE128" (concatMap toMACTest $ zip is vectors128)
    , testGroup "SHAKE256" (concatMap toMACTest $ zip is vectors256)
    ]
    where toMACTest (i, MACVector{..}) =
            [ testCase (show i) (macResult @=? KMAC.kmac macString macKey macSecret)
            , testCase ("incr-" ++ show i) (macResult @=?
                        KMAC.finalize (KMAC.update (KMAC.initialize macString macKey) macSecret))
            ]
          is :: [Int]
          is = [1..]

data MacIncremental a = MacIncremental ByteString ByteString ByteString (KMAC.KMAC a)
    deriving (Show,Eq)

instance KMAC.HashSHAKE a => Arbitrary (MacIncremental a) where
    arbitrary = do
        str <- arbitraryBSof 0 49
        key <- arbitraryBSof 1 89
        msg <- arbitraryBSof 1 99
        return $ MacIncremental str key msg (KMAC.kmac str key msg)

data MacIncrementalList a = MacIncrementalList ByteString ByteString [ByteString] (KMAC.KMAC a)
    deriving (Show,Eq)

instance KMAC.HashSHAKE a => Arbitrary (MacIncrementalList a) where
    arbitrary = do
        str  <- arbitraryBSof 0 49
        key  <- arbitraryBSof 1 89
        msgs <- choose (1,20) >>= \n -> replicateM n (arbitraryBSof 1 99)
        return $ MacIncrementalList str key msgs (KMAC.kmac str key (B.concat msgs))

macIncrementalTests :: [TestTree]
macIncrementalTests =
    [ testIncrProperties "SHAKE128_256" (SHAKE128 :: SHAKE128 256)
    , testIncrProperties "SHAKE256_512" (SHAKE256 :: SHAKE256 512)
    ]
  where
        testIncrProperties :: KMAC.HashSHAKE a => TestName -> a -> TestTree
        testIncrProperties name a = testGroup name
            [ testProperty "list-one" (prop_inc0 a)
            , testProperty "list-multi" (prop_inc1 a)
            ]

        prop_inc0 :: KMAC.HashSHAKE a => a -> MacIncremental a -> Bool
        prop_inc0 _ (MacIncremental str secret msg result) =
            result `assertEq` KMAC.finalize (KMAC.update (KMAC.initialize str secret) msg)

        prop_inc1 :: KMAC.HashSHAKE a => a -> MacIncrementalList a -> Bool
        prop_inc1 _ (MacIncrementalList str secret msgs result) =
            result `assertEq` KMAC.finalize (foldl' KMAC.update (KMAC.initialize str secret) msgs)

tests = testGroup "KMAC"
    [ testGroup "KATs" macTests
    , testGroup "properties" macIncrementalTests
    ]
