{-# LANGUAGE CPP #-}
module Tests.Negative
  ( testTree -- :: TestTree
  ) where

#if ! MIN_VERSION_base(4,11,0)
import           Data.Monoid
#endif

import           Data.Version

import           Test.Tasty
import           Test.Tasty.HUnit

import           Codec.Serialise
import           Codec.Serialise.Encoding
import           Codec.CBOR.Write          as CBOR.Write

--------------------------------------------------------------------------------
-- Tests and properties

testInvalidMaybe :: Assertion
testInvalidMaybe = assertIsBad "properly decoded invalid Maybe!" val
  where
    enc = encodeListLen 2 -- only 'ListLen 0' and 'ListLen 1' are used
    val = badRoundTrip enc :: Failed (Maybe Int)

testInvalidEither :: Assertion
testInvalidEither = assertIsBad "properly decoded invalid Either!" val
  where
    -- expects a list of length two, with a tag of 0 or 1 only
    enc = encodeListLen 2
       <> encodeWord 2 -- invalid tag
       <> encodeWord 0
    val = badRoundTrip enc :: Failed (Either Int Int)

testInvalidVersion :: Assertion
testInvalidVersion = assertIsBad "properly decoded invalid Version!" val
  where
    -- expects a tag of 0 and length of 3, not 4
    enc = encodeListLen 4
       <> encodeWord 0 -- tag is zero
       <> encodeWord 0
       <> encodeWord 0
       <> encodeWord 0
    val = badRoundTrip enc :: Failed Version

--------------------------------------------------------------------------------
-- TestTree API

testTree :: TestTree
testTree = testGroup "Negative tests"
  [ testCase "decoding invalid Maybe"   testInvalidMaybe
  , testCase "decoding invalid Either"  testInvalidEither
  , testCase "decoding invalid Version" testInvalidVersion
  ]

--------------------------------------------------------------------------------
-- Utilities

-- Simple utility to take an @'Encoding'@ and try to deserialise it as
-- some user specified type. Useful for writing 'bad' encoders that give
-- some bad output we attempt to deserialise.

type Failed a = Either DeserialiseFailure a

badRoundTrip :: Serialise a => Encoding -> Failed a
badRoundTrip enc = deserialiseOrFail (CBOR.Write.toLazyByteString enc)

-- | Check if a @'Failed' a@ actually failed.
didFail :: Failed a -> Bool
didFail (Left  _) = True
didFail (Right _) = False

-- | Assert that a @'Failed' a@ actually failed.
assertIsBad :: String -> Failed a -> Assertion
assertIsBad msg v = assertBool msg (didFail v)
