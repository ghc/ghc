-- Issue #135: Ensure that surrogate characters round-trip
--
module Tests.Regress.Issue135
  ( testTree -- :: TestTree
  ) where

import           Codec.Serialise

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.HUnit

newtype StringWithSurrogates = StringWithSurrogates String
                             deriving (Show)

instance Arbitrary StringWithSurrogates where
  arbitrary =
      fmap StringWithSurrogates $ listOf1
      $ oneof [choose ('\xd800', '\xdfff'), arbitrary]

prop_surrogateRoundtrip :: StringWithSurrogates -> Property
prop_surrogateRoundtrip (StringWithSurrogates s) =
    s === deserialise (serialise s)

roundTrips :: (Eq a, Serialise a) => a -> Bool
roundTrips x = x == deserialise (serialise x)

testTree :: TestTree
testTree =
  testGroup "Issue 135 - surrogate characters round-trip"
    [ testCase "simple reproduction case"   (True @=? all (\c -> c == deserialise (serialise c)) ['\xdc80'..'\xdcff'])
    , testCase "all Chars round-trip" ([] @=? filter (not . roundTrips) ['\x0000'..'\x10ffff'])
    , testProperty "surrogates round-trip" prop_surrogateRoundtrip
    ]
