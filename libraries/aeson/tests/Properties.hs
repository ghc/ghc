{-# LANGUAGE NoImplicitPrelude #-}

module Properties (module Properties) where

import Prelude.Compat

import Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Property)
import PropUtils
import PropertyGeneric
import PropertyKeys
import PropertyRoundTrip
import PropertyTH


tests :: TestTree
tests = testGroup "properties" [
  testGroup "encode" [
      testProperty "encodeDouble" encodeDouble
    , testProperty "encodeInteger" encodeInteger
    ]
  , testProperty "read . show = id" roundtripReadShow
  , roundTripTests
  , keysTests
  , testGroup "toFromJSON" [
      testProperty "Integer" (toFromJSON :: Integer -> Property)
    , testProperty "Double" (toFromJSON :: Double -> Property)
    , testProperty "Maybe Integer" (toFromJSON :: Maybe Integer -> Property)
    , testProperty "Either Integer Double" (toFromJSON :: Either Integer Double -> Property)
    , testProperty "Either Integer Integer" (toFromJSON :: Either Integer Integer -> Property)
    ]
  , testGroup "failure messages" [
      testProperty "modify failure" modifyFailureProp
    , testProperty "parserThrowError" parserThrowErrorProp
    , testProperty "parserCatchError" parserCatchErrorProp
    ]
  , genericTests
  , templateHaskellTests
  ]
