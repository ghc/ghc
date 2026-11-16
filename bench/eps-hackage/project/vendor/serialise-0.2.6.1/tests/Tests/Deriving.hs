{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Deriving (testTree) where

import           GHC.Generics

import qualified Codec.Serialise as Serialise
import           Codec.CBOR.FlatTerm

import           Test.Tasty
import           Test.Tasty.HUnit

-- | A unit type
data AUnit = AUnit
           deriving (Generic, Eq, Show)
instance Serialise.Serialise AUnit

testAUnit :: TestTree
testAUnit = testAgainstFile "a unit" x rep
  where
    x = AUnit
    rep = [TkListLen 1, TkInt 0]

-- | A simple case exercising many of the cases implemented by the generic
-- deriving mechinery
data ARecord = ARecord String Int ARecord
             | ANull
             deriving (Generic, Eq, Show)
instance Serialise.Serialise ARecord

testARecord :: TestTree
testARecord = testAgainstFile "a record" x rep
  where
    x = ARecord "hello" 42 (ARecord "world" 52 ANull)
    rep = [TkListLen 4, TkInt 0, TkString "hello", TkInt 42,
           TkListLen 4, TkInt 0, TkString "world", TkInt 52,
           TkListLen 1, TkInt 1
          ]

newtype ANewtype = ANewtype Int
                 deriving (Generic, Eq, Show)
instance Serialise.Serialise ANewtype

testANewtype :: TestTree
testANewtype = testAgainstFile "a newtype" x rep
  where
    x = ANewtype 42
    rep = [TkListLen 2, TkInt 0, TkInt 42]

testAgainstFile :: (Eq a, Show a, Serialise.Serialise a)
                => String -> a -> FlatTerm -> TestTree
testAgainstFile name x expected =
    testGroup name
      [ testCase "serialise" $ do
            let actual = toFlatTerm $ Serialise.encode x
            expected @=? actual
      , testCase "deserialise" $ do
            case fromFlatTerm Serialise.decode expected of
              Left err -> fail err
              Right actual -> x @=? actual
      ]

testTree :: TestTree
testTree =
    testGroup "Stability of derived instances"
      [ testAUnit
      , testARecord
      , testANewtype
      ]
