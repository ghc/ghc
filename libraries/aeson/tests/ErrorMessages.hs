{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ErrorMessages
  (
    tests
  ) where

import Prelude.Compat

import Data.Aeson (FromJSON(..), Value, json)
import Data.Aeson.Types (Parser)
import Data.Aeson.Parser (eitherDecodeWith)
import Data.Aeson.Internal (formatError, iparse)
import Data.Algorithm.Diff (PolyDiff (..), getGroupedDiff)
import Data.Proxy (Proxy(..))
import Data.Semigroup ((<>))
import Data.Sequence (Seq)
import Instances ()
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, TestName)
import Test.Tasty.Golden.Advanced (goldenTest)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as HM

import Encoders
import Types

tests :: [TestTree]
tests =
  [ aesonGoldenTest "simple" "tests/golden/simple.expected" output
  , aesonGoldenTest "generic" "tests/golden/generic.expected" (outputGeneric G)
  , aesonGoldenTest "generic" "tests/golden/th.expected" (outputGeneric TH)
  ]

output :: Output
output = concat
  [ testFor "Int" (Proxy :: Proxy Int)
      [ "\"\""
      , "[]"
      , "{}"
      , "null"
      ]

  , testFor "Integer" (Proxy :: Proxy Integer)
      [ "44.44"
      ]

  , testFor "Natural" (Proxy :: Proxy Natural)
      [ "44.44"
      , "-50"
      ]

  , testFor "String" (Proxy :: Proxy String)
      [ "1"
      , "[]"
      , "{}"
      , "null"
      ]

  , testFor "HashMap" (Proxy :: Proxy (HM.HashMap String Int))
      [ "\"\""
      , "[]"
      ]

    -- issue #356
  , testFor "Either" (Proxy :: Proxy (Int, Either (Int, Bool) ()))
      [ "[1,{\"Left\":[2,3]}]"
      ]

    -- issue #358
  , testFor "Seq" (Proxy :: Proxy (Seq Int))
      [ "[0,1,true]"
      ]
  ]

data Choice = TH | G

outputGeneric :: Choice -> Output
outputGeneric choice = concat
  [ testWith "OneConstructor"
      (select
        thOneConstructorParseJSONDefault
        gOneConstructorParseJSONDefault)
      [ "\"X\""
      , "[0]"
      ]

  , testWith "Nullary"
      (select
        thNullaryParseJSONString
        gNullaryParseJSONString)
      [ "\"X\""
      , "[]"
      ]

  , testWithSomeType "SomeType (tagged)"
      (select
        thSomeTypeParseJSONTaggedObject
        gSomeTypeParseJSONTaggedObject)
      [ "{\"tag\": \"unary\", \"contents\": true}"
      , "{\"tag\": \"unary\"}"
      , "{\"tag\": \"record\"}"
      , "{\"tag\": \"record\", \"testone\": true, \"testtwo\": null, \"testthree\": null}"
      , "{\"tag\": \"X\"}"
      , "{}"
      , "[]"
      ]

  , testWithSomeType "SomeType (single-field)"
      (select
        thSomeTypeParseJSONObjectWithSingleField
        gSomeTypeParseJSONObjectWithSingleField)
      [ "{\"unary\": {}}"
      , "{\"unary\": []}"
      , "{\"X\": []}"
      , "{\"record\": {}, \"W\":{}}"
      , "{}"
      , "[]"
      , "{\"unary\""
      , "{\"unary\":"
      , "{\"unary\":1"
      ]

  , testWithSomeType "SomeType (two-element array)"
      (select
        thSomeTypeParseJSON2ElemArray
        gSomeTypeParseJSON2ElemArray)
      [ "[\"unary\", true]"
      , "[\"record\", null]"
      , "[\"X\", 0]"
      , "[null, 0]"
      , "[]"
      , "{}"
      , "[1"
      , "[1,"
      ]

  , testWithSomeType "SomeType (reject unknown fields)"
      (select
        thSomeTypeParseJSONRejectUnknownFields
        gSomeTypeParseJSONRejectUnknownFields)
      [ "{\"tag\": \"record\", \"testone\": 1.0, \"testZero\": 1}"
      , "{\"testZero\": 1}"
      , "{\"tag\": \"record\", \"testone\": true, \"testtwo\": null, \"testthree\": null}"
      ]

  , testWithFoo "Foo (reject unknown fields)"
      (select
        thFooParseJSONRejectUnknownFields
        gFooParseJSONRejectUnknownFields)
      [ "{\"tag\": \"foo\"}"
      ]

  , testWithFoo "Foo (reject unknown fields, tagged single)"
      (select
        thFooParseJSONRejectUnknownFieldsTagged
        gFooParseJSONRejectUnknownFieldsTagged)
      [ "{\"tag\": \"foo\", \"unknownField\": 0}"
      ]

  , testWith "EitherTextInt"
      (select
        thEitherTextIntParseJSONUntaggedValue
        gEitherTextIntParseJSONUntaggedValue)
      [ "\"X\""
      , "[]"
      ]

  , testWith "Product2 Int Bool"
      (select
        thProduct2ParseJSON
        gProduct2ParseJSON)
      [ "[1, null]"
      , "[]"
      , "{}"
      ]
  ]
  where
    select a b = case choice of
      TH -> a
      G -> b

-- Test infrastructure

type Output = [String]

outputLine :: String -> Output
outputLine = pure

aesonGoldenTest :: TestName -> FilePath -> Output -> TestTree
aesonGoldenTest name ref out = goldenTest name (L.readFile ref) act cmp upd
  where
    act = pure (L.pack (unlines out))
    upd = L.writeFile ref
    cmp x y | x == y = return Nothing
    cmp x y = return $ Just $ unlines $
        concatMap f (getGroupedDiff (L.lines x) (L.lines y))
      where
        f (First xs)  = map (cons3 '-' . L.unpack) xs
        f (Second ys) = map (cons3 '+' . L.unpack) ys
        -- we print unchanged lines too. It shouldn't be a problem while we have
        -- reasonably small examples
        f (Both xs _) = map (cons3 ' ' . L.unpack) xs
        -- we add three characters, so the changed lines are easier to spot
        cons3 c cs = c : c : c : ' ' : cs

testWith :: Show a => String -> (Value -> Parser a) -> [L.ByteString] -> Output
testWith name parser ts =
  outputLine name <>
  foldMap (\s ->
    case eitherDecodeWith json (iparse parser) s of
      Left err -> outputLine $ uncurry formatError err
      Right a -> outputLine $ show a) ts

testFor :: forall a proxy. (FromJSON a, Show a)
        => String -> proxy a -> [L.ByteString] -> Output
testFor name _ = testWith name (parseJSON :: Value -> Parser a)

testWithSomeType :: String -> (Value -> Parser (SomeType Int)) -> [L.ByteString] -> Output
testWithSomeType = testWith

testWithFoo :: String -> (Value -> Parser Foo) -> [L.ByteString] -> Output
testWithFoo = testWith
