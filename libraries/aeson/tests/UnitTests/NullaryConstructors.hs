{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

module UnitTests.NullaryConstructors
    (
      nullaryConstructors
    ) where

import Prelude.Compat

import Data.Aeson (decode, eitherDecode, fromEncoding, Value)
import Data.Aeson.Internal (IResult (..), iparse)
import Data.Aeson.Types (Parser)
import Data.ByteString.Builder (toLazyByteString)
import Data.Foldable (for_)
import Data.Maybe (fromJust)
import Encoders
import Test.Tasty.HUnit ((@=?), Assertion)
import Types
import qualified Data.ByteString.Lazy.Char8 as L

nullaryConstructors :: [Assertion]
nullaryConstructors =
  [ dec "\"C1\""           @=? thNullaryToJSONString C1
  , dec "\"C1\""           @=? gNullaryToJSONString C1
  , dec "{\"c1\":[]}"      @=? thNullaryToJSONObjectWithSingleField C1
  , dec "{\"c1\":[]}"      @=? gNullaryToJSONObjectWithSingleField C1
  , dec "[\"c1\",[]]"      @=? gNullaryToJSON2ElemArray C1
  , dec "[\"c1\",[]]"      @=? thNullaryToJSON2ElemArray C1
  , dec "{\"tag\":\"c1\"}" @=? thNullaryToJSONTaggedObject C1
  , dec "{\"tag\":\"c1\"}" @=? gNullaryToJSONTaggedObject C1

  , decE "\"C1\""           @=? enc (gNullaryToEncodingString C1)
  , decE "\"C1\""           @=? enc (thNullaryToEncodingString C1)
  , decE "[\"c1\",[]]"      @=? enc (gNullaryToEncoding2ElemArray C1)
  , decE "[\"c1\",[]]"      @=? enc (thNullaryToEncoding2ElemArray C1)
  , decE "{\"c1\":[]}"      @=? enc (thNullaryToEncodingObjectWithSingleField C1)
  , decE "{\"c1\":[]}"      @=? enc (gNullaryToEncodingObjectWithSingleField C1)
  , decE "{\"tag\":\"c1\"}" @=? enc (thNullaryToEncodingTaggedObject C1)
  , decE "{\"tag\":\"c1\"}" @=? enc (gNullaryToEncodingTaggedObject C1)

  , ISuccess C1 @=? parse thNullaryParseJSONTaggedObject          (dec "{\"tag\":\"c1\"}")
  , ISuccess C1 @=? parse gNullaryParseJSONTaggedObject           (dec "{\"tag\":\"c1\"}")

  , ISuccess C1 @=? parse thNullaryParseJSONString                (dec "\"C1\"")
  , ISuccess C1 @=? parse gNullaryParseJSONString                 (dec "\"C1\"")
  , ISuccess C1 @=? parse thNullaryParseJSON2ElemArray            (dec  "[\"c1\",[]]")
  , ISuccess C1 @=? parse gNullaryParseJSON2ElemArray             (dec  "[\"c1\",[]]")
  , ISuccess C1 @=? parse thNullaryParseJSONObjectWithSingleField (dec  "{\"c1\":[]}")
  , ISuccess C1 @=? parse gNullaryParseJSONObjectWithSingleField  (dec  "{\"c1\":[]}")
    -- Make sure that the old `"contents" : []' is still allowed
  , ISuccess C1 @=? parse thNullaryParseJSONTaggedObject          (dec "{\"tag\":\"c1\",\"contents\":[]}")
  , ISuccess C1 @=? parse gNullaryParseJSONTaggedObject           (dec "{\"tag\":\"c1\",\"contents\":[]}")

  , for_ [("kC1", C1), ("kC2", C2), ("kC3", C3)] $ \(jkey, key) -> do
      Right   jkey @=? gNullaryToJSONKey key
      ISuccess key @=? parse gNullaryFromJSONKey jkey
  ]
  where
    enc = eitherDecode . toLazyByteString . fromEncoding
    dec :: L.ByteString -> Value
    dec = fromJust . decode
    decE :: L.ByteString -> Either String Value
    decE = eitherDecode
    parse :: (a -> Parser b) -> a -> IResult b
    parse parsejson v = iparse parsejson v
