{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.QuickCheck (Arbitrary(..))

import Control.Monad (liftM)
import qualified Data.ByteString.Base64          as Base64
import qualified Data.ByteString.Base64.Lazy     as LBase64
import qualified Data.ByteString.Base64.URL      as Base64URL
import qualified Data.ByteString.Base64.URL.Lazy as LBase64URL
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.String
import Test.HUnit hiding (Test)


main :: IO ()
main = defaultMain tests


data Impl bs = Impl
  { _label :: String
  , _encode :: bs -> bs
  , _decode :: bs -> Either String bs
  , _lenient :: bs -> bs
  }

data UrlImpl bs = UrlImpl
  { _labelUrl :: String
  , _encodeUrl :: bs -> bs
  , _decodeUrl :: bs -> Either String bs
  , _encodeUrlNopad :: bs -> bs
  , _decodeUrlNopad :: bs -> Either String bs
  , _decodeUrlPad :: bs -> Either String bs
  , _lenientUrl :: bs -> bs
  }

tests :: [Test]
tests =
  [ testGroup "property tests"
    [ testsRegular b64impl
    , testsRegular lb64impl
    , testsURL b64uimpl
    , testsURL lb64uimpl
    ]
  , testGroup "unit tests"
    [ base64UrlUnitTests
    , lazyBase64UrlUnitTests
    ]
  ]
  where
    b64impl = Impl "Base64" Base64.encode Base64.decode Base64.decodeLenient
    lb64impl = Impl "LBase64" LBase64.encode LBase64.decode LBase64.decodeLenient

    b64uimpl = UrlImpl
      "Base64URL"
      Base64URL.encode
      Base64URL.decode
      Base64URL.encodeUnpadded
      Base64URL.decodeUnpadded
      Base64URL.decodePadded
      Base64URL.decodeLenient

    lb64uimpl = UrlImpl
      "LBase64URL"
      LBase64URL.encode
      LBase64URL.decode
      LBase64URL.encodeUnpadded
      LBase64URL.decodeUnpadded
      LBase64URL.decodePadded
      LBase64URL.decodeLenient


testsRegular
  :: ( IsString bs
     , AllRepresentations bs
     , Show bs
     , Eq bs
     , Arbitrary bs
     )
  => Impl bs
  -> Test
testsRegular = testsWith base64_testData

testsURL
  :: ( IsString bs
     , AllRepresentations bs
     , Show bs
     , Eq bs
     , Arbitrary bs
     )
  => UrlImpl bs
  -> Test
testsURL (UrlImpl l e d eu du dp dl) = testGroup l
  [ testsWith base64url_testData (Impl "Arbitrary Padding" e d dl)
  , testsWith base64url_testData (Impl "Required Padding" e dp dl)
  , testsWith base64url_testData_nopad (Impl "No padding" eu du dl)
  , testProperty "prop_url_pad_roundtrip" $ \bs -> Right bs == dp (e bs)
  , testProperty "prop_url_nopad_roundtrip" $ \bs -> Right bs == du (eu bs)
  , testProperty "prop_url_decode_invariant" $ \bs ->
      ((du (eu bs)) == (d (e bs))) || ((dp (e bs)) == d (e bs))
  ]

testsWith
  :: ( IsString bs
     , AllRepresentations bs
     , Show bs
     , Eq bs
     , Arbitrary bs
     )
  => [(bs, bs)]
  -> Impl bs
  -> Test
testsWith testData impl = testGroup label
    [ testProperty "decodeEncode" $
      genericDecodeEncode encode decode
    , testProperty "decodeEncode Lenient" $
      genericDecodeEncode encode (liftM Right lenient)
    , testGroup "base64-string tests" (string_tests testData impl)
    ]
  where
    label = _label impl
    encode = _encode impl
    decode = _decode impl
    lenient = _lenient impl

instance Arbitrary ByteString where
  arbitrary = liftM B.pack arbitrary

-- Ideally the arbitrary instance would have arbitrary chunks as well as
-- arbitrary content
instance Arbitrary L.ByteString where
  arbitrary = liftM L.pack arbitrary

-- | Decoding an encoded sintrg should produce the original string.
genericDecodeEncode
  :: (Arbitrary bs, Eq bs)
  => (bs -> bs)
  -> (bs -> Either String bs)
  -> bs -> Bool
genericDecodeEncode enc dec x =
  case dec (enc x) of
    Left  _  -> False
    Right x' -> x == x'

--
-- Unit tests from base64-string
-- Copyright (c) Ian Lynagh, 2005, 2007.
--

string_tests
  :: forall bs
  . ( IsString bs
    , AllRepresentations bs
    , Show bs
    , Eq bs
    )
  => [(bs, bs)]
  -> Impl bs
  -> [Test]
string_tests testData (Impl _ encode decode decodeLenient) =
    base64_string_test encode decode testData ++ base64_string_test encode decodeLenient' testData
  where
    decodeLenient' = liftM Right decodeLenient


base64_testData :: IsString bs => [(bs, bs)]
base64_testData = [("",                "")
                  ,("\0",              "AA==")
                  ,("\255",            "/w==")
                  ,("E",               "RQ==")
                  ,("Ex",              "RXg=")
                  ,("Exa",             "RXhh")
                  ,("Exam",            "RXhhbQ==")
                  ,("Examp",           "RXhhbXA=")
                  ,("Exampl",          "RXhhbXBs")
                  ,("Example",         "RXhhbXBsZQ==")
                  ,("Ex\0am\254ple",   "RXgAYW3+cGxl")
                  ,("Ex\0am\255ple",   "RXgAYW3/cGxl")
                  ]

base64url_testData :: IsString bs => [(bs, bs)]
base64url_testData = [("",                "")
                     ,("\0",              "AA==")
                     ,("\255",            "_w==")
                     ,("E",               "RQ==")
                     ,("Ex",              "RXg=")
                     ,("Exa",             "RXhh")
                     ,("Exam",            "RXhhbQ==")
                     ,("Examp",           "RXhhbXA=")
                     ,("Exampl",          "RXhhbXBs")
                     ,("Example",         "RXhhbXBsZQ==")
                     ,("Ex\0am\254ple",   "RXgAYW3-cGxl")
                     ,("Ex\0am\255ple",   "RXgAYW3_cGxl")
                     ]

base64url_testData_nopad :: IsString bs => [(bs, bs)]
base64url_testData_nopad = [("",                "")
                           ,("\0",              "AA")
                           ,("\255",            "_w")
                           ,("E",               "RQ")
                           ,("Ex",              "RXg")
                           ,("Exa",             "RXhh")
                           ,("Exam",            "RXhhbQ")
                           ,("Examp",           "RXhhbXA")
                           ,("Exampl",          "RXhhbXBs")
                           ,("Example",         "RXhhbXBsZQ")
                           ,("Ex\0am\254ple",   "RXgAYW3-cGxl")
                           ,("Ex\0am\255ple",   "RXgAYW3_cGxl")
                           ]
-- | Generic test given encod enad decode funstions and a
-- list of (plain, encoded) pairs
base64_string_test
  :: ( AllRepresentations bs
     , Eq bs
     , Show bs
     )
  => (bs -> bs)
  -> (bs -> Either String bs)
  -> [(bs, bs)]
  -> [Test]
base64_string_test enc dec testData =
      [ testCase ("base64-string: Encode " ++ show plain)
                 (encoded_plain @?= rawEncoded)
      | (rawPlain, rawEncoded) <- testData
      , -- For lazy ByteStrings, we want to check not only ["foo"], but
        -- also ["f","oo"], ["f", "o", "o"] and ["fo", "o"]. The
        -- allRepresentations function gives us all representations of a
        -- lazy ByteString.
        plain <- allRepresentations rawPlain
      , let encoded_plain = enc plain
      ] ++
      [ testCase ("base64-string: Decode " ++ show encoded) (decoded_encoded @?= Right rawPlain)
      | (rawPlain, rawEncoded) <- testData
      , -- Again, we need to try all representations of lazy ByteStrings.
        encoded <- allRepresentations rawEncoded
      , let decoded_encoded = dec encoded
      ]

class AllRepresentations a where
    allRepresentations :: a -> [a]

instance AllRepresentations ByteString where
    allRepresentations bs = [bs]

instance AllRepresentations L.ByteString where
    -- TODO: Use L.toStrict instead of (B.concat . L.toChunks) once
    -- we can rely on a new enough bytestring
    allRepresentations = map L.fromChunks . allChunks . B.concat . L.toChunks
        where allChunks b
               | B.length b < 2 = [[b]]
               | otherwise = concat
                 [ map (prefix :) (allChunks suffix)
                 | let splits = zip (B.inits b) (B.tails b)
                             -- We don't want the first split (empty prefix)
                             -- The last split (empty suffix) gives us the
                             -- [b] case (toChunks ignores an "" element).
                 , (prefix, suffix) <- tail splits
                 ]

base64UrlUnitTests :: Test
base64UrlUnitTests = testGroup "Base64URL unit tests"
    [ testGroup "URL decodePadded"
      [ padtest "<" "PA=="
      , padtest "<<" "PDw="
      , padtest "<<?" "PDw_"
      , padtest "<<??" "PDw_Pw=="
      , padtest "<<??>" "PDw_Pz4="
      , padtest "<<??>>" "PDw_Pz4-"
      ]

    , testGroup "URL decodeUnpadded"
      [ nopadtest "<" "PA"
      , nopadtest "<<" "PDw"
      , nopadtest "<<?" "PDw_"
      , nopadtest "<<??" "PDw_Pw"
      , nopadtest "<<??>" "PDw_Pz4"
      , nopadtest "<<??>>" "PDw_Pz4-"
      ]

    , testGroup "Padding validity"
      [ testCase "Padding fails everywhere but end" $ do
          Base64.decode "=eAoeAo=" @=? Left "invalid padding at offset: 0"
          Base64.decode "e=AoeAo=" @=? Left "invalid padding at offset: 1"
          Base64.decode "eA=oeAo=" @=? Left "invalid padding at offset: 2"
          Base64.decode "eAo=eAo=" @=? Left "invalid padding at offset: 3"
          Base64.decode "eAoe=Ao=" @=? Left "invalid padding at offset: 4"
          Base64.decode "eAoeA=o=" @=? Left "invalid padding at offset: 5"
      ]
    , testGroup "Non-canonical encodings fail and canonical encodings succeed"
      [ testCase "roundtrip for d ~ ZA==" $ do
        Base64.decode "ZE==" @=? Left "non-canonical encoding detected at offset: 1"
        Base64.decode "ZK==" @=? Left "non-canonical encoding detected at offset: 1"
        Base64.decode "ZA==" @=? Right "d"
      , testCase "roundtrip for f` ~ ZmA=" $ do
        Base64.decode "ZmC=" @=? Left "non-canonical encoding detected at offset: 2"
        Base64.decode "ZmD=" @=? Left "non-canonical encoding detected at offset: 2"
        Base64.decode "ZmA=" @=? Right "f`"

      , testCase "roundtrip for foo` ~ Zm9vYA==" $ do
        Base64.decode "Zm9vYE==" @=? Left "non-canonical encoding detected at offset: 5"
        Base64.decode "Zm9vYK==" @=? Left "non-canonical encoding detected at offset: 5"
        Base64.decode "Zm9vYA==" @=? Right "foo`"

      , testCase "roundtrip for foob` ~ Zm9vYmA=" $ do
        Base64.decode "Zm9vYmC=" @=? Left "non-canonical encoding detected at offset: 6"
        Base64.decode "Zm9vYmD=" @=? Left "non-canonical encoding detected at offset: 6"
        Base64.decode "Zm9vYmA=" @=? Right "foob`"
      ]
    , testGroup "Base64URL padding case unit tests"
      [ testCase "stress arbitarily padded URL strings" $ do
          Base64URL.decode "P" @=? Left "Base64-encoded bytestring has invalid size"
          Base64URL.decode "PA" @=? Right "<"
          Base64URL.decode "PDw" @=? Right "<<"
          Base64URL.decode "PDw_" @=? Right "<<?"
      , testCase "stress padded URL strings" $ do
          Base64URL.decodePadded "=" @=? Left "Base64-encoded bytestring has invalid size"
          Base64URL.decodePadded "PA==" @=? Right "<"
          Base64URL.decodePadded "PDw=" @=? Right "<<"
          Base64URL.decodePadded "PDw_" @=? Right "<<?"
      , testCase "stress unpadded URL strings" $ do
          Base64URL.decodeUnpadded "P" @=? Left "Base64-encoded bytestring has invalid size"
          Base64URL.decodeUnpadded "PA" @=? Right "<"
          Base64URL.decodeUnpadded "PDw" @=? Right "<<"
          Base64URL.decodeUnpadded "PDw_" @=? Right "<<?"
      ]

    , testGroup "Base64Url branch coverage"
      [ testCase "Invalid staggered padding" $ do
        Base64URL.decode "=A==" @=? Left "invalid padding at offset: 0"
        Base64URL.decode "P===" @=? Left "invalid padding at offset: 1"
      , testCase "Invalid character coverage - final chunk" $ do
        Base64URL.decode "%D==" @=? Left "invalid character at offset: 0"
        Base64URL.decode "P%==" @=? Left "invalid character at offset: 1"
        Base64URL.decode "PD%=" @=? Left "invalid character at offset: 2"
        Base64URL.decode "PA=%" @=? Left "invalid character at offset: 3"
        Base64URL.decode "PDw%" @=? Left "invalid character at offset: 3"
      , testCase "Invalid character coverage - decode chunk" $ do
        Base64URL.decode "%Dw_PDw_" @=? Left "invalid character at offset: 0"
        Base64URL.decode "P%w_PDw_" @=? Left "invalid character at offset: 1"
        Base64URL.decode "PD%_PDw_" @=? Left "invalid character at offset: 2"
        Base64URL.decode "PDw%PDw_" @=? Left "invalid character at offset: 3"
      , testCase "Invalid padding in body" $ do
        Base64URL.decode "PD=_PDw_" @=? Left "invalid padding at offset: 2"
        Base64URL.decode "PDw=PDw_" @=? Left "invalid padding at offset: 3"
      ]
    ]
  where
    padtest s t = testCase (show $ if t == "" then "empty" else t) $ do
      let u = Base64URL.decodeUnpadded t
          v = Base64URL.decodePadded t

      if BS.last t == 0x3d
      then do
        assertEqual "Padding required: no padding fails" u $
          Left "Base64-encoded bytestring required to be unpadded"

        assertEqual "Padding required: padding succeeds" v $
          Right s
      else do
        --
        assertEqual "String has no padding: decodes should coincide" u $
          Right s
        assertEqual "String has no padding: decodes should coincide" v $
          Right s

    nopadtest s t = testCase (show $ if t == "" then "empty" else t) $ do
        let u = Base64URL.decodePadded t
            v = Base64URL.decodeUnpadded t

        if BS.length t `mod` 4 == 0
        then do
          assertEqual "String has no padding: decodes should coincide" u $
            Right s
          assertEqual "String has no padding: decodes should coincide" v $
            Right s
        else do
          assertEqual "Unpadded required: padding fails" u $
            Left "Base64-encoded bytestring is unpadded or has invalid padding"

          assertEqual "Unpadded required: unpadding succeeds" v $
            Right s

lazyBase64UrlUnitTests :: Test
lazyBase64UrlUnitTests = testGroup "LBase64URL unit tests"
    [ testGroup "URL decodePadded"
      [ padtest "<" "PA=="
      , padtest "<<" "PDw="
      , padtest "<<?" "PDw_"
      , padtest "<<??" "PDw_Pw=="
      , padtest "<<??>" "PDw_Pz4="
      , padtest "<<??>>" "PDw_Pz4-"
      ]

    , testGroup "URL decodeUnpadded"
      [ nopadtest "<" "PA"
      , nopadtest "<<" "PDw"
      , nopadtest "<<?" "PDw_"
      , nopadtest "<<??" "PDw_Pw"
      , nopadtest "<<??>" "PDw_Pz4"
      , nopadtest "<<??>>" "PDw_Pz4-"
      ]

    , testGroup "Padding validity"
      [ testCase "Padding fails everywhere but end" $ do
          LBase64.decode "=eAoeAo=" @=? Left "invalid padding at offset: 0"
          LBase64.decode "e=AoeAo=" @=? Left "invalid padding at offset: 1"
          LBase64.decode "eA=oeAo=" @=? Left "invalid padding at offset: 2"
          LBase64.decode "eAo=eAo=" @=? Left "invalid padding at offset: 3"
          LBase64.decode "eAoe=Ao=" @=? Left "invalid padding at offset: 4"
          LBase64.decode "eAoeA=o=" @=? Left "invalid padding at offset: 5"
      ]

    , testGroup "LBase64URL padding case unit tests"
      [ testCase "stress arbitarily padded URL strings" $ do
          LBase64URL.decode "P" @=? Left "Base64-encoded bytestring has invalid size"
          LBase64URL.decode "PA" @=? Right "<"
          LBase64URL.decode "PDw" @=? Right "<<"
          LBase64URL.decode "PDw_" @=? Right "<<?"
      , testCase "stress padded URL strings" $ do
          LBase64URL.decodePadded "=" @=? Left "Base64-encoded bytestring has invalid size"
          LBase64URL.decodePadded "PA==" @=? Right "<"
          LBase64URL.decodePadded "PDw=" @=? Right "<<"
          LBase64URL.decodePadded "PDw_" @=? Right "<<?"
      , testCase "stress unpadded URL strings" $ do
          LBase64URL.decodeUnpadded "P" @=? Left "Base64-encoded bytestring has invalid size"
          LBase64URL.decodeUnpadded "PA" @=? Right "<"
          LBase64URL.decodeUnpadded "PDw" @=? Right "<<"
          LBase64URL.decodeUnpadded "PDw_" @=? Right "<<?"
      ]

    , testGroup "LBase64Url branch coverage"
      [ testCase "Invalid staggered padding" $ do
        LBase64URL.decode "=A==" @=? Left "invalid padding at offset: 0"
        LBase64URL.decode "P===" @=? Left "invalid padding at offset: 1"
      , testCase "Invalid character coverage - final chunk" $ do
        LBase64URL.decode "%D==" @=? Left "invalid character at offset: 0"
        LBase64URL.decode "P%==" @=? Left "invalid character at offset: 1"
        LBase64URL.decode "PD%=" @=? Left "invalid character at offset: 2"
        LBase64URL.decode "PA=%" @=? Left "invalid character at offset: 3"
        LBase64URL.decode "PDw%" @=? Left "invalid character at offset: 3"
      , testCase "Invalid character coverage - decode chunk" $ do
        LBase64URL.decode "%Dw_PDw_" @=? Left "invalid character at offset: 0"
        LBase64URL.decode "P%w_PDw_" @=? Left "invalid character at offset: 1"
        LBase64URL.decode "PD%_PDw_" @=? Left "invalid character at offset: 2"
        LBase64URL.decode "PDw%PDw_" @=? Left "invalid character at offset: 3"
      , testCase "Invalid padding in body" $ do
        LBase64URL.decode "PD=_PDw_" @=? Left "invalid padding at offset: 2"
        LBase64URL.decode "PDw=PDw_" @=? Left "invalid padding at offset: 3"
      ]
    ]
  where
    padtest s t = testCase (show $ if t == "" then "empty" else t) $ do
      let u = LBase64URL.decodeUnpadded t
          v = LBase64URL.decodePadded t

      if L.last t == '='
      then do
        assertEqual "Padding required: no padding fails" u $
          Left "Base64-encoded bytestring required to be unpadded"

        assertEqual "Padding required: padding succeeds" v $
          Right s
      else do
        --
        assertEqual "String has no padding: decodes should coincide" u $
          Right s
        assertEqual "String has no padding: decodes should coincide" v $
          Right s

    nopadtest s t = testCase (show $ if t == "" then "empty" else t) $ do
        let u = LBase64URL.decodePadded t
            v = LBase64URL.decodeUnpadded t

        if L.length t `mod` 4 == 0
        then do
          assertEqual "String has no padding: decodes should coincide" u $
            Right s
          assertEqual "String has no padding: decodes should coincide" v $
            Right s
        else do
          assertEqual "Unpadded required: padding fails" u $
            Left "Base64-encoded bytestring is unpadded or has invalid padding"

          assertEqual "Unpadded required: unpadding succeeds" v $
            Right s
