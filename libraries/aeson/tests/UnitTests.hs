{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

-- For Data.Aeson.Types.camelTo
{-# OPTIONS_GHC -fno-warn-deprecations #-}

#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
#endif

module UnitTests
    (
      ioTests
    , tests
    , withEmbeddedJSONTest
    ) where

import Prelude.Compat

import Control.Applicative (Const)
import Control.Monad (forM, forM_)
import Data.Aeson ((.=), (.:), (.:?), (.:!), FromJSON(..), FromJSONKeyFunction(..), FromJSONKey(..), ToJSON1(..), decode, eitherDecode, encode, fromJSON, genericParseJSON, genericToEncoding, genericToJSON, object, withObject, withEmbeddedJSON)
import Data.Aeson.Internal (JSONPathElement(..), formatError)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Aeson.TH (deriveJSON, deriveToJSON, deriveToJSON1)
import Data.Aeson.Text (encodeToTextBuilder)
import Data.Aeson.Parser
  ( json, jsonLast, jsonAccum, jsonNoDup
  , json', jsonLast', jsonAccum', jsonNoDup')
import Data.Aeson.Types
  ( Options(..), Result(Success, Error), ToJSON(..)
  , Value(Array, Bool, Null, Number, Object, String), camelTo, camelTo2
  , defaultOptions, formatPath, formatRelativePath, omitNothingFields, parse)
import Data.Attoparsec.ByteString (Parser, parseOnly)
import Data.Char (toUpper)
import Data.Either.Compat (isLeft, isRight)
import Data.Hashable (hash)
import Data.HashMap.Strict (HashMap)
import Data.List (sort, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific, scientific)
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Format.Compat (parseTimeM, defaultTimeLocale)
import GHC.Generics (Generic)
import Instances ()
import Numeric.Natural (Natural)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>), takeExtension, takeFileName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, assertEqual, testCase, (@?=))
import Text.Printf (printf)
import UnitTests.NullaryConstructors (nullaryConstructors)
import qualified Data.ByteString as S
import qualified Data.ByteString.Base16.Lazy as LBase16
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as Vector
import qualified ErrorMessages
import qualified SerializationFormatSpec

-- Asserts that we can use both modules at once in the test suite.
import Data.Aeson.Parser.UnescapeFFI ()
import Data.Aeson.Parser.UnescapePure ()

tests :: TestTree
tests = testGroup "unit" [
    testGroup "SerializationFormatSpec" SerializationFormatSpec.tests
  , testGroup "ErrorMessages" ErrorMessages.tests
  , testGroup "camelCase" [
      testCase "camelTo" $ roundTripCamel "aName"
    , testCase "camelTo" $ roundTripCamel "another"
    , testCase "camelTo" $ roundTripCamel "someOtherName"
    , testCase "camelTo" $
        assertEqual "" "camel_apicase" (camelTo '_' "CamelAPICase")
    , testCase "camelTo2" $ roundTripCamel2 "aName"
    , testCase "camelTo2" $ roundTripCamel2 "another"
    , testCase "camelTo2" $ roundTripCamel2 "someOtherName"
    , testCase "camelTo2" $
        assertEqual "" "camel_api_case" (camelTo2 '_' "CamelAPICase")
    ]
  , testGroup "encoding" [
      testCase "goodProducer" goodProducer
    ]
  , testGroup "utctime" [
      testCase "good" utcTimeGood
    , testCase "bad"  utcTimeBad
    ]
  , testGroup "formatError" [
      testCase "example 1" formatErrorExample
    ]
  , testGroup ".:, .:?, .:!" $ fmap (testCase "-") dotColonMark
  , testGroup "Hashable laws" $ fmap (testCase "-") hashableLaws
  , testGroup "Object construction" $ fmap (testCase "-") objectConstruction
  , testGroup "Issue #351" $ fmap (testCase "-") issue351
  , testGroup "Nullary constructors" $ fmap (testCase "-") nullaryConstructors
  , testGroup "FromJSONKey" $ fmap (testCase "-") fromJSONKeyAssertions
  , testCase "PR #455" pr455
  , testCase "Unescape string (PR #477)" unescapeString
  , testCase "Show Options" showOptions
  , testGroup "SingleMaybeField" singleMaybeField
  , testCase "withEmbeddedJSON" withEmbeddedJSONTest
  , testCase "SingleFieldCon" singleFieldCon
  , testGroup "UnknownFields" unknownFields
  , testGroup "Ordering of object keys" keyOrdering
  , testCase "Ratio with denominator 0" ratioDenominator0
  , testCase "Rational parses number"   rationalNumber
  , testCase "Big rational"             bigRationalDecoding
  , testCase "Small rational"           smallRationalDecoding
  , testCase "Big scientific exponent" bigScientificExponent
  , testCase "Big integer decoding" bigIntegerDecoding
  , testCase "Big natural decading" bigNaturalDecoding
  , testCase "Big integer key decoding" bigIntegerKeyDecoding
  , testGroup "QQ.Simple"
    [ testCase "example" $
      assertEqual "" (object ["foo" .= True]) [aesonQQ| {"foo": true } |]
    ]
  ]

roundTripCamel :: String -> Assertion
roundTripCamel name = assertEqual "" name (camelFrom '_' $ camelTo '_' name)

roundTripCamel2 :: String -> Assertion
roundTripCamel2 name = assertEqual "" name (camelFrom '_' $ camelTo2 '_' name)

camelFrom :: Char -> String -> String
camelFrom c s = let (p:ps) = split c s
                in concat $ p : map capitalize ps
  where
    split c' s' = map L.unpack $ L.split c' $ L.pack s'
    capitalize t = toUpper (head t) : tail t


data Wibble = Wibble {
    wibbleString :: String
  , wibbleInt :: Int
  } deriving (Generic, Show, Eq)

instance FromJSON Wibble

instance ToJSON Wibble where
    toJSON     = genericToJSON defaultOptions
    toEncoding = genericToEncoding defaultOptions

-- Test that if we put a bomb in a data structure, but only demand
-- part of it via lazy encoding, we do not unexpectedly fail.
goodProducer :: Assertion
goodProducer = assertEqual "partial encoding should not explode on undefined"
                           '{' (L.head (encode wibble))
  where
    wibble = Wibble {
                 wibbleString = replicate k 'a'
               , wibbleInt = 1
               }
    k | arch32bit = 4047
      | otherwise = 4030
    arch32bit     = (maxBound :: Int) == 2147483647

-- Test decoding various UTC time formats
--
-- Note: the incomplete pattern matches for UTCTimes are completely
-- intentional.  The test expects these parses to succeed.  If the
-- pattern matches fails, there's a bug in either the test or in aeson
-- and needs to be investigated.
utcTimeGood :: Assertion
utcTimeGood = do
  let ts1 = "2015-01-01T12:13:00.00Z" :: LT.Text
  let ts2 = "2015-01-01T12:13:00Z" :: LT.Text
  -- 'T' between date and time is not required, can be space
  let ts3 = "2015-01-03 12:13:00.00Z" :: LT.Text
  let ts4 = "2015-01-03 12:13:00.125Z" :: LT.Text
  let (Just (t1 ::  UTCTime)) = parseWithAeson ts1
  let (Just (t2 ::  UTCTime)) = parseWithAeson ts2
  let (Just (t3 ::  UTCTime)) = parseWithAeson ts3
  let (Just (t4 ::  UTCTime)) = parseWithAeson ts4
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" ts1) t1
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" ts2) t2
  assertEqual "utctime" (parseWithRead "%F %T%QZ" ts3) t3
  assertEqual "utctime" (parseWithRead "%F %T%QZ" ts4) t4
  -- Time zones.  Both +HHMM and +HH:MM are allowed for timezone
  -- offset, and MM may be omitted.
  let ts5 = "2015-01-01T12:30:00.00+00" :: LT.Text
  let ts6 = "2015-01-01T12:30:00.00+01:15" :: LT.Text
  let ts7 = "2015-01-01T12:30:00.00-02" :: LT.Text
  let ts8 = "2015-01-01T22:00:00.00-03" :: LT.Text
  let ts9 = "2015-01-01T22:00:00.00-04:30" :: LT.Text
  let (Just (t5 ::  UTCTime)) = parseWithAeson ts5
  let (Just (t6 ::  UTCTime)) = parseWithAeson ts6
  let (Just (t7 ::  UTCTime)) = parseWithAeson ts7
  let (Just (t8 ::  UTCTime)) = parseWithAeson ts8
  let (Just (t9 ::  UTCTime)) = parseWithAeson ts9
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-01-01T12:30:00.00Z") t5
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-01-01T11:15:00.00Z") t6
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-01-01T14:30:00Z") t7
  -- ts8 wraps around to the next day in UTC
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-01-02T01:00:00Z") t8
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-01-02T02:30:00Z") t9

  -- Seconds in Time can be omitted
  let ts10 = "2015-01-03T12:13Z" :: LT.Text
  let ts11 = "2015-01-03 12:13Z" :: LT.Text
  let ts12 = "2015-01-01T12:30-02" :: LT.Text
  let (Just (t10 ::  UTCTime)) = parseWithAeson ts10
  let (Just (t11 ::  UTCTime)) = parseWithAeson ts11
  let (Just (t12 ::  UTCTime)) = parseWithAeson ts12
  assertEqual "utctime" (parseWithRead "%FT%H:%MZ" ts10) t10
  assertEqual "utctime" (parseWithRead "%F %H:%MZ" ts11) t11
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-01-01T14:30:00Z") t12

  -- leap seconds are included correctly
  let ts13 = "2015-08-23T23:59:60.128+00" :: LT.Text
  let (Just (t13 ::  UTCTime)) = parseWithAeson ts13
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-08-23T23:59:60.128Z") t13
  let ts14 = "2015-08-23T23:59:60.999999999999+00" :: LT.Text
  let (Just (t14 ::  UTCTime)) = parseWithAeson ts14
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-08-23T23:59:60.999999999999Z") t14

  where
    parseWithRead :: String -> LT.Text -> UTCTime
    parseWithRead f s =
      fromMaybe (error "parseTime input malformed") . parseTimeM True defaultTimeLocale f . LT.unpack $ s
    parseWithAeson :: LT.Text -> Maybe UTCTime
    parseWithAeson s = decode . LT.encodeUtf8 $ LT.concat ["\"", s, "\""]

-- Test that a few non-timezone qualified timestamp formats get
-- rejected if decoding to UTCTime.
utcTimeBad :: Assertion
utcTimeBad = do
  verifyFailParse "2000-01-01T12:13:00" -- missing Zulu time not allowed (some TZ required)
  verifyFailParse "2000-01-01 12:13:00" -- missing Zulu time not allowed (some TZ required)
  verifyFailParse "2000-01-01"          -- date only not OK
  verifyFailParse "2000-01-01Z"         -- date only not OK
  verifyFailParse "2015-01-01T12:30:00.00+00Z" -- no Zulu if offset given
  verifyFailParse "2015-01-01T12:30:00.00+00:00Z" -- no Zulu if offset given
  verifyFailParse "2015-01-03 12:13:00.Z" -- decimal at the end but no digits
  verifyFailParse "2015-01-03 12:13.000Z" -- decimal at the end, but no seconds
  verifyFailParse "2015-01-03 23:59:61Z"  -- exceeds allowed seconds per day
  where
    verifyFailParse (s :: LT.Text) =
      let (dec :: Maybe UTCTime) = decode . LT.encodeUtf8 $ LT.concat ["\"", s, "\""] in
      assertEqual "verify failure" Nothing dec

-- Non identifier keys should be escaped & enclosed in brackets
formatErrorExample :: Assertion
formatErrorExample =
  let rhs = formatError [Index 0, Key "foo", Key "bar", Key "a.b.c", Key "", Key "'\\", Key "end"] "error msg"
      lhs = "Error in $[0].foo.bar['a.b.c']['']['\\'\\\\'].end: error msg"
  in assertEqual "formatError example" lhs rhs

formatPathExample :: Assertion
formatPathExample =
  let rhs = formatPath [Key "x", Index 0]
      lhs = "$.x[0]"
  in assertEqual "formatPath example" lhs rhs

formatRelativePathExample :: Assertion
formatRelativePathExample =
  let rhs = formatRelativePath [Key "x", Index 0]
      lhs = ".x[0]"
  in assertEqual "formatRelativePath example" lhs rhs

------------------------------------------------------------------------------
-- Comparison (.:?) and (.:!)
------------------------------------------------------------------------------

newtype T1 = T1 (Maybe Int) deriving (Eq, Show)
newtype T2 = T2 (Maybe Int) deriving (Eq, Show)
newtype T3 = T3 (Maybe Int) deriving (Eq, Show)

instance FromJSON T1 where parseJSON = fmap T1 . withObject "T1" (.: "value")
instance FromJSON T2 where parseJSON = fmap T2 . withObject "T2" (.:? "value")
instance FromJSON T3 where parseJSON = fmap T3 . withObject "T3" (.:! "value")

dotColonMark :: [Assertion]
dotColonMark = [
    assertEqual ".:  not-present" Nothing               (decode ex1 :: Maybe T1)
  , assertEqual ".:  42"          (Just (T1 (Just 42))) (decode ex2 :: Maybe T1)
  , assertEqual ".:  null"        (Just (T1 Nothing))   (decode ex3 :: Maybe T1)

  , assertEqual ".:? not-present" (Just (T2 Nothing))   (decode ex1 :: Maybe T2)
  , assertEqual ".:? 42"          (Just (T2 (Just 42))) (decode ex2 :: Maybe T2)
  , assertEqual ".:? null"        (Just (T2 Nothing))   (decode ex3 :: Maybe T2)

  , assertEqual ".:! not-present" (Just (T3 Nothing))   (decode ex1 :: Maybe T3)
  , assertEqual ".:! 42"          (Just (T3 (Just 42))) (decode ex2 :: Maybe T3)
  , assertEqual ".:! null"        Nothing               (decode ex3 :: Maybe T3)
  ]
  where ex1 = "{}"
        ex2 = "{\"value\": 42 }"
        ex3 = "{\"value\": null }"

------------------------------------------------------------------------------
-- Check that the hashes of two equal Value are the same
------------------------------------------------------------------------------

hashableLaws :: [Assertion]
hashableLaws = [
    assertEqual "Hashable Object" (hash a) (hash b)
  ]
  where
  a = object ["223" .= False, "807882556" .= True]
  b = object ["807882556" .= True, "223" .= False]

------------------------------------------------------------------------------
-- Check that an alternative way to construct objects works
------------------------------------------------------------------------------

objectConstruction :: [Assertion]
objectConstruction = [
    assertEqual "Equal objects constructed differently" recommended notRecommended
  ]
  where
    recommended = object ["foo" .= True, "bar" .= (-1 :: Int)]
    notRecommended = Object (mconcat ["foo" .= True, "bar" .= (-1 :: Int)])

-------------------------------------------------------------------------------
-- ToJSONKey
-------------------------------------------------------------------------------

newtype MyText = MyText Text
    deriving (FromJSONKey)

newtype MyText' = MyText' Text

instance FromJSONKey MyText' where
    fromJSONKey = fmap MyText' fromJSONKey
    fromJSONKeyList = error "not used"

fromJSONKeyAssertions :: [Assertion]
fromJSONKeyAssertions =
    [ assertIsCoerce  "Text"            (fromJSONKey :: FromJSONKeyFunction Text)
    , assertIsCoerce  "Tagged Int Text" (fromJSONKey :: FromJSONKeyFunction (Tagged Int Text))
    , assertIsCoerce  "MyText"          (fromJSONKey :: FromJSONKeyFunction MyText)

#if __GLASGOW_HASKELL__ >= 710
    , assertIsCoerce' "MyText'"         (fromJSONKey :: FromJSONKeyFunction MyText')
    , assertIsCoerce  "Const Text"      (fromJSONKey :: FromJSONKeyFunction (Const Text ()))
#endif
    ]
  where
    assertIsCoerce :: String -> FromJSONKeyFunction a -> Assertion
    assertIsCoerce _ FromJSONKeyCoerce = pure ()
    assertIsCoerce n _                 = assertFailure n

#if __GLASGOW_HASKELL__ >= 710
    assertIsCoerce' :: String -> FromJSONKeyFunction a -> Assertion
    assertIsCoerce' _ FromJSONKeyCoerce = pure ()
    assertIsCoerce' n _                 = pickWithRules (assertFailure n) (pure ())

-- | Pick the first when RULES are enabled, e.g. optimisations are on
pickWithRules
    :: a -- ^ Pick this when RULES are on
    -> a -- ^ use this otherwise
    -> a
pickWithRules _ = id
{-# NOINLINE pickWithRules #-}
{-# RULES "pickWithRules/rule" [0] forall x. pickWithRules x = const x #-}
#endif

------------------------------------------------------------------------------
-- Regressions
------------------------------------------------------------------------------

-- A regression test for: https://github.com/bos/aeson/issues/351
overlappingRegression :: FromJSON a => L.ByteString -> [a]
overlappingRegression bs = fromMaybe [] $ decode bs

issue351 :: [Assertion]
issue351 = [
    assertEqual "Int"  ([1, 2, 3] :: [Int])  $ overlappingRegression "[1, 2, 3]"
  , assertEqual "Char" ("abc"     :: String) $ overlappingRegression "\"abc\""
  , assertEqual "Char" (""        :: String) $ overlappingRegression "[\"a\", \"b\", \"c\"]"
  ]

------------------------------------------------------------------------------
-- Comparison between bytestring and text encoders
------------------------------------------------------------------------------

ioTests :: IO [TestTree]
ioTests = do
  enc <- encoderComparisonTests
  js <- jsonTestSuite
  return [enc, js]

encoderComparisonTests :: IO TestTree
encoderComparisonTests = do
  encoderTests <- forM testFiles $ \file0 -> do
      let file = "benchmarks/json-data/" ++ file0
      return $ testCase file $ do
          inp <- L.readFile file
          case eitherDecode inp of
            Left  err -> assertFailure $ "Decoding failure: " ++ err
            Right val -> assertEqual "" (encode val) (encodeViaText val)
  return $ testGroup "encoders" encoderTests
 where
  encodeViaText :: Value -> L.ByteString
  encodeViaText =
      TLE.encodeUtf8 . TLB.toLazyText . encodeToTextBuilder . toJSON

  testFiles =
    [ "example.json"
    , "integers.json"
    , "jp100.json"
    , "numbers.json"
    , "twitter10.json"
    , "twitter20.json"
    , "geometry.json"
    , "jp10.json"
    , "jp50.json"
    , "twitter1.json"
    , "twitter100.json"
    , "twitter50.json"
    ]

-- A regression test for: https://github.com/bos/aeson/issues/293
data MyRecord = MyRecord {_field1 :: Maybe Int, _field2 :: Maybe Bool}

data MyRecord2 = MyRecord2 {_field3 :: Maybe Int, _field4 :: Maybe Bool}
  deriving Generic

instance ToJSON   MyRecord2
instance FromJSON MyRecord2

-- A regression test for: https://github.com/bos/aeson/pull/477
unescapeString :: Assertion
unescapeString = do
  assertEqual "Basic escaping"
     (Right ("\" / \\ \b \f \n \r \t" :: String))
     (eitherDecode "\"\\\" \\/ \\\\ \\b \\f \\n \\r \\t\"")

  forM_ [minBound .. maxBound :: Char] $ \ c ->
    let s = LT.pack [c] in
    assertEqual (printf "UTF-16 encoded '\\x%X'" c)
      (Right s) (eitherDecode $ utf16Char s)
  where
    utf16Char = formatString . LBase16.encode . LT.encodeUtf16BE
    formatString s
      | L.length s == 4 = L.concat ["\"\\u", s, "\""]
      | L.length s == 8 =
          L.concat ["\"\\u", L.take 4 s, "\\u", L.drop 4 s, "\""]
      | otherwise = error "unescapeString: can't happen"

-- JSONTestSuite

jsonTestSuiteTest :: FilePath -> TestTree
jsonTestSuiteTest path = testCase fileName $ do
    payload <- L.readFile path
    let result = eitherDecode payload :: Either String Value
    assertBool fileName $ case take 2 fileName of
      "i_" -> isRight result
      "n_" -> isLeft result
      "y_" -> isRight result
      _    -> isRight result -- test_transform tests have inconsistent names
  where
    fileName = takeFileName path

-- Build a collection of tests based on the current contents of the
-- JSONTestSuite test directories.

jsonTestSuite :: IO TestTree
jsonTestSuite = do
  let suitePath = "tests/JSONTestSuite"
  let suites = ["test_parsing", "test_transform"]
  testPaths <- fmap (sort . concat) . forM suites $ \suite -> do
    let dir = suitePath </> suite
    entries <- getDirectoryContents dir
    let ok name = takeExtension name == ".json" &&
                  not (name `HashSet.member` blacklist)
    return . map (dir </>) . filter ok $ entries
  return $ testGroup "JSONTestSuite" $ map jsonTestSuiteTest testPaths

-- The set expected-to-be-failing JSONTestSuite tests.
-- Not all of these failures are genuine bugs.
-- Of those that are bugs, not all are worth fixing.

blacklist :: HashSet.HashSet String
-- blacklist = HashSet.empty
blacklist = _blacklist

_blacklist :: HashSet.HashSet String
_blacklist = HashSet.fromList [
    "i_object_key_lone_2nd_surrogate.json"
  , "i_string_1st_surrogate_but_2nd_missing.json"
  , "i_string_1st_valid_surrogate_2nd_invalid.json"
  , "i_string_UTF-16LE_with_BOM.json"
  , "i_string_UTF-16_invalid_lonely_surrogate.json"
  , "i_string_UTF-16_invalid_surrogate.json"
  , "i_string_UTF-8_invalid_sequence.json"
  , "i_string_incomplete_surrogate_and_escape_valid.json"
  , "i_string_incomplete_surrogate_pair.json"
  , "i_string_incomplete_surrogates_escape_valid.json"
  , "i_string_invalid_lonely_surrogate.json"
  , "i_string_invalid_surrogate.json"
  , "i_string_inverted_surrogates_U+1D11E.json"
  , "i_string_lone_second_surrogate.json"
  , "i_string_not_in_unicode_range.json"
  , "i_string_truncated-utf-8.json"
  , "i_structure_UTF-8_BOM_empty_object.json"
  , "string_1_escaped_invalid_codepoint.json"
  , "string_1_invalid_codepoint.json"
  , "string_1_invalid_codepoints.json"
  , "string_2_escaped_invalid_codepoints.json"
  , "string_2_invalid_codepoints.json"
  , "string_3_escaped_invalid_codepoints.json"
  , "string_3_invalid_codepoints.json"
  , "y_string_utf16BE_no_BOM.json"
  , "y_string_utf16LE_no_BOM.json"
  ]

-- A regression test for: https://github.com/bos/aeson/pull/455
data Foo a = FooNil | FooCons (Foo Int)

pr455 :: Assertion
pr455 = assertEqual "FooCons FooNil"
          (toJSON foo) (liftToJSON undefined undefined foo)
  where
    foo :: Foo Int
    foo = FooCons FooNil

showOptions :: Assertion
showOptions =
    assertEqual
        "Show Options"
        (  "Options {"
        ++   "fieldLabelModifier =~ \"exampleField\""
        ++ ", constructorTagModifier =~ \"ExampleConstructor\""
        ++ ", allNullaryToStringTag = True"
        ++ ", omitNothingFields = False"
        ++ ", sumEncoding = TaggedObject {tagFieldName = \"tag\", contentsFieldName = \"contents\"}"
        ++ ", unwrapUnaryRecords = False"
        ++ ", tagSingleConstructors = False"
        ++ ", rejectUnknownFields = False"
        ++ "}")
        (show defaultOptions)

newtype SingleMaybeField = SingleMaybeField { smf :: Maybe Int }
  deriving (Eq, Show, Generic)

singleMaybeField :: [TestTree]
singleMaybeField = do
  (gName, gToJSON, gToEncoding, gFromJSON) <-
    [ ("generic", genericToJSON opts, genericToEncoding opts, parse (genericParseJSON opts))
    , ("th", toJSON, toEncoding, fromJSON) ]
  return $
    testCase gName $ do
      assertEqual "toJSON"     Null (gToJSON v)
      assertEqual "toEncoding" (toEncoding (gToJSON v)) (gToEncoding v)
      assertEqual "fromJSON"   (Success v) (gFromJSON Null)
  where
    v = SingleMaybeField Nothing
    opts = defaultOptions{omitNothingFields=True,unwrapUnaryRecords=True}


newtype EmbeddedJSONTest = EmbeddedJSONTest Int
  deriving (Eq, Show)

instance FromJSON EmbeddedJSONTest where
  parseJSON =
    withObject "Object" $ \o ->
      EmbeddedJSONTest <$> (o .: "prop" >>= withEmbeddedJSON "Quoted Int" parseJSON)

withEmbeddedJSONTest :: Assertion
withEmbeddedJSONTest =
  assertEqual "Unquote embedded JSON" (Right $ EmbeddedJSONTest 1) (eitherDecode "{\"prop\":\"1\"}")

-- Regression test for https://github.com/bos/aeson/issues/627
newtype SingleFieldCon = SingleFieldCon Int deriving (Eq, Show, Generic)

instance FromJSON SingleFieldCon where
  parseJSON = genericParseJSON defaultOptions{unwrapUnaryRecords=True}
  -- This option should have no effect on this type

singleFieldCon :: Assertion
singleFieldCon =
  assertEqual "fromJSON" (Right (SingleFieldCon 0)) (eitherDecode "0")

newtype UnknownFields = UnknownFields { knownField :: Int }
  deriving (Eq, Show, Generic)
newtype UnknownFieldsTag = UnknownFieldsTag { tag :: Int }
  deriving (Eq, Show, Generic)
newtype UnknownFieldsUnaryTagged = UnknownFieldsUnaryTagged { knownFieldUnaryTagged :: Int }
  deriving (Eq, Show, Generic)
data UnknownFieldsSum
  = UnknownFields1 { knownField1 :: Int }
  | UnknownFields2 { knownField2 :: Int }
  deriving (Eq, Show, Generic)

unknownFields :: [TestTree]
unknownFields = concat
    [ testsUnary
        "unary-unknown"
        (object [("knownField", Number 1), ("unknownField", Number 1)])
        (Error "nknown fields: [\"unknownField\"]" :: Result UnknownFields)
    , testsUnary
        "unary-unknown-tag"
        (object [("knownField", Number 1), ("tag", String "UnknownFields")])
        (Error "nknown fields: [\"tag\"]" :: Result UnknownFields)
    , testsUnaryTag
        "unary-explicit-tag"
        (object [("tag", Number 1)])
        (Success $ UnknownFieldsTag 1)
    , testsSum
        "sum-tag"
        (object [("knownField1", Number 1), ("tag", String "UnknownFields1")])
        (Success $ UnknownFields1 1)
    , testsSum
        "sum-unknown-in-branch"
        (object [("knownField1", Number 1), ("knownField2", Number 1), ("tag", String "UnknownFields1")])
        (Error "nknown fields: [\"knownField2\"]" :: Result UnknownFieldsSum)
    , testsSum
        "sum-unknown"
        (object [("knownField1", Number 1), ("unknownField", Number 1), ("tag", String "UnknownFields1")])
        (Error "nknown fields: [\"unknownField\"]" :: Result UnknownFieldsSum)
    , testsTagged
        "unary-tagged"
        (object [("knownFieldUnaryTagged", Number 1), ("tag", String "UnknownFieldsUnaryTagged")])
        (Success $ UnknownFieldsUnaryTagged 1)
    , -- Just a case to verify that the tag isn't optional, this is likely already tested by other unit tests
      testsTagged
        "unary-tagged-notag"
        (object [("knownFieldUnaryTagged", Number 1)])
        (Error "key \"tag\" not found" :: Result UnknownFieldsUnaryTagged)
    , testsTagged
        "unary-tagged-unknown"
        (object [ ("knownFieldUnaryTagged", Number 1), ("unknownField", Number 1)
                , ("tag", String "UnknownFieldsUnaryTagged")])
        (Error "nknown fields: [\"unknownField\"]" :: Result UnknownFieldsUnaryTagged)
    ]
    where
        opts = defaultOptions{rejectUnknownFields=True}
        taggedOpts = opts{tagSingleConstructors=True}
        assertApprox :: (Show a, Eq a) => Result a -> Result a -> IO ()
        assertApprox (Error expected) (Error actual) | expected `isSuffixOf` actual = return ()
        assertApprox expected actual = assertEqual "fromJSON" expected actual
        testsBase :: (Show a, Eq a) => (Value -> Result a) -> (Value -> Result a)
                                    -> String -> Value -> Result a -> [TestTree]
        testsBase th g name value expected =
            [ testCase (name ++ "-th") $ assertApprox expected (th value)
            , testCase (name ++ "-generic") $ assertApprox expected (g value)
            ]
        testsUnary :: String -> Value -> Result UnknownFields -> [TestTree]
        testsUnary = testsBase fromJSON (parse (genericParseJSON opts))
        testsUnaryTag :: String -> Value -> Result UnknownFieldsTag -> [TestTree]
        testsUnaryTag = testsBase fromJSON (parse (genericParseJSON opts))
        testsSum :: String -> Value -> Result UnknownFieldsSum -> [TestTree]
        testsSum = testsBase fromJSON (parse (genericParseJSON opts))
        testsTagged :: String -> Value -> Result UnknownFieldsUnaryTagged -> [TestTree]
        testsTagged = testsBase fromJSON (parse (genericParseJSON taggedOpts))

testParser :: (Eq a, Show a)
           => String -> Parser a -> S.ByteString -> Either String a -> TestTree
testParser name json_ s expected =
  testCase name (parseOnly json_ s @?= expected)

keyOrdering :: [TestTree]
keyOrdering =
  [ testParser "json" json
      "{\"k\":true,\"k\":false}" $
      Right (Object (HashMap.fromList [("k", Bool True)]))
  , testParser "jsonLast" jsonLast
      "{\"k\":true,\"k\":false}" $
      Right (Object (HashMap.fromList [("k", Bool False)]))
  , testParser "jsonAccum" jsonAccum
      "{\"k\":true,\"k\":false}" $
      Right (Object (HashMap.fromList [("k", Array (Vector.fromList [Bool True, Bool False]))]))
  , testParser "jsonNoDup" jsonNoDup
      "{\"k\":true,\"k\":false}" $
      Left "Failed reading: found duplicate key: \"k\""

  , testParser "json'" json'
      "{\"k\":true,\"k\":false}" $
      Right (Object (HashMap.fromList [("k", Bool True)]))
  , testParser "jsonLast'" jsonLast'
      "{\"k\":true,\"k\":false}" $
      Right (Object (HashMap.fromList [("k", Bool False)]))
  , testParser "jsonAccum'" jsonAccum'
      "{\"k\":true,\"k\":false}" $
      Right (Object (HashMap.fromList [("k", Array (Vector.fromList [Bool True, Bool False]))]))
  , testParser "jsonNoDup'" jsonNoDup'
      "{\"k\":true,\"k\":false}" $
      Left "Failed reading: found duplicate key: \"k\""
  ]

ratioDenominator0 :: Assertion
ratioDenominator0 =
  assertEqual "Ratio with denominator 0"
    (Left "Error in $: Ratio denominator was 0")
    (eitherDecode "{ \"numerator\": 1, \"denominator\": 0 }" :: Either String Rational)

rationalNumber :: Assertion
rationalNumber =
  assertEqual "Ratio with denominator 0"
    (Right 1.37)
    (eitherDecode "1.37" :: Either String Rational)

bigRationalDecoding :: Assertion
bigRationalDecoding =
  assertEqual "Decoding an Integer with a large exponent should fail"
    (Left "Error in $: parsing Ratio failed, found a number with exponent 2000, but it must not be greater than 1024 or less than -1024")
    ((eitherDecode :: L.ByteString -> Either String Rational) "1e2000")

smallRationalDecoding :: Assertion
smallRationalDecoding =
  assertEqual "Decoding an Integer with a large exponent should fail"
    (Left "Error in $: parsing Ratio failed, found a number with exponent -2000, but it must not be greater than 1024 or less than -1024")
    ((eitherDecode :: L.ByteString -> Either String Rational) "1e-2000")


bigScientificExponent :: Assertion
bigScientificExponent =
  assertEqual "Encoding an integral scientific with a large exponent should normalize it"
    "1.0e2000"
    (encode (scientific 1 2000 :: Scientific))

bigIntegerDecoding :: Assertion
bigIntegerDecoding =
  assertEqual "Decoding an Integer with a large exponent should fail"
    (Left "Error in $: parsing Integer failed, found a number with exponent 2000, but it must not be greater than 1024")
    ((eitherDecode :: L.ByteString -> Either String Integer) "1e2000")

bigNaturalDecoding :: Assertion
bigNaturalDecoding =
  assertEqual "Decoding a Natural with a large exponent should fail"
    (Left "Error in $: parsing Natural failed, found a number with exponent 2000, but it must not be greater than 1024")
    ((eitherDecode :: L.ByteString -> Either String Natural) "1e2000")

bigIntegerKeyDecoding :: Assertion
bigIntegerKeyDecoding =
  assertEqual "Decoding an Integer key with a large exponent should fail"
    (Left "Error in $['1e2000']: parsing Integer failed, found a number with exponent 2000, but it must not be greater than 1024")
    ((eitherDecode :: L.ByteString -> Either String (HashMap Integer Value)) "{ \"1e2000\": null }")

bigNaturalKeyDecoding :: Assertion
bigNaturalKeyDecoding =
  assertEqual "Decoding an Integer key with a large exponent should fail"
    (Left "Error in $['1e2000']: found a number with exponent 2000, but it must not be greater than 1024")
    ((eitherDecode :: L.ByteString -> Either String (HashMap Natural Value)) "{ \"1e2000\": null }")

-- A regression test for: https://github.com/bos/aeson/issues/757
type family Fam757 :: * -> *
type instance Fam757 = Maybe
newtype Newtype757 a = MkNewtype757 (Fam757 a)

deriveJSON defaultOptions{omitNothingFields=True} ''MyRecord

deriveToJSON  defaultOptions ''Foo
deriveToJSON1 defaultOptions ''Foo

deriveJSON defaultOptions{omitNothingFields=True,unwrapUnaryRecords=True} ''SingleMaybeField

deriveJSON defaultOptions{rejectUnknownFields=True} ''UnknownFields
deriveJSON defaultOptions{rejectUnknownFields=True} ''UnknownFieldsTag
deriveJSON defaultOptions{tagSingleConstructors=True,rejectUnknownFields=True} ''UnknownFieldsUnaryTagged
deriveJSON defaultOptions{rejectUnknownFields=True} ''UnknownFieldsSum

deriveToJSON1 defaultOptions ''Newtype757
