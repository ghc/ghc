{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

------------------------------------------------------------------------------
-- These tests assert that the JSON serialization doesn't change by accident.
-----------------------------------------------------------------------------

module SerializationFormatSpec
  (
    tests
  ) where

import Prelude.Compat

import Control.Applicative (Const(..))
import Data.Aeson (FromJSON(..), decode, eitherDecode, encode, genericParseJSON, genericToEncoding, genericToJSON)
import Data.Aeson.Types (Options(..), SumEncoding(..), ToJSON(..), defaultOptions)
import Data.Fixed (Pico)
import Data.Foldable (for_, toList)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))
import Data.Functor.Sum (Sum(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy (Proxy(..))
import Data.Scientific (Scientific)
import Data.Tagged (Tagged(..))
import Data.These (These (..))
import Data.Time (fromGregorian)
import Data.Time.Calendar.Month.Compat (fromYearMonth)
import Data.Time.Calendar.Quarter.Compat (fromYearQuarter, QuarterOfYear (..))
import Data.Time.Calendar.Compat (CalendarDiffDays (..), DayOfWeek (..))
import Data.Time.LocalTime.Compat (CalendarDiffTime (..))
import Data.Time.Clock.System.Compat (SystemTime (..))
import Data.Word (Word8)
import GHC.Generics (Generic)
import Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, assertEqual, testCase)
import Types (Approx(..), Compose3, Compose3', I)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HashSet
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as M
import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Tree as Tree
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as Vector
import qualified Data.Fix as F
import qualified Data.Strict as S

tests :: [TestTree]
tests =
  [
    testGroup "To JSON representation" $ fmap assertJsonEncodingExample jsonEncodingExamples
  , testGroup "From JSON representation" $ fmap assertJsonExample jsonDecodingExamples
  , testGroup "To/From JSON representation" $ fmap assertJsonExample jsonExamples

  ]

jsonExamples :: [Example]
jsonExamples =
  [
    example "Either Left" "{\"Left\":1}"  (Left 1 :: Either Int Int)
  , example "Either Right" "{\"Right\":1}"  (Right 1 :: Either Int Int)
  , example "Nothing"  "null"  (Nothing :: Maybe Int)
  -- Maybe serialising is lossy
  -- https://github.com/bos/aeson/issues/376
  , Example "Just Nothing" ["null"] (Just Nothing :: Maybe (Maybe Int)) Nothing
  , example "Just"  "1"  (Just 1 :: Maybe Int)
  , example "Proxy Int" "null"  (Proxy :: Proxy Int)
  , example "Tagged Char Int" "1"  (Tagged 1 :: Tagged Char Int)
    -- Test Tagged instance is polykinded
  , example "Tagged 123 Int" "1"  (Tagged 1 :: Tagged 123 Int)
  , example "Const Char Int" "\"c\""  (Const 'c' :: Const Char Int)
  , example "Tuple" "[1,2]"  ((1, 2) :: (Int, Int))
  , example "NonEmpty" "[1,2,3]"  (1 :| [2, 3] :: NonEmpty Int)
  , example "Seq" "[1,2,3]"  (Seq.fromList [1, 2, 3] ::  Seq.Seq Int)
  , example "DList" "[1,2,3]"  (DList.fromList [1, 2, 3] :: DList.DList Int)
  , example "()" "[]"  ()

  , ndExample "HashMap Int Int"
        [ "{\"0\":1,\"2\":3}", "{\"2\":3,\"0\":1}"]
        (HM.fromList [(0,1),(2,3)] :: HM.HashMap Int Int)
  , ndExample "Map Int Int"
        [ "{\"0\":1,\"2\":3}", "{\"2\":3,\"0\":1}"]
        (M.fromList [(0,1),(2,3)] :: M.Map Int Int)
  , ndExample "Map (Tagged Int Int) Int"
        [ "{\"0\":1,\"2\":3}", "{\"2\":3,\"0\":1}"]
        (M.fromList [(Tagged 0,1),(Tagged 2,3)] :: M.Map (Tagged Int Int) Int)
  , example "Map [Int] Int"
        "[[[0],1],[[2],3]]"
        (M.fromList [([0],1),([2],3)] :: M.Map [Int] Int)
  , ndExample "Map [Char] Int"
        [ "{\"ab\":1,\"cd\":3}", "{\"cd\":3,\"ab\":1}" ]
        (M.fromList [("ab",1),("cd",3)] :: M.Map String Int)
  , ndExample "Map [I Char] Int"
        [ "{\"ab\":1,\"cd\":3}", "{\"cd\":3,\"ab\":1}" ]
        (M.fromList [(map pure "ab",1),(map pure "cd",3)] :: M.Map [I Char] Int)

  , example "nan :: Double" "null"  (Approx $ 0/0 :: Approx Double)

  , example "Ordering LT" "\"LT\"" LT
  , example "Ordering EQ" "\"EQ\"" EQ
  , example "Ordering GT" "\"GT\"" GT

  , example "Float" "3.14" (3.14 :: Float)
  , example "Pico" "3.14" (3.14 :: Pico)
  , example "Scientific" "3.14" (3.14 :: Scientific)

  , example "UUID" "\"c2cc10e1-57d6-4b6f-9899-38d972112d8c\"" $ UUID.fromWords
      0xc2cc10e1 0x57d64b6f 0x989938d9 0x72112d8c

  , example "Set Int" "[1,2,3]" (Set.fromList [3, 2, 1] :: Set.Set Int)
  , example "IntSet"  "[1,2,3]" (IntSet.fromList [3, 2, 1])
  , example "IntMap" "[[1,2],[3,4]]" (IntMap.fromList [(3,4), (1,2)] :: IntMap.IntMap Int)
  , example "Vector" "[1,2,3]" (Vector.fromList [1, 2, 3] :: Vector.Vector Int)
  , example "HashSet Int" "[1,2,3]" (HashSet.fromList [3, 2, 1] :: HashSet.HashSet Int)
  , example "Tree Int" "[1,[[2,[[3,[]],[4,[]]]],[5,[]]]]" (let n = Tree.Node in n 1 [n 2 [n 3 [], n 4 []], n 5 []] :: Tree.Tree Int)

  -- Three separate cases, as ordering in HashMap is not defined
  , example "HashMap Float Int, NaN" "{\"NaN\":1}"  (Approx $ HM.singleton (0/0) 1 :: Approx (HM.HashMap Float Int))
  , example "HashMap Float Int, Infinity" "{\"Infinity\":1}"  (HM.singleton (1/0) 1 :: HM.HashMap Float Int)
  , example "HashMap Float Int, +Infinity" "{\"-Infinity\":1}"  (HM.singleton (negate 1/0) 1 :: HM.HashMap Float Int)

  -- Functors
  , example "Identity Int" "1"  (pure 1 :: Identity Int)

  , example "Identity Char" "\"x\""      (pure 'x' :: Identity Char)
  , example "Identity String" "\"foo\""  (pure "foo" :: Identity String)
  , example "[Identity Char]" "\"xy\""   ([pure 'x', pure 'y'] :: [Identity Char])

  , example "Maybe Char" "\"x\""              (pure 'x' :: Maybe Char)
  , example "Maybe String" "\"foo\""          (pure "foo" :: Maybe String)
  , example "Maybe [Identity Char]" "\"xy\""  (pure [pure 'x', pure 'y'] :: Maybe [Identity Char])

  , example "Day; year >= 1000" "\"1999-10-12\""        (fromGregorian 1999    10 12)
  , example "Day; year > 0 && < 1000" "\"0500-03-04\""  (fromGregorian 500     3  4)
  , example "Day; year == 0" "\"0000-02-20\""           (fromGregorian 0       2  20)
  , example "Day; year < 0" "\"-0234-01-01\""           (fromGregorian (-234)  1  1)
  , example "Day; year < -1000" "\"-1234-01-01\""       (fromGregorian (-1234) 1  1)

  , example "Month; year >= 1000" "\"1999-10\""        (fromYearMonth 1999    10)
  , example "Month; year > 0 && < 1000" "\"0500-03\""  (fromYearMonth 500     3)
  , example "Month; year == 0" "\"0000-02\""           (fromYearMonth 0       2)
  , example "Month; year < 0" "\"-0234-01\""           (fromYearMonth (-234)  1)
  , example "Month; year < -1000" "\"-1234-01\""       (fromYearMonth (-1234) 1)

  , example "Quarter; year >= 1000" "\"1999-q1\""        (fromYearQuarter 1999    Q1)
  , example "Quarter; year > 0 && < 1000" "\"0500-q4\""  (fromYearQuarter 500     Q4)
  , example "Quarter; year == 0" "\"0000-q3\""           (fromYearQuarter 0       Q3)
  , example "Quarter; year < 0" "\"-0234-q2\""           (fromYearQuarter (-234)  Q2)
  , example "Quarter; year < -1000" "\"-1234-q1\""       (fromYearQuarter (-1234) Q1)

  , example "QuarterOfYear" "\"q1\"" Q1

  , example "Product I Maybe Int" "[1,2]"         (Pair (pure 1) (pure 2) :: Product I Maybe Int)
  , example "Product I Maybe Int" "[1,null]"      (Pair (pure 1) Nothing :: Product I Maybe Int)
  , example "Product I [] Char" "[\"a\",\"foo\"]" (Pair (pure 'a') "foo" :: Product I [] Char)

  , example "Sum I [] Int: InL"  "{\"InL\":1}"       (InL (pure 1) :: Sum I [] Int)
  , example "Sum I [] Int: InR"  "{\"InR\":[1,2]}"   (InR [1, 2] :: Sum I [] Int)
  , example "Sum I [] Char: InR" "{\"InR\":\"foo\"}" (InR "foo" :: Sum I [] Char)

  , example "Compose I  I  Int" "1"      (pure 1 :: Compose I I   Int)
  , example "Compose I  [] Int" "[1]"    (pure 1 :: Compose I []  Int)
  , example "Compose [] I  Int" "[1]"    (pure 1 :: Compose [] I  Int)
  , example "Compose [] [] Int" "[[1]]"  (pure 1 :: Compose [] [] Int)

  , example "Compose I  I  Char" "\"x\""    (pure 'x' :: Compose I  I  Char)
  , example "Compose I  [] Char" "\"x\""    (pure 'x' :: Compose I  [] Char)
  , example "Compose [] I  Char" "\"x\""    (pure 'x' :: Compose [] I  Char)
  , example "Compose [] [] Char" "[\"x\"]"  (pure 'x' :: Compose [] [] Char)

  , example "Compose3 I  I  I  Char" "\"x\""      (pure 'x' :: Compose3 I  I  I  Char)
  , example "Compose3 I  I  [] Char" "\"x\""      (pure 'x' :: Compose3 I  I  [] Char)
  , example "Compose3 I  [] I  Char" "\"x\""      (pure 'x' :: Compose3 I  [] I  Char)
  , example "Compose3 I  [] [] Char" "[\"x\"]"    (pure 'x' :: Compose3 I  [] [] Char)
  , example "Compose3 [] I  I  Char" "\"x\""      (pure 'x' :: Compose3 [] I  I  Char)
  , example "Compose3 [] I  [] Char" "[\"x\"]"    (pure 'x' :: Compose3 [] I  [] Char)
  , example "Compose3 [] [] I  Char" "[\"x\"]"    (pure 'x' :: Compose3 [] [] I  Char)
  , example "Compose3 [] [] [] Char" "[[\"x\"]]"  (pure 'x' :: Compose3 [] [] [] Char)

  , example "Compose3' I  I  I  Char" "\"x\""      (pure 'x' :: Compose3' I  I  I  Char)
  , example "Compose3' I  I  [] Char" "\"x\""      (pure 'x' :: Compose3' I  I  [] Char)
  , example "Compose3' I  [] I  Char" "\"x\""      (pure 'x' :: Compose3' I  [] I  Char)
  , example "Compose3' I  [] [] Char" "[\"x\"]"    (pure 'x' :: Compose3' I  [] [] Char)
  , example "Compose3' [] I  I  Char" "\"x\""      (pure 'x' :: Compose3' [] I  I  Char)
  , example "Compose3' [] I  [] Char" "[\"x\"]"    (pure 'x' :: Compose3' [] I  [] Char)
  , example "Compose3' [] [] I  Char" "[\"x\"]"    (pure 'x' :: Compose3' [] [] I  Char)
  , example "Compose3' [] [] [] Char" "[[\"x\"]]"  (pure 'x' :: Compose3' [] [] [] Char)

  , example "MyEither Int String: Left"  "42"      (MyLeft 42     :: MyEither Int String)
  , example "MyEither Int String: Right" "\"foo\"" (MyRight "foo" :: MyEither Int String)

  -- newtypes from Monoid/Semigroup
  , example "Monoid.Dual Int" "2" (pure 2 :: Monoid.Dual Int)
  , example "Monoid.First Int" "2" (pure 2 :: Monoid.First Int)
  , example "Monoid.Last Int" "2" (pure 2 :: Monoid.Last Int)
  , example "Semigroup.Min Int" "2" (pure 2 :: Semigroup.Min Int)
  , example "Semigroup.Max Int" "2" (pure 2 :: Semigroup.Max Int)
  , example "Semigroup.First Int" "2" (pure 2 :: Semigroup.First Int)
  , example "Semigroup.Last Int" "2" (pure 2 :: Semigroup.Last Int)
  , example "Semigroup.WrappedMonoid Int" "2" (Semigroup.WrapMonoid 2 :: Semigroup.WrappedMonoid Int)
  , example "Semigroup.Option Just" "2" (pure 2 :: Semigroup.Option Int)
  , example "Semigroup.Option Nothing" "null" (Semigroup.Option (Nothing :: Maybe Bool))

  -- time 1.9
  , example "SystemTime" "123.123456789" (MkSystemTime 123 123456789)
  , Example "SystemTime" ["124.23456789"]
    (MkSystemTime 123 1234567890)
    (MkSystemTime 124 234567890)
  , ndExample "CalendarDiffTime"
    [ "{\"months\":12,\"time\":456.789}", "{\"time\":456.789,\"months\":12}" ]
    (CalendarDiffTime 12 456.789)
  , ndExample "CalendarDiffDays"
    [ "{\"months\":12,\"days\":20}", "{\"days\":20,\"months\":12}" ]
    (CalendarDiffDays 12 20)
  , example "DayOfWeek" "\"monday\"" Monday

  -- these
  , example "These: This" "{\"This\":\"x\"}" (This 'x'   :: These Char Bool)
  , example "These: That" "{\"That\":true}"  (That True  :: These Char Bool)
  , ndExample "These"
    [ "{\"This\":\"y\",\"That\":false}"
    , "{\"That\":false,\"This\":\"y\"}"
    ]
    (These 'y' False)

  -- data-fix and strict
  , ndExample "Fix Strict.These"
    [ "{\"This\":true,\"That\":{\"That\":{\"This\":false}}}"
    , "{\"That\":{\"That\":{\"This\":false}},\"This\":true}"
    ]
    (F.Fix (S.These True (F.Fix (S.That (F.Fix (S.This False))))))

  -- Mu and Nu are similar.
  , ndExample "Mu Strict.These"
    [ "{\"This\":true,\"That\":{\"That\":{\"This\":false}}}"
    , "{\"That\":{\"That\":{\"This\":false}},\"This\":true}"
    ]
    $ F.unfoldMu F.unFix $ F.Fix (S.These True (F.Fix (S.That (F.Fix (S.This False)))))

  , ndExample "Nu Strict.These"
    [ "{\"This\":true,\"That\":{\"That\":{\"This\":false}}}"
    , "{\"That\":{\"That\":{\"This\":false}},\"This\":true}"
    ]
    $ F.unfoldNu F.unFix $ F.Fix (S.These True (F.Fix (S.That (F.Fix (S.This False)))))
  ]

jsonEncodingExamples :: [Example]
jsonEncodingExamples =
  [
  -- infinities cannot be recovered, null is decoded as NaN
    example "inf :: Double" "null" (Approx $ 1/0 :: Approx Double)
  , example "-inf :: Double" "null" (Approx $ -1/0 :: Approx Double)
  ]

jsonDecodingExamples :: [Example]
jsonDecodingExamples = [
  -- Maybe serialising is lossy
  -- https://github.com/bos/aeson/issues/376
    MaybeExample "Nothing"      "null" (Just Nothing :: Maybe (Maybe Int))
  , MaybeExample "Just"         "1"    (Just $ Just 1 :: Maybe (Maybe Int))
  , MaybeExample "Just Nothing" "null" (Just Nothing :: Maybe (Maybe (Maybe Int)))
  -- Integral values are truncated, and overflowed
  -- https://github.com/bos/aeson/issues/317
  , MaybeExample "Word8 3"    "3"    (Just 3 :: Maybe Word8)
  , MaybeExample "Word8 3.00" "3.00" (Just 3 :: Maybe Word8)
  , MaybeExample "Word8 3.14" "3.14" (Nothing :: Maybe Word8)
  , MaybeExample "Word8 -1"   "-1"   (Nothing :: Maybe Word8)
  , MaybeExample "Word8 300"  "300"  (Nothing :: Maybe Word8)
  -- Negative zero year, encoding never produces such:
  , MaybeExample "Day -0000-02-03" "\"-0000-02-03\"" (Just (fromGregorian 0 2 3))
  ]

data Example where
  Example
    :: (Eq a, Show a, ToJSON a, FromJSON a)
    => String          -- name
    -> [L.ByteString]  -- encoded variants
    -> a               -- input
    -> a               -- decoded
    -> Example         -- empty bytestring will fail, any p [] == False

  MaybeExample
    :: (Eq a, Show a, FromJSON a)
    => String -> L.ByteString -> Maybe a -> Example

example :: (Eq a, Show a, ToJSON a, FromJSON a)
        => String -> L.ByteString -> a -> Example
example n bs x = Example n [bs] x x

-- | Non-deterministic example, input encodes to some of bytestrings.
ndExample :: (Eq a, Show a, ToJSON a, FromJSON a)
          => String -> [L.ByteString] -> a -> Example
ndExample n bss x = Example n bss x x


data MyEither a b = MyLeft a | MyRight b
  deriving (Generic, Show, Eq)

instance (ToJSON a, ToJSON b) => ToJSON (MyEither a b) where
    toJSON = genericToJSON defaultOptions { sumEncoding = UntaggedValue }
    toEncoding = genericToEncoding defaultOptions { sumEncoding = UntaggedValue }

instance (FromJSON a, FromJSON b) => FromJSON (MyEither a b) where
    parseJSON = genericParseJSON defaultOptions { sumEncoding = UntaggedValue }

assertJsonExample :: Example -> TestTree
assertJsonExample (Example name bss val val') = testCase name $ do
    assertSomeEqual "encode"           bss        (encode val)
    assertSomeEqual "encode/via value" bss        (encode $ toJSON val)
    for_ bss $ \bs ->
        assertEqual "decode"           (Right val') (eitherDecode bs)

assertJsonExample (MaybeExample name bs mval) = testCase name $
    assertEqual "decode" mval (decode bs)

assertJsonEncodingExample :: Example -> TestTree
assertJsonEncodingExample (Example name bss val _) = testCase name $ do
    assertSomeEqual "encode"           bss (encode val)
    assertSomeEqual "encode/via value" bss (encode $ toJSON val)
assertJsonEncodingExample (MaybeExample name _ _) = testCase name $
    assertFailure "cannot encode MaybeExample"

assertSomeEqual :: (Eq a, Show a, Foldable f) => String -> f a -> a -> IO ()
assertSomeEqual preface expected actual
    | actual `elem` expected = return ()
    | otherwise = assertFailure $ preface
        ++ ": expecting one of " ++ show (toList expected)
        ++ ", got " ++ show actual

