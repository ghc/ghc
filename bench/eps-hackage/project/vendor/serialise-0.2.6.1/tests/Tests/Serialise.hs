{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tests.Serialise
  ( testTree     -- :: TestTree
  , testGenerics -- :: TestTree
  ) where

#if MIN_VERSION_base(4,8,0)
import           Data.Functor.Identity
import           Numeric.Natural
#endif

#if MIN_VERSION_base(4,9,0)
import           Data.List.NonEmpty ( NonEmpty )
import qualified Data.Semigroup as Semigroup
#endif

#if MIN_VERSION_base(4,10,0)
import qualified Type.Reflection                as Refl
#endif

import           Data.Char (ord)
import           Data.Complex
import           Data.Int
import           Data.Fixed
import           Data.Monoid as Monoid
import           Data.Ord
import           Data.Ratio
import           Data.Time
import           Data.Word
import           GHC.Exts (IsList(..))
import           GHC.Float (float2Double)
import           Data.Version

import           Data.Typeable
import           Control.Applicative
import           Foreign.C.Types

import           Test.QuickCheck  hiding (Fixed(..))
import           Test.Tasty
import           Test.Tasty.QuickCheck hiding (Fixed(..))
import           Test.QuickCheck.Instances ()
import           Test.Tasty.HUnit
import           GHC.Generics  (Generic)
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Text
import qualified Data.Text.Lazy                 as Text.Lazy
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BS.Lazy
import qualified Data.ByteString.Short          as BSS
import qualified Data.ByteString.Short.Internal as BSS
import           System.Exit (ExitCode(..))

import qualified Codec.CBOR.ByteArray           as CBOR.BA
import           Codec.CBOR.FlatTerm (toFlatTerm, fromFlatTerm)
import           Codec.Serialise
import           Codec.Serialise.Encoding
import           Codec.Serialise.Decoding
import qualified Codec.Serialise.Properties     as Props

import qualified Data.HashMap.Strict        as HashMap
import qualified Data.HashSet               as HashSet
import qualified Data.IntMap                as IntMap
import qualified Data.IntSet                as IntSet
import qualified Data.Map                   as Map
import qualified Data.Sequence              as Sequence
import qualified Data.Set                   as Set
import qualified Data.Tree                  as Tree
import qualified Data.Vector                as Vector
import qualified Data.Vector.Unboxed        as Vector.Unboxed
import qualified Data.Vector.Storable       as Vector.Storable
import qualified Data.Vector.Primitive      as Vector.Primitive
import           GHC.Fingerprint.Type (Fingerprint(..))
import           Data.Primitive.ByteArray   as Prim

import           Tests.Orphanage()
import           Tests.Serialise.Canonical

--------------------------------------------------------------------------------
-- Tests and properties

-- | Simple proxy type, used as a witness.
data T a = T

-- | Ensure that serializing and deserializing a term results in the original
-- term.
prop_serialiseId :: (Serialise a, Eq a) => T a -> a -> Bool
prop_serialiseId _ = Props.serialiseIdentity

-- | Ensure that serializing and deserializing a term (into @'FlatTerm'@ form)
-- results in the original term.
prop_flatTermId :: (Serialise a, Eq a) => T a -> a -> Bool
prop_flatTermId _ = Props.flatTermIdentity

-- | Ensure that serializing a term into a @'FlatTerm'@ always gives us a
-- valid @'FlatTerm'@ back.
prop_validFlatTerm :: Serialise a => T a -> a -> Bool
prop_validFlatTerm _ = Props.hasValidFlatTerm

format :: (Typeable a, Show a, Serialise a) => a -> Tokens -> TestTree
format a toks
  = testCase (show (typeOf a) ++ ": " ++ show a)
  $ toks @=? toks'
  where
    Encoding f = encode a
    toks'      = f TkEnd

--------------------------------------------------------------------------------
-- Corner case or specific properties to test

-- | Ensure that when we encode a Float but decode as a Double, we get the same
-- value.
prop_encodeFloatToDouble :: Float -> Bool
prop_encodeFloatToDouble x = Right dbl == fromFlatTerm dec ft
  where
    dbl = float2Double x

    dec = decode :: Decoder s Double
    ft  = toFlatTerm (encode x)

--------------------------------------------------------------------------------
-- Ensure we can decode UTCTimes when using tag 1 (offset from epoch)

prop_decodeTag1UTCTimeInteger :: TestTree
prop_decodeTag1UTCTimeInteger = testCase "Decode tag 1 UTCTime (Integer)" $
  Right mar21 @=? fromFlatTerm dec (toFlatTerm (Encoding toks))
  where
    toks e = TkTag 1 $ TkInteger 1363896240 $ e
    mar21 = UTCTime (fromGregorian 2013 3 21) (timeOfDayToTime (TimeOfDay 20 4 0))
    dec = decode :: Decoder s UTCTime

prop_decodeTag1UTCTimeDouble :: TestTree
prop_decodeTag1UTCTimeDouble = testCase "Decode tag 1 UTCTime (Double)" $
  Right mar21 @=? fromFlatTerm dec (toFlatTerm (Encoding toks))
  where
    toks e = TkTag 1 $ TkFloat64 1363896240.5 $ e
    mar21 = UTCTime (fromGregorian 2013 3 21) (timeOfDayToTime (TimeOfDay 20 4 0.5))
    dec = decode :: Decoder s UTCTime

--------------------------------------------------------------------------------
-- TestTree API

testTree :: TestTree
testTree = testGroup "Serialise class"
  [ testGroup "Corner cases"
      [ testProperty "decode float to double"      prop_encodeFloatToDouble
      , prop_decodeTag1UTCTimeInteger
      , prop_decodeTag1UTCTimeDouble
      ]
  , testGroup "Simple instance invariants"
      [ mkTest (T :: T ())
      , mkTest (T :: T Bool)
      , mkTest (T :: T Int)
      , mkTest (T :: T Int8)
      , mkTest (T :: T Int16)
      , mkTest (T :: T Int32)
      , mkTest (T :: T Int64)
      , mkTest (T :: T Word)
      , mkTest (T :: T Word8)
      , mkTest (T :: T Word16)
      , mkTest (T :: T Word32)
      , mkTest (T :: T Word64)
      , mkTest (T :: T (Canonical Int))
      , mkTest (T :: T (Canonical Int8))
      , mkTest (T :: T (Canonical Int16))
      , mkTest (T :: T (Canonical Int32))
      , mkTest (T :: T (Canonical Int64))
      , mkTest (T :: T (Canonical Word))
      , mkTest (T :: T (Canonical Word8))
      , mkTest (T :: T (Canonical Word16))
      , mkTest (T :: T (Canonical Word32))
      , mkTest (T :: T (Canonical Word64))
      , mkTest (T :: T Integer)
      , mkTest (T :: T Float)
      , mkTest (T :: T Double)
      , mkTest (T :: T (Canonical Integer))
      , mkTest (T :: T (Canonical Float))
      , mkTest (T :: T (Canonical Double))
      , mkTest (T :: T [()])
#if MIN_VERSION_base(4,10,0)
      , mkTest (T :: T (Refl.SomeTypeRep))
#endif
#if MIN_VERSION_base(4,9,0)
      , mkTest (T :: T (NonEmpty ()))
      , mkTest (T :: T (Semigroup.Min ()))
      , mkTest (T :: T (Semigroup.Max ()))
      , mkTest (T :: T (Semigroup.First ()))
      , mkTest (T :: T (Semigroup.Last ()))
#if !MIN_VERSION_base(4,16,0)
      , mkTest (T :: T (Semigroup.Option ()))
#endif
      , mkTest (T :: T (Semigroup.WrappedMonoid ()))
#endif
#if MIN_VERSION_base(4,7,0)
      , mkTest (T :: T (Fixed E0))
      , mkTest (T :: T (Fixed E1))
      , mkTest (T :: T (Fixed E2))
      , mkTest (T :: T (Fixed E3))
      , mkTest (T :: T (Fixed E6))
      , mkTest (T :: T (Fixed E9))
      , mkTest (T :: T (Fixed E12))
      , mkTest (T :: T (Proxy ()))
#endif
      , mkTest (T :: T Char)
      , mkTest (T :: T CChar)
      , mkTest (T :: T CSChar)
      , mkTest (T :: T CUChar)
      , mkTest (T :: T CShort)
      , mkTest (T :: T CUShort)
      , mkTest (T :: T CInt)
      , mkTest (T :: T CUInt)
      , mkTest (T :: T CLong)
      , mkTest (T :: T CULong)
      , mkTest (T :: T CPtrdiff)
      , mkTest (T :: T CSize)
      , mkTest (T :: T CWchar)
      , mkTest (T :: T CSigAtomic)
      , mkTest (T :: T CLLong)
      , mkTest (T :: T CULLong)
      , mkTest (T :: T CIntPtr)
      , mkTest (T :: T CUIntPtr)
      , mkTest (T :: T CIntMax)
      , mkTest (T :: T CUIntMax)
      , mkTest (T :: T CClock)
      , mkTest (T :: T CTime)
      , mkTest (T :: T CUSeconds_)
      , mkTest (T :: T CSUSeconds)
      , mkTest (T :: T CFloat)
      , mkTest (T :: T CDouble)
      , mkTest (T :: T (Int, Char))
      , mkTest (T :: T (Int, Char, Bool))
      , mkTest (T :: T (Int, Char, Bool, String))
      , mkTest (T :: T (Int, Char, Bool, String, ()))
      , mkTest (T :: T (Int, Char, Bool, String, (), Maybe Char))
      , mkTest (T :: T (Int, Char, Bool, String, (), Maybe Char, Maybe ()))
      , mkTest (T :: T (Maybe Int))
      , mkTest (T :: T (Either String Int))
      , mkTest (T :: T String)
      , mkTest (T :: T Text.Text)
      , mkTest (T :: T Text.Lazy.Text)
      , mkTest (T :: T BS.ByteString)
      , mkTest (T :: T BS.Lazy.ByteString)
      , mkTest (T :: T BSS.ShortByteString)
      , mkTest (T :: T BytesByteArray)
      , mkTest (T :: T Utf8ByteArray)
      , mkTest (T :: T [Int])
      , mkTest (T :: T UTCTime)
      , mkTest (T :: T Version)
      , mkTest (T :: T Fingerprint)
      , mkTest (T :: T ExitCode)
      , mkTest (T :: T (Ratio Integer))
      , mkTest (T :: T (Complex Double))
      , mkTest (T :: T (Const Int ()))
      , mkTest (T :: T (ZipList Int))
      , mkTest (T :: T (ZipList Char))
      , mkTest (T :: T Ordering)
      , mkTest (T :: T (Down Int64))
      , mkTest (T :: T (Dual (Maybe (Sum Int))))
      , mkTest (T :: T All)
      , mkTest (T :: T Any)
#if MIN_VERSION_base(4,8,0)
      , mkTest (T :: T (Alt Maybe Int))
      , mkTest (T :: T (Identity ()))
      , mkTest (T :: T Natural)
#endif
      , mkTest (T :: T (Sum Int))
      , mkTest (T :: T (Product Int))
      , mkTest (T :: T (Map.Map Int String))
      , mkTest (T :: T (Sequence.Seq Int))
      , mkTest (T :: T (Set.Set Int))
      , mkTest (T :: T IntSet.IntSet)
      , mkTest (T :: T (IntMap.IntMap String))
      , mkTest (T :: T (HashMap.HashMap Int String))
      , mkTest (T :: T (HashSet.HashSet Int))
      , mkTest (T :: T (Tree.Tree (Int, String, ())))
      , mkTest (T :: T (Vector.Vector Int))
      , mkTest (T :: T (Vector.Unboxed.Vector (Int,Bool)))
      , mkTest (T :: T (Vector.Storable.Vector Int))
      , mkTest (T :: T (Vector.Primitive.Vector Int))
      -- generics:
      , mkTest (T :: T Unit)
      , mkTest (T :: T P1)
      , mkTest (T :: T P2)
      , mkTest (T :: T P3)
      , mkTest (T :: T C4)
      , mkTest (T :: T (List Int))
      ]
  ]

testGenerics :: TestTree
testGenerics = testGroup "Format of Generics encoding"
  [ format
      Unit
      (TkListLen 1 $ TkWord 0 $ TkEnd)
  , format
      (P1 12)
      (TkListLen 2 $ TkWord 0 $ TkInt 12 $ TkEnd)
  , format
      (N1 12)
      (TkListLen 2 $ TkWord 0 $ TkInt 12 $ TkEnd)
  , format
      (P2 12 0.5)
      (TkListLen 3 $ TkWord 0 $ TkInt 12 $ TkFloat32 0.5 $ TkEnd)
  , format
      (P3 12 0.5 "asdf")
      (TkListLen 4 $ TkWord 0 $ TkInt 12 $ TkFloat32 0.5 $ tkStr "asdf" $ TkEnd)
  , format
      (C1 12)
      (TkListLen 2 $ TkWord 0 $ TkInt 12 $ TkEnd)
  , format
      (C2 12 0.5)
      (TkListLen 3 $ TkWord 1 $ TkInt 12 $ TkFloat32 0.5 $ TkEnd)
  , format
      (C3 12 0.5 "asdf")
      (TkListLen 4 $ TkWord 2 $ TkInt 12 $ TkFloat32 0.5 $ tkStr "asdf" $ TkEnd)
  , format
       C4
      (TkListLen 1 $ TkWord 3 $ TkEnd)
  , format
      (Cons "foo" (Cons "bar" Nil) :: List String)
      (TkListLen 3 $ TkWord 0 $ tkStr "foo" $
       TkListLen 3 $ TkWord 0 $ tkStr "bar" $
       TkListLen 1 $ TkWord 1 $
       TkEnd)
  ]

  where tkStr = TkUtf8ByteArray . fromList . map (fromIntegral . ord)

--------------------------------------------------------------------------------
-- Extra machinery

-- A simple alias to make the following properties more convenient to write
type BasicType a = (Arbitrary a, Typeable a, Serialise a, Eq a, Show a)

mkTest :: forall a. BasicType a => T a -> TestTree
mkTest t = testGroup ("type: " ++ show (typeOf (undefined :: a)))
  [ testProperty "cbor roundtrip"      (prop_serialiseId   t)
  , testProperty "flat roundtrip"      (prop_flatTermId    t)
  , testProperty "flat term is valid"  (prop_validFlatTerm t)
  ]

--------------------------------------------------------------------------------
-- Various data types

-- Wrapper for CUSeconds with Arbitrary instance that works on x86_32.
newtype CUSeconds_ = CUSeconds_ CUSeconds
  deriving (Eq, Show, Typeable)

instance Arbitrary CUSeconds_ where
  arbitrary = CUSeconds_ . CUSeconds <$> arbitraryBoundedIntegral

instance Serialise CUSeconds_ where
  encode (CUSeconds_ s) = encode s
  decode = CUSeconds_ <$> decode

--------------------------------------------------------------------------------
-- Generic data types

data Unit = Unit
          deriving (Show,Eq,Typeable,Generic)
data P1 = P1 Int
          deriving (Show,Eq,Typeable,Generic)
newtype N1 = N1 Int
          deriving (Show,Eq,Typeable,Generic)
data P2 = P2 Int Float
          deriving (Show,Eq,Typeable,Generic)
data P3 = P3 Int Float String
          deriving (Show,Eq,Typeable,Generic)

data C4 = C1 Int
        | C2 Int Float
        | C3 Int Float String
        | C4
        deriving (Show,Eq,Typeable,Generic)

data List a = Cons a (List a)
            | Nil
            deriving (Show,Eq,Typeable,Generic)

instance Serialise Unit
instance Serialise P1
instance Serialise N1
instance Serialise P2
instance Serialise P3
instance Serialise C4
instance Serialise a => Serialise (List a)


instance Arbitrary Unit where
    arbitrary = pure Unit

instance Arbitrary P1 where
    arbitrary = P1 <$> arbitrary
instance Arbitrary N1 where
    arbitrary = N1 <$> arbitrary
instance Arbitrary P2 where
    arbitrary = P2 <$> arbitrary <*> arbitrary
instance Arbitrary P3 where
    arbitrary = P3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary C4 where
    arbitrary = oneof [ C1 <$> arbitrary
                      , C2 <$> arbitrary <*> arbitrary
                      , C3 <$> arbitrary <*> arbitrary <*> arbitrary
                      , pure C4
                      ]

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = cnv <$> arbitrary
      where
        cnv :: [a] -> List a
        cnv = foldr Cons Nil

newtype BytesByteArray = BytesBA CBOR.BA.ByteArray
                       deriving (Eq, Ord, Show, Typeable)

instance Serialise BytesByteArray where
    encode (BytesBA ba) = encodeByteArray $ CBOR.BA.toSliced ba
    decode = BytesBA <$> decodeByteArray

instance Arbitrary BytesByteArray where
    arbitrary = BytesBA . fromList <$> arbitrary

newtype Utf8ByteArray = Utf8BA CBOR.BA.ByteArray
                      deriving (Eq, Ord, Show, Typeable)

instance Serialise Utf8ByteArray where
    encode (Utf8BA ba) = encodeUtf8ByteArray $ CBOR.BA.toSliced ba
    decode = Utf8BA <$> decodeUtf8ByteArray

instance Arbitrary Utf8ByteArray where
    arbitrary = do
        bss <- BSS.toShort . Text.encodeUtf8 <$> arbitrary
        case bss of
          BSS.SBS ba -> return $ Utf8BA $ CBOR.BA.BA $ Prim.ByteArray ba
