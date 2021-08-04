{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving, MagicHash,
    Rank2Types, UnboxedTuples #-}
{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}

-- | QuickCheck tests for the 'Data.Hashable' module.  We test
-- functions by comparing the C and Haskell implementations.

module Properties (properties) where

import Data.Hashable (Hashable, hash, hashByteArray, hashPtr,
         Hashed, hashed, unhashed, hashWithSalt)
import Data.Hashable.Generic (genericHashWithSalt)
import Data.Hashable.Lifted (hashWithSalt1)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.List (nub)
import Control.Monad (ap, liftM)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Marshal.Array (withArray)
import GHC.Base (ByteArray#, Int(..), newByteArray#, writeWord8Array#)
import GHC.Exts (unsafeCoerce#)
import GHC.ST (ST(..), runST)
import GHC.Word (Word8(..))
import Test.QuickCheck hiding ((.&.))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import GHC.Generics

#if MIN_VERSION_bytestring(0,10,4)
import qualified Data.ByteString.Short as BS
#endif

------------------------------------------------------------------------
-- * Properties

instance Arbitrary T.Text where
    arbitrary = T.pack `fmap` arbitrary

instance Arbitrary TL.Text where
    arbitrary = TL.pack `fmap` arbitrary

instance Arbitrary B.ByteString where
    arbitrary   = B.pack `fmap` arbitrary

instance Arbitrary BL.ByteString where
    arbitrary   = sized $ \n -> resize (round (sqrt (toEnum n :: Double)))
                  ((BL.fromChunks . map (B.pack . nonEmpty)) `fmap` arbitrary)
      where nonEmpty (NonEmpty a) = a

#if MIN_VERSION_bytestring(0,10,4)
instance Arbitrary BS.ShortByteString where
    arbitrary   = BS.pack `fmap` arbitrary
#endif

-- | Validate the implementation by comparing the C and Haskell
-- versions.
pHash :: [Word8] -> Bool
pHash xs = unsafePerformIO $ withArray xs $ \ p ->
    (hashByteArray (fromList xs) 0 len ==) `fmap` hashPtr p len
  where len = length xs

-- | Content equality implies hash equality.
pText :: T.Text -> T.Text -> Bool
pText a b = if (a == b) then (hash a == hash b) else True

-- | Content equality implies hash equality.
pTextLazy :: TL.Text -> TL.Text -> Bool
pTextLazy a b = if (a == b) then (hash a == hash b) else True

-- | A small positive integer.
newtype ChunkSize = ChunkSize { unCS :: Int }
    deriving (Eq, Ord, Num, Integral, Real, Enum)

instance Show ChunkSize where show = show . unCS

instance Arbitrary ChunkSize where
    arbitrary = (ChunkSize . (`mod` maxChunkSize)) `fmap`
                (arbitrary `suchThat` ((/=0) . (`mod` maxChunkSize)))
        where maxChunkSize = 16

-- | Ensure that the rechunk function causes a rechunked string to
-- still match its original form.
pTextRechunk :: T.Text -> NonEmptyList ChunkSize -> Bool
pTextRechunk t cs = TL.fromStrict t == rechunkText t cs

-- | Lazy strings must hash to the same value no matter how they are
-- chunked.
pTextLazyRechunked :: T.Text
                   -> NonEmptyList ChunkSize -> NonEmptyList ChunkSize -> Bool
pTextLazyRechunked t cs0 cs1 =
    hash (rechunkText t cs0) == hash (rechunkText t cs1)

-- | Break up a string into chunks of different sizes.
rechunkText :: T.Text -> NonEmptyList ChunkSize -> TL.Text
rechunkText t0 (NonEmpty cs0) = TL.fromChunks . go t0 . cycle $ cs0
  where
    go t _ | T.null t = []
    go t (c:cs)       = a : go b cs
      where (a,b)     = T.splitAt (unCS c) t
    go _ []           = error "Properties.rechunk - The 'impossible' happened!"

#if MIN_VERSION_bytestring(0,10,4)
-- | Content equality implies hash equality.
pBSShort :: BS.ShortByteString -> BS.ShortByteString -> Bool
pBSShort a b = if (a == b) then (hash a == hash b) else True
#endif

-- | Content equality implies hash equality.
pBS :: B.ByteString -> B.ByteString -> Bool
pBS a b = if (a == b) then (hash a == hash b) else True

-- | Content equality implies hash equality.
pBSLazy :: BL.ByteString -> BL.ByteString -> Bool
pBSLazy a b = if (a == b) then (hash a == hash b) else True

-- | Break up a string into chunks of different sizes.
rechunkBS :: B.ByteString -> NonEmptyList ChunkSize -> BL.ByteString
rechunkBS t0 (NonEmpty cs0) = BL.fromChunks . go t0 . cycle $ cs0
  where
    go t _ | B.null t = []
    go t (c:cs)       = a : go b cs
      where (a,b)     = B.splitAt (unCS c) t
    go _ []           = error "Properties.rechunkBS - The 'impossible' happened!"

-- | Ensure that the rechunk function causes a rechunked string to
-- still match its original form.
pBSRechunk :: B.ByteString -> NonEmptyList ChunkSize -> Bool
pBSRechunk t cs = fromStrict t == rechunkBS t cs

-- | Lazy bytestrings must hash to the same value no matter how they
-- are chunked.
pBSLazyRechunked :: B.ByteString
                 -> NonEmptyList ChunkSize -> NonEmptyList ChunkSize -> Bool
pBSLazyRechunked t cs1 cs2 = hash (rechunkBS t cs1) == hash (rechunkBS t cs2)

-- This wrapper is required by 'runST'.
data ByteArray = BA { unBA :: ByteArray# }

-- | Create a 'ByteArray#' from a list of 'Word8' values.
fromList :: [Word8] -> ByteArray#
fromList xs0 = unBA (runST $ ST $ \ s1# ->
    case newByteArray# len# s1# of
        (# s2#, marr# #) -> case go s2# 0 marr# xs0 of
            s3# -> (# s3#, BA (unsafeCoerce# marr#) #))
  where
    !(I# len#) = length xs0
    go s# _         _     []           = s#
    go s# i@(I# i#) marr# ((W8# x):xs) =
        case writeWord8Array# marr# i# x s# of
            s2# -> go s2# (i + 1) marr# xs

-- Generics

data Product2 a b = Product2 a b
                    deriving (Generic)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Product2 a b) where
    arbitrary = Product2 `liftM` arbitrary `ap` arbitrary

instance (Hashable a, Hashable b) => Hashable (Product2 a b)

data Product3 a b c = Product3 a b c
                    deriving (Generic)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Product3 a b c) where
    arbitrary = Product3 `liftM` arbitrary `ap` arbitrary `ap` arbitrary

instance (Hashable a, Hashable b, Hashable c) => Hashable (Product3 a b c)

-- Hashes of all product types of the same shapes should be the same.

pProduct2 :: Int -> String -> Bool
pProduct2 x y = hash (x, y) == hash (Product2 x y)

pProduct3 :: Double -> Maybe Bool -> (Int, String) -> Bool
pProduct3 x y z = hash (x, y, z) == hash (Product3 x y z)

data Sum2 a b = S2a a | S2b b
                deriving (Eq, Ord, Show, Generic)

instance (Hashable a, Hashable b) => Hashable (Sum2 a b)

data Sum3 a b c = S3a a | S3b b | S3c c
                  deriving (Eq, Ord, Show, Generic)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Sum3 a b c) where
  arbitrary = oneof
    [ fmap S3a arbitrary
    , fmap S3b arbitrary
    , fmap S3c arbitrary
    ]

instance (Hashable a, Hashable b, Hashable c) => Hashable (Sum3 a b c)

-- Hashes of the same parameter, but with different sum constructors,
-- should differ. (They might legitimately collide, but that's
-- vanishingly unlikely.)

pSum2_differ :: Int -> Bool
pSum2_differ x = nub hs == hs
  where hs = [ hash (S2a x :: Sum2 Int Int)
             , hash (S2b x :: Sum2 Int Int) ]

pSum3_differ :: Int -> Bool
pSum3_differ x = nub hs == hs
  where hs = [ hash (S3a x :: Sum3 Int Int Int)
             , hash (S3b x :: Sum3 Int Int Int)
             , hash (S3c x :: Sum3 Int Int Int) ]

pGeneric :: Sum3 Int Bool String -> Int -> Bool
pGeneric x salt = hashWithSalt salt x == genericHashWithSalt salt x

instance (Arbitrary a, Hashable a) => Arbitrary (Hashed a) where
  arbitrary = fmap hashed arbitrary
  shrink xs = map hashed $ shrink $ unhashed xs

pLiftedHashed :: Int -> Hashed (Either Int String) -> Bool
pLiftedHashed s h = hashWithSalt s h == hashWithSalt1 s h

properties :: [Test]
properties =
    [ testProperty "bernstein" pHash
    , testGroup "text"
      [ testProperty "text/strict" pText
      , testProperty "text/lazy" pTextLazy
      , testProperty "text/rechunk" pTextRechunk
      , testProperty "text/rechunked" pTextLazyRechunked
      ]
    , testGroup "bytestring"
      [ testProperty "bytestring/strict" pBS
      , testProperty "bytestring/lazy" pBSLazy
#if MIN_VERSION_bytestring(0,10,4)
      , testProperty "bytestring/short" pBSShort
#endif
      , testProperty "bytestring/rechunk" pBSRechunk
      , testProperty "bytestring/rechunked" pBSLazyRechunked
      ]
    , testGroup "generics"
      [
      -- Note: "product2" and "product3" have been temporarily
      -- disabled until we have added a 'hash' method to the GHashable
      -- class. Until then (a,b) hashes to a different value than (a
      -- :*: b). While this is not incorrect, it would be nicer if
      -- they didn't. testProperty "product2" pProduct2 , testProperty
      -- "product3" pProduct3
        testProperty "sum2_differ" pSum2_differ
      , testProperty "sum3_differ" pSum3_differ
      , testProperty "genericHashWithSalt" pGeneric
      ]
    , testGroup "lifted law"
      [ testProperty "Hashed" pLiftedHashed
      ]
    ]

------------------------------------------------------------------------
-- Utilities

fromStrict :: B.ByteString -> BL.ByteString
#if MIN_VERSION_bytestring(0,10,0)
fromStrict = BL.fromStrict
#else
fromStrict b = BL.fromChunks [b]
#endif
