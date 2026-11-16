{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Tests.Serialise.Canonical where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Data.Bits
import Data.Int
import Data.Typeable
import Data.Word
import Test.QuickCheck

import Codec.CBOR.Decoding
import Codec.CBOR.Encoding as E
import Codec.Serialise.Class

newtype Canonical a = Canonical { fromCanonical :: a }
  deriving (Typeable, Eq, Show)

-- | Generate "proper" big integers (as standard Arbitrary Integer instance
-- doesn't really do that) to test canonicity.
instance Arbitrary (Canonical Integer) where
  arbitrary = do
    c <- choose (1, 5)
    neg <- arbitrary
    Canonical . (if neg then negate else id) . foldr combine 0
      <$> vectorOf c arbitrary
    where
      combine :: Word64 -> Integer -> Integer
      combine v acc = (acc `shiftL` finiteBitSize v) + toInteger v

deriving instance Arbitrary (Canonical Word)
deriving instance Arbitrary (Canonical Word8)
deriving instance Arbitrary (Canonical Word16)
deriving instance Arbitrary (Canonical Word32)
deriving instance Arbitrary (Canonical Word64)
deriving instance Arbitrary (Canonical Int)
deriving instance Arbitrary (Canonical Int8)
deriving instance Arbitrary (Canonical Int16)
deriving instance Arbitrary (Canonical Int32)
deriving instance Arbitrary (Canonical Int64)
deriving instance Arbitrary (Canonical Float)
deriving instance Arbitrary (Canonical Double)

----------------------------------------

instance Serialise (Canonical Word) where
  encode = encodeWord . fromCanonical
  decode = Canonical <$> decodeWordCanonical

instance Serialise (Canonical Word8) where
  encode = encodeWord8 . fromCanonical
  decode = Canonical <$> decodeWord8Canonical

instance Serialise (Canonical Word16) where
  encode = encodeWord16 . fromCanonical
  decode = Canonical <$> decodeWord16Canonical

instance Serialise (Canonical Word32) where
  encode = encodeWord32 . fromCanonical
  decode = Canonical <$> decodeWord32Canonical

instance Serialise (Canonical Word64) where
  encode = encodeWord64 . fromCanonical
  decode = Canonical <$> decodeWord64Canonical

instance Serialise (Canonical Int) where
  encode = encodeInt . fromCanonical
  decode = Canonical <$> decodeIntCanonical

instance Serialise (Canonical Int8) where
  encode = encodeInt8 . fromCanonical
  decode = Canonical <$> decodeInt8Canonical

instance Serialise (Canonical Int16) where
  encode = encodeInt16 . fromCanonical
  decode = Canonical <$> decodeInt16Canonical

instance Serialise (Canonical Int32) where
  encode = encodeInt32 . fromCanonical
  decode = Canonical <$> decodeInt32Canonical

instance Serialise (Canonical Int64) where
  encode = encodeInt64 . fromCanonical
  decode = Canonical <$> decodeInt64Canonical

instance Serialise (Canonical Integer) where
  encode = encodeInteger . fromCanonical
  decode = Canonical <$> decodeIntegerCanonical

instance Serialise (Canonical Float) where
    encode = E.encodeFloat . fromCanonical
    decode = Canonical <$> decodeFloatCanonical

instance Serialise (Canonical Double) where
    encode = encodeDouble . fromCanonical
    decode = Canonical <$> decodeDoubleCanonical
