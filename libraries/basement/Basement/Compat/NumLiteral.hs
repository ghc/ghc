{-# Language CPP #-}
-- |
-- Module      : Basement.Compat.NumLiteral
-- License     : BSD-style
-- Maintainer  : Foundation
--
-- Literal support for Integral and Fractional
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Basement.Compat.NumLiteral
    ( Integral(..)
    , Fractional(..)
    , HasNegation(..)
    ) where

import           Prelude (Int, Integer, Rational, Float, Double)
import           Data.Word (Word8, Word16, Word32, Word64, Word)
import           Data.Int (Int8, Int16, Int32, Int64)
import           Basement.Compat.C.Types
import qualified Prelude
import           Basement.Compat.Natural
import           Foreign.Ptr (IntPtr)

-- | Integral Literal support
--
-- e.g. 123 :: Integer
--      123 :: Word8
class Integral a where
    fromInteger :: Integer -> a

-- | Fractional Literal support
--
-- e.g. 1.2  :: Double
--      0.03 :: Float
class Fractional a where
    fromRational :: Rational -> a

-- | Negation support
--
-- e.g. -(f x)
class HasNegation a where
    negate :: a -> a

instance Integral Integer where
    fromInteger a = a
instance Integral Natural where
    fromInteger a = Prelude.fromInteger a
instance Integral Int where
    fromInteger a = Prelude.fromInteger a
instance Integral Word where
    fromInteger a = Prelude.fromInteger a
instance Integral Word8 where
    fromInteger a = Prelude.fromInteger a
instance Integral Word16 where
    fromInteger a = Prelude.fromInteger a
instance Integral Word32 where
    fromInteger a = Prelude.fromInteger a
instance Integral Word64 where
    fromInteger a = Prelude.fromInteger a
instance Integral Int8 where
    fromInteger a = Prelude.fromInteger a
instance Integral Int16 where
    fromInteger a = Prelude.fromInteger a
instance Integral Int32 where
    fromInteger a = Prelude.fromInteger a
instance Integral Int64 where
    fromInteger a = Prelude.fromInteger a
instance Integral IntPtr where
    fromInteger a = Prelude.fromInteger a

instance Integral Float where
    fromInteger a = Prelude.fromInteger a
instance Integral Double where
    fromInteger a = Prelude.fromInteger a

instance Integral CChar where
    fromInteger a = Prelude.fromInteger a
instance Integral CSChar where
    fromInteger a = Prelude.fromInteger a
instance Integral CUChar where
    fromInteger a = Prelude.fromInteger a
instance Integral CShort where
    fromInteger a = Prelude.fromInteger a
instance Integral CUShort where
    fromInteger a = Prelude.fromInteger a
instance Integral CInt where
    fromInteger a = Prelude.fromInteger a
instance Integral CUInt where
    fromInteger a = Prelude.fromInteger a
instance Integral CLong where
    fromInteger a = Prelude.fromInteger a
instance Integral CULong where
    fromInteger a = Prelude.fromInteger a
instance Integral CPtrdiff where
    fromInteger a = Prelude.fromInteger a
instance Integral CSize where
    fromInteger a = Prelude.fromInteger a
instance Integral CWchar where
    fromInteger a = Prelude.fromInteger a
instance Integral CSigAtomic where
    fromInteger a = Prelude.fromInteger a
instance Integral CLLong where
    fromInteger a = Prelude.fromInteger a
instance Integral CULLong where
    fromInteger a = Prelude.fromInteger a
#if MIN_VERSION_base(4, 10, 0)
instance Integral CBool where
    fromInteger a = Prelude.fromInteger a
#endif
instance Integral CIntPtr where
    fromInteger a = Prelude.fromInteger a
instance Integral CUIntPtr where
    fromInteger a = Prelude.fromInteger a
instance Integral CIntMax where
    fromInteger a = Prelude.fromInteger a
instance Integral CUIntMax where
    fromInteger a = Prelude.fromInteger a
instance Integral CClock where
    fromInteger a = Prelude.fromInteger a
instance Integral CTime where
    fromInteger a = Prelude.fromInteger a
instance Integral CUSeconds where
    fromInteger a = Prelude.fromInteger a
instance Integral CSUSeconds where
    fromInteger a = Prelude.fromInteger a
instance Integral COff where
    fromInteger a = Prelude.fromInteger a
instance Integral CFloat where
    fromInteger a = Prelude.fromInteger a
instance Integral CDouble where
    fromInteger a = Prelude.fromInteger a

instance HasNegation Integer where
    negate = Prelude.negate
instance HasNegation Int where
    negate = Prelude.negate
instance HasNegation Int8 where
    negate = Prelude.negate
instance HasNegation Int16 where
    negate = Prelude.negate
instance HasNegation Int32 where
    negate = Prelude.negate
instance HasNegation Int64 where
    negate = Prelude.negate
instance HasNegation Word where
    negate = Prelude.negate
instance HasNegation Word8 where
    negate = Prelude.negate
instance HasNegation Word16 where
    negate = Prelude.negate
instance HasNegation Word32 where
    negate = Prelude.negate
instance HasNegation Word64 where
    negate = Prelude.negate

instance HasNegation Float where
    negate = Prelude.negate
instance HasNegation Double where
    negate = Prelude.negate

instance HasNegation CChar where
    negate = Prelude.negate
instance HasNegation CSChar where
    negate = Prelude.negate
instance HasNegation CShort where
    negate = Prelude.negate
instance HasNegation CInt where
    negate = Prelude.negate
instance HasNegation CLong where
    negate = Prelude.negate
instance HasNegation CPtrdiff where
    negate = Prelude.negate
instance HasNegation CWchar where
    negate = Prelude.negate
instance HasNegation CLLong where
    negate = Prelude.negate
instance HasNegation CIntMax where
    negate = Prelude.negate

instance HasNegation CFloat where
    negate = Prelude.negate
instance HasNegation CDouble where
    negate = Prelude.negate

instance Fractional Rational where
    fromRational a = Prelude.fromRational a
instance Fractional Float where
    fromRational a = Prelude.fromRational a
instance Fractional Double where
    fromRational a = Prelude.fromRational a

instance Fractional CFloat where
    fromRational a = Prelude.fromRational a
instance Fractional CDouble where
    fromRational a = Prelude.fromRational a
