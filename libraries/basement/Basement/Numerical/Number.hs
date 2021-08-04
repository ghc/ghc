{-# Language CPP #-}
module Basement.Numerical.Number
    ( IsIntegral(..)
    , IsNatural(..)
    ) where

import           Basement.Compat.Base
import           Basement.Compat.C.Types
import           Basement.Compat.Natural
import           Basement.Compat.NumLiteral
import           Data.Bits
import qualified Prelude

-- | Number literals, convertible through the generic Integer type.
--
-- all number are Enum'erable, meaning that you can move to
-- next element
class (Integral a, Eq a, Ord a) => IsIntegral a where
    {-# MINIMAL toInteger #-}
    toInteger :: a -> Integer

-- | Non Negative Number literals, convertible through the generic Natural type
class IsIntegral a => IsNatural a where
    {-# MINIMAL toNatural #-}
    toNatural :: a -> Natural

instance IsIntegral Integer where
    toInteger i = i
instance IsIntegral Int where
    toInteger i = Prelude.toInteger i
instance IsIntegral Int8 where
    toInteger i = Prelude.toInteger i
instance IsIntegral Int16 where
    toInteger i = Prelude.toInteger i
instance IsIntegral Int32 where
    toInteger i = Prelude.toInteger i
instance IsIntegral Int64 where
    toInteger i = Prelude.toInteger i
instance IsIntegral Natural where
    toInteger i = Prelude.toInteger i
instance IsIntegral Word where
    toInteger i = Prelude.toInteger i
instance IsIntegral Word8 where
    toInteger i = Prelude.toInteger i
instance IsIntegral Word16 where
    toInteger i = Prelude.toInteger i
instance IsIntegral Word32 where
    toInteger i = Prelude.toInteger i
instance IsIntegral Word64 where
    toInteger i = Prelude.toInteger i

instance IsIntegral CChar where
    toInteger i = Prelude.toInteger i
instance IsIntegral CSChar where
    toInteger i = Prelude.toInteger i
instance IsIntegral CUChar where
    toInteger i = Prelude.toInteger i
instance IsIntegral CShort where
    toInteger i = Prelude.toInteger i
instance IsIntegral CUShort where
    toInteger i = Prelude.toInteger i
instance IsIntegral CInt where
    toInteger i = Prelude.toInteger i
instance IsIntegral CUInt where
    toInteger i = Prelude.toInteger i
instance IsIntegral CLong where
    toInteger i = Prelude.toInteger i
instance IsIntegral CULong where
    toInteger i = Prelude.toInteger i
instance IsIntegral CPtrdiff where
    toInteger i = Prelude.toInteger i
instance IsIntegral CSize where
    toInteger i = Prelude.toInteger i
instance IsIntegral CWchar where
    toInteger i = Prelude.toInteger i
instance IsIntegral CSigAtomic where
    toInteger i = Prelude.toInteger i
instance IsIntegral CLLong where
    toInteger i = Prelude.toInteger i
instance IsIntegral CULLong where
    toInteger i = Prelude.toInteger i
#if MIN_VERSION_base(4,10,0)
instance IsIntegral CBool where
    toInteger i = Prelude.toInteger i
#endif
instance IsIntegral CIntPtr where
    toInteger i = Prelude.toInteger i
instance IsIntegral CUIntPtr where
    toInteger i = Prelude.toInteger i
instance IsIntegral CIntMax where
    toInteger i = Prelude.toInteger i
instance IsIntegral CUIntMax where
    toInteger i = Prelude.toInteger i

instance IsNatural Natural where
    toNatural i = i
instance IsNatural Word where
    toNatural i = Prelude.fromIntegral i
instance IsNatural Word8 where
    toNatural i = Prelude.fromIntegral i
instance IsNatural Word16 where
    toNatural i = Prelude.fromIntegral i
instance IsNatural Word32 where
    toNatural i = Prelude.fromIntegral i
instance IsNatural Word64 where
    toNatural i = Prelude.fromIntegral i

instance IsNatural CUChar where
    toNatural i = Prelude.fromIntegral i
instance IsNatural CUShort where
    toNatural i = Prelude.fromIntegral i
instance IsNatural CUInt where
    toNatural i = Prelude.fromIntegral i
instance IsNatural CULong where
    toNatural i = Prelude.fromIntegral i
instance IsNatural CSize where
    toNatural i = Prelude.fromIntegral i
instance IsNatural CULLong where
    toNatural i = Prelude.fromIntegral i
instance IsNatural CUIntPtr where
    toNatural i = Prelude.fromIntegral i
instance IsNatural CUIntMax where
    toNatural i = Prelude.fromIntegral i
