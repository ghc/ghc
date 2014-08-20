{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , MagicHash
           , GeneralizedNewtypeDeriving
  #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE AutoDeriveTypeable, StandaloneDeriving #-}
-- XXX -fno-warn-unused-binds stops us warning about unused constructors,
-- but really we should just remove them if we don't want them

-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.C.Types
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Mapping of C types to corresponding Haskell types.
--
-----------------------------------------------------------------------------

module Foreign.C.Types
        ( -- * Representations of C types
          -- $ctypes

          -- ** Integral types
          -- | These types are are represented as @newtype@s of
          -- types in "Data.Int" and "Data.Word", and are instances of
          -- 'Prelude.Eq', 'Prelude.Ord', 'Prelude.Num', 'Prelude.Read',
          -- 'Prelude.Show', 'Prelude.Enum', 'Typeable', 'Storable',
          -- 'Prelude.Bounded', 'Prelude.Real', 'Prelude.Integral' and
          -- 'Bits'.
          CChar(..),    CSChar(..),   CUChar(..)
        , CShort(..),   CUShort(..),  CInt(..),      CUInt(..)
        , CLong(..),    CULong(..)
        , CPtrdiff(..), CSize(..),    CWchar(..),    CSigAtomic(..)
        , CLLong(..),   CULLong(..)
        , CIntPtr(..),  CUIntPtr(..), CIntMax(..),   CUIntMax(..)

          -- ** Numeric types
          -- | These types are represented as @newtype@s of basic
          -- foreign types, and are instances of
          -- 'Prelude.Eq', 'Prelude.Ord', 'Prelude.Num', 'Prelude.Read',
          -- 'Prelude.Show', 'Prelude.Enum', 'Typeable' and 'Storable'.
        , CClock(..),   CTime(..),    CUSeconds(..), CSUSeconds(..)

        -- extracted from CTime, because we don't want this comment in
        -- the Haskell language reports:

        -- | To convert 'CTime' to 'Data.Time.UTCTime', use the following:
        --
        -- > \t -> posixSecondsToUTCTime (realToFrac t :: POSIXTime)
        --

          -- ** Floating types
          -- | These types are are represented as @newtype@s of
          -- 'Prelude.Float' and 'Prelude.Double', and are instances of
          -- 'Prelude.Eq', 'Prelude.Ord', 'Prelude.Num', 'Prelude.Read',
          -- 'Prelude.Show', 'Prelude.Enum', 'Typeable', 'Storable',
          -- 'Prelude.Real', 'Prelude.Fractional', 'Prelude.Floating',
          -- 'Prelude.RealFrac' and 'Prelude.RealFloat'.
        , CFloat(..),   CDouble(..)
        -- XXX GHC doesn't support CLDouble yet
        -- , CLDouble(..)

          -- ** Other types

          -- Instances of: Eq and Storable
        , CFile,        CFpos,     CJmpBuf
        ) where

import Foreign.Storable
import Data.Bits        ( Bits(..), FiniteBits(..) )
import Data.Int         ( Int8,  Int16,  Int32,  Int64  )
import Data.Word        ( Word8, Word16, Word32, Word64 )
import Data.Typeable

import GHC.Base
import GHC.Float
import GHC.Enum
import GHC.Real
import GHC.Show
import GHC.Read
import GHC.Num

#include "HsBaseConfig.h"
#include "CTypes.h"

-- | Haskell type representing the C @char@ type.
INTEGRAL_TYPE(CChar,HTYPE_CHAR)
-- | Haskell type representing the C @signed char@ type.
INTEGRAL_TYPE(CSChar,HTYPE_SIGNED_CHAR)
-- | Haskell type representing the C @unsigned char@ type.
INTEGRAL_TYPE(CUChar,HTYPE_UNSIGNED_CHAR)

-- | Haskell type representing the C @short@ type.
INTEGRAL_TYPE(CShort,HTYPE_SHORT)
-- | Haskell type representing the C @unsigned short@ type.
INTEGRAL_TYPE(CUShort,HTYPE_UNSIGNED_SHORT)

-- | Haskell type representing the C @int@ type.
INTEGRAL_TYPE(CInt,HTYPE_INT)
-- | Haskell type representing the C @unsigned int@ type.
INTEGRAL_TYPE(CUInt,HTYPE_UNSIGNED_INT)

-- | Haskell type representing the C @long@ type.
INTEGRAL_TYPE(CLong,HTYPE_LONG)
-- | Haskell type representing the C @unsigned long@ type.
INTEGRAL_TYPE(CULong,HTYPE_UNSIGNED_LONG)

-- | Haskell type representing the C @long long@ type.
INTEGRAL_TYPE(CLLong,HTYPE_LONG_LONG)
-- | Haskell type representing the C @unsigned long long@ type.
INTEGRAL_TYPE(CULLong,HTYPE_UNSIGNED_LONG_LONG)

{-# RULES
"fromIntegral/a->CChar"   fromIntegral = \x -> CChar   (fromIntegral x)
"fromIntegral/a->CSChar"  fromIntegral = \x -> CSChar  (fromIntegral x)
"fromIntegral/a->CUChar"  fromIntegral = \x -> CUChar  (fromIntegral x)
"fromIntegral/a->CShort"  fromIntegral = \x -> CShort  (fromIntegral x)
"fromIntegral/a->CUShort" fromIntegral = \x -> CUShort (fromIntegral x)
"fromIntegral/a->CInt"    fromIntegral = \x -> CInt    (fromIntegral x)
"fromIntegral/a->CUInt"   fromIntegral = \x -> CUInt   (fromIntegral x)
"fromIntegral/a->CLong"   fromIntegral = \x -> CLong   (fromIntegral x)
"fromIntegral/a->CULong"  fromIntegral = \x -> CULong  (fromIntegral x)
"fromIntegral/a->CLLong"  fromIntegral = \x -> CLLong  (fromIntegral x)
"fromIntegral/a->CULLong" fromIntegral = \x -> CULLong (fromIntegral x)

"fromIntegral/CChar->a"   fromIntegral = \(CChar   x) -> fromIntegral x
"fromIntegral/CSChar->a"  fromIntegral = \(CSChar  x) -> fromIntegral x
"fromIntegral/CUChar->a"  fromIntegral = \(CUChar  x) -> fromIntegral x
"fromIntegral/CShort->a"  fromIntegral = \(CShort  x) -> fromIntegral x
"fromIntegral/CUShort->a" fromIntegral = \(CUShort x) -> fromIntegral x
"fromIntegral/CInt->a"    fromIntegral = \(CInt    x) -> fromIntegral x
"fromIntegral/CUInt->a"   fromIntegral = \(CUInt   x) -> fromIntegral x
"fromIntegral/CLong->a"   fromIntegral = \(CLong   x) -> fromIntegral x
"fromIntegral/CULong->a"  fromIntegral = \(CULong  x) -> fromIntegral x
"fromIntegral/CLLong->a"  fromIntegral = \(CLLong  x) -> fromIntegral x
"fromIntegral/CULLong->a" fromIntegral = \(CULLong x) -> fromIntegral x
 #-}

-- | Haskell type representing the C @float@ type.
FLOATING_TYPE(CFloat,HTYPE_FLOAT)
-- | Haskell type representing the C @double@ type.
FLOATING_TYPE(CDouble,HTYPE_DOUBLE)
-- XXX GHC doesn't support CLDouble yet

{-# RULES
"realToFrac/a->CFloat"    realToFrac = \x -> CFloat   (realToFrac x)
"realToFrac/a->CDouble"   realToFrac = \x -> CDouble  (realToFrac x)

"realToFrac/CFloat->a"    realToFrac = \(CFloat   x) -> realToFrac x
"realToFrac/CDouble->a"   realToFrac = \(CDouble  x) -> realToFrac x
 #-}

-- GHC doesn't support CLDouble yet
-- "realToFrac/a->CLDouble"  realToFrac = \x -> CLDouble (realToFrac x)
-- "realToFrac/CLDouble->a"  realToFrac = \(CLDouble x) -> realToFrac x

-- | Haskell type representing the C @ptrdiff_t@ type.
INTEGRAL_TYPE(CPtrdiff,HTYPE_PTRDIFF_T)
-- | Haskell type representing the C @size_t@ type.
INTEGRAL_TYPE(CSize,HTYPE_SIZE_T)
-- | Haskell type representing the C @wchar_t@ type.
INTEGRAL_TYPE(CWchar,HTYPE_WCHAR_T)
-- | Haskell type representing the C @sig_atomic_t@ type.
INTEGRAL_TYPE(CSigAtomic,HTYPE_SIG_ATOMIC_T)

{-# RULES
"fromIntegral/a->CPtrdiff"   fromIntegral = \x -> CPtrdiff   (fromIntegral x)
"fromIntegral/a->CSize"      fromIntegral = \x -> CSize      (fromIntegral x)
"fromIntegral/a->CWchar"     fromIntegral = \x -> CWchar     (fromIntegral x)
"fromIntegral/a->CSigAtomic" fromIntegral = \x -> CSigAtomic (fromIntegral x)

"fromIntegral/CPtrdiff->a"   fromIntegral = \(CPtrdiff   x) -> fromIntegral x
"fromIntegral/CSize->a"      fromIntegral = \(CSize      x) -> fromIntegral x
"fromIntegral/CWchar->a"     fromIntegral = \(CWchar     x) -> fromIntegral x
"fromIntegral/CSigAtomic->a" fromIntegral = \(CSigAtomic x) -> fromIntegral x
 #-}

-- | Haskell type representing the C @clock_t@ type.
ARITHMETIC_TYPE(CClock,HTYPE_CLOCK_T)
-- | Haskell type representing the C @time_t@ type.
ARITHMETIC_TYPE(CTime,HTYPE_TIME_T)
-- | Haskell type representing the C @useconds_t@ type.
--
-- /Since: 4.4.0.0/
ARITHMETIC_TYPE(CUSeconds,HTYPE_USECONDS_T)
-- | Haskell type representing the C @suseconds_t@ type.
--
-- /Since: 4.4.0.0/
ARITHMETIC_TYPE(CSUSeconds,HTYPE_SUSECONDS_T)

-- FIXME: Implement and provide instances for Eq and Storable
-- | Haskell type representing the C @FILE@ type.
data CFile = CFile
-- | Haskell type representing the C @fpos_t@ type.
data CFpos = CFpos
-- | Haskell type representing the C @jmp_buf@ type.
data CJmpBuf = CJmpBuf

INTEGRAL_TYPE(CIntPtr,HTYPE_INTPTR_T)
INTEGRAL_TYPE(CUIntPtr,HTYPE_UINTPTR_T)
INTEGRAL_TYPE(CIntMax,HTYPE_INTMAX_T)
INTEGRAL_TYPE(CUIntMax,HTYPE_UINTMAX_T)

{-# RULES
"fromIntegral/a->CIntPtr"  fromIntegral = \x -> CIntPtr  (fromIntegral x)
"fromIntegral/a->CUIntPtr" fromIntegral = \x -> CUIntPtr (fromIntegral x)
"fromIntegral/a->CIntMax"  fromIntegral = \x -> CIntMax  (fromIntegral x)
"fromIntegral/a->CUIntMax" fromIntegral = \x -> CUIntMax (fromIntegral x)
 #-}

-- C99 types which are still missing include:
-- wint_t, wctrans_t, wctype_t

{- $ctypes

These types are needed to accurately represent C function prototypes,
in order to access C library interfaces in Haskell.  The Haskell system
is not required to represent those types exactly as C does, but the
following guarantees are provided concerning a Haskell type @CT@
representing a C type @t@:

* If a C function prototype has @t@ as an argument or result type, the
  use of @CT@ in the corresponding position in a foreign declaration
  permits the Haskell program to access the full range of values encoded
  by the C type; and conversely, any Haskell value for @CT@ has a valid
  representation in C.

* @'sizeOf' ('Prelude.undefined' :: CT)@ will yield the same value as
  @sizeof (t)@ in C.

* @'alignment' ('Prelude.undefined' :: CT)@ matches the alignment
  constraint enforced by the C implementation for @t@.

* The members 'peek' and 'poke' of the 'Storable' class map all values
  of @CT@ to the corresponding value of @t@ and vice versa.

* When an instance of 'Prelude.Bounded' is defined for @CT@, the values
  of 'Prelude.minBound' and 'Prelude.maxBound' coincide with @t_MIN@
  and @t_MAX@ in C.

* When an instance of 'Prelude.Eq' or 'Prelude.Ord' is defined for @CT@,
  the predicates defined by the type class implement the same relation
  as the corresponding predicate in C on @t@.

* When an instance of 'Prelude.Num', 'Prelude.Read', 'Prelude.Integral',
  'Prelude.Fractional', 'Prelude.Floating', 'Prelude.RealFrac', or
  'Prelude.RealFloat' is defined for @CT@, the arithmetic operations
  defined by the type class implement the same function as the
  corresponding arithmetic operations (if available) in C on @t@.

* When an instance of 'Bits' is defined for @CT@, the bitwise operation
  defined by the type class implement the same function as the
  corresponding bitwise operation in C on @t@.

-}

