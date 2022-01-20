{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
-- XXX -Wno-unused-binds stops us warning about unused constructors,
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

          -- ** #platform# Platform differences
          -- | This module contains platform specific information about types.
          -- __/As such, the types presented on this page reflect the/__
          -- __/platform on which the documentation was generated and may/__
          -- __/not coincide with the types on your platform./__

          -- ** Integral types
          -- | These types are represented as @newtype@s of
          -- types in "Data.Int" and "Data.Word", and are instances of
          -- 'Prelude.Eq', 'Prelude.Ord', 'Prelude.Num', 'Prelude.Read',
          -- 'Prelude.Show', 'Prelude.Enum', 'Data.Typeable.Typeable',
          -- 'Storable', 'Prelude.Bounded', 'Prelude.Real', 'Prelude.Integral'
          -- and 'Bits'.
          CChar(..),    CSChar(..),   CUChar(..)
        , CShort(..),   CUShort(..),  CInt(..),      CUInt(..)
        , CLong(..),    CULong(..)
        , CPtrdiff(..), CSize(..),    CWchar(..),    CSigAtomic(..)
        , CLLong(..),   CULLong(..), CBool(..)
        , CIntPtr(..),  CUIntPtr(..), CIntMax(..),   CUIntMax(..)

          -- ** Numeric types
          -- | These types are represented as @newtype@s of basic
          -- foreign types, and are instances of
          -- 'Prelude.Eq', 'Prelude.Ord', 'Prelude.Num', 'Prelude.Read',
          -- 'Prelude.Show', 'Prelude.Enum', 'Data.Typeable.Typeable' and
          -- 'Storable'.
        , CClock(..),   CTime(..),    CUSeconds(..), CSUSeconds(..)

        -- extracted from CTime, because we don't want this comment in
        -- the Haskell language reports:

        -- | To convert 'CTime' to 'Data.Time.UTCTime', use the following:
        --
        -- > \t -> posixSecondsToUTCTime (realToFrac t :: POSIXTime)
        --

          -- ** Floating types
          -- | These types are represented as @newtype@s of
          -- 'Prelude.Float' and 'Prelude.Double', and are instances of
          -- 'Prelude.Eq', 'Prelude.Ord', 'Prelude.Num', 'Prelude.Read',
          -- 'Prelude.Show', 'Prelude.Enum', 'Data.Typeable.Typeable', 'Storable',
          -- 'Prelude.Real', 'Prelude.Fractional', 'Prelude.Floating',
          -- 'Prelude.RealFrac' and 'Prelude.RealFloat'. That does mean
          -- that `CFloat`'s (respectively `CDouble`'s) instances of
          -- 'Prelude.Eq', 'Prelude.Ord', 'Prelude.Num' and
          -- 'Prelude.Fractional' are as badly behaved as `Prelude.Float`'s
          -- (respectively `Prelude.Double`'s).
        , CFloat(..),   CDouble(..)
        -- XXX GHC doesn't support CLDouble yet
        -- , CLDouble(..)

          -- See Note [Exporting constructors of marshallable foreign types]
          -- in Foreign.Ptr for why the constructors for these newtypes are
          -- exported.

          -- ** Other types

          -- Instances of: Eq and Storable
        , CFile,        CFpos,     CJmpBuf
        ) where

import Foreign.Storable
import Data.Bits        ( Bits(..), FiniteBits(..) )
import Data.Int         ( Int8,  Int16,  Int32,  Int64  )
import Data.Word        ( Word8, Word16, Word32, Word64 )

import GHC.Base
import GHC.Float
import GHC.Enum
import GHC.Real
import GHC.Show
import GHC.Read
import GHC.Num
import GHC.Ix

#include "HsBaseConfig.h"
#include "CTypes.h"

-- | Haskell type representing the C @char@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
INTEGRAL_TYPE(CChar,"char",HTYPE_CHAR)
-- | Haskell type representing the C @signed char@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
INTEGRAL_TYPE(CSChar,"signed char",HTYPE_SIGNED_CHAR)
-- | Haskell type representing the C @unsigned char@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
INTEGRAL_TYPE(CUChar,"unsigned char",HTYPE_UNSIGNED_CHAR)

-- | Haskell type representing the C @short@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
INTEGRAL_TYPE(CShort,"short",HTYPE_SHORT)
-- | Haskell type representing the C @unsigned short@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
INTEGRAL_TYPE(CUShort,"unsigned short",HTYPE_UNSIGNED_SHORT)

-- | Haskell type representing the C @int@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
INTEGRAL_TYPE(CInt,"int",HTYPE_INT)
-- | Haskell type representing the C @unsigned int@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
INTEGRAL_TYPE(CUInt,"unsigned int",HTYPE_UNSIGNED_INT)

-- | Haskell type representing the C @long@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
INTEGRAL_TYPE(CLong,"long",HTYPE_LONG)
-- | Haskell type representing the C @unsigned long@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
INTEGRAL_TYPE(CULong,"unsigned long",HTYPE_UNSIGNED_LONG)

-- | Haskell type representing the C @long long@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
INTEGRAL_TYPE(CLLong,"long long",HTYPE_LONG_LONG)
-- | Haskell type representing the C @unsigned long long@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
INTEGRAL_TYPE(CULLong,"unsigned long long",HTYPE_UNSIGNED_LONG_LONG)

-- | Haskell type representing the C @bool@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
--
-- @since 4.10.0.0
INTEGRAL_TYPE(CBool,"bool",HTYPE_BOOL)

-- | Haskell type representing the C @float@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
FLOATING_TYPE(CFloat,"float",HTYPE_FLOAT)
-- | Haskell type representing the C @double@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
FLOATING_TYPE(CDouble,"double",HTYPE_DOUBLE)
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
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
INTEGRAL_TYPE(CPtrdiff,"ptrdiff_t",HTYPE_PTRDIFF_T)
-- | Haskell type representing the C @size_t@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
INTEGRAL_TYPE(CSize,"size_t",HTYPE_SIZE_T)
-- | Haskell type representing the C @wchar_t@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
INTEGRAL_TYPE(CWchar,"wchar_t",HTYPE_WCHAR_T)
-- | Haskell type representing the C @sig_atomic_t@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
INTEGRAL_TYPE(CSigAtomic,"sig_atomic_t",HTYPE_SIG_ATOMIC_T)

-- | Haskell type representing the C @clock_t@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
ARITHMETIC_TYPE(CClock,"clock_t",HTYPE_CLOCK_T)
-- | Haskell type representing the C @time_t@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
ARITHMETIC_TYPE(CTime,"time_t",HTYPE_TIME_T)
-- | Haskell type representing the C @useconds_t@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
--
-- @since 4.4.0.0

ARITHMETIC_TYPE(CUSeconds,"useconds_t",HTYPE_USECONDS_T)
-- | Haskell type representing the C @suseconds_t@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
--
-- @since 4.4.0.0
ARITHMETIC_TYPE(CSUSeconds,"suseconds_t",HTYPE_SUSECONDS_T)

-- FIXME: Implement and provide instances for Eq and Storable
-- | Haskell type representing the C @FILE@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
data CFile = CFile
-- | Haskell type representing the C @fpos_t@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
data CFpos = CFpos
-- | Haskell type representing the C @jmp_buf@ type.
-- /(The concrete types of "Foreign.C.Types#platform" are platform-specific.)/
data CJmpBuf = CJmpBuf

INTEGRAL_TYPE(CIntPtr,"intptr_t",HTYPE_INTPTR_T)
INTEGRAL_TYPE(CUIntPtr,"uintptr_t",HTYPE_UINTPTR_T)
INTEGRAL_TYPE(CIntMax,"intmax_t",HTYPE_INTMAX_T)
INTEGRAL_TYPE(CUIntMax,"uintmax_t",HTYPE_UINTMAX_T)

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

