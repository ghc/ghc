{-# OPTIONS -fno-implicit-prelude #-}
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
#ifndef __NHC__
	( -- Integral types, instances of: Eq, Ord, Num, Read, Show, Enum,
	  -- Typeable, Storable, Bounded, Real, Integral, Bits
	  CChar,  CSChar,  CUChar
	, CShort, CUShort, CInt,   CUInt
	, CLong,  CULong
	, CPtrdiff, CSize, CWchar, CSigAtomic
        , CLLong, CULLong
	  -- Numeric types, instances of: Eq, Ord, Num, Read, Show, Enum,
	  -- Typeable, Storable
	, CClock,   CTime

	  -- Floating types, instances of: Eq, Ord, Num, Read, Show, Enum,
	  -- Typeable, Storable, Real, Fractional, Floating, RealFrac,
	  -- RealFloat 
	, CFloat,  CDouble, CLDouble
#else
	( -- Exported non-abstractly in nhc98 to fix an interface file problem.
	  CChar(..),    CSChar(..),  CUChar(..)
	, CShort(..),   CUShort(..), CInt(..),   CUInt(..)
	, CLong(..),    CULong(..)
	, CPtrdiff(..), CSize(..),   CWchar(..), CSigAtomic(..)
        , CLLong(..),   CULLong(..)
	, CClock(..),   CTime(..)
	, CFloat(..),   CDouble(..), CLDouble(..)
#endif

          -- Instances of: Eq and Storable
	, CFile,        CFpos,     CJmpBuf
	) where

#ifdef __NHC__
import NHC.FFI
  ( CChar(..),    CSChar(..),  CUChar(..)
  , CShort(..),   CUShort(..), CInt(..),   CUInt(..)
  , CLong(..),    CULong(..),  CLLong(..), CULLong(..)
  , CPtrdiff(..), CSize(..),   CWchar(..), CSigAtomic(..)
  , CClock(..),   CTime(..)
  , CFloat(..),   CDouble(..), CLDouble(..)
  , CFile,        CFpos,       CJmpBuf
  , Storable(..)
  )
#else

import Foreign.Storable
import Data.Bits	( Bits(..) )
import Data.Int		( Int8,  Int16,  Int32,  Int64  )
import Data.Word	( Word8, Word16, Word32, Word64 )
import Data.Typeable

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.Float
import GHC.Enum
import GHC.Real
import GHC.Show
import GHC.Read
import GHC.Num
#else
import Control.Monad
import Foreign.Ptr
#endif

#include "Typeable.h"
#include "CTypes.h"

INTEGRAL_TYPE(CChar,tyConCChar,"CChar",HTYPE_CHAR)
INTEGRAL_TYPE(CSChar,tyConCSChar,"CSChar",HTYPE_SIGNED_CHAR)
INTEGRAL_TYPE(CUChar,tyConCUChar,"CUChar",HTYPE_UNSIGNED_CHAR)

INTEGRAL_TYPE(CShort,tyConCShort,"CShort",HTYPE_SHORT)
INTEGRAL_TYPE(CUShort,tyConCUShort,"CUShort",HTYPE_UNSIGNED_SHORT)

INTEGRAL_TYPE(CInt,tyConCInt,"CInt",HTYPE_INT)
INTEGRAL_TYPE(CUInt,tyConCUInt,"CUInt",HTYPE_UNSIGNED_INT)

INTEGRAL_TYPE(CLong,tyConCLong,"CLong",HTYPE_LONG)
INTEGRAL_TYPE(CULong,tyConCULong,"CULong",HTYPE_UNSIGNED_LONG)

INTEGRAL_TYPE(CLLong,tyConCLLong,"CLLong",HTYPE_LONG_LONG)
INTEGRAL_TYPE(CULLong,tyConCULLong,"CULLong",HTYPE_UNSIGNED_LONG_LONG)

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

FLOATING_TYPE(CFloat,tyConCFloat,"CFloat",HTYPE_FLOAT)
FLOATING_TYPE(CDouble,tyConCDouble,"CDouble",HTYPE_DOUBLE)
-- HACK: Currently no long double in the FFI, so we simply re-use double
FLOATING_TYPE(CLDouble,tyConCLDouble,"CLDouble",HTYPE_DOUBLE)

{-# RULES
"realToFrac/a->CFloat"    realToFrac = \x -> CFloat   (realToFrac x)
"realToFrac/a->CDouble"   realToFrac = \x -> CDouble  (realToFrac x)
"realToFrac/a->CLDouble"  realToFrac = \x -> CLDouble (realToFrac x)

"realToFrac/CFloat->a"    realToFrac = \(CFloat   x) -> realToFrac x
"realToFrac/CDouble->a"   realToFrac = \(CDouble  x) -> realToFrac x
"realToFrac/CLDouble->a"  realToFrac = \(CLDouble x) -> realToFrac x
 #-}

INTEGRAL_TYPE(CPtrdiff,tyConCPtrdiff,"CPtrdiff",HTYPE_PTRDIFF_T)
INTEGRAL_TYPE(CSize,tyConCSize,"CSize",HTYPE_SIZE_T)
INTEGRAL_TYPE(CWchar,tyConCWchar,"CWchar",HTYPE_WCHAR_T)
INTEGRAL_TYPE(CSigAtomic,tyConCSigAtomic,"CSigAtomic",HTYPE_SIG_ATOMIC_T)

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

INTEGRAL_TYPE(CClock,tyConCClock,"CClock",HTYPE_CLOCK_T)
INTEGRAL_TYPE(CTime,tyConCTime,"CTime",HTYPE_TIME_T)

-- FIXME: Implement and provide instances for Eq and Storable
data CFile = CFile
data CFpos = CFpos
data CJmpBuf = CJmpBuf

-- C99 types which are still missing include:
-- intptr_t, uintptr_t, intmax_t, uintmax_t, wint_t, wctrans_t, wctype_t

#endif
