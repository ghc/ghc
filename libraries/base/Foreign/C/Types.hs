{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.C.Types
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Mapping of C types to corresponding Haskell types. A cool hack...
--
-----------------------------------------------------------------------------

module Foreign.C.Types
	( -- Integral types, instances of: Eq, Ord, Num, Read, Show, Enum,
	  -- Typeable, Storable, Bounded, Real, Integral, Bits
	  CChar(..),  CSChar(..),  CUChar(..)
	, CShort(..), CUShort(..), CInt(..),   CUInt(..)
	, CLong(..),  CULong(..),  CLLong(..), CULLong(..)

	  -- Floating types, instances of: Eq, Ord, Num, Read, Show, Enum,
	  -- Typeable, Storable, Real, Fractional, Floating, RealFrac,
	  -- RealFloat 
	, CFloat(..),  CDouble(..), CLDouble(..)
	) where

import Data.Bits	( Bits(..) )
import Data.Int		( Int8,  Int16,  Int32,  Int64  )
import Data.Word	( Word8, Word16, Word32, Word64 )
import Data.Dynamic

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.Float
import GHC.Enum
import GHC.Real
import GHC.Show
import GHC.Read
import GHC.Num
#endif

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


#include "Dynamic.h"
INSTANCE_TYPEABLE0(CChar,cCharTc,"CChar")
INSTANCE_TYPEABLE0(CSChar,cSCharTc,"CSChar")
INSTANCE_TYPEABLE0(CUChar,cUCharTc,"CUChar")

INSTANCE_TYPEABLE0(CShort,cShortTc,"CShort")
INSTANCE_TYPEABLE0(CUShort,cUShortTc,"CUShort")

INSTANCE_TYPEABLE0(CInt,cIntTc,"CInt")
INSTANCE_TYPEABLE0(CUInt,cUIntTc,"CUInt")

INSTANCE_TYPEABLE0(CLong,cLongTc,"CLong")
INSTANCE_TYPEABLE0(CULong,cULongTc,"CULong")

INSTANCE_TYPEABLE0(CLLong,cLLongTc,"CLLong")
INSTANCE_TYPEABLE0(CULLong,cULLongTc,"CULLong")

INSTANCE_TYPEABLE0(CFloat,cFloatTc,"CFloat")
INSTANCE_TYPEABLE0(CDouble,cDoubleTc,"CDouble")
INSTANCE_TYPEABLE0(CLDouble,cLDoubleTc,"CLDouble")
