{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- 
-- Module      :  Foreign.C.TypesISO
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- $Id: TypesISO.hs,v 1.3 2002/02/05 17:32:25 simonmar Exp $
--
-- A mapping of C types defined by the ISO C standard to corresponding Haskell
-- types. Like CTypes, this is a cool hack...
--
-----------------------------------------------------------------------------

module Foreign.C.TypesISO
	( -- Integral types, instances of: Eq, Ord, Num, Read, Show, Enum,
	  -- Typeable, Storable, Bounded, Real, Integral, Bits
	  CPtrdiff(..), CSize(..), CWchar(..), CSigAtomic(..)

	  -- Numeric types, instances of: Eq, Ord, Num, Read, Show, Enum,
	  -- Typeable, Storable
	, CClock(..),   CTime(..),

          -- Instances of: Eq and Storable
	, CFile,        CFpos,     CJmpBuf
	) where

import Data.Bits	( Bits(..) )
import Data.Int
import Data.Word
import Data.Dynamic

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.Enum
import GHC.Real
import GHC.Show
import GHC.Read
import GHC.Num
#endif

#include "CTypes.h"

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

#include "Dynamic.h"
INSTANCE_TYPEABLE0(CPtrdiff,cPtrdiffTc,"CPtrdiff")
INSTANCE_TYPEABLE0(CSize,cSizeTc,"CSize")
INSTANCE_TYPEABLE0(CWchar,cWcharTc,"CWchar")
INSTANCE_TYPEABLE0(CSigAtomic,cSigAtomicTc,"CSigAtomic")
INSTANCE_TYPEABLE0(CClock,cClockTc,"CClock")
INSTANCE_TYPEABLE0(CTime,cTimeTc,"CTime")
