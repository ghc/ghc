{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Ptr
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides typed pointers to foreign data.  It is part
-- of the Foreign Function Interface (FFI) and will normally be
-- imported via the "Foreign" module.
--
-----------------------------------------------------------------------------

module Foreign.Ptr (

    -- * Data pointers
    
    Ptr,      -- data Ptr a
    nullPtr,      -- :: Ptr a
    castPtr,      -- :: Ptr a -> Ptr b
    plusPtr,      -- :: Ptr a -> Int -> Ptr b
    alignPtr,     -- :: Ptr a -> Int -> Ptr a
    minusPtr,     -- :: Ptr a -> Ptr b -> Int
    
    -- * Function pointers
    
    FunPtr,      -- data FunPtr a
    nullFunPtr,      -- :: FunPtr a
    castFunPtr,      -- :: FunPtr a -> FunPtr b
    castFunPtrToPtr, -- :: FunPtr a -> Ptr b
    castPtrToFunPtr, -- :: Ptr a -> FunPtr b
    
    freeHaskellFunPtr, -- :: FunPtr a -> IO ()
    -- Free the function pointer created by foreign export dynamic.

    -- * Integral types with lossless conversion to/from pointers
    IntPtr,
    ptrToIntPtr,
    intPtrToPtr,
    WordPtr,
    ptrToWordPtr,
    wordPtrToPtr
 ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Ptr
import GHC.IOBase
import GHC.Base
import GHC.Num
import GHC.List
import GHC.Read
import GHC.Real
import GHC.Show
import GHC.Enum
import GHC.Word		( Word(..) )
import Data.Bits
import Data.Typeable 	( Typeable(..), mkTyCon, mkTyConApp )
import Numeric
import Foreign.C.Types

import Foreign.Storable
import Data.Int
import Data.Word
#endif

#ifdef __NHC__
import NHC.FFI
  ( Ptr
  , nullPtr
  , castPtr
  , plusPtr
  , alignPtr
  , minusPtr
  , FunPtr
  , nullFunPtr
  , castFunPtr
  , castFunPtrToPtr
  , castPtrToFunPtr
  , freeHaskellFunPtr
  )
#endif

#ifdef __HUGS__
import Hugs.Ptr
#endif

#ifdef __GLASGOW_HASKELL__
-- | Release the storage associated with the given 'FunPtr', which
-- must have been obtained from a wrapper stub.  This should be called
-- whenever the return value from a foreign import wrapper function is
-- no longer required; otherwise, the storage it uses will leak.
foreign import ccall unsafe "freeHaskellFunctionPtr"
    freeHaskellFunPtr :: FunPtr a -> IO ()

#include "HsBaseConfig.h"
#include "CTypes.h"

-- | An unsigend integral type that can be losslessly converted to and from
-- @Ptr@.
INTEGRAL_TYPE(WordPtr,tyConWordPtr,"WordPtr",Word)
	-- Word and Int are guaranteed pointer-sized in GHC

-- | A sigend integral type that can be losslessly converted to and from
-- @Ptr@.
INTEGRAL_TYPE(IntPtr,tyConIntPtr,"IntPtr",Int)
	-- Word and Int are guaranteed pointer-sized in GHC

-- | casts a @Ptr@ to a @WordPtr@
ptrToWordPtr :: Ptr a -> WordPtr
ptrToWordPtr (Ptr a#) = WordPtr (W# (int2Word# (addr2Int# a#)))

-- | casts a @WordPtr@ to a @Ptr@
wordPtrToPtr :: WordPtr -> Ptr a
wordPtrToPtr (WordPtr (W# w#)) = Ptr (int2Addr# (word2Int# w#))

-- | casts a @Ptr@ to an @IntPtr@
ptrToIntPtr :: Ptr a -> IntPtr
ptrToIntPtr (Ptr a#) = IntPtr (I# (addr2Int# a#))

-- | casts an @IntPtr@ to a @Ptr@
intPtrToPtr :: IntPtr -> Ptr a
intPtrToPtr (IntPtr (I# i#)) = Ptr (int2Addr# i#)
#endif
