{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}

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

    Ptr,
    nullPtr,
    castPtr,
    plusPtr,
    alignPtr,
    minusPtr,

    -- * Function pointers

    FunPtr,
    nullFunPtr,
    castFunPtr,
    castFunPtrToPtr,
    castPtrToFunPtr,

    freeHaskellFunPtr,
    -- Free the function pointer created by foreign export dynamic.

    -- * Integral types with lossless conversion to and from pointers
    IntPtr(..),
    ptrToIntPtr,
    intPtrToPtr,
    WordPtr(..),
    ptrToWordPtr,
    wordPtrToPtr

    -- See Note [Exporting constructors of marshallable foreign types]
    -- for why the constructors for IntPtr and WordPtr are exported.
 ) where

import GHC.Ptr
import GHC.Base
import GHC.Num
import GHC.Read
import GHC.Real
import GHC.Show
import GHC.Enum
import GHC.Ix

import Data.Bits
import Foreign.Storable ( Storable(..) )

-- | Release the storage associated with the given 'FunPtr', which
-- must have been obtained from a wrapper stub.  This should be called
-- whenever the return value from a foreign import wrapper function is
-- no longer required; otherwise, the storage it uses will leak.
foreign import ccall unsafe "freeHaskellFunctionPtr"
    freeHaskellFunPtr :: FunPtr a -> IO ()

#include "HsBaseConfig.h"
#include "CTypes.h"

-- | An unsigned integral type that can be losslessly converted to and from
-- @Ptr@. This type is also compatible with the C99 type @uintptr_t@, and
-- can be marshalled to and from that type safely.
INTEGRAL_TYPE(WordPtr,"uintptr_t",Word)
        -- Word and Int are guaranteed pointer-sized in GHC

-- | A signed integral type that can be losslessly converted to and from
-- @Ptr@.  This type is also compatible with the C99 type @intptr_t@, and
-- can be marshalled to and from that type safely.
INTEGRAL_TYPE(IntPtr,"intptr_t",Int)
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

{-
Note [Exporting constructors of marshallable foreign types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
One might expect that IntPtr, WordPtr, and the other newtypes in the
Foreign.C.Types and System.Posix.Types modules to be abstract, but this is not
the case in GHC (see #5229 and #11983). In fact, we deliberately export
the constructors for these datatypes in order to satisfy a requirement of the
Haskell 2010 Report (§ 8.4.2) that if a newtype is used in a foreign
declaration, then its constructor must be visible.

This requirement was motivated by the fact that using a type in a foreign
declaration necessarily exposes some information about the type to the user,
so being able to use abstract types in a foreign declaration breaks their
abstraction (see #3008). As a result, the constructors of all FFI-related
newtypes in base must be exported in order to be useful for FFI programming,
even at the cost of exposing their underlying, architecture-dependent types.
-}
