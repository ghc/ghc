{-# OPTIONS -fno-implicit-prelude #-}
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

 ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Ptr
import GHC.IOBase
import GHC.Err
import GHC.Base
import GHC.Num
import GHC.List
import GHC.Show
import Numeric
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
#include "MachDeps.h"

#if (WORD_SIZE_IN_BITS == 32 || WORD_SIZE_IN_BITS == 64)
instance Show (Ptr a) where
   showsPrec p (Ptr a) rs = pad_out (showHex (word2Integer(int2Word#(addr2Int# a))) "") rs
     where
        -- want 0s prefixed to pad it out to a fixed length.
       pad_out ls rs = 
	  '0':'x':(replicate (2*SIZEOF_HSPTR - length ls) '0') ++ ls ++ rs
       -- word2Integer :: Word# -> Integer (stolen from Word.lhs)
       word2Integer w = case word2Integer# w of
			(# s, d #) -> J# s d

instance Show (FunPtr a) where
   showsPrec p = showsPrec p . castFunPtrToPtr
#endif

foreign import ccall unsafe "freeHaskellFunctionPtr"
    freeHaskellFunPtr :: FunPtr a -> IO ()
#endif
