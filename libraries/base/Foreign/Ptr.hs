{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- 
-- Module      :  Foreign.Ptr
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- $Id: Ptr.hs,v 1.2 2001/07/03 11:37:50 simonmar Exp $
--
-- Pointer types.
--
-----------------------------------------------------------------------------

module Foreign.Ptr (
    --------------------------------------------------------------------
    -- Data pointers.
    
    Ptr(..),      -- data Ptr a
    nullPtr,      -- :: Ptr a
    castPtr,      -- :: Ptr a -> Ptr b
    plusPtr,      -- :: Ptr a -> Int -> Ptr b
    alignPtr,     -- :: Ptr a -> Int -> Ptr a
    minusPtr,     -- :: Ptr a -> Ptr b -> Int
    
    --------------------------------------------------------------------
    -- Function pointers.
    
    FunPtr(..),      -- data FunPtr a
    nullFunPtr,      -- :: FunPtr a
    castFunPtr,      -- :: FunPtr a -> FunPtr b
    castFunPtrToPtr, -- :: FunPtr a -> Ptr b
    castPtrToFunPtr, -- :: Ptr a -> FunPtr b
    
    freeHaskellFunPtr, -- :: FunPtr a -> IO ()
    -- Free the function pointer created by foreign export dynamic.

 ) where

import Data.Dynamic

#ifdef __GLASGOW_HASKELL__
import GHC.Ptr
import GHC.IOBase
import GHC.Err
#endif

foreign import "freeHaskellFunctionPtr" unsafe
    freeHaskellFunPtr :: FunPtr a -> IO ()

#include "Dynamic.h"
INSTANCE_TYPEABLE1(Ptr,ptrTc,"Ptr")
