{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.ForeignPtr
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- $Id: ForeignPtr.hs,v 1.4 2002/04/24 16:31:44 simonmar Exp $
--
-- This module defines foreign pointers, i.e. addresses with associated
-- finalizers.
--
-----------------------------------------------------------------------------

module Foreign.ForeignPtr
        ( ForeignPtr,            -- abstract, instance of: Eq
        , newForeignPtr          -- :: Ptr a -> IO () -> IO (ForeignPtr a)
        , addForeignPtrFinalizer -- :: ForeignPtr a -> IO () -> IO ()
	, withForeignPtr         -- :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
	, foreignPtrToPtr	 -- :: ForeignPtr a -> Ptr a
	, touchForeignPtr        -- :: ForeignPtr a -> IO ()
	, castForeignPtr	 -- :: ForeignPtr a -> ForeignPtr b
        ) 
	where

import Foreign.Ptr
import Data.Dynamic

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.IOBase
import GHC.Num
import GHC.Err
#endif

#include "Dynamic.h"
INSTANCE_TYPEABLE1(ForeignPtr,foreignPtrTc,"ForeignPtr")

#ifdef __GLASGOW_HASKELL__
data ForeignPtr a = ForeignPtr ForeignObj#
instance CCallable (ForeignPtr a)

eqForeignPtr  :: ForeignPtr a -> ForeignPtr a -> Bool
eqForeignPtr (ForeignPtr fo1#) (ForeignPtr fo2#) = eqForeignObj# fo1# fo2#

instance Eq (ForeignPtr a) where 
    p == q = eqForeignPtr p q
    p /= q = not (eqForeignPtr p q)

newForeignPtr :: Ptr a -> IO () -> IO (ForeignPtr a)
newForeignPtr p finalizer
  = do fObj <- mkForeignPtr p
       addForeignPtrFinalizer fObj finalizer
       return fObj

addForeignPtrFinalizer :: ForeignPtr a -> IO () -> IO ()
addForeignPtrFinalizer (ForeignPtr fo) finalizer = 
  IO $ \s -> case mkWeak# fo () finalizer s of { (# s1, w #) -> (# s1, () #) }

mkForeignPtr :: Ptr a -> IO (ForeignPtr a) {- not exported -}
mkForeignPtr (Ptr obj) =  IO ( \ s# ->
    case mkForeignObj# obj s# of
      (# s1#, fo# #) -> (# s1#,  ForeignPtr fo# #) )

touchForeignPtr :: ForeignPtr a -> IO ()
touchForeignPtr (ForeignPtr fo) 
   = IO $ \s -> case touch# fo s of s -> (# s, () #)

withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withForeignPtr fo io
  = do r <- io (foreignPtrToPtr fo)
       touchForeignPtr fo
       return r

foreignPtrToPtr :: ForeignPtr a -> Ptr a
foreignPtrToPtr (ForeignPtr fo) = Ptr (foreignObjToAddr# fo)

castForeignPtr (ForeignPtr a) = ForeignPtr a
#endif

