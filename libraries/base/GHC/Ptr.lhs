\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Ptr
-- Copyright   :  (c) The FFI Task Force, 2000-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Ptr' and 'FunPtr' types and operations.
--
-----------------------------------------------------------------------------

module GHC.Ptr where

import GHC.Base

------------------------------------------------------------------------
-- Data pointers.

data Ptr a = Ptr Addr# deriving (Eq, Ord)

nullPtr :: Ptr a
nullPtr = Ptr nullAddr#

castPtr :: Ptr a -> Ptr b
castPtr (Ptr addr) = Ptr addr

plusPtr :: Ptr a -> Int -> Ptr b
plusPtr (Ptr addr) (I# d) = Ptr (plusAddr# addr d)

alignPtr :: Ptr a -> Int -> Ptr a
alignPtr addr@(Ptr a) (I# i)
  = case remAddr# a i of {
      0# -> addr;
      n -> Ptr (plusAddr# a (i -# n)) }

minusPtr :: Ptr a -> Ptr b -> Int
minusPtr (Ptr a1) (Ptr a2) = I# (minusAddr# a1 a2)

instance CCallable   (Ptr a)
instance CReturnable (Ptr a)

------------------------------------------------------------------------
-- Function pointers for the default calling convention.

data FunPtr a = FunPtr Addr# deriving (Eq, Ord)

nullFunPtr :: FunPtr a
nullFunPtr = FunPtr nullAddr#

castFunPtr :: FunPtr a -> FunPtr b
castFunPtr (FunPtr addr) = FunPtr addr

castFunPtrToPtr :: FunPtr a -> Ptr b
castFunPtrToPtr (FunPtr addr) = Ptr addr

castPtrToFunPtr :: Ptr a -> FunPtr b
castPtrToFunPtr (Ptr addr) = FunPtr addr

instance CCallable   (FunPtr a)
instance CReturnable (FunPtr a)

\end{code}

