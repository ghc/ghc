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
-- ^ A value of type @'Ptr' a@ represents a pointer to an object, or an
-- array of objects, which may be marshalled to or from Haskell values
-- of type @a@.
--
--  The type @a@ will normally be an instance of class
-- 'Foreign.Storable.Storable' which provides the marshalling operations.


-- |The constant 'nullPtr' contains a distinguished value of 'Ptr'
-- that is not associated with a valid memory location.
nullPtr :: Ptr a
nullPtr = Ptr nullAddr#

-- |The 'castPtr' function casts a pointer from one type to another.
castPtr :: Ptr a -> Ptr b
castPtr (Ptr addr) = Ptr addr

-- |Advances the given address by the given offset in bytes.
plusPtr :: Ptr a -> Int -> Ptr b
plusPtr (Ptr addr) (I# d) = Ptr (plusAddr# addr d)

-- |Given an arbitrary address and an alignment constraint,
-- 'alignPtr' yields the next higher address that fulfills the
-- alignment constraint.  An alignment constraint @x@ is fulfilled by
-- any address divisible by @x@.  This operation is idempotent.
alignPtr :: Ptr a -> Int -> Ptr a
alignPtr addr@(Ptr a) (I# i)
  = case remAddr# a i of {
      0# -> addr;
      n -> Ptr (plusAddr# a (i -# n)) }

-- |Computes the offset required to get from the first to the second
-- argument.  We have 
--
-- > p2 == p1 `plusPtr` (p2 `minusPtr` p1)
minusPtr :: Ptr a -> Ptr b -> Int
minusPtr (Ptr a1) (Ptr a2) = I# (minusAddr# a1 a2)

------------------------------------------------------------------------
-- Function pointers for the default calling convention.

data FunPtr a = FunPtr Addr# deriving (Eq, Ord)
-- ^ A value of type @'FunPtr' a@ is a pointer to a piece of code. It
-- may be the pointer to a C function or to a Haskell function created
-- using @foreign export dynamic@.  A @foreign export
-- dynamic@ should normally be declared to produce a
-- 'FunPtr' of the correct type.  For example:
--
-- > type Compare = Int -> Int -> Bool
-- > foreign export dynamic mkCompare :: Compare -> IO (FunPtr Compare)

-- |The constant 'nullFunPtr' contains a
-- distinguished value of 'Ptr' that is not
-- associated with a valid memory location
nullFunPtr :: FunPtr a
nullFunPtr = FunPtr nullAddr#

-- |Casts a 'FunPtr' to a 'FunPtr' of a different type
castFunPtr :: FunPtr a -> FunPtr b
castFunPtr (FunPtr addr) = FunPtr addr

-- |Casts a 'FunPtr' to a 'Ptr'
castFunPtrToPtr :: FunPtr a -> Ptr b
castFunPtrToPtr (FunPtr addr) = Ptr addr

-- |Casts a 'Ptr' to a 'FunPtr'
castPtrToFunPtr :: Ptr a -> FunPtr b
castPtrToFunPtr (Ptr addr) = FunPtr addr
\end{code}

