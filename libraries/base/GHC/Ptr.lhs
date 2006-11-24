\begin{code}
{-# OPTIONS_GHC -fno-implicit-prelude #-}
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

-- #hide
module GHC.Ptr where

import GHC.Base
import GHC.Show
import GHC.Num
import GHC.List ( length, replicate )
import Numeric		( showHex )

#include "MachDeps.h"

------------------------------------------------------------------------
-- Data pointers.

data Ptr a = Ptr Addr# deriving (Eq, Ord)
-- ^ A value of type @'Ptr' a@ represents a pointer to an object, or an
-- array of objects, which may be marshalled to or from Haskell values
-- of type @a@.
--
-- The type @a@ will often be an instance of class
-- 'Foreign.Storable.Storable' which provides the marshalling operations.
-- However this is not essential, and you can provide your own operations
-- to access the pointer.  For example you might write small foreign
-- functions to get or set the fields of a C @struct@.

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
-- ^ A value of type @'FunPtr' a@ is a pointer to a function callable
-- from foreign code.  The type @a@ will normally be a /foreign type/,
-- a function type with zero or more arguments where
--
-- * the argument types are /marshallable foreign types/,
--   i.e. 'Char', 'Int', 'Prelude.Double', 'Prelude.Float',
--   'Bool', 'Data.Int.Int8', 'Data.Int.Int16', 'Data.Int.Int32',
--   'Data.Int.Int64', 'Data.Word.Word8', 'Data.Word.Word16',
--   'Data.Word.Word32', 'Data.Word.Word64', @'Ptr' a@, @'FunPtr' a@,
--   @'Foreign.StablePtr.StablePtr' a@ or a renaming of any of these
--   using @newtype@.
-- 
-- * the return type is either a marshallable foreign type or has the form
--   @'Prelude.IO' t@ where @t@ is a marshallable foreign type or @()@.
--
-- A value of type @'FunPtr' a@ may be a pointer to a foreign function,
-- either returned by another foreign function or imported with a
-- a static address import like
--
-- > foreign import ccall "stdlib.h &free"
-- >   p_free :: FunPtr (Ptr a -> IO ())
--
-- or a pointer to a Haskell function created using a /wrapper/ stub
-- declared to produce a 'FunPtr' of the correct type.  For example:
--
-- > type Compare = Int -> Int -> Bool
-- > foreign import ccall "wrapper"
-- >   mkCompare :: Compare -> IO (FunPtr Compare)
--
-- Calls to wrapper stubs like @mkCompare@ allocate storage, which
-- should be released with 'Foreign.Ptr.freeHaskellFunPtr' when no
-- longer required.
--
-- To convert 'FunPtr' values to corresponding Haskell functions, one
-- can define a /dynamic/ stub for the specific foreign type, e.g.
--
-- > type IntFunction = CInt -> IO ()
-- > foreign import ccall "dynamic" 
-- >   mkFun :: FunPtr IntFunction -> IntFunction

-- |The constant 'nullFunPtr' contains a
-- distinguished value of 'FunPtr' that is not
-- associated with a valid memory location.
nullFunPtr :: FunPtr a
nullFunPtr = FunPtr nullAddr#

-- |Casts a 'FunPtr' to a 'FunPtr' of a different type.
castFunPtr :: FunPtr a -> FunPtr b
castFunPtr (FunPtr addr) = FunPtr addr

-- |Casts a 'FunPtr' to a 'Ptr'.
--
-- /Note:/ this is valid only on architectures where data and function
-- pointers range over the same set of addresses, and should only be used
-- for bindings to external libraries whose interface already relies on
-- this assumption.
castFunPtrToPtr :: FunPtr a -> Ptr b
castFunPtrToPtr (FunPtr addr) = Ptr addr

-- |Casts a 'Ptr' to a 'FunPtr'.
--
-- /Note:/ this is valid only on architectures where data and function
-- pointers range over the same set of addresses, and should only be used
-- for bindings to external libraries whose interface already relies on
-- this assumption.
castPtrToFunPtr :: Ptr a -> FunPtr b
castPtrToFunPtr (Ptr addr) = FunPtr addr


------------------------------------------------------------------------
-- Show instances for Ptr and FunPtr
-- I have absolutely no idea why the WORD_SIZE_IN_BITS stuff is here

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
\end{code}

