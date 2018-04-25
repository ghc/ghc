{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, ScopedTypeVariables, BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Storable
-- Copyright   :  (c) The FFI task force 2001
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The module "Foreign.Storable" provides most elementary support for
-- marshalling and is part of the language-independent portion of the
-- Foreign Function Interface (FFI), and will normally be imported via
-- the "Foreign" module.
--
-----------------------------------------------------------------------------

module Foreign.Storable
        ( Storable(
             sizeOf,
             alignment,
             peekElemOff,
             pokeElemOff,
             peekByteOff,
             pokeByteOff,
             peek,
             poke)
        ) where


#include "MachDeps.h"
#include "HsBaseConfig.h"

import GHC.Storable
import GHC.Stable       ( StablePtr )
import GHC.Num
import GHC.Int
import GHC.Word
import GHC.Ptr
import GHC.Base
import GHC.Fingerprint.Type
import Data.Bits
import GHC.Real

{- |
The member functions of this class facilitate writing values of
primitive types to raw memory (which may have been allocated with the
above mentioned routines) and reading values from blocks of raw
memory.  The class, furthermore, includes support for computing the
storage requirements and alignment restrictions of storable types.

Memory addresses are represented as values of type @'Ptr' a@, for some
@a@ which is an instance of class 'Storable'.  The type argument to
'Ptr' helps provide some valuable type safety in FFI code (you can\'t
mix pointers of different types without an explicit cast), while
helping the Haskell type system figure out which marshalling method is
needed for a given pointer.

All marshalling between Haskell and a foreign language ultimately
boils down to translating Haskell data structures into the binary
representation of a corresponding data structure of the foreign
language and vice versa.  To code this marshalling in Haskell, it is
necessary to manipulate primitive data types stored in unstructured
memory blocks.  The class 'Storable' facilitates this manipulation on
all types for which it is instantiated, which are the standard basic
types of Haskell, the fixed size @Int@ types ('Int8', 'Int16',
'Int32', 'Int64'), the fixed size @Word@ types ('Word8', 'Word16',
'Word32', 'Word64'), 'StablePtr', all types from "Foreign.C.Types",
as well as 'Ptr'.
-}

class Storable a where
   {-# MINIMAL sizeOf, alignment,
               (peek | peekElemOff | peekByteOff),
               (poke | pokeElemOff | pokeByteOff) #-}

   sizeOf      :: a -> Int
   -- ^ Computes the storage requirements (in bytes) of the argument.
   -- The value of the argument is not used.

   alignment   :: a -> Int
   -- ^ Computes the alignment constraint of the argument.  An
   -- alignment constraint @x@ is fulfilled by any address divisible
   -- by @x@.  The value of the argument is not used.

   peekElemOff :: Ptr a -> Int      -> IO a
   -- ^       Read a value from a memory area regarded as an array
   --         of values of the same kind.  The first argument specifies
   --         the start address of the array and the second the index into
   --         the array (the first element of the array has index
   --         @0@).  The following equality holds,
   -- 
   -- > peekElemOff addr idx = IOExts.fixIO $ \result ->
   -- >   peek (addr `plusPtr` (idx * sizeOf result))
   --
   --         Note that this is only a specification, not
   --         necessarily the concrete implementation of the
   --         function.

   pokeElemOff :: Ptr a -> Int -> a -> IO ()
   -- ^       Write a value to a memory area regarded as an array of
   --         values of the same kind.  The following equality holds:
   -- 
   -- > pokeElemOff addr idx x = 
   -- >   poke (addr `plusPtr` (idx * sizeOf x)) x

   peekByteOff :: Ptr b -> Int      -> IO a
   -- ^       Read a value from a memory location given by a base
   --         address and offset.  The following equality holds:
   --
   -- > peekByteOff addr off = peek (addr `plusPtr` off)

   pokeByteOff :: Ptr b -> Int -> a -> IO ()
   -- ^       Write a value to a memory location given by a base
   --         address and offset.  The following equality holds:
   --
   -- > pokeByteOff addr off x = poke (addr `plusPtr` off) x
  
   peek        :: Ptr a      -> IO a
   -- ^ Read a value from the given memory location.
   --
   --  Note that the peek and poke functions might require properly
   --  aligned addresses to function correctly.  This is architecture
   --  dependent; thus, portable code should ensure that when peeking or
   --  poking values of some type @a@, the alignment
   --  constraint for @a@, as given by the function
   --  'alignment' is fulfilled.

   poke        :: Ptr a -> a -> IO ()
   -- ^ Write the given value to the given memory location.  Alignment
   -- restrictions might apply; see 'peek'.
 
   -- circular default instances
   peekElemOff = peekElemOff_ undefined
      where peekElemOff_ :: a -> Ptr a -> Int -> IO a
            peekElemOff_ undef ptr off = peekByteOff ptr (off * sizeOf undef)
   pokeElemOff ptr off val = pokeByteOff ptr (off * sizeOf val) val

   peekByteOff ptr off = peek (ptr `plusPtr` off)
   pokeByteOff ptr off = poke (ptr `plusPtr` off)

   peek ptr = peekElemOff ptr 0
   poke ptr = pokeElemOff ptr 0

-- | @since 4.9.0.0
instance Storable () where
  sizeOf _ = 0
  alignment _ = 1
  peek _ = return ()
  poke _ _ = return ()

-- System-dependent, but rather obvious instances

-- | @since 2.01
instance Storable Bool where
   sizeOf _          = sizeOf (undefined::HTYPE_INT)
   alignment _       = alignment (undefined::HTYPE_INT)
   peekElemOff p i   = liftM (/= (0::HTYPE_INT)) $ peekElemOff (castPtr p) i
   pokeElemOff p i x = pokeElemOff (castPtr p) i (if x then 1 else 0::HTYPE_INT)

#define STORABLE(T,size,align,read,write)       \
instance Storable (T) where {                   \
    sizeOf    _ = size;                         \
    alignment _ = align;                        \
    peekElemOff = read;                         \
    pokeElemOff = write }

-- | @since 2.01
STORABLE(Char,SIZEOF_INT32,ALIGNMENT_INT32,
         readWideCharOffPtr,writeWideCharOffPtr)

-- | @since 2.01
STORABLE(Int,SIZEOF_HSINT,ALIGNMENT_HSINT,
         readIntOffPtr,writeIntOffPtr)

-- | @since 2.01
STORABLE(Word,SIZEOF_HSWORD,ALIGNMENT_HSWORD,
         readWordOffPtr,writeWordOffPtr)

-- | @since 2.01
STORABLE((Ptr a),SIZEOF_HSPTR,ALIGNMENT_HSPTR,
         readPtrOffPtr,writePtrOffPtr)

-- | @since 2.01
STORABLE((FunPtr a),SIZEOF_HSFUNPTR,ALIGNMENT_HSFUNPTR,
         readFunPtrOffPtr,writeFunPtrOffPtr)

-- | @since 2.01
STORABLE((StablePtr a),SIZEOF_HSSTABLEPTR,ALIGNMENT_HSSTABLEPTR,
         readStablePtrOffPtr,writeStablePtrOffPtr)

-- | @since 2.01
STORABLE(Float,SIZEOF_HSFLOAT,ALIGNMENT_HSFLOAT,
         readFloatOffPtr,writeFloatOffPtr)

-- | @since 2.01
STORABLE(Double,SIZEOF_HSDOUBLE,ALIGNMENT_HSDOUBLE,
         readDoubleOffPtr,writeDoubleOffPtr)

-- | @since 2.01
STORABLE(Word8,SIZEOF_WORD8,ALIGNMENT_WORD8,
         readWord8OffPtr,writeWord8OffPtr)

-- | @since 2.01
STORABLE(Word16,SIZEOF_WORD16,ALIGNMENT_WORD16,
         readWord16OffPtr,writeWord16OffPtr)

-- | @since 2.01
STORABLE(Word32,SIZEOF_WORD32,ALIGNMENT_WORD32,
         readWord32OffPtr,writeWord32OffPtr)

-- | @since 2.01
STORABLE(Word64,SIZEOF_WORD64,ALIGNMENT_WORD64,
         readWord64OffPtr,writeWord64OffPtr)

-- | @since 2.01
STORABLE(Int8,SIZEOF_INT8,ALIGNMENT_INT8,
         readInt8OffPtr,writeInt8OffPtr)

-- | @since 2.01
STORABLE(Int16,SIZEOF_INT16,ALIGNMENT_INT16,
         readInt16OffPtr,writeInt16OffPtr)

-- | @since 2.01
STORABLE(Int32,SIZEOF_INT32,ALIGNMENT_INT32,
         readInt32OffPtr,writeInt32OffPtr)

-- | @since 2.01
STORABLE(Int64,SIZEOF_INT64,ALIGNMENT_INT64,
         readInt64OffPtr,writeInt64OffPtr)

-- | @since 4.8.0.0
instance (Storable a, Integral a) => Storable (Ratio a) where
    sizeOf _    = 2 * sizeOf (undefined :: a)
    alignment _ = alignment (undefined :: a )
    peek p           = do
                        q <- return $ castPtr p
                        r <- peek q
                        i <- peekElemOff q 1
                        return (r % i)
    poke p (r :% i)  = do
                        q <-return $  (castPtr p)
                        poke q r
                        pokeElemOff q 1 i

-- XXX: here to avoid orphan instance in GHC.Fingerprint
-- | @since 4.4.0.0
instance Storable Fingerprint where
  sizeOf _ = 16
  alignment _ = 8
  peek = peekFingerprint
  poke = pokeFingerprint

-- peek/poke in fixed BIG-endian 128-bit format
peekFingerprint :: Ptr Fingerprint -> IO Fingerprint
peekFingerprint p0 = do
      let peekW64 :: Ptr Word8 -> Int -> Word64 -> IO Word64
          peekW64 _  0  !i = return i
          peekW64 !p !n !i = do
                w8 <- peek p
                peekW64 (p `plusPtr` 1) (n-1) 
                    ((i `shiftL` 8) .|. fromIntegral w8)

      high <- peekW64 (castPtr p0) 8 0
      low  <- peekW64 (castPtr p0 `plusPtr` 8) 8 0
      return (Fingerprint high low)

pokeFingerprint :: Ptr Fingerprint -> Fingerprint -> IO ()
pokeFingerprint p0 (Fingerprint high low) = do
      let pokeW64 :: Ptr Word8 -> Int -> Word64 -> IO ()
          pokeW64 _ 0  _  = return ()
          pokeW64 p !n !i = do
                pokeElemOff p (n-1) (fromIntegral i)
                pokeW64 p (n-1) (i `shiftR` 8)

      pokeW64 (castPtr p0) 8 high
      pokeW64 (castPtr p0 `plusPtr` 8) 8 low
