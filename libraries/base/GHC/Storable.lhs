% -----------------------------------------------------------------------------
% $Id: Storable.lhs,v 1.3 2001/12/21 15:07:25 simonmar Exp $
%
% (c) The FFI task force, 2000
%

A class for primitive marshaling

\begin{code}
{-# OPTIONS -fno-implicit-prelude -monly-3-regs #-}

#include "MachDeps.h"

module GHC.Storable
	( Storable(
	     sizeOf,         -- :: a -> Int
	     alignment,      -- :: a -> Int
	     peekElemOff,    -- :: Ptr a -> Int      -> IO a
	     pokeElemOff,    -- :: Ptr a -> Int -> a -> IO ()
	     peekByteOff,    -- :: Ptr b -> Int      -> IO a
	     pokeByteOff,    -- :: Ptr b -> Int -> a -> IO ()
	     peek,           -- :: Ptr a             -> IO a
	     poke,           -- :: Ptr a        -> a -> IO ()
	     destruct)       -- :: Ptr a             -> IO ()
        ) where
\end{code}

\begin{code}
import Control.Monad		( liftM )
import Foreign.C.Types
import Foreign.C.TypesISO

#ifdef __GLASGOW_HASKELL__
import GHC.Stable	( StablePtr )
import GHC.Num
import GHC.Int
import GHC.Word
import GHC.Stable
import Foreign.Ptr
import GHC.Float
import GHC.Err
import GHC.IOBase
import GHC.Base
#endif
\end{code}

Primitive marshaling

Minimal complete definition: sizeOf, alignment, and one definition
in each of the peek/poke families.

\begin{code}
class Storable a where

   -- sizeOf/alignment *never* use their first argument
   sizeOf      :: a -> Int
   alignment   :: a -> Int

   -- replacement for read-/write???OffAddr
   peekElemOff :: Ptr a -> Int      -> IO a
   pokeElemOff :: Ptr a -> Int -> a -> IO ()

   -- the same with *byte* offsets
   peekByteOff :: Ptr b -> Int      -> IO a
   pokeByteOff :: Ptr b -> Int -> a -> IO ()

   -- ... and with no offsets at all
   peek        :: Ptr a      -> IO a
   poke        :: Ptr a -> a -> IO ()

   -- free memory associated with the object
   -- (except the object pointer itself)
   destruct    :: Ptr a -> IO ()

   -- circular default instances
   peekElemOff = peekElemOff_ undefined
      where peekElemOff_ :: a -> Ptr a -> Int -> IO a
            peekElemOff_ undef ptr off = peekByteOff ptr (off * sizeOf undef)
   pokeElemOff ptr off val = pokeByteOff ptr (off * sizeOf val) val

   peekByteOff ptr off = peek (ptr `plusPtr` off)
   pokeByteOff ptr off = poke (ptr `plusPtr` off)

   peek ptr = peekElemOff ptr 0
   poke ptr = pokeElemOff ptr 0

   destruct _ = return ()
\end{code}

System-dependent, but rather obvious instances

\begin{code}
instance Storable Bool where
   sizeOf _          = sizeOf (undefined::CInt)
   alignment _       = alignment (undefined::CInt)
   peekElemOff p i   = liftM (/= (0::CInt)) $ peekElemOff (castPtr p) i
   pokeElemOff p i x = pokeElemOff (castPtr p) i (if x then 1 else 0::CInt)

#define STORABLE(T,size,align,read,write)	\
instance Storable (T) where {			\
    sizeOf    _ = size;				\
    alignment _ = align;			\
    peekElemOff = read;				\
    pokeElemOff = write }

STORABLE(Char,SIZEOF_INT32,ALIGNMENT_INT32,
	 readWideCharOffPtr,writeWideCharOffPtr)

STORABLE(Int,SIZEOF_HSINT,ALIGNMENT_HSINT,
	 readIntOffPtr,writeIntOffPtr)

STORABLE(Word,SIZEOF_HSWORD,ALIGNMENT_HSWORD,
	 readWordOffPtr,writeWordOffPtr)

STORABLE((Ptr a),SIZEOF_HSPTR,ALIGNMENT_HSPTR,
	 readPtrOffPtr,writePtrOffPtr)

STORABLE((FunPtr a),SIZEOF_HSFUNPTR,ALIGNMENT_HSFUNPTR,
	 readFunPtrOffPtr,writeFunPtrOffPtr)

STORABLE((StablePtr a),SIZEOF_HSSTABLEPTR,ALIGNMENT_HSSTABLEPTR,
	 readStablePtrOffPtr,writeStablePtrOffPtr)

STORABLE(Float,SIZEOF_HSFLOAT,ALIGNMENT_HSFLOAT,
	 readFloatOffPtr,writeFloatOffPtr)

STORABLE(Double,SIZEOF_HSDOUBLE,ALIGNMENT_HSDOUBLE,
	 readDoubleOffPtr,writeDoubleOffPtr)

STORABLE(Word8,SIZEOF_WORD8,ALIGNMENT_WORD8,
	 readWord8OffPtr,writeWord8OffPtr)

STORABLE(Word16,SIZEOF_WORD16,ALIGNMENT_WORD16,
	 readWord16OffPtr,writeWord16OffPtr)

STORABLE(Word32,SIZEOF_WORD32,ALIGNMENT_WORD32,
	 readWord32OffPtr,writeWord32OffPtr)

STORABLE(Word64,SIZEOF_WORD64,ALIGNMENT_WORD64,
	 readWord64OffPtr,writeWord64OffPtr)

STORABLE(Int8,SIZEOF_INT8,ALIGNMENT_INT8,
	 readInt8OffPtr,writeInt8OffPtr)

STORABLE(Int16,SIZEOF_INT16,ALIGNMENT_INT16,
	 readInt16OffPtr,writeInt16OffPtr)

STORABLE(Int32,SIZEOF_INT32,ALIGNMENT_INT32,
	 readInt32OffPtr,writeInt32OffPtr)

STORABLE(Int64,SIZEOF_INT64,ALIGNMENT_INT64,
	 readInt64OffPtr,writeInt64OffPtr)

#define NSTORABLE(T) \
instance Storable T where { \
   sizeOf    (T x)       = sizeOf x ; \
   alignment (T x)       = alignment x ; \
   peekElemOff a i       = liftM T (peekElemOff (castPtr a) i) ; \
   pokeElemOff a i (T x) = pokeElemOff (castPtr a) i x }

NSTORABLE(CChar)
NSTORABLE(CSChar)
NSTORABLE(CUChar)
NSTORABLE(CShort)
NSTORABLE(CUShort)
NSTORABLE(CInt)
NSTORABLE(CUInt)
NSTORABLE(CLong)
NSTORABLE(CULong)
NSTORABLE(CLLong)
NSTORABLE(CULLong)
NSTORABLE(CFloat)
NSTORABLE(CDouble)
NSTORABLE(CLDouble)
NSTORABLE(CPtrdiff)
NSTORABLE(CSize)
NSTORABLE(CWchar)
NSTORABLE(CSigAtomic)
NSTORABLE(CClock)
NSTORABLE(CTime)
\end{code}

Helper functions

\begin{code}
#ifdef __GLASGOW_HASKELL__

readWideCharOffPtr  :: Ptr Char          -> Int -> IO Char
readIntOffPtr       :: Ptr Int           -> Int -> IO Int
readWordOffPtr      :: Ptr Word          -> Int -> IO Word
readPtrOffPtr       :: Ptr (Ptr a)       -> Int -> IO (Ptr a)
readFunPtrOffPtr    :: Ptr (FunPtr a)    -> Int -> IO (FunPtr a)
readFloatOffPtr     :: Ptr Float         -> Int -> IO Float
readDoubleOffPtr    :: Ptr Double        -> Int -> IO Double
readStablePtrOffPtr :: Ptr (StablePtr a) -> Int -> IO (StablePtr a)
readInt8OffPtr      :: Ptr Int8          -> Int -> IO Int8
readInt16OffPtr     :: Ptr Int16         -> Int -> IO Int16
readInt32OffPtr     :: Ptr Int32         -> Int -> IO Int32
readInt64OffPtr     :: Ptr Int64         -> Int -> IO Int64
readWord8OffPtr     :: Ptr Word8         -> Int -> IO Word8
readWord16OffPtr    :: Ptr Word16        -> Int -> IO Word16
readWord32OffPtr    :: Ptr Word32        -> Int -> IO Word32
readWord64OffPtr    :: Ptr Word64        -> Int -> IO Word64

readWideCharOffPtr (Ptr a) (I# i)
  = IO $ \s -> case readWideCharOffAddr# a i s  of (# s2, x #) -> (# s2, C# x #)
readIntOffPtr (Ptr a) (I# i)
  = IO $ \s -> case readIntOffAddr# a i s       of (# s2, x #) -> (# s2, I# x #)
readWordOffPtr (Ptr a) (I# i)
  = IO $ \s -> case readWordOffAddr# a i s      of (# s2, x #) -> (# s2, W# x #)
readPtrOffPtr (Ptr a) (I# i)
  = IO $ \s -> case readAddrOffAddr# a i s      of (# s2, x #) -> (# s2, Ptr x #)
readFunPtrOffPtr (Ptr a) (I# i)
  = IO $ \s -> case readAddrOffAddr# a i s      of (# s2, x #) -> (# s2, FunPtr x #)
readFloatOffPtr (Ptr a) (I# i)
  = IO $ \s -> case readFloatOffAddr# a i s     of (# s2, x #) -> (# s2, F# x #)
readDoubleOffPtr (Ptr a) (I# i)
  = IO $ \s -> case readDoubleOffAddr# a i s    of (# s2, x #) -> (# s2, D# x #)
readStablePtrOffPtr (Ptr a) (I# i)
  = IO $ \s -> case readStablePtrOffAddr# a i s of (# s2, x #) -> (# s2, StablePtr x #)
readInt8OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readInt8OffAddr# a i s      of (# s2, x #) -> (# s2, I8# x #)
readWord8OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readWord8OffAddr# a i s     of (# s2, x #) -> (# s2, W8# x #)
readInt16OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readInt16OffAddr# a i s     of (# s2, x #) -> (# s2, I16# x #)
readWord16OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readWord16OffAddr# a i s    of (# s2, x #) -> (# s2, W16# x #)
readInt32OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readInt32OffAddr# a i s     of (# s2, x #) -> (# s2, I32# x #)
readWord32OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readWord32OffAddr# a i s    of (# s2, x #) -> (# s2, W32# x #)
readInt64OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readInt64OffAddr# a i s     of (# s2, x #) -> (# s2, I64# x #)
readWord64OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readWord64OffAddr# a i s    of (# s2, x #) -> (# s2, W64# x #)

writeWideCharOffPtr  :: Ptr Char          -> Int -> Char        -> IO ()
writeIntOffPtr       :: Ptr Int           -> Int -> Int         -> IO ()
writeWordOffPtr      :: Ptr Word          -> Int -> Word        -> IO ()
writePtrOffPtr       :: Ptr (Ptr a)       -> Int -> Ptr a       -> IO ()
writeFunPtrOffPtr    :: Ptr (FunPtr a)    -> Int -> FunPtr a    -> IO ()
writeFloatOffPtr     :: Ptr Float         -> Int -> Float       -> IO ()
writeDoubleOffPtr    :: Ptr Double        -> Int -> Double      -> IO ()
writeStablePtrOffPtr :: Ptr (StablePtr a) -> Int -> StablePtr a -> IO ()
writeInt8OffPtr      :: Ptr Int8          -> Int -> Int8        -> IO ()
writeInt16OffPtr     :: Ptr Int16         -> Int -> Int16       -> IO ()
writeInt32OffPtr     :: Ptr Int32         -> Int -> Int32       -> IO ()
writeInt64OffPtr     :: Ptr Int64         -> Int -> Int64       -> IO ()
writeWord8OffPtr     :: Ptr Word8         -> Int -> Word8       -> IO ()
writeWord16OffPtr    :: Ptr Word16        -> Int -> Word16      -> IO ()
writeWord32OffPtr    :: Ptr Word32        -> Int -> Word32      -> IO ()
writeWord64OffPtr    :: Ptr Word64        -> Int -> Word64      -> IO ()

writeWideCharOffPtr (Ptr a) (I# i) (C# x)
  = IO $ \s -> case writeWideCharOffAddr# a i x s  of s2 -> (# s2, () #)
writeIntOffPtr (Ptr a) (I# i) (I# x)
  = IO $ \s -> case writeIntOffAddr# a i x s       of s2 -> (# s2, () #)
writeWordOffPtr (Ptr a) (I# i) (W# x)
  = IO $ \s -> case writeWordOffAddr# a i x s      of s2 -> (# s2, () #)
writePtrOffPtr (Ptr a) (I# i) (Ptr x)
  = IO $ \s -> case writeAddrOffAddr# a i x s      of s2 -> (# s2, () #)
writeFunPtrOffPtr (Ptr a) (I# i) (FunPtr x)
  = IO $ \s -> case writeAddrOffAddr# a i x s      of s2 -> (# s2, () #)
writeFloatOffPtr (Ptr a) (I# i) (F# x)
  = IO $ \s -> case writeFloatOffAddr# a i x s     of s2 -> (# s2, () #)
writeDoubleOffPtr (Ptr a) (I# i) (D# x)
  = IO $ \s -> case writeDoubleOffAddr# a i x s    of s2 -> (# s2, () #)
writeStablePtrOffPtr (Ptr a) (I# i) (StablePtr x)
  = IO $ \s -> case writeStablePtrOffAddr# a i x s of s2 -> (# s2 , () #)
writeInt8OffPtr (Ptr a) (I# i) (I8# x)
  = IO $ \s -> case writeInt8OffAddr# a i x s      of s2 -> (# s2, () #)
writeWord8OffPtr (Ptr a) (I# i) (W8# x)
  = IO $ \s -> case writeWord8OffAddr# a i x s     of s2 -> (# s2, () #)
writeInt16OffPtr (Ptr a) (I# i) (I16# x)
  = IO $ \s -> case writeInt16OffAddr# a i x s     of s2 -> (# s2, () #)
writeWord16OffPtr (Ptr a) (I# i) (W16# x)
  = IO $ \s -> case writeWord16OffAddr# a i x s    of s2 -> (# s2, () #)
writeInt32OffPtr (Ptr a) (I# i) (I32# x)
  = IO $ \s -> case writeInt32OffAddr# a i x s     of s2 -> (# s2, () #)
writeWord32OffPtr (Ptr a) (I# i) (W32# x)
  = IO $ \s -> case writeWord32OffAddr# a i x s    of s2 -> (# s2, () #)
writeInt64OffPtr (Ptr a) (I# i) (I64# x)
  = IO $ \s -> case writeInt64OffAddr# a i x s     of s2 -> (# s2, () #)
writeWord64OffPtr (Ptr a) (I# i) (W64# x)
  = IO $ \s -> case writeWord64OffAddr# a i x s    of s2 -> (# s2, () #)

#endif /* __GLASGOW_HASKELL__ */
\end{code}
