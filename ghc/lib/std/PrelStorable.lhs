% -----------------------------------------------------------------------------
% $Id: PrelStorable.lhs,v 1.2 2001/02/05 11:49:20 chak Exp $
%
% (c) The FFI task force, 2000
%

A class for primitive marshaling

\begin{code}
#include "MachDeps.h"

module PrelStorable
	( Storable(
	     sizeOf,         -- :: a -> Int
	     alignment,      -- :: a -> Int
	     peekElemOff,    -- :: Ptr a -> Int      -> IO a
	     pokeElemOff,    -- :: Ptr a -> Int -> a -> IO ()
	     peekByteOff,    -- :: Ptr b -> Int      -> IO a
	     pokeByteOff,    -- :: Ptr b -> Int -> a -> IO ()
	     peek,           -- :: Ptr a             -> IO a
	     poke)           -- :: Ptr a        -> a -> IO ()
        ) where
\end{code}

\begin{code}
import Char		( chr, ord )
import Monad		( liftM )

#ifdef __GLASGOW_HASKELL__
import PrelStable	( StablePtr )
import PrelInt
import PrelWord
import PrelCTypes
import PrelCTypesISO
import PrelStable
import PrelPtr
import PrelFloat
import PrelIOBase
import PrelBase
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

   -- circular default instances
   peekElemOff = peekElemOff_ undefined
      where peekElemOff_ :: a -> Ptr a -> Int -> IO a
            peekElemOff_ undef ptr off = peekByteOff ptr (off * sizeOf undef)
   pokeElemOff ptr off val = pokeByteOff ptr (off * sizeOf val) val

   peekByteOff ptr off = peek (ptr `plusPtr` off)
   pokeByteOff ptr off = poke (ptr `plusPtr` off)

   peek ptr = peekElemOff ptr 0
   poke ptr = pokeElemOff ptr 0
\end{code}

System-dependent, but rather obvious instances

\begin{code}
instance Storable Char where
   sizeOf _          = sizeOf (undefined::Word32)
   alignment _       = alignment (undefined::Word32)
   peekElemOff p i   = liftM (chr . fromIntegral) $ peekElemOff (castPtr p::Ptr Word32) i
   pokeElemOff p i x = pokeElemOff (castPtr p::Ptr Word32) i (fromIntegral (ord x))

instance Storable Bool where
   sizeOf _          = sizeOf (undefined::CInt)
   alignment _       = alignment (undefined::CInt)
   peekElemOff p i   = liftM (/= (0::CInt)) $ peekElemOff (castPtr p) i
   pokeElemOff p i x = pokeElemOff (castPtr p) i (if x then 1 else 0::CInt)

instance Storable (FunPtr a) where
   sizeOf          (FunPtr x) = sizeOf x
   alignment       (FunPtr x) = alignment x
   peekElemOff p i            = liftM FunPtr $ peekElemOff (castPtr p) i
   pokeElemOff p i (FunPtr x) = pokeElemOff (castPtr p) i x

#define STORABLE(T,size,align,read,write)		\
instance Storable (T) where {				\
    sizeOf    _       = size;				\
    alignment _       = align;				\
    peekElemOff a i   = read a i;			\
    pokeElemOff a i x = write a i x }

STORABLE(Int,SIZEOF_INT,ALIGNMENT_INT,
	 readIntOffPtr,writeIntOffPtr)

STORABLE((Ptr a),SIZEOF_VOID_P,ALIGNMENT_VOID_P,
	 readPtrOffPtr,writePtrOffPtr)

STORABLE((StablePtr a),SIZEOF_VOID_P,ALIGNMENT_VOID_P,
	 readStablePtrOffPtr,writeStablePtrOffPtr)

STORABLE(Float,SIZEOF_FLOAT,ALIGNMENT_FLOAT,
	 readFloatOffPtr,writeFloatOffPtr)

STORABLE(Double,SIZEOF_DOUBLE,ALIGNMENT_DOUBLE,
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

readIntOffPtr         :: Ptr Int           -> Int -> IO Int
readPtrOffPtr         :: Ptr (Ptr a)       -> Int -> IO (Ptr a)
readFloatOffPtr       :: Ptr Float         -> Int -> IO Float
readDoubleOffPtr      :: Ptr Double        -> Int -> IO Double
readStablePtrOffPtr   :: Ptr (StablePtr a) -> Int -> IO (StablePtr a)
readInt8OffPtr        :: Ptr Int8          -> Int -> IO Int8
readInt16OffPtr       :: Ptr Int16         -> Int -> IO Int16
readInt32OffPtr       :: Ptr Int32         -> Int -> IO Int32
readInt64OffPtr       :: Ptr Int64         -> Int -> IO Int64
readWord8OffPtr       :: Ptr Word8         -> Int -> IO Word8
readWord16OffPtr      :: Ptr Word16        -> Int -> IO Word16
readWord32OffPtr      :: Ptr Word32        -> Int -> IO Word32
readWord64OffPtr      :: Ptr Word64        -> Int -> IO Word64

readIntOffPtr (Ptr a) (I# i)
  = IO $ \s -> case readIntOffAddr# a i s        of { (# s,x #) -> (# s, I# x #) }
readPtrOffPtr (Ptr a) (I# i)
  = IO $ \s -> case readAddrOffAddr# a i s       of { (# s,x #) -> (# s, Ptr x #) }
readFloatOffPtr (Ptr a) (I# i)
  = IO $ \s -> case readFloatOffAddr# a i s      of { (# s,x #) -> (# s, F# x #) }
readDoubleOffPtr (Ptr a) (I# i)
  = IO $ \s -> case readDoubleOffAddr# a i s     of { (# s,x #) -> (# s, D# x #) }
readStablePtrOffPtr (Ptr a) (I# i)
  = IO $ \s -> case readStablePtrOffAddr# a i s  of { (# s,x #) -> (# s, StablePtr x #) }

readInt8OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readInt8OffAddr# a i s of (# s, w #) -> (# s, I8# w #)

readInt16OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readInt16OffAddr# a i s of (# s, w #) -> (# s, I16# w #)

readInt32OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readInt32OffAddr# a i s of (# s, w #) -> (# s, I32# w #)

#if WORD_SIZE_IN_BYTES == 8
readInt64OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readIntOffAddr# a i s of (# s, w #) -> (# s, I64# w #)
#else
readInt64OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readInt64OffAddr# a i s of (# s, w #) -> (# s, I64# w #)
#endif


writeIntOffPtr        :: Ptr Int            -> Int -> Int          -> IO ()
writePtrOffPtr        :: Ptr (Ptr a)        -> Int -> Ptr a        -> IO ()
writeFloatOffPtr      :: Ptr Float          -> Int -> Float        -> IO ()
writeDoubleOffPtr     :: Ptr Double         -> Int -> Double       -> IO ()
writeStablePtrOffPtr  :: Ptr (StablePtr a)  -> Int -> StablePtr a  -> IO ()
writeInt8OffPtr       :: Ptr Int8           -> Int -> Int8         -> IO ()
writeInt16OffPtr      :: Ptr Int16          -> Int -> Int16        -> IO ()
writeInt32OffPtr      :: Ptr Int32          -> Int -> Int32        -> IO ()
writeInt64OffPtr      :: Ptr Int64          -> Int -> Int64        -> IO ()
writeWord8OffPtr      :: Ptr Word8          -> Int -> Word8        -> IO ()
writeWord16OffPtr     :: Ptr Word16         -> Int -> Word16       -> IO ()
writeWord32OffPtr     :: Ptr Word32         -> Int -> Word32       -> IO ()
writeWord64OffPtr     :: Ptr Word64         -> Int -> Word64       -> IO ()

writeIntOffPtr (Ptr a#) (I# i#) (I# e#) = IO $ \ s# ->
      case (writeIntOffAddr#  a# i# e# s#) of s2# -> (# s2#, () #)

writePtrOffPtr (Ptr a#) (I# i#) (Ptr e#) = IO $ \ s# ->
      case (writeAddrOffAddr#  a# i# e# s#) of s2# -> (# s2#, () #)

writeFloatOffPtr (Ptr a#) (I# i#) (F# e#) = IO $ \ s# ->
      case (writeFloatOffAddr#  a# i# e# s#) of s2# -> (# s2#, () #)

writeDoubleOffPtr (Ptr a#) (I# i#) (D# e#) = IO $ \ s# ->
      case (writeDoubleOffAddr#  a# i# e# s#) of s2# -> (# s2#, () #)

writeStablePtrOffPtr (Ptr a#) (I# i#) (StablePtr e#) = IO $ \ s# ->
      case (writeStablePtrOffAddr#  a# i# e# s#) of s2# -> (# s2# , () #)

writeInt8OffPtr (Ptr a#) (I# i#) (I8# w#) = IO $ \ s# ->
      case (writeInt8OffAddr# a# i# w# s#) of s2# -> (# s2#, () #)

writeInt16OffPtr (Ptr a#) (I# i#) (I16# w#) = IO $ \ s# ->
      case (writeInt16OffAddr# a# i# w# s#) of s2# -> (# s2#, () #)

writeInt32OffPtr (Ptr a#) (I# i#) (I32# w#) = IO $ \ s# ->
      case (writeInt32OffAddr# a# i# w# s#) of s2# -> (# s2#, () #)

#if WORD_SIZE_IN_BYTES == 8
writeInt64OffPtr (Ptr a#) (I# i#) (I64# w#) = IO $ \ s# ->
      case (writeIntOffAddr#  a# i# w# s#) of s2# -> (# s2#, () #)
#else
writeInt64OffPtr (Ptr a#) (I# i#) (I64# w#) = IO $ \ s# ->
      case (writeInt64OffAddr#  a# i# w# s#) of s2# -> (# s2#, () #)
#endif

readWord8OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readWord8OffAddr# a i s of (# s, w #) -> (# s, W8# w #)

readWord16OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readWord16OffAddr# a i s of (# s, w #) -> (# s, W16# w #)

readWord32OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readWord32OffAddr# a i s of (# s, w #) -> (# s, W32# w #)

#if WORD_SIZE_IN_BYTES == 8
readWord64OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readWordOffAddr# a i s of (# s, w #) -> (# s, W64# w #)
#else
readWord64OffPtr (Ptr a) (I# i)
  = IO $ \s -> case readWord64OffAddr# a i s of (# s, w #) -> (# s, W64# w #)
#endif

writeWord8OffPtr (Ptr a#) (I# i#) (W8# w#) = IO $ \ s# ->
      case (writeWord8OffAddr# a# i# w# s#) of s2# -> (# s2#, () #)

writeWord16OffPtr (Ptr a#) (I# i#) (W16# w#) = IO $ \ s# ->
      case (writeWord16OffAddr# a# i# w# s#) of s2# -> (# s2#, () #)

writeWord32OffPtr (Ptr a#) (I# i#) (W32# w#) = IO $ \ s# ->
      case (writeWord32OffAddr# a# i# w# s#) of s2# -> (# s2#, () #)

#if WORD_SIZE_IN_BYTES == 8
writeWord64OffPtr (Ptr a#) (I# i#) (W64# w#) = IO $ \ s# ->
      case (writeWordOffAddr#  a# i# w# s#) of s2# -> (# s2#, () #)
#else
writeWord64OffPtr (Ptr a#) (I# i#) (W64# w#) = IO $ \ s# ->
      case (writeWord64OffAddr#  a# i# w# s#) of s2# -> (# s2#, () #)
#endif

#endif /* __GLASGOW_HASKELL__ */
\end{code}
