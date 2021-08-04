{-# LANGUAGE CPP, UnboxedTuples, MagicHash, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DeriveGeneric #-}
#endif

#include "HsBaseConfig.h"

-- |
-- Module      : Data.Primitive.Types
-- Copyright   : (c) Roman Leshchinskiy 2009-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Basic types and classes for primitive array operations
--

module Data.Primitive.Types (
  Prim(..)
  ,sizeOf, alignment, defaultSetByteArray#, defaultSetOffAddr#
  ,PrimStorable(..)
  ,Ptr(..)
) where

import Control.Monad.Primitive
import Data.Primitive.MachDeps
import Data.Primitive.Internal.Operations
import Foreign.Ptr (IntPtr,intPtrToPtr,ptrToIntPtr)
import Foreign.Ptr (WordPtr,wordPtrToPtr,ptrToWordPtr)
import Foreign.C.Types
import System.Posix.Types

import GHC.Base (
    Int(..), Char(..),
  )
import GHC.Float (
    Float(..), Double(..)
  )
import GHC.Word (
    Word(..), Word8(..), Word16(..), Word32(..), Word64(..)
  )
import GHC.Int (
    Int8(..), Int16(..), Int32(..), Int64(..)
  )

import GHC.Ptr (
    Ptr(..), FunPtr(..)
  )
import GHC.Stable (
    StablePtr(..)
  )

import GHC.Exts
#if __GLASGOW_HASKELL__ >= 706
    hiding (setByteArray#)
#endif


import Data.Primitive.Internal.Compat ( isTrue# )
import Foreign.Storable (Storable)


import qualified Foreign.Storable as FS

#if __GLASGOW_HASKELL__ >= 710
import GHC.IO (IO(..))
import qualified GHC.Exts
#endif


import Control.Applicative (Const(..))
#if MIN_VERSION_base(4,8,0)
import Data.Functor.Identity (Identity(..))
import qualified Data.Monoid as Monoid
#endif
#if MIN_VERSION_base(4,6,0)
import Data.Ord (Down(..))
#else
import GHC.Exts (Down(..))
#endif
#if MIN_VERSION_base(4,9,0)
import qualified Data.Semigroup as Semigroup
#endif

-- | Class of types supporting primitive array operations. This includes
-- interfacing with GC-managed memory (functions suffixed with @ByteArray#@)
-- and interfacing with unmanaged memory (functions suffixed with @Addr#@).
-- Endianness is platform-dependent.
class Prim a where

  -- | Size of values of type @a@. The argument is not used.
  sizeOf#    :: a -> Int#

  -- | Alignment of values of type @a@. The argument is not used.
  alignment# :: a -> Int#

  -- | Read a value from the array. The offset is in elements of type
  -- @a@ rather than in bytes.
  indexByteArray# :: ByteArray# -> Int# -> a

  -- | Read a value from the mutable array. The offset is in elements of type
  -- @a@ rather than in bytes.
  readByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)

  -- | Write a value to the mutable array. The offset is in elements of type
  -- @a@ rather than in bytes.
  writeByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s

  -- | Fill a slice of the mutable array with a value. The offset and length
  -- of the chunk are in elements of type @a@ rather than in bytes.
  setByteArray# :: MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s

  -- | Read a value from a memory position given by an address and an offset.
  -- The memory block the address refers to must be immutable. The offset is in
  -- elements of type @a@ rather than in bytes.
  indexOffAddr# :: Addr# -> Int# -> a

  -- | Read a value from a memory position given by an address and an offset.
  -- The offset is in elements of type @a@ rather than in bytes.
  readOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, a #)

  -- | Write a value to a memory position given by an address and an offset.
  -- The offset is in elements of type @a@ rather than in bytes.
  writeOffAddr# :: Addr# -> Int# -> a -> State# s -> State# s

  -- | Fill a memory block given by an address, an offset and a length.
  -- The offset and length are in elements of type @a@ rather than in bytes.
  setOffAddr# :: Addr# -> Int# -> Int# -> a -> State# s -> State# s

-- | Size of values of type @a@. The argument is not used.
--
-- This function has existed since 0.1, but was moved from 'Data.Primitive'
-- to 'Data.Primitive.Types' in version 0.6.3.0
sizeOf :: Prim a => a -> Int
sizeOf x = I# (sizeOf# x)

-- | Alignment of values of type @a@. The argument is not used.
--
-- This function has existed since 0.1, but was moved from 'Data.Primitive'
-- to 'Data.Primitive.Types' in version 0.6.3.0
alignment :: Prim a => a -> Int
alignment x = I# (alignment# x)

-- | An implementation of 'setByteArray#' that calls 'writeByteArray#'
-- to set each element. This is helpful when writing a 'Prim' instance
-- for a multi-word data type for which there is no cpu-accelerated way
-- to broadcast a value to contiguous memory. It is typically used
-- alongside 'defaultSetOffAddr#'. For example:
--
-- > data Trip = Trip Int Int Int
-- >
-- > instance Prim Trip
-- >   sizeOf# _ = 3# *# sizeOf# (undefined :: Int)
-- >   alignment# _ = alignment# (undefined :: Int)
-- >   indexByteArray# arr# i# = ...
-- >   readByteArray# arr# i# = ...
-- >   writeByteArray# arr# i# (Trip a b c) =
-- >     \s0 -> case writeByteArray# arr# (3# *# i#) a s0 of
-- >        s1 -> case writeByteArray# arr# ((3# *# i#) +# 1#) b s1 of
-- >          s2 -> case writeByteArray# arr# ((3# *# i#) +# 2# ) c s2 of
-- >            s3 -> s3
-- >   setByteArray# = defaultSetByteArray#
-- >   indexOffAddr# addr# i# = ...
-- >   readOffAddr# addr# i# = ...
-- >   writeOffAddr# addr# i# (Trip a b c) =
-- >     \s0 -> case writeOffAddr# addr# (3# *# i#) a s0 of
-- >        s1 -> case writeOffAddr# addr# ((3# *# i#) +# 1#) b s1 of
-- >          s2 -> case writeOffAddr# addr# ((3# *# i#) +# 2# ) c s2 of
-- >            s3 -> s3
-- >   setOffAddr# = defaultSetOffAddr#
defaultSetByteArray# :: Prim a => MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
defaultSetByteArray# arr# i# len# ident = go 0#
  where
  go ix# s0 = if isTrue# (ix# <# len#)
    then case writeByteArray# arr# (i# +# ix#) ident s0 of
      s1 -> go (ix# +# 1#) s1
    else s0

-- | An implementation of 'setOffAddr#' that calls 'writeOffAddr#'
-- to set each element. The documentation of 'defaultSetByteArray#'
-- provides an example of how to use this.
defaultSetOffAddr# :: Prim a => Addr# -> Int# -> Int# -> a -> State# s -> State# s
defaultSetOffAddr# addr# i# len# ident = go 0#
  where
  go ix# s0 = if isTrue# (ix# <# len#)
    then case writeOffAddr# addr# (i# +# ix#) ident s0 of
      s1 -> go (ix# +# 1#) s1
    else s0

-- | Newtype that uses a 'Prim' instance to give rise to a 'Storable' instance.
-- This type is intended to be used with the @DerivingVia@ extension available
-- in GHC 8.6 and up. For example, consider a user-defined 'Prim' instance for
-- a multi-word data type.
--
-- > data Uuid = Uuid Word64 Word64
-- >   deriving Storable via (PrimStorable Uuid)
-- > instance Prim Uuid where ...
--
-- Writing the 'Prim' instance is tedious and unavoidable, but the 'Storable'
-- instance comes for free once the 'Prim' instance is written.
newtype PrimStorable a = PrimStorable { getPrimStorable :: a }

instance Prim a => Storable (PrimStorable a) where
  sizeOf _ = sizeOf (undefined :: a)
  alignment _ = alignment (undefined :: a)
  peekElemOff (Ptr addr#) (I# i#) =
    primitive $ \s0# -> case readOffAddr# addr# i# s0# of
      (# s1, x #) -> (# s1, PrimStorable x #)
  pokeElemOff (Ptr addr#) (I# i#) (PrimStorable a) = primitive_ $ \s# ->
    writeOffAddr# addr# i# a s#

#define derivePrim(ty, ctr, sz, align, idx_arr, rd_arr, wr_arr, set_arr, idx_addr, rd_addr, wr_addr, set_addr) \
instance Prim (ty) where {                                      \
  sizeOf# _ = unI# sz                                           \
; alignment# _ = unI# align                                     \
; indexByteArray# arr# i# = ctr (idx_arr arr# i#)               \
; readByteArray#  arr# i# s# = case rd_arr arr# i# s# of        \
                        { (# s1#, x# #) -> (# s1#, ctr x# #) }  \
; writeByteArray# arr# i# (ctr x#) s# = wr_arr arr# i# x# s#    \
; setByteArray# arr# i# n# (ctr x#) s#                          \
    = let { i = fromIntegral (I# i#)                            \
          ; n = fromIntegral (I# n#)                            \
          } in                                                  \
      case unsafeCoerce# (internal (set_arr arr# i n x#)) s# of \
        { (# s1#, _ #) -> s1# }                                 \
                                                                \
; indexOffAddr# addr# i# = ctr (idx_addr addr# i#)              \
; readOffAddr#  addr# i# s# = case rd_addr addr# i# s# of       \
                        { (# s1#, x# #) -> (# s1#, ctr x# #) }  \
; writeOffAddr# addr# i# (ctr x#) s# = wr_addr addr# i# x# s#   \
; setOffAddr# addr# i# n# (ctr x#) s#                           \
    = let { i = fromIntegral (I# i#)                            \
          ; n = fromIntegral (I# n#)                            \
          } in                                                  \
      case unsafeCoerce# (internal (set_addr addr# i n x#)) s# of \
        { (# s1#, _ #) -> s1# }                                 \
; {-# INLINE sizeOf# #-}                                        \
; {-# INLINE alignment# #-}                                     \
; {-# INLINE indexByteArray# #-}                                \
; {-# INLINE readByteArray# #-}                                 \
; {-# INLINE writeByteArray# #-}                                \
; {-# INLINE setByteArray# #-}                                  \
; {-# INLINE indexOffAddr# #-}                                  \
; {-# INLINE readOffAddr# #-}                                   \
; {-# INLINE writeOffAddr# #-}                                  \
; {-# INLINE setOffAddr# #-}                                    \
}

#if __GLASGOW_HASKELL__ >= 710
liberate# :: State# s -> State# r
liberate# = unsafeCoerce#
shimmedSetWord8Array# :: MutableByteArray# s -> Int -> Int -> Word# -> IO ()
shimmedSetWord8Array# m (I# off) (I# len) w = IO (\s -> (# liberate# (GHC.Exts.setByteArray# m off len (GHC.Exts.word2Int# w) (liberate# s)), () #))
shimmedSetInt8Array# :: MutableByteArray# s -> Int -> Int -> Int# -> IO ()
shimmedSetInt8Array# m (I# off) (I# len) i = IO (\s -> (# liberate# (GHC.Exts.setByteArray# m off len i (liberate# s)), () #))
#else
shimmedSetWord8Array# :: MutableByteArray# s -> CPtrdiff -> CSize -> Word# -> IO ()
shimmedSetWord8Array# = setWord8Array#
shimmedSetInt8Array# :: MutableByteArray# s -> CPtrdiff -> CSize -> Int# -> IO ()
shimmedSetInt8Array# = setInt8Array#
#endif

unI# :: Int -> Int#
unI# (I# n#) = n#

derivePrim(Word, W#, sIZEOF_WORD, aLIGNMENT_WORD,
           indexWordArray#, readWordArray#, writeWordArray#, setWordArray#,
           indexWordOffAddr#, readWordOffAddr#, writeWordOffAddr#, setWordOffAddr#)
derivePrim(Word8, W8#, sIZEOF_WORD8, aLIGNMENT_WORD8,
           indexWord8Array#, readWord8Array#, writeWord8Array#, shimmedSetWord8Array#,
           indexWord8OffAddr#, readWord8OffAddr#, writeWord8OffAddr#, setWord8OffAddr#)
derivePrim(Word16, W16#, sIZEOF_WORD16, aLIGNMENT_WORD16,
           indexWord16Array#, readWord16Array#, writeWord16Array#, setWord16Array#,
           indexWord16OffAddr#, readWord16OffAddr#, writeWord16OffAddr#, setWord16OffAddr#)
derivePrim(Word32, W32#, sIZEOF_WORD32, aLIGNMENT_WORD32,
           indexWord32Array#, readWord32Array#, writeWord32Array#, setWord32Array#,
           indexWord32OffAddr#, readWord32OffAddr#, writeWord32OffAddr#, setWord32OffAddr#)
derivePrim(Word64, W64#, sIZEOF_WORD64, aLIGNMENT_WORD64,
           indexWord64Array#, readWord64Array#, writeWord64Array#, setWord64Array#,
           indexWord64OffAddr#, readWord64OffAddr#, writeWord64OffAddr#, setWord64OffAddr#)
derivePrim(Int, I#, sIZEOF_INT, aLIGNMENT_INT,
           indexIntArray#, readIntArray#, writeIntArray#, setIntArray#,
           indexIntOffAddr#, readIntOffAddr#, writeIntOffAddr#, setIntOffAddr#)
derivePrim(Int8, I8#, sIZEOF_INT8, aLIGNMENT_INT8,
           indexInt8Array#, readInt8Array#, writeInt8Array#, shimmedSetInt8Array#,
           indexInt8OffAddr#, readInt8OffAddr#, writeInt8OffAddr#, setInt8OffAddr#)
derivePrim(Int16, I16#, sIZEOF_INT16, aLIGNMENT_INT16,
           indexInt16Array#, readInt16Array#, writeInt16Array#, setInt16Array#,
           indexInt16OffAddr#, readInt16OffAddr#, writeInt16OffAddr#, setInt16OffAddr#)
derivePrim(Int32, I32#, sIZEOF_INT32, aLIGNMENT_INT32,
           indexInt32Array#, readInt32Array#, writeInt32Array#, setInt32Array#,
           indexInt32OffAddr#, readInt32OffAddr#, writeInt32OffAddr#, setInt32OffAddr#)
derivePrim(Int64, I64#, sIZEOF_INT64, aLIGNMENT_INT64,
           indexInt64Array#, readInt64Array#, writeInt64Array#, setInt64Array#,
           indexInt64OffAddr#, readInt64OffAddr#, writeInt64OffAddr#, setInt64OffAddr#)
derivePrim(Float, F#, sIZEOF_FLOAT, aLIGNMENT_FLOAT,
           indexFloatArray#, readFloatArray#, writeFloatArray#, setFloatArray#,
           indexFloatOffAddr#, readFloatOffAddr#, writeFloatOffAddr#, setFloatOffAddr#)
derivePrim(Double, D#, sIZEOF_DOUBLE, aLIGNMENT_DOUBLE,
           indexDoubleArray#, readDoubleArray#, writeDoubleArray#, setDoubleArray#,
           indexDoubleOffAddr#, readDoubleOffAddr#, writeDoubleOffAddr#, setDoubleOffAddr#)
derivePrim(Char, C#, sIZEOF_CHAR, aLIGNMENT_CHAR,
           indexWideCharArray#, readWideCharArray#, writeWideCharArray#, setWideCharArray#,
           indexWideCharOffAddr#, readWideCharOffAddr#, writeWideCharOffAddr#, setWideCharOffAddr#)
derivePrim(Ptr a, Ptr, sIZEOF_PTR, aLIGNMENT_PTR,
           indexAddrArray#, readAddrArray#, writeAddrArray#, setAddrArray#,
           indexAddrOffAddr#, readAddrOffAddr#, writeAddrOffAddr#, setAddrOffAddr#)
derivePrim(StablePtr a, StablePtr, sIZEOF_PTR, aLIGNMENT_PTR,
           indexStablePtrArray#, readStablePtrArray#, writeStablePtrArray#, setStablePtrArray#,
           indexStablePtrOffAddr#, readStablePtrOffAddr#, writeStablePtrOffAddr#, setStablePtrOffAddr#)
derivePrim(FunPtr a, FunPtr, sIZEOF_PTR, aLIGNMENT_PTR,
           indexAddrArray#, readAddrArray#, writeAddrArray#, setAddrArray#,
           indexAddrOffAddr#, readAddrOffAddr#, writeAddrOffAddr#, setAddrOffAddr#)

-- Prim instances for newtypes in Foreign.C.Types
deriving instance Prim CChar
deriving instance Prim CSChar
deriving instance Prim CUChar
deriving instance Prim CShort
deriving instance Prim CUShort
deriving instance Prim CInt
deriving instance Prim CUInt
deriving instance Prim CLong
deriving instance Prim CULong
deriving instance Prim CPtrdiff
deriving instance Prim CSize
deriving instance Prim CWchar
deriving instance Prim CSigAtomic
deriving instance Prim CLLong
deriving instance Prim CULLong
#if MIN_VERSION_base(4,10,0)
deriving instance Prim CBool
#endif
deriving instance Prim CIntPtr
deriving instance Prim CUIntPtr
deriving instance Prim CIntMax
deriving instance Prim CUIntMax
deriving instance Prim CClock
deriving instance Prim CTime
deriving instance Prim CUSeconds
deriving instance Prim CSUSeconds
deriving instance Prim CFloat
deriving instance Prim CDouble

-- Prim instances for newtypes in System.Posix.Types
#if defined(HTYPE_DEV_T)
deriving instance Prim CDev
#endif
#if defined(HTYPE_INO_T)
deriving instance Prim CIno
#endif
#if defined(HTYPE_MODE_T)
deriving instance Prim CMode
#endif
#if defined(HTYPE_OFF_T)
deriving instance Prim COff
#endif
#if defined(HTYPE_PID_T)
deriving instance Prim CPid
#endif
#if defined(HTYPE_SSIZE_T)
deriving instance Prim CSsize
#endif
#if defined(HTYPE_GID_T)
deriving instance Prim CGid
#endif
#if defined(HTYPE_NLINK_T)
deriving instance Prim CNlink
#endif
#if defined(HTYPE_UID_T)
deriving instance Prim CUid
#endif
#if defined(HTYPE_CC_T)
deriving instance Prim CCc
#endif
#if defined(HTYPE_SPEED_T)
deriving instance Prim CSpeed
#endif
#if defined(HTYPE_TCFLAG_T)
deriving instance Prim CTcflag
#endif
#if defined(HTYPE_RLIM_T)
deriving instance Prim CRLim
#endif
#if defined(HTYPE_BLKSIZE_T)
deriving instance Prim CBlkSize
#endif
#if defined(HTYPE_BLKCNT_T)
deriving instance Prim CBlkCnt
#endif
#if defined(HTYPE_CLOCKID_T)
deriving instance Prim CClockId
#endif
#if defined(HTYPE_FSBLKCNT_T)
deriving instance Prim CFsBlkCnt
#endif
#if defined(HTYPE_FSFILCNT_T)
deriving instance Prim CFsFilCnt
#endif
#if defined(HTYPE_ID_T)
deriving instance Prim CId
#endif
#if defined(HTYPE_KEY_T)
deriving instance Prim CKey
#endif
#if defined(HTYPE_TIMER_T)
deriving instance Prim CTimer
#endif
deriving instance Prim Fd

-- Andrew Martin: The instances for WordPtr and IntPtr are written out by
-- hand in a tedious way. We cannot use GND because the data constructors for
-- these types were not available before GHC 8.2. The CPP for generating code
-- for the Int and Word types does not work here. There is a way to clean this
-- up a little with CPP, and if anyone wants to do that, go for it. In the
-- meantime, I am going to ship this with the instances written out by hand.

-- | @since 0.7.1.0
instance Prim WordPtr where
  sizeOf# _ = sizeOf# (undefined :: Ptr ()) 
  alignment# _ = alignment# (undefined :: Ptr ()) 
  indexByteArray# a i = ptrToWordPtr (indexByteArray# a i)
  readByteArray# a i s0 = case readByteArray# a i s0 of
    (# s1, p #) -> (# s1, ptrToWordPtr p #)
  writeByteArray# a i wp = writeByteArray# a i (wordPtrToPtr wp)
  setByteArray# a i n wp = setByteArray# a i n (wordPtrToPtr wp)
  indexOffAddr# a i = ptrToWordPtr (indexOffAddr# a i)
  readOffAddr# a i s0 = case readOffAddr# a i s0 of
    (# s1, p #) -> (# s1, ptrToWordPtr p #)
  writeOffAddr# a i wp = writeOffAddr# a i (wordPtrToPtr wp)
  setOffAddr# a i n wp = setOffAddr# a i n (wordPtrToPtr wp)
  
-- | @since 0.7.1.0
instance Prim IntPtr where
  sizeOf# _ = sizeOf# (undefined :: Ptr ()) 
  alignment# _ = alignment# (undefined :: Ptr ()) 
  indexByteArray# a i = ptrToIntPtr (indexByteArray# a i)
  readByteArray# a i s0 = case readByteArray# a i s0 of
    (# s1, p #) -> (# s1, ptrToIntPtr p #)
  writeByteArray# a i wp = writeByteArray# a i (intPtrToPtr wp)
  setByteArray# a i n wp = setByteArray# a i n (intPtrToPtr wp)
  indexOffAddr# a i = ptrToIntPtr (indexOffAddr# a i)
  readOffAddr# a i s0 = case readOffAddr# a i s0 of
    (# s1, p #) -> (# s1, ptrToIntPtr p #)
  writeOffAddr# a i wp = writeOffAddr# a i (intPtrToPtr wp)
  setOffAddr# a i n wp = setOffAddr# a i n (intPtrToPtr wp)

-- | @since 0.6.5.0
deriving instance Prim a => Prim (Const a b)
-- | @since 0.6.5.0
deriving instance Prim a => Prim (Down a)
#if MIN_VERSION_base(4,8,0)
-- | @since 0.6.5.0
deriving instance Prim a => Prim (Identity a)
-- | @since 0.6.5.0
deriving instance Prim a => Prim (Monoid.Dual a)
-- | @since 0.6.5.0
deriving instance Prim a => Prim (Monoid.Sum a)
-- | @since 0.6.5.0
deriving instance Prim a => Prim (Monoid.Product a)
#endif
#if MIN_VERSION_base(4,9,0)
-- | @since 0.6.5.0
deriving instance Prim a => Prim (Semigroup.First a)
-- | @since 0.6.5.0
deriving instance Prim a => Prim (Semigroup.Last a)
-- | @since 0.6.5.0
deriving instance Prim a => Prim (Semigroup.Min a)
-- | @since 0.6.5.0
deriving instance Prim a => Prim (Semigroup.Max a)
#endif
