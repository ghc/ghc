{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

#include "MachDeps.h"

module GHC.Primitive.Monad
  ( PrimMonad(..)
  , Prim(..)
  , primitive_
  , defaultSetByteArray#
  , defaultSetOffAddr#
  ) where

import GHC.Base
import GHC.Int (Int32(I32#),Int16(I16#))
import GHC.Word (Word32(W32#))
import Foreign.C.Types (CInt(..),CShort(..))
import System.Posix.Types (Fd(..))
import GHC.ST (ST(..))

-- All the code in this module is copied directly from
-- the primitive library.

-- | Class of monads which can perform primitive state-transformer actions
class Monad m => PrimMonad m where
  -- | State token type
  type PrimState m

  -- | Execute a primitive operation
  primitive :: (State# (PrimState m) -> (# State# (PrimState m), a #)) -> m a

instance PrimMonad IO where
  type PrimState IO = RealWorld
  primitive = IO
  {-# INLINE primitive #-}

instance PrimMonad (ST s) where
  type PrimState (ST s) = s
  primitive = ST
  {-# INLINE primitive #-}


-- | Class of types supporting primitive array operations
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

deriving newtype instance Prim CInt
deriving newtype instance Prim CShort
deriving newtype instance Prim Fd

instance Prim Int where
  sizeOf# _ = SIZEOF_HSINT#
  alignment# _ = ALIGNMENT_HSINT#
  indexByteArray# arr i = I# (indexIntArray# arr i)
  readByteArray# arr i s0 = case readIntArray# arr i s0 of
    (# s1, r #) -> (# s1, I# r #)
  writeByteArray# arr i (I# r) s0 = writeIntArray# arr i r s0
  setByteArray# = defaultSetByteArray#
  indexOffAddr# arr i = I# (indexIntOffAddr# arr i)
  readOffAddr# arr i s0 = case readIntOffAddr# arr i s0 of
    (# s1, r #) -> (# s1, I# r #)
  writeOffAddr# arr i (I# r) s0 = writeIntOffAddr# arr i r s0
  setOffAddr# = defaultSetOffAddr#

instance Prim Int16 where
  sizeOf# _ = 2#
  alignment# _ = 2#
  indexByteArray# arr i = I16# (indexInt16Array# arr i)
  readByteArray# arr i s0 = case readInt16Array# arr i s0 of
    (# s1, r #) -> (# s1, I16# r #)
  writeByteArray# arr i (I16# r) s0 = writeInt16Array# arr i r s0
  setByteArray# = defaultSetByteArray#
  indexOffAddr# arr i = I16# (indexInt16OffAddr# arr i)
  readOffAddr# arr i s0 = case readInt16OffAddr# arr i s0 of
    (# s1, r #) -> (# s1, I16# r #)
  writeOffAddr# arr i (I16# r) s0 = writeInt16OffAddr# arr i r s0
  setOffAddr# = defaultSetOffAddr#

instance Prim Int32 where
  sizeOf# _ = 4#
  alignment# _ = 4#
  indexByteArray# arr i = I32# (indexInt32Array# arr i)
  readByteArray# arr i s0 = case readInt32Array# arr i s0 of
    (# s1, r #) -> (# s1, I32# r #)
  writeByteArray# arr i (I32# r) s0 = writeInt32Array# arr i r s0
  setByteArray# = defaultSetByteArray#
  indexOffAddr# arr i = I32# (indexInt32OffAddr# arr i)
  readOffAddr# arr i s0 = case readInt32OffAddr# arr i s0 of
    (# s1, r #) -> (# s1, I32# r #)
  writeOffAddr# arr i (I32# r) s0 = writeInt32OffAddr# arr i r s0
  setOffAddr# = defaultSetOffAddr#

instance Prim Word32 where
  sizeOf# _ = 4#
  alignment# _ = 4#
  indexByteArray# arr i = W32# (indexWord32Array# arr i)
  readByteArray# arr i s0 = case readWord32Array# arr i s0 of
    (# s1, r #) -> (# s1, W32# r #)
  writeByteArray# arr i (W32# r) s0 = writeWord32Array# arr i r s0
  setByteArray# = defaultSetByteArray#
  indexOffAddr# arr i = W32# (indexWord32OffAddr# arr i)
  readOffAddr# arr i s0 = case readWord32OffAddr# arr i s0 of
    (# s1, r #) -> (# s1, W32# r #)
  writeOffAddr# arr i (W32# r) s0 = writeWord32OffAddr# arr i r s0
  setOffAddr# = defaultSetOffAddr#


-- | Execute a primitive operation with no result
primitive_ :: PrimMonad m
              => (State# (PrimState m) -> State# (PrimState m)) -> m ()
{-# INLINE primitive_ #-}
primitive_ f = primitive (\s# ->
    case f s# of
        s'# -> (# s'#, () #))

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


