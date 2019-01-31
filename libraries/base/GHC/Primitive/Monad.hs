{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Primitive.Monad
  ( PrimMonad(..)
  , Prim(..)
  , primitive_
  , defaultSetByteArray#
  , defaultSetOffAddr#
  ) where

import GHC.Base
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


