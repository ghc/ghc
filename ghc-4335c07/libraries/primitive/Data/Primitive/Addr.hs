{-# LANGUAGE MagicHash, UnboxedTuples #-}

-- |
-- Module      : Data.Primitive.Addr
-- Copyright   : (c) Roman Leshchinskiy 2009-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Primitive operations on machine addresses
--

module Data.Primitive.Addr (
  -- * Types
  Addr(..),

  -- * Address arithmetic
  nullAddr, plusAddr, minusAddr, remAddr,

  -- * Element access
  indexOffAddr, readOffAddr, writeOffAddr,

  -- * Block operations
  copyAddr, moveAddr, setAddr
) where

import Control.Monad.Primitive
import Data.Primitive.Types

import GHC.Base ( Int(..) )
import GHC.Prim

import GHC.Ptr
import Foreign.Marshal.Utils


-- | The null address
nullAddr :: Addr
nullAddr = Addr nullAddr#

infixl 6 `plusAddr`, `minusAddr`
infixl 7 `remAddr`

-- | Offset an address by the given number of bytes
plusAddr :: Addr -> Int -> Addr
plusAddr (Addr a#) (I# i#) = Addr (plusAddr# a# i#)

-- | Distance in bytes between two addresses. The result is only valid if the
-- difference fits in an 'Int'.
minusAddr :: Addr -> Addr -> Int
minusAddr (Addr a#) (Addr b#) = I# (minusAddr# a# b#)

-- | The remainder of the address and the integer.
remAddr :: Addr -> Int -> Int
remAddr (Addr a#) (I# i#) = I# (remAddr# a# i#)

-- | Read a value from a memory position given by an address and an offset.
-- The memory block the address refers to must be immutable. The offset is in
-- elements of type @a@ rather than in bytes.
indexOffAddr :: Prim a => Addr -> Int -> a
{-# INLINE indexOffAddr #-}
indexOffAddr (Addr addr#) (I# i#) = indexOffAddr# addr# i#

-- | Read a value from a memory position given by an address and an offset.
-- The offset is in elements of type @a@ rather than in bytes.
readOffAddr :: (Prim a, PrimMonad m) => Addr -> Int -> m a
{-# INLINE readOffAddr #-}
readOffAddr (Addr addr#) (I# i#) = primitive (readOffAddr# addr# i#)

-- | Write a value to a memory position given by an address and an offset.
-- The offset is in elements of type @a@ rather than in bytes.
writeOffAddr :: (Prim a, PrimMonad m) => Addr -> Int -> a -> m ()
{-# INLINE writeOffAddr #-}
writeOffAddr (Addr addr#) (I# i#) x = primitive_ (writeOffAddr# addr# i# x)

-- | Copy the given number of bytes from the second 'Addr' to the first. The
-- areas may not overlap.
copyAddr :: PrimMonad m => Addr         -- ^ destination address
                        -> Addr         -- ^ source address
                        -> Int          -- ^ number of bytes
                        -> m ()
{-# INLINE copyAddr #-}
copyAddr (Addr dst#) (Addr src#) n
  = unsafePrimToPrim $ copyBytes (Ptr dst#) (Ptr src#) n

-- | Copy the given number of bytes from the second 'Addr' to the first. The
-- areas may overlap.
moveAddr :: PrimMonad m => Addr         -- ^ destination address
                        -> Addr         -- ^ source address
                        -> Int          -- ^ number of bytes
                        -> m ()
{-# INLINE moveAddr #-}
moveAddr (Addr dst#) (Addr src#) n
  = unsafePrimToPrim $ moveBytes (Ptr dst#) (Ptr src#) n

-- | Fill a memory block of with the given value. The length is in
-- elements of type @a@ rather than in bytes.
setAddr :: (Prim a, PrimMonad m) => Addr -> Int -> a -> m ()
{-# INLINE setAddr #-}
setAddr (Addr addr#) (I# n#) x = primitive_ (setOffAddr# addr# 0# n# x)

