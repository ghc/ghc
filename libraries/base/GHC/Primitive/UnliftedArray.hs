{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Primitive.UnliftedArray
  ( UnliftedArray(..)
  , MutableUnliftedArray(..)
  , PrimUnlifted
  , newUnliftedArray
  , unsafeNewUnliftedArray
  , readUnliftedArray
  , writeUnliftedArray
  , setUnliftedArray
  , deleteIndexMutableUnliftedArray
  , copyMutableUnliftedArray
  , sizeofMutableUnliftedArray
  , traverseMutableUnliftedArray
  , mutableUnliftedArrayFromList
  , sizeofUnliftedArray
  , indexUnliftedArray
  , unsafeFreezeUnliftedArray
  , replicateUnliftedArrayP
  ) where

import GHC.Num ((+),(-))
import GHC.Base
import GHC.List (length)
import GHC.Primitive.Monad (PrimMonad(..),primitive_)

import qualified GHC.Primitive.Array as A
import qualified GHC.Primitive.SmallArray as SA
import qualified GHC.MVar as GM

class PrimUnlifted a where
  toArrayArray# :: a -> ArrayArray#
  fromArrayArray# :: ArrayArray# -> a

-- | Immutable arrays that efficiently store types that are simple wrappers
-- around unlifted primitive types. The values of the unlifted type are
-- stored directly, eliminating a layer of indirection.
data UnliftedArray e = UnliftedArray ArrayArray#

-- | Mutable arrays that efficiently store types that are simple wrappers
-- around unlifted primitive types. The values of the unlifted type are
-- stored directly, eliminating a layer of indirection.
data MutableUnliftedArray s e = MutableUnliftedArray (MutableArrayArray# s)

instance PrimUnlifted (A.Array a) where
  toArrayArray# (A.Array a#) = unsafeCoerce# a#
  fromArrayArray# aa# = A.Array (unsafeCoerce# aa#)

instance PrimUnlifted (A.MutableArray s a) where
  toArrayArray# (A.MutableArray ma#) = unsafeCoerce# ma#
  fromArrayArray# aa# = A.MutableArray (unsafeCoerce# aa#)

instance PrimUnlifted (SA.SmallArray a) where
  toArrayArray# (SA.SmallArray sa#) = unsafeCoerce# sa#
  fromArrayArray# aa# = SA.SmallArray (unsafeCoerce# aa#)

instance PrimUnlifted (SA.SmallMutableArray s a) where
  toArrayArray# (SA.SmallMutableArray sma#) = unsafeCoerce# sma#
  fromArrayArray# aa# = SA.SmallMutableArray (unsafeCoerce# aa#)

instance PrimUnlifted (GM.MVar a) where
  toArrayArray# (GM.MVar mv#) = unsafeCoerce# mv#
  fromArrayArray# mv# = GM.MVar (unsafeCoerce# mv#)

-- | Creates a new 'MutableUnliftedArray'. This function is unsafe because it
-- initializes all elements of the array as pointers to the array itself. Attempting
-- to read one of these elements before writing to it is in effect an unsafe
-- coercion from the @MutableUnliftedArray s a@ to the element type.
unsafeNewUnliftedArray
  :: (PrimMonad m)
  => Int -- ^ size
  -> m (MutableUnliftedArray (PrimState m) a)
unsafeNewUnliftedArray (I# i#) = primitive $ \s -> case newArrayArray# i# s of
  (# s', maa# #) -> (# s', MutableUnliftedArray maa# #)
{-# inline unsafeNewUnliftedArray #-}


-- | Creates a new 'MutableUnliftedArray' with the specified value as initial
-- contents. This is slower than 'unsafeNewUnliftedArray', but safer.
newUnliftedArray
  :: (PrimMonad m, PrimUnlifted a)
  => Int -- ^ size
  -> a -- ^ initial value
  -> m (MutableUnliftedArray (PrimState m) a)
newUnliftedArray len v =
  unsafeNewUnliftedArray len >>= \mua -> setUnliftedArray mua v >> return mua
{-# inline newUnliftedArray #-}


-- | Gets the value at the specified position of a 'MutableUnliftedArray'.
readUnliftedArray
  :: (PrimMonad m, PrimUnlifted a)
  => MutableUnliftedArray (PrimState m) a -- ^ source
  -> Int -- ^ index
  -> m a
readUnliftedArray (MutableUnliftedArray maa#) (I# i#)
  = primitive $ \s -> case readArrayArrayArray# maa# i# s of
      (# s', aa# #) -> (# s',  fromArrayArray# aa# #)
{-# inline readUnliftedArray #-}

-- | Sets the value at the specified position of a 'MutableUnliftedArray'.
writeUnliftedArray
  :: (PrimMonad m, PrimUnlifted a)
  => MutableUnliftedArray (PrimState m) a -- ^ destination
  -> Int -- ^ index
  -> a -- ^ value
  -> m ()
writeUnliftedArray (MutableUnliftedArray maa#) (I# i#) a
  = primitive_ (writeArrayArrayArray# maa# i# (toArrayArray# a))
{-# inline writeUnliftedArray #-}


-- | Sets all the positions in an unlifted array to the designated value.
setUnliftedArray
  :: (PrimMonad m, PrimUnlifted a)
  => MutableUnliftedArray (PrimState m) a -- ^ destination
  -> a -- ^ value to fill with
  -> m ()
setUnliftedArray mua v = loop $ sizeofMutableUnliftedArray mua - 1
 where
 loop i | i < 0     = return ()
        | otherwise = writeUnliftedArray mua i v >> loop (i-1)
{-# inline setUnliftedArray #-}


-- | Yields the length of an 'UnliftedArray'.
sizeofUnliftedArray :: UnliftedArray e -> Int
sizeofUnliftedArray (UnliftedArray aa#) = I# (sizeofArrayArray# aa#)
{-# inline sizeofUnliftedArray #-}

-- | Yields the length of a 'MutableUnliftedArray'.
sizeofMutableUnliftedArray :: MutableUnliftedArray s e -> Int
sizeofMutableUnliftedArray (MutableUnliftedArray maa#)
  = I# (sizeofMutableArrayArray# maa#)
{-# inline sizeofMutableUnliftedArray #-}

deleteIndexMutableUnliftedArray :: PrimMonad m
  => MutableUnliftedArray (PrimState m) a
  -> Int
  -> m (MutableUnliftedArray (PrimState m) a)
{-# inline deleteIndexMutableUnliftedArray #-}
deleteIndexMutableUnliftedArray !src !ix = do
  let len = sizeofMutableUnliftedArray src
  dst <- unsafeNewUnliftedArray (len - 1)
  copyMutableUnliftedArray dst 0 src 0 ix
  copyMutableUnliftedArray dst ix src (ix + 1) (len - (ix + 1))
  pure dst


-- | Copies the contents of one mutable array into another.
copyMutableUnliftedArray
  :: (PrimMonad m)
  => MutableUnliftedArray (PrimState m) a -- ^ destination
  -> Int -- ^ offset into destination
  -> MutableUnliftedArray (PrimState m) a -- ^ source
  -> Int -- ^ offset into source
  -> Int -- ^ number of elements to copy
  -> m ()
copyMutableUnliftedArray
  (MutableUnliftedArray dst) (I# doff)
  (MutableUnliftedArray src) (I# soff) (I# ln) =
    primitive_ $ copyMutableArrayArray# src soff dst doff ln
{-# inline copyMutableUnliftedArray #-}

traverseMutableUnliftedArray
  :: (PrimUnlifted a, PrimUnlifted b, PrimMonad m)
  => (a -> m b)
  -> MutableUnliftedArray (PrimState m) a
  -> m (MutableUnliftedArray (PrimState m) b)
{-# INLINE traverseMutableUnliftedArray #-}
traverseMutableUnliftedArray f = \ !ary ->
  let
    !sz = sizeofMutableUnliftedArray ary
    go !i !mary
      | i == sz = pure mary
      | otherwise = do
          a <- readUnliftedArray ary i
          b <- f a
          writeUnliftedArray mary i b
          go (i + 1) mary
  in do
    mary <- unsafeNewUnliftedArray sz
    go 0 mary

mutableUnliftedArrayFromList :: (PrimMonad m, PrimUnlifted a)
  => [a] -> m (MutableUnliftedArray (PrimState m) a)
{-# INLINE mutableUnliftedArrayFromList #-}
mutableUnliftedArrayFromList l =
  mutableUnliftedArrayFromListN (length l) l

-- There is no safety check to ensure that the length of the
-- length matches the provided length. Caller beware.
mutableUnliftedArrayFromListN :: (PrimMonad m, PrimUnlifted a) => Int -> [a] -> m (MutableUnliftedArray (PrimState m) a)
{-# INLINE mutableUnliftedArrayFromListN #-}
mutableUnliftedArrayFromListN n xs = do
  sma <- unsafeNewUnliftedArray n
  mutableUnliftedArrayFromListWorker sma xs
  pure sma

mutableUnliftedArrayFromListWorker :: (PrimMonad m, PrimUnlifted a)
  => MutableUnliftedArray (PrimState m) a -> [a] -> m ()
{-# INLINE mutableUnliftedArrayFromListWorker #-}
mutableUnliftedArrayFromListWorker sma e = do
  let go !_ [] = pure ()
      go !ix (x : xs) = do
        writeUnliftedArray sma ix x
        go (ix + 1) xs
  go 0 e

die :: String -> String -> a
{-# INLINE die #-}
die fun problem = error $ "GHC.Primitive.UnliftedArray." ++ fun ++ ": " ++ problem


-- Internal indexing function.
--
-- Note: ArrayArray# is strictly evaluated, so this should have similar
-- consequences to indexArray#, where matching on the unboxed single causes the
-- array access to happen.
indexUnliftedArrayU
  :: PrimUnlifted a
  => UnliftedArray a
  -> Int
  -> (# a #)
indexUnliftedArrayU (UnliftedArray src#) (I# i#)
  = case indexArrayArrayArray# src# i# of
      aa# -> (# fromArrayArray# aa# #)
{-# inline indexUnliftedArrayU #-}

-- | Gets the value at the specified position of an 'UnliftedArray'.
indexUnliftedArray
  :: PrimUnlifted a
  => UnliftedArray a -- ^ source
  -> Int -- ^ index
  -> a
indexUnliftedArray ua i
  = case indexUnliftedArrayU ua i of (# v #) -> v
{-# inline indexUnliftedArray #-}

-- | Execute the monadic action the given number of times and store the
-- results in an array.
replicateUnliftedArrayP :: (PrimMonad m, PrimUnlifted a)
  => Int
  -> m a
  -> m (UnliftedArray a)
{-# inline replicateUnliftedArrayP #-}
replicateUnliftedArrayP sz f = do
  marr <- unsafeNewUnliftedArray sz
  let go !ix = if ix < sz
        then do
          b <- f
          writeUnliftedArray marr ix b
          go (ix + 1)
        else return ()
  go 0
  unsafeFreezeUnliftedArray marr


-- | Freezes a 'MutableUnliftedArray', yielding an 'UnliftedArray'. This simply
-- marks the array as frozen in place, so it should only be used when no further
-- modifications to the mutable array will be performed.
unsafeFreezeUnliftedArray
  :: (PrimMonad m)
  => MutableUnliftedArray (PrimState m) a
  -> m (UnliftedArray a)
{-# inline unsafeFreezeUnliftedArray #-}
unsafeFreezeUnliftedArray (MutableUnliftedArray maa#)
  = primitive $ \s -> case unsafeFreezeArrayArray# maa# s of
      (# s', aa# #) -> (# s', UnliftedArray aa# #)

