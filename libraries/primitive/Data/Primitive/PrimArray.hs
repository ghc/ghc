{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}


-- |
-- Module      : Data.Primitive.PrimArray
-- Copyright   : (c) Roman Leshchinskiy 2009-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Arrays of unboxed primitive types. The function provided by this module
-- match the behavior of those provided by @Data.Primitive.ByteArray@, and
-- the underlying types and primops that back them are the same.
-- However, the type constructors 'PrimArray' and 'MutablePrimArray' take one additional
-- argument than their respective counterparts 'ByteArray' and 'MutableByteArray'.
-- This argument is used to designate the type of element in the array.
-- Consequently, all function this modules accepts length and incides in
-- terms of elements, not bytes.
--
-- @since 0.6.4.0
module Data.Primitive.PrimArray
  ( -- * Types
    PrimArray(..)
  , MutablePrimArray(..)
    -- * Allocation
  , newPrimArray
  , newPinnedPrimArray
  , newAlignedPinnedPrimArray
  , resizeMutablePrimArray
#if __GLASGOW_HASKELL__ >= 710
  , shrinkMutablePrimArray
#endif
    -- * Element Access
  , readPrimArray
  , writePrimArray
  , indexPrimArray
    -- * Freezing and Thawing
  , freezePrimArray
  , unsafeFreezePrimArray
  , unsafeThawPrimArray
    -- * Block Operations
  , copyPrimArray
  , copyMutablePrimArray
#if __GLASGOW_HASKELL__ >= 708
  , copyPrimArrayToPtr
  , copyMutablePrimArrayToPtr
#endif
  , clonePrimArray
  , cloneMutablePrimArray
  , setPrimArray
    -- * Information
  , sameMutablePrimArray
  , getSizeofMutablePrimArray
  , sizeofMutablePrimArray
  , sizeofPrimArray
  , primArrayContents
  , mutablePrimArrayContents
#if __GLASGOW_HASKELL__ >= 802
  , isPrimArrayPinned
  , isMutablePrimArrayPinned
#endif
    -- * List Conversion
  , primArrayToList
  , primArrayFromList
  , primArrayFromListN
    -- * Folding
  , foldrPrimArray
  , foldrPrimArray'
  , foldlPrimArray
  , foldlPrimArray'
  , foldlPrimArrayM'
    -- * Effectful Folding
  , traversePrimArray_
  , itraversePrimArray_
    -- * Map/Create
  , mapPrimArray
  , imapPrimArray
  , generatePrimArray
  , replicatePrimArray
  , filterPrimArray
  , mapMaybePrimArray
    -- * Effectful Map/Create
    -- $effectfulMapCreate
    -- ** Lazy Applicative
  , traversePrimArray
  , itraversePrimArray
  , generatePrimArrayA
  , replicatePrimArrayA
  , filterPrimArrayA
  , mapMaybePrimArrayA
    -- ** Strict Primitive Monadic
  , traversePrimArrayP
  , itraversePrimArrayP
  , generatePrimArrayP
  , replicatePrimArrayP
  , filterPrimArrayP
  , mapMaybePrimArrayP
  ) where

import GHC.Exts
import GHC.Base ( Int(..) )
import Data.Primitive.Internal.Compat (isTrue#)
import Data.Primitive.Types
import Data.Primitive.ByteArray (ByteArray(..))
import Data.Monoid (Monoid(..),(<>))
import Control.Applicative
import Control.DeepSeq
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.List as L
import qualified Data.Primitive.ByteArray as PB
import qualified Data.Primitive.Types as PT
import qualified GHC.ST as GHCST

#if MIN_VERSION_base(4,7,0)
import GHC.Exts (IsList(..))
#endif

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup as SG
#endif

#if __GLASGOW_HASKELL__ >= 802
import qualified GHC.Exts as Exts
#endif

-- | Arrays of unboxed elements. This accepts types like 'Double', 'Char',
-- 'Int', and 'Word', as well as their fixed-length variants ('Word8',
-- 'Word16', etc.). Since the elements are unboxed, a 'PrimArray' is strict
-- in its elements. This differs from the behavior of 'Array', which is lazy
-- in its elements.
data PrimArray a = PrimArray ByteArray#

instance NFData (PrimArray a) where
  rnf (PrimArray _) = ()

-- | Mutable primitive arrays associated with a primitive state token.
-- These can be written to and read from in a monadic context that supports
-- sequencing such as 'IO' or 'ST'. Typically, a mutable primitive array will
-- be built and then convert to an immutable primitive array using
-- 'unsafeFreezePrimArray'. However, it is also acceptable to simply discard
-- a mutable primitive array since it lives in managed memory and will be
-- garbage collected when no longer referenced.
data MutablePrimArray s a = MutablePrimArray (MutableByteArray# s)

instance Eq (MutablePrimArray s a) where
  (==) = sameMutablePrimArray

instance NFData (MutablePrimArray s a) where
  rnf (MutablePrimArray _) = ()

sameByteArray :: ByteArray# -> ByteArray# -> Bool
sameByteArray ba1 ba2 =
    case reallyUnsafePtrEquality# (unsafeCoerce# ba1 :: ()) (unsafeCoerce# ba2 :: ()) of
#if __GLASGOW_HASKELL__ >= 708
      r -> isTrue# r
#else
      1# -> True
      _ -> False
#endif

-- | @since 0.6.4.0
instance (Eq a, Prim a) => Eq (PrimArray a) where
  a1@(PrimArray ba1#) == a2@(PrimArray ba2#)
    | sameByteArray ba1# ba2# = True
    | sz1 /= sz2 = False
    | otherwise = loop (quot sz1 (sizeOf (undefined :: a)) - 1)
    where
    -- Here, we take the size in bytes, not in elements. We do this
    -- since it allows us to defer performing the division to
    -- calculate the size in elements.
    sz1 = PB.sizeofByteArray (ByteArray ba1#)
    sz2 = PB.sizeofByteArray (ByteArray ba2#)
    loop !i
      | i < 0 = True
      | otherwise = indexPrimArray a1 i == indexPrimArray a2 i && loop (i-1)
  {-# INLINE (==) #-}

-- | Lexicographic ordering. Subject to change between major versions.
--
--   @since 0.6.4.0
instance (Ord a, Prim a) => Ord (PrimArray a) where
  compare a1@(PrimArray ba1#) a2@(PrimArray ba2#)
    | sameByteArray ba1# ba2# = EQ
    | otherwise = loop 0
    where
    sz1 = PB.sizeofByteArray (ByteArray ba1#)
    sz2 = PB.sizeofByteArray (ByteArray ba2#)
    sz = quot (min sz1 sz2) (sizeOf (undefined :: a))
    loop !i
      | i < sz = compare (indexPrimArray a1 i) (indexPrimArray a2 i) <> loop (i+1)
      | otherwise = compare sz1 sz2
  {-# INLINE compare #-}

#if MIN_VERSION_base(4,7,0)
-- | @since 0.6.4.0
instance Prim a => IsList (PrimArray a) where
  type Item (PrimArray a) = a
  fromList = primArrayFromList
  fromListN = primArrayFromListN
  toList = primArrayToList
#endif

-- | @since 0.6.4.0
instance (Show a, Prim a) => Show (PrimArray a) where
  showsPrec p a = showParen (p > 10) $
    showString "fromListN " . shows (sizeofPrimArray a) . showString " "
      . shows (primArrayToList a)

die :: String -> String -> a
die fun problem = error $ "Data.Primitive.PrimArray." ++ fun ++ ": " ++ problem

-- | Create a 'PrimArray' from a list.
--
-- @primArrayFromList vs = `byteArrayFromListN` (length vs) vs@
primArrayFromList :: Prim a => [a] -> PrimArray a
primArrayFromList vs = primArrayFromListN (L.length vs) vs

-- | Create a 'PrimArray' from a list of a known length. If the length
--   of the list does not match the given length, this throws an exception.
primArrayFromListN :: forall a. Prim a => Int -> [a] -> PrimArray a
primArrayFromListN len vs = runST run where
  run :: forall s. ST s (PrimArray a)
  run = do
    arr <- newPrimArray len
    let go :: [a] -> Int -> ST s ()
        go [] !ix = if ix == len
          then return ()
          else die "fromListN" "list length less than specified size"
        go (a : as) !ix = if ix < len
          then do
            writePrimArray arr ix a
            go as (ix + 1)
          else die "fromListN" "list length greater than specified size"
    go vs 0
    unsafeFreezePrimArray arr

-- | Convert the primitive array to a list.
{-# INLINE primArrayToList #-}
primArrayToList :: forall a. Prim a => PrimArray a -> [a]
primArrayToList xs = build (\c n -> foldrPrimArray c n xs)

primArrayToByteArray :: PrimArray a -> PB.ByteArray
primArrayToByteArray (PrimArray x) = PB.ByteArray x

byteArrayToPrimArray :: ByteArray -> PrimArray a
byteArrayToPrimArray (PB.ByteArray x) = PrimArray x

#if MIN_VERSION_base(4,9,0)
-- | @since 0.6.4.0
instance Semigroup (PrimArray a) where
  x <> y = byteArrayToPrimArray (primArrayToByteArray x SG.<> primArrayToByteArray y)
  sconcat = byteArrayToPrimArray . SG.sconcat . fmap primArrayToByteArray
  stimes i arr = byteArrayToPrimArray (SG.stimes i (primArrayToByteArray arr))
#endif

-- | @since 0.6.4.0
instance Monoid (PrimArray a) where
  mempty = emptyPrimArray
#if !(MIN_VERSION_base(4,11,0))
  mappend x y = byteArrayToPrimArray (mappend (primArrayToByteArray x) (primArrayToByteArray y))
#endif
  mconcat = byteArrayToPrimArray . mconcat . map primArrayToByteArray

-- | The empty primitive array.
emptyPrimArray :: PrimArray a
{-# NOINLINE emptyPrimArray #-}
emptyPrimArray = runST $ primitive $ \s0# -> case newByteArray# 0# s0# of
  (# s1#, arr# #) -> case unsafeFreezeByteArray# arr# s1# of
    (# s2#, arr'# #) -> (# s2#, PrimArray arr'# #)

-- | Create a new mutable primitive array of the given length. The
-- underlying memory is left uninitialized.
--
-- /Note:/ this function does not check if the input is non-negative.
newPrimArray :: forall m a. (PrimMonad m, Prim a) => Int -> m (MutablePrimArray (PrimState m) a)
{-# INLINE newPrimArray #-}
newPrimArray (I# n#)
  = primitive (\s# ->
      case newByteArray# (n# *# sizeOf# (undefined :: a)) s# of
        (# s'#, arr# #) -> (# s'#, MutablePrimArray arr# #)
    )

-- | Resize a mutable primitive array. The new size is given in elements.
--
-- This will either resize the array in-place or, if not possible, allocate the
-- contents into a new, unpinned array and copy the original array\'s contents.
--
-- To avoid undefined behaviour, the original 'MutablePrimArray' shall not be
-- accessed anymore after a 'resizeMutablePrimArray' has been performed.
-- Moreover, no reference to the old one should be kept in order to allow
-- garbage collection of the original 'MutablePrimArray' in case a new
-- 'MutablePrimArray' had to be allocated.
resizeMutablePrimArray :: forall m a. (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a
  -> Int -- ^ new size
  -> m (MutablePrimArray (PrimState m) a)
{-# INLINE resizeMutablePrimArray #-}
#if __GLASGOW_HASKELL__ >= 710
resizeMutablePrimArray (MutablePrimArray arr#) (I# n#)
  = primitive (\s# -> case resizeMutableByteArray# arr# (n# *# sizeOf# (undefined :: a)) s# of
                        (# s'#, arr'# #) -> (# s'#, MutablePrimArray arr'# #))
#else
resizeMutablePrimArray arr n
  = do arr' <- newPrimArray n
       copyMutablePrimArray arr' 0 arr 0 (min (sizeofMutablePrimArray arr) n)
       return arr'
#endif

-- Although it is possible to shim resizeMutableByteArray for old GHCs, this
-- is not the case with shrinkMutablePrimArray.
#if __GLASGOW_HASKELL__ >= 710
-- | Shrink a mutable primitive array. The new size is given in elements.
-- It must be smaller than the old size. The array will be resized in place.
-- This function is only available when compiling with GHC 7.10 or newer.
shrinkMutablePrimArray :: forall m a. (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a
  -> Int -- ^ new size
  -> m ()
{-# INLINE shrinkMutablePrimArray #-}
shrinkMutablePrimArray (MutablePrimArray arr#) (I# n#)
  = primitive_ (shrinkMutableByteArray# arr# (n# *# sizeOf# (undefined :: a)))
#endif

-- | Read a value from the array at the given index.
--
-- /Note:/ this function does not do bounds checking.
readPrimArray :: (Prim a, PrimMonad m) => MutablePrimArray (PrimState m) a -> Int -> m a
{-# INLINE readPrimArray #-}
readPrimArray (MutablePrimArray arr#) (I# i#)
  = primitive (readByteArray# arr# i#)

-- | Write an element to the given index.
--
-- /Note:/ this function does not do bounds checking.
writePrimArray ::
     (Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a -- ^ array
  -> Int -- ^ index
  -> a -- ^ element
  -> m ()
{-# INLINE writePrimArray #-}
writePrimArray (MutablePrimArray arr#) (I# i#) x
  = primitive_ (writeByteArray# arr# i# x)

-- | Copy part of a mutable array into another mutable array.
--   In the case that the destination and
--   source arrays are the same, the regions may overlap.
--
-- /Note:/ this function does not do bounds or overlap checking.
copyMutablePrimArray :: forall m a.
     (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ destination array
  -> Int -- ^ offset into destination array
  -> MutablePrimArray (PrimState m) a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of elements to copy
  -> m ()
{-# INLINE copyMutablePrimArray #-}
copyMutablePrimArray (MutablePrimArray dst#) (I# doff#) (MutablePrimArray src#) (I# soff#) (I# n#)
  = primitive_ (copyMutableByteArray#
      src#
      (soff# *# (sizeOf# (undefined :: a)))
      dst#
      (doff# *# (sizeOf# (undefined :: a)))
      (n# *# (sizeOf# (undefined :: a)))
    )

-- | Copy part of an array into another mutable array.
--
-- /Note:/ this function does not do bounds or overlap checking.
copyPrimArray :: forall m a.
     (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ destination array
  -> Int -- ^ offset into destination array
  -> PrimArray a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of elements to copy
  -> m ()
{-# INLINE copyPrimArray #-}
copyPrimArray (MutablePrimArray dst#) (I# doff#) (PrimArray src#) (I# soff#) (I# n#)
  = primitive_ (copyByteArray#
      src#
      (soff# *# (sizeOf# (undefined :: a)))
      dst#
      (doff# *# (sizeOf# (undefined :: a)))
      (n# *# (sizeOf# (undefined :: a)))
    )

#if __GLASGOW_HASKELL__ >= 708
-- | Copy a slice of an immutable primitive array to an address.
--   The offset and length are given in elements of type @a@.
--   This function assumes that the 'Prim' instance of @a@
--   agrees with the 'Storable' instance. This function is only
--   available when building with GHC 7.8 or newer.
--
-- /Note:/ this function does not do bounds or overlap checking.
copyPrimArrayToPtr :: forall m a. (PrimMonad m, Prim a)
  => Ptr a -- ^ destination pointer
  -> PrimArray a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of prims to copy
  -> m ()
{-# INLINE copyPrimArrayToPtr #-}
copyPrimArrayToPtr (Ptr addr#) (PrimArray ba#) (I# soff#) (I# n#) =
    primitive (\ s# ->
        let s'# = copyByteArrayToAddr# ba# (soff# *# siz#) addr# (n# *# siz#) s#
        in (# s'#, () #))
  where siz# = sizeOf# (undefined :: a)

-- | Copy a slice of an immutable primitive array to an address.
--   The offset and length are given in elements of type @a@.
--   This function assumes that the 'Prim' instance of @a@
--   agrees with the 'Storable' instance. This function is only
--   available when building with GHC 7.8 or newer.
--
-- /Note:/ this function does not do bounds or overlap checking.
copyMutablePrimArrayToPtr :: forall m a. (PrimMonad m, Prim a)
  => Ptr a -- ^ destination pointer
  -> MutablePrimArray (PrimState m) a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of prims to copy
  -> m ()
{-# INLINE copyMutablePrimArrayToPtr #-}
copyMutablePrimArrayToPtr (Ptr addr#) (MutablePrimArray mba#) (I# soff#) (I# n#) =
    primitive (\ s# ->
        let s'# = copyMutableByteArrayToAddr# mba# (soff# *# siz#) addr# (n# *# siz#) s#
        in (# s'#, () #))
  where siz# = sizeOf# (undefined :: a)
#endif

-- | Fill a slice of a mutable primitive array with a value.
--
-- /Note:/ this function does not do bounds checking.
setPrimArray
  :: (Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a -- ^ array to fill
  -> Int -- ^ offset into array
  -> Int -- ^ number of values to fill
  -> a -- ^ value to fill with
  -> m ()
{-# INLINE setPrimArray #-}
setPrimArray (MutablePrimArray dst#) (I# doff#) (I# sz#) x
  = primitive_ (PT.setByteArray# dst# doff# sz# x)

-- | Get the size of a mutable primitive array in elements. Unlike 'sizeofMutablePrimArray',
-- this function ensures sequencing in the presence of resizing.
getSizeofMutablePrimArray :: forall m a. (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ array
  -> m Int
{-# INLINE getSizeofMutablePrimArray #-}
#if __GLASGOW_HASKELL__ >= 801
getSizeofMutablePrimArray (MutablePrimArray arr#)
  = primitive (\s# ->
      case getSizeofMutableByteArray# arr# s# of
        (# s'#, sz# #) -> (# s'#, I# (quotInt# sz# (sizeOf# (undefined :: a))) #)
    )
#else
-- On older GHCs, it is not possible to resize a byte array, so
-- this provides behavior consistent with the implementation for
-- newer GHCs.
getSizeofMutablePrimArray arr
  = return (sizeofMutablePrimArray arr)
#endif

-- | Size of the mutable primitive array in elements. This function shall not
--   be used on primitive arrays that are an argument to or a result of
--   'resizeMutablePrimArray' or 'shrinkMutablePrimArray'.
sizeofMutablePrimArray :: forall s a. Prim a => MutablePrimArray s a -> Int
{-# INLINE sizeofMutablePrimArray #-}
sizeofMutablePrimArray (MutablePrimArray arr#) =
  I# (quotInt# (sizeofMutableByteArray# arr#) (sizeOf# (undefined :: a)))

-- | Check if the two arrays refer to the same memory block.
sameMutablePrimArray :: MutablePrimArray s a -> MutablePrimArray s a -> Bool
{-# INLINE sameMutablePrimArray #-}
sameMutablePrimArray (MutablePrimArray arr#) (MutablePrimArray brr#)
  = isTrue# (sameMutableByteArray# arr# brr#)

-- | Create an immutable copy of a slice of a primitive array. The offset and
-- length are given in elements.
--
-- This operation makes a copy of the specified section, so it is safe to
-- continue using the mutable array afterward.
freezePrimArray
  :: (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ source
  -> Int                              -- ^ offset in elements
  -> Int                              -- ^ length in elements
  -> m (PrimArray a)
{-# INLINE freezePrimArray #-}
freezePrimArray !src !off !len = do
  dst <- newPrimArray len
  copyMutablePrimArray dst 0 src off len
  unsafeFreezePrimArray dst

-- | Convert a mutable byte array to an immutable one without copying. The
-- array should not be modified after the conversion.
unsafeFreezePrimArray
  :: PrimMonad m => MutablePrimArray (PrimState m) a -> m (PrimArray a)
{-# INLINE unsafeFreezePrimArray #-}
unsafeFreezePrimArray (MutablePrimArray arr#)
  = primitive (\s# -> case unsafeFreezeByteArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, PrimArray arr'# #))

-- | Convert an immutable array to a mutable one without copying. The
-- original array should not be used after the conversion.
unsafeThawPrimArray
  :: PrimMonad m => PrimArray a -> m (MutablePrimArray (PrimState m) a)
{-# INLINE unsafeThawPrimArray #-}
unsafeThawPrimArray (PrimArray arr#)
  = primitive (\s# -> (# s#, MutablePrimArray (unsafeCoerce# arr#) #))

-- | Read a primitive value from the primitive array.
--
-- /Note:/ this function does not do bounds checking.
indexPrimArray :: forall a. Prim a => PrimArray a -> Int -> a
{-# INLINE indexPrimArray #-}
indexPrimArray (PrimArray arr#) (I# i#) = indexByteArray# arr# i#

-- | Get the size, in elements, of the primitive array.
sizeofPrimArray :: forall a. Prim a => PrimArray a -> Int
{-# INLINE sizeofPrimArray #-}
sizeofPrimArray (PrimArray arr#) = I# (quotInt# (sizeofByteArray# arr#) (sizeOf# (undefined :: a)))

#if __GLASGOW_HASKELL__ >= 802
-- | Check whether or not the byte array is pinned. Pinned primitive arrays cannot
--   be moved by the garbage collector. It is safe to use 'primArrayContents'
--   on such byte arrays. This function is only available when compiling with
--   GHC 8.2 or newer.
--
--   @since 0.7.1.0
isPrimArrayPinned :: PrimArray a -> Bool
{-# INLINE isPrimArrayPinned #-}
isPrimArrayPinned (PrimArray arr#) = isTrue# (Exts.isByteArrayPinned# arr#)

-- | Check whether or not the mutable primitive array is pinned. This function is
--   only available when compiling with GHC 8.2 or newer.
--
--   @since 0.7.1.0
isMutablePrimArrayPinned :: MutablePrimArray s a -> Bool
{-# INLINE isMutablePrimArrayPinned #-}
isMutablePrimArrayPinned (MutablePrimArray marr#) = isTrue# (Exts.isMutableByteArrayPinned# marr#)
#endif

-- | Lazy right-associated fold over the elements of a 'PrimArray'.
{-# INLINE foldrPrimArray #-}
foldrPrimArray :: forall a b. Prim a => (a -> b -> b) -> b -> PrimArray a -> b
foldrPrimArray f z arr = go 0
  where
    !sz = sizeofPrimArray arr
    go !i
      | sz > i = f (indexPrimArray arr i) (go (i+1))
      | otherwise = z

-- | Strict right-associated fold over the elements of a 'PrimArray'.
{-# INLINE foldrPrimArray' #-}
foldrPrimArray' :: forall a b. Prim a => (a -> b -> b) -> b -> PrimArray a -> b
foldrPrimArray' f z0 arr = go (sizeofPrimArray arr - 1) z0
  where
    go !i !acc
      | i < 0 = acc
      | otherwise = go (i - 1) (f (indexPrimArray arr i) acc)

-- | Lazy left-associated fold over the elements of a 'PrimArray'.
{-# INLINE foldlPrimArray #-}
foldlPrimArray :: forall a b. Prim a => (b -> a -> b) -> b -> PrimArray a -> b
foldlPrimArray f z arr = go (sizeofPrimArray arr - 1)
  where
    go !i
      | i < 0 = z
      | otherwise = f (go (i - 1)) (indexPrimArray arr i)

-- | Strict left-associated fold over the elements of a 'PrimArray'.
{-# INLINE foldlPrimArray' #-}
foldlPrimArray' :: forall a b. Prim a => (b -> a -> b) -> b -> PrimArray a -> b
foldlPrimArray' f z0 arr = go 0 z0
  where
    !sz = sizeofPrimArray arr
    go !i !acc
      | i < sz = go (i + 1) (f acc (indexPrimArray arr i))
      | otherwise = acc

-- | Strict left-associated fold over the elements of a 'PrimArray'.
{-# INLINE foldlPrimArrayM' #-}
foldlPrimArrayM' :: (Prim a, Monad m) => (b -> a -> m b) -> b -> PrimArray a -> m b
foldlPrimArrayM' f z0 arr = go 0 z0
  where
    !sz = sizeofPrimArray arr
    go !i !acc1
      | i < sz = do
          acc2 <- f acc1 (indexPrimArray arr i)
          go (i + 1) acc2
      | otherwise = return acc1

-- | Traverse a primitive array. The traversal forces the resulting values and
-- writes them to the new primitive array as it performs the monadic effects.
-- Consequently:
--
-- >>> traversePrimArrayP (\x -> print x $> bool x undefined (x == 2)) (fromList [1, 2, 3 :: Int])
-- 1
-- 2
-- *** Exception: Prelude.undefined
--
-- In many situations, 'traversePrimArrayP' can replace 'traversePrimArray',
-- changing the strictness characteristics of the traversal but typically improving
-- the performance. Consider the following short-circuiting traversal:
--
-- > incrPositiveA :: PrimArray Int -> Maybe (PrimArray Int)
-- > incrPositiveA xs = traversePrimArray (\x -> bool Nothing (Just (x + 1)) (x > 0)) xs
--
-- This can be rewritten using 'traversePrimArrayP'. To do this, we must
-- change the traversal context to @MaybeT (ST s)@, which has a 'PrimMonad'
-- instance:
--
-- > incrPositiveB :: PrimArray Int -> Maybe (PrimArray Int)
-- > incrPositiveB xs = runST $ runMaybeT $ traversePrimArrayP
-- >   (\x -> bool (MaybeT (return Nothing)) (MaybeT (return (Just (x + 1)))) (x > 0))
-- >   xs
--
-- Benchmarks demonstrate that the second implementation runs 150 times
-- faster than the first. It also results in fewer allocations.
{-# INLINE traversePrimArrayP #-}
traversePrimArrayP :: (PrimMonad m, Prim a, Prim b)
  => (a -> m b)
  -> PrimArray a
  -> m (PrimArray b)
traversePrimArrayP f arr = do
  let !sz = sizeofPrimArray arr
  marr <- newPrimArray sz
  let go !ix = if ix < sz
        then do
          b <- f (indexPrimArray arr ix)
          writePrimArray marr ix b
          go (ix + 1)
        else return ()
  go 0
  unsafeFreezePrimArray marr

-- | Filter the primitive array, keeping the elements for which the monadic
-- predicate evaluates true.
{-# INLINE filterPrimArrayP #-}
filterPrimArrayP :: (PrimMonad m, Prim a)
  => (a -> m Bool)
  -> PrimArray a
  -> m (PrimArray a)
filterPrimArrayP f arr = do
  let !sz = sizeofPrimArray arr
  marr <- newPrimArray sz
  let go !ixSrc !ixDst = if ixSrc < sz
        then do
          let a = indexPrimArray arr ixSrc
          b <- f a
          if b
            then do
              writePrimArray marr ixDst a
              go (ixSrc + 1) (ixDst + 1)
            else go (ixSrc + 1) ixDst
        else return ixDst
  lenDst <- go 0 0
  marr' <- resizeMutablePrimArray marr lenDst
  unsafeFreezePrimArray marr'

-- | Map over the primitive array, keeping the elements for which the monadic
-- predicate provides a 'Just'.
{-# INLINE mapMaybePrimArrayP #-}
mapMaybePrimArrayP :: (PrimMonad m, Prim a, Prim b)
  => (a -> m (Maybe b))
  -> PrimArray a
  -> m (PrimArray b)
mapMaybePrimArrayP f arr = do
  let !sz = sizeofPrimArray arr
  marr <- newPrimArray sz
  let go !ixSrc !ixDst = if ixSrc < sz
        then do
          let a = indexPrimArray arr ixSrc
          mb <- f a
          case mb of
            Just b -> do
              writePrimArray marr ixDst b
              go (ixSrc + 1) (ixDst + 1)
            Nothing -> go (ixSrc + 1) ixDst
        else return ixDst
  lenDst <- go 0 0
  marr' <- resizeMutablePrimArray marr lenDst
  unsafeFreezePrimArray marr'

-- | Generate a primitive array by evaluating the monadic generator function
-- at each index.
{-# INLINE generatePrimArrayP #-}
generatePrimArrayP :: (PrimMonad m, Prim a)
  => Int -- ^ length
  -> (Int -> m a) -- ^ generator
  -> m (PrimArray a)
generatePrimArrayP sz f = do
  marr <- newPrimArray sz
  let go !ix = if ix < sz
        then do
          b <- f ix
          writePrimArray marr ix b
          go (ix + 1)
        else return ()
  go 0
  unsafeFreezePrimArray marr

-- | Execute the monadic action the given number of times and store the
-- results in a primitive array.
{-# INLINE replicatePrimArrayP #-}
replicatePrimArrayP :: (PrimMonad m, Prim a)
  => Int
  -> m a
  -> m (PrimArray a)
replicatePrimArrayP sz f = do
  marr <- newPrimArray sz
  let go !ix = if ix < sz
        then do
          b <- f
          writePrimArray marr ix b
          go (ix + 1)
        else return ()
  go 0
  unsafeFreezePrimArray marr


-- | Map over the elements of a primitive array.
{-# INLINE mapPrimArray #-}
mapPrimArray :: (Prim a, Prim b)
  => (a -> b)
  -> PrimArray a
  -> PrimArray b
mapPrimArray f arr = runST $ do
  let !sz = sizeofPrimArray arr
  marr <- newPrimArray sz
  let go !ix = if ix < sz
        then do
          let b = f (indexPrimArray arr ix)
          writePrimArray marr ix b
          go (ix + 1)
        else return ()
  go 0
  unsafeFreezePrimArray marr

-- | Indexed map over the elements of a primitive array.
{-# INLINE imapPrimArray #-}
imapPrimArray :: (Prim a, Prim b)
  => (Int -> a -> b)
  -> PrimArray a
  -> PrimArray b
imapPrimArray f arr = runST $ do
  let !sz = sizeofPrimArray arr
  marr <- newPrimArray sz
  let go !ix = if ix < sz
        then do
          let b = f ix (indexPrimArray arr ix)
          writePrimArray marr ix b
          go (ix + 1)
        else return ()
  go 0
  unsafeFreezePrimArray marr

-- | Filter elements of a primitive array according to a predicate.
{-# INLINE filterPrimArray #-}
filterPrimArray :: Prim a
  => (a -> Bool)
  -> PrimArray a
  -> PrimArray a
filterPrimArray p arr = runST $ do
  let !sz = sizeofPrimArray arr
  marr <- newPrimArray sz
  let go !ixSrc !ixDst = if ixSrc < sz
        then do
          let !a = indexPrimArray arr ixSrc
          if p a
            then do
              writePrimArray marr ixDst a
              go (ixSrc + 1) (ixDst + 1)
            else go (ixSrc + 1) ixDst
        else return ixDst
  dstLen <- go 0 0
  marr' <- resizeMutablePrimArray marr dstLen
  unsafeFreezePrimArray marr'

-- | Filter the primitive array, keeping the elements for which the monadic
-- predicate evaluates true.
filterPrimArrayA ::
     (Applicative f, Prim a)
  => (a -> f Bool) -- ^ mapping function
  -> PrimArray a -- ^ primitive array
  -> f (PrimArray a)
filterPrimArrayA f = \ !ary ->
  let
    !len = sizeofPrimArray ary
    go !ixSrc
      | ixSrc == len = pure $ IxSTA $ \ixDst _ -> return ixDst
      | otherwise = let x = indexPrimArray ary ixSrc in
          liftA2
            (\keep (IxSTA m) -> IxSTA $ \ixDst mary -> if keep
              then writePrimArray (MutablePrimArray mary) ixDst x >> m (ixDst + 1) mary
              else m ixDst mary
            )
            (f x)
            (go (ixSrc + 1))
  in if len == 0
     then pure emptyPrimArray
     else runIxSTA len <$> go 0

-- | Map over the primitive array, keeping the elements for which the applicative
-- predicate provides a 'Just'.
mapMaybePrimArrayA ::
     (Applicative f, Prim a, Prim b)
  => (a -> f (Maybe b)) -- ^ mapping function
  -> PrimArray a -- ^ primitive array
  -> f (PrimArray b)
mapMaybePrimArrayA f = \ !ary ->
  let
    !len = sizeofPrimArray ary
    go !ixSrc
      | ixSrc == len = pure $ IxSTA $ \ixDst _ -> return ixDst
      | otherwise = let x = indexPrimArray ary ixSrc in
          liftA2
            (\mb (IxSTA m) -> IxSTA $ \ixDst mary -> case mb of
              Just b -> writePrimArray (MutablePrimArray mary) ixDst b >> m (ixDst + 1) mary
              Nothing -> m ixDst mary
            )
            (f x)
            (go (ixSrc + 1))
  in if len == 0
     then pure emptyPrimArray
     else runIxSTA len <$> go 0

-- | Map over a primitive array, optionally discarding some elements. This
--   has the same behavior as @Data.Maybe.mapMaybe@.
{-# INLINE mapMaybePrimArray #-}
mapMaybePrimArray :: (Prim a, Prim b)
  => (a -> Maybe b)
  -> PrimArray a
  -> PrimArray b
mapMaybePrimArray p arr = runST $ do
  let !sz = sizeofPrimArray arr
  marr <- newPrimArray sz
  let go !ixSrc !ixDst = if ixSrc < sz
        then do
          let !a = indexPrimArray arr ixSrc
          case p a of
            Just b -> do
              writePrimArray marr ixDst b
              go (ixSrc + 1) (ixDst + 1)
            Nothing -> go (ixSrc + 1) ixDst
        else return ixDst
  dstLen <- go 0 0
  marr' <- resizeMutablePrimArray marr dstLen
  unsafeFreezePrimArray marr'


-- | Traverse a primitive array. The traversal performs all of the applicative
-- effects /before/ forcing the resulting values and writing them to the new
-- primitive array. Consequently:
--
-- >>> traversePrimArray (\x -> print x $> bool x undefined (x == 2)) (fromList [1, 2, 3 :: Int])
-- 1
-- 2
-- 3
-- *** Exception: Prelude.undefined
--
-- The function 'traversePrimArrayP' always outperforms this function, but it
-- requires a 'PrimMonad' constraint, and it forces the values as
-- it performs the effects.
traversePrimArray ::
     (Applicative f, Prim a, Prim b)
  => (a -> f b) -- ^ mapping function
  -> PrimArray a -- ^ primitive array
  -> f (PrimArray b)
traversePrimArray f = \ !ary ->
  let
    !len = sizeofPrimArray ary
    go !i
      | i == len = pure $ STA $ \mary -> unsafeFreezePrimArray (MutablePrimArray mary)
      | x <- indexPrimArray ary i
      = liftA2 (\b (STA m) -> STA $ \mary ->
                  writePrimArray (MutablePrimArray mary) i b >> m mary)
               (f x) (go (i + 1))
  in if len == 0
     then pure emptyPrimArray
     else runSTA len <$> go 0

-- | Traverse a primitive array with the index of each element.
itraversePrimArray ::
     (Applicative f, Prim a, Prim b)
  => (Int -> a -> f b)
  -> PrimArray a
  -> f (PrimArray b)
itraversePrimArray f = \ !ary ->
  let
    !len = sizeofPrimArray ary
    go !i
      | i == len = pure $ STA $ \mary -> unsafeFreezePrimArray (MutablePrimArray mary)
      | x <- indexPrimArray ary i
      = liftA2 (\b (STA m) -> STA $ \mary ->
                  writePrimArray (MutablePrimArray mary) i b >> m mary)
               (f i x) (go (i + 1))
  in if len == 0
     then pure emptyPrimArray
     else runSTA len <$> go 0

-- | Traverse a primitive array with the indices. The traversal forces the
-- resulting values and writes them to the new primitive array as it performs
-- the monadic effects.
{-# INLINE itraversePrimArrayP #-}
itraversePrimArrayP :: (Prim a, Prim b, PrimMonad m)
  => (Int -> a -> m b)
  -> PrimArray a
  -> m (PrimArray b)
itraversePrimArrayP f arr = do
  let !sz = sizeofPrimArray arr
  marr <- newPrimArray sz
  let go !ix
        | ix < sz = do
            writePrimArray marr ix =<< f ix (indexPrimArray arr ix)
            go (ix + 1)
        | otherwise = return ()
  go 0
  unsafeFreezePrimArray marr

-- | Generate a primitive array.
{-# INLINE generatePrimArray #-}
generatePrimArray :: Prim a
  => Int -- ^ length
  -> (Int -> a) -- ^ element from index
  -> PrimArray a
generatePrimArray len f = runST $ do
  marr <- newPrimArray len
  let go !ix = if ix < len
        then do
          writePrimArray marr ix (f ix)
          go (ix + 1)
        else return ()
  go 0
  unsafeFreezePrimArray marr

-- | Create a primitive array by copying the element the given
-- number of times.
{-# INLINE replicatePrimArray #-}
replicatePrimArray :: Prim a
  => Int -- ^ length
  -> a -- ^ element
  -> PrimArray a
replicatePrimArray len a = runST $ do
  marr <- newPrimArray len
  setPrimArray marr 0 len a
  unsafeFreezePrimArray marr

-- | Generate a primitive array by evaluating the applicative generator
-- function at each index.
{-# INLINE generatePrimArrayA #-}
generatePrimArrayA ::
     (Applicative f, Prim a)
  => Int -- ^ length
  -> (Int -> f a) -- ^ element from index
  -> f (PrimArray a)
generatePrimArrayA len f =
  let
    go !i
      | i == len = pure $ STA $ \mary -> unsafeFreezePrimArray (MutablePrimArray mary)
      | otherwise
      = liftA2 (\b (STA m) -> STA $ \mary ->
                  writePrimArray (MutablePrimArray mary) i b >> m mary)
               (f i) (go (i + 1))
  in if len == 0
     then pure emptyPrimArray
     else runSTA len <$> go 0

-- | Execute the applicative action the given number of times and store the
-- results in a vector.
{-# INLINE replicatePrimArrayA #-}
replicatePrimArrayA ::
     (Applicative f, Prim a)
  => Int -- ^ length
  -> f a -- ^ applicative element producer
  -> f (PrimArray a)
replicatePrimArrayA len f =
  let
    go !i
      | i == len = pure $ STA $ \mary -> unsafeFreezePrimArray (MutablePrimArray mary)
      | otherwise
      = liftA2 (\b (STA m) -> STA $ \mary ->
                  writePrimArray (MutablePrimArray mary) i b >> m mary)
               f (go (i + 1))
  in if len == 0
     then pure emptyPrimArray
     else runSTA len <$> go 0

-- | Traverse the primitive array, discarding the results. There
-- is no 'PrimMonad' variant of this function since it would not provide
-- any performance benefit.
traversePrimArray_ ::
     (Applicative f, Prim a)
  => (a -> f b)
  -> PrimArray a
  -> f ()
traversePrimArray_ f a = go 0 where
  !sz = sizeofPrimArray a
  go !ix = if ix < sz
    then f (indexPrimArray a ix) *> go (ix + 1)
    else pure ()

-- | Traverse the primitive array with the indices, discarding the results.
-- There is no 'PrimMonad' variant of this function since it would not
-- provide any performance benefit.
itraversePrimArray_ ::
     (Applicative f, Prim a)
  => (Int -> a -> f b)
  -> PrimArray a
  -> f ()
itraversePrimArray_ f a = go 0 where
  !sz = sizeofPrimArray a
  go !ix = if ix < sz
    then f ix (indexPrimArray a ix) *> go (ix + 1)
    else pure ()

newtype IxSTA a = IxSTA {_runIxSTA :: forall s. Int -> MutableByteArray# s -> ST s Int}

runIxSTA :: forall a. Prim a
  => Int -- maximum possible size
  -> IxSTA a
  -> PrimArray a
runIxSTA !szUpper = \ (IxSTA m) -> runST $ do
  ar :: MutablePrimArray s a <- newPrimArray szUpper
  sz <- m 0 (unMutablePrimArray ar)
  ar' <- resizeMutablePrimArray ar sz
  unsafeFreezePrimArray ar'
{-# INLINE runIxSTA #-}

newtype STA a = STA {_runSTA :: forall s. MutableByteArray# s -> ST s (PrimArray a)}

runSTA :: forall a. Prim a => Int -> STA a -> PrimArray a
runSTA !sz = \ (STA m) -> runST $ newPrimArray sz >>= \ (ar :: MutablePrimArray s a) -> m (unMutablePrimArray ar)
{-# INLINE runSTA #-}

unMutablePrimArray :: MutablePrimArray s a -> MutableByteArray# s
unMutablePrimArray (MutablePrimArray m) = m

{- $effectfulMapCreate
The naming conventions adopted in this section are explained in the
documentation of the @Data.Primitive@ module.
-}

-- | Create a /pinned/ primitive array of the specified size in elements. The garbage
-- collector is guaranteed not to move it.
--
-- @since 0.7.1.0
newPinnedPrimArray :: forall m a. (PrimMonad m, Prim a)
  => Int -> m (MutablePrimArray (PrimState m) a)
{-# INLINE newPinnedPrimArray #-}
newPinnedPrimArray (I# n#)
  = primitive (\s# -> case newPinnedByteArray# (n# *# sizeOf# (undefined :: a)) s# of
                        (# s'#, arr# #) -> (# s'#, MutablePrimArray arr# #))

-- | Create a /pinned/ primitive array of the specified size in elements and
-- with the alignment given by its 'Prim' instance. The garbage collector is
-- guaranteed not to move it.
--
-- @since 0.7.0.0
newAlignedPinnedPrimArray :: forall m a. (PrimMonad m, Prim a)
  => Int -> m (MutablePrimArray (PrimState m) a)
{-# INLINE newAlignedPinnedPrimArray #-}
newAlignedPinnedPrimArray (I# n#)
  = primitive (\s# -> case newAlignedPinnedByteArray# (n# *# sizeOf# (undefined :: a)) (alignment# (undefined :: a)) s# of
                        (# s'#, arr# #) -> (# s'#, MutablePrimArray arr# #))

-- | Yield a pointer to the array's data. This operation is only safe on
-- /pinned/ prim arrays allocated by 'newPinnedByteArray' or
-- 'newAlignedPinnedByteArray'.
--
-- @since 0.7.1.0
primArrayContents :: PrimArray a -> Ptr a
{-# INLINE primArrayContents #-}
primArrayContents (PrimArray arr#) = Ptr (byteArrayContents# arr#)

-- | Yield a pointer to the array's data. This operation is only safe on
-- /pinned/ byte arrays allocated by 'newPinnedByteArray' or
-- 'newAlignedPinnedByteArray'.
--
-- @since 0.7.1.0
mutablePrimArrayContents :: MutablePrimArray s a -> Ptr a
{-# INLINE mutablePrimArrayContents #-}
mutablePrimArrayContents (MutablePrimArray arr#)
  = Ptr (byteArrayContents# (unsafeCoerce# arr#))

-- | Return a newly allocated array with the specified subrange of the
-- provided array. The provided array should contain the full subrange
-- specified by the two Ints, but this is not checked.
clonePrimArray :: Prim a
  => PrimArray a -- ^ source array
  -> Int     -- ^ offset into destination array
  -> Int     -- ^ number of elements to copy
  -> PrimArray a
{-# INLINE clonePrimArray #-}
clonePrimArray src off n = runPrimArray $ do
  dst <- newPrimArray n
  copyPrimArray dst 0 src off n
  return dst

-- | Return a newly allocated mutable array with the specified subrange of
-- the provided mutable array. The provided mutable array should contain the
-- full subrange specified by the two Ints, but this is not checked.
cloneMutablePrimArray :: (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ source array
  -> Int -- ^ offset into destination array
  -> Int -- ^ number of elements to copy
  -> m (MutablePrimArray (PrimState m) a)
{-# INLINE cloneMutablePrimArray #-}
cloneMutablePrimArray src off n = do
  dst <- newPrimArray n
  copyMutablePrimArray dst 0 src off n
  return dst

#if MIN_VERSION_base(4,10,0) /* In new GHCs, runRW# is available. */
runPrimArray
  :: (forall s. ST s (MutablePrimArray s a))
  -> PrimArray a
runPrimArray m = PrimArray (runPrimArray# m)

runPrimArray#
  :: (forall s. ST s (MutablePrimArray s a))
  -> ByteArray#
runPrimArray# m = case runRW# $ \s ->
  case unST m s of { (# s', MutablePrimArray mary# #) ->
  unsafeFreezeByteArray# mary# s'} of (# _, ary# #) -> ary#

unST :: ST s a -> State# s -> (# State# s, a #)
unST (GHCST.ST f) = f
#else /* In older GHCs, runRW# is not available. */
runPrimArray
  :: (forall s. ST s (MutablePrimArray s a))
  -> PrimArray a
runPrimArray m = runST $ m >>= unsafeFreezePrimArray
#endif
