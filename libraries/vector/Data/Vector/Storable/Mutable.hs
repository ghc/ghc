{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, MagicHash, MultiParamTypeClasses, ScopedTypeVariables #-}



-- |
-- Module      : Data.Vector.Storable.Mutable
-- Copyright   : (c) Roman Leshchinskiy 2009-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Mutable vectors based on Storable.
--

module Data.Vector.Storable.Mutable(
  -- * Mutable vectors of 'Storable' types
  MVector(..), IOVector, STVector, Storable,

  -- * Accessors

  -- ** Length information
  length, null,

  -- ** Extracting subvectors
  slice, init, tail, take, drop, splitAt,
  unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop,

  -- ** Overlapping
  overlaps,

  -- * Construction

  -- ** Initialisation
  new, unsafeNew, replicate, replicateM, generate, generateM, clone,

  -- ** Growing
  grow, unsafeGrow,

  -- ** Restricting memory usage
  clear,

  -- * Accessing individual elements
  read, write, modify, modifyM, swap, exchange,
  unsafeRead, unsafeWrite, unsafeModify, unsafeModifyM, unsafeSwap, unsafeExchange,

  -- * Folds
  mapM_, imapM_, forM_, iforM_,
  foldl, foldl', foldM, foldM',
  foldr, foldr', foldrM, foldrM',
  ifoldl, ifoldl', ifoldM, ifoldM',
  ifoldr, ifoldr', ifoldrM, ifoldrM',

  -- * Modifying vectors
  nextPermutation,

  -- ** Filling and copying
  set, copy, move, unsafeCopy, unsafeMove,

  -- * Unsafe conversions
  unsafeCast,


  -- * Raw pointers
  unsafeFromForeignPtr, unsafeFromForeignPtr0,
  unsafeToForeignPtr,   unsafeToForeignPtr0,
  unsafeWith
) where

import Control.DeepSeq ( NFData(rnf)
#if MIN_VERSION_deepseq(1,4,3)
                       , NFData1(liftRnf)
#endif
                       )

import qualified Data.Vector.Generic.Mutable as G
import Data.Vector.Storable.Internal

import Foreign.Storable
import Foreign.ForeignPtr

#if __GLASGOW_HASKELL__ >= 706
import GHC.ForeignPtr (mallocPlainForeignPtrAlignedBytes)
#elif __GLASGOW_HASKELL__ >= 700
import Data.Primitive.ByteArray (MutableByteArray(..), newAlignedPinnedByteArray,
                                 unsafeFreezeByteArray)
import GHC.Prim (byteArrayContents#, unsafeCoerce#)
import GHC.ForeignPtr
#endif

import GHC.Base ( Int(..) )

import Foreign.Ptr (castPtr,plusPtr)
import Foreign.Marshal.Array ( advancePtr, copyArray, moveArray )

import Control.Monad.Primitive
import Data.Primitive.Types (Prim)
import qualified Data.Primitive.Types as DPT

import GHC.Word (Word8, Word16, Word32, Word64)
import GHC.Ptr (Ptr(..))

import Prelude hiding ( length, null, replicate, reverse, map, read,
                        take, drop, splitAt, init, tail, foldr, foldl, mapM_ )

import Data.Typeable ( Typeable )


-- Data.Vector.Internal.Check is not needed
#define NOT_VECTOR_MODULE
#include "vector.h"



-- | Mutable 'Storable'-based vectors
data MVector s a = MVector {-# UNPACK #-} !Int
                           {-# UNPACK #-} !(ForeignPtr a)
        deriving ( Typeable )

type IOVector = MVector RealWorld
type STVector s = MVector s

instance NFData (MVector s a) where
  rnf (MVector _ _) = ()

#if MIN_VERSION_deepseq(1,4,3)
instance NFData1 (MVector s) where
  liftRnf _ (MVector _ _) = ()
#endif

instance Storable a => G.MVector MVector a where
  {-# INLINE basicLength #-}
  basicLength (MVector n _) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m (MVector _ fp) = MVector m (updPtr (`advancePtr` j) fp)

  -- FIXME: this relies on non-portable pointer comparisons
  {-# INLINE basicOverlaps #-}
  basicOverlaps (MVector m fp) (MVector n fq)
    = between p q (q `advancePtr` n) || between q p (p `advancePtr` m)
    where
      between x y z = x >= y && x < z
      p = getPtr fp
      q = getPtr fq

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n
    | n < 0 = error $ "Storable.basicUnsafeNew: negative length: " ++ show n
    | n > mx = error $ "Storable.basicUnsafeNew: length too large: " ++ show n
    | otherwise = unsafePrimToPrim $ do
        fp <- mallocVector n
        return $ MVector n fp
    where
      size = sizeOf (undefined :: a) `max` 1
      mx = maxBound `quot` size :: Int

  {-# INLINE basicInitialize #-}
  basicInitialize = storableZero

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MVector _ fp) i
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp (`peekElemOff` i)

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MVector _ fp) i x
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp $ \p -> pokeElemOff p i x

  {-# INLINE basicSet #-}
  basicSet = storableSet

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector n fp) (MVector _ fq)
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp $ \p ->
      unsafeWithForeignPtr fq $ \q ->
      copyArray p q n

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MVector n fp) (MVector _ fq)
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp $ \p ->
      unsafeWithForeignPtr fq $ \q ->
      moveArray p q n

storableZero :: forall a m. (Storable a, PrimMonad m) => MVector (PrimState m) a -> m ()
{-# INLINE storableZero #-}
storableZero (MVector n fp) = unsafePrimToPrim . unsafeWithForeignPtr fp $ \ptr-> do
  memsetPrimPtr_vector (castPtr ptr) byteSize (0 :: Word8)
 where
 x :: a
 x = undefined
 byteSize :: Int
 byteSize = n * sizeOf x

storableSet :: (Storable a, PrimMonad m) => MVector (PrimState m) a -> a -> m ()
{-# INLINE storableSet #-}
storableSet (MVector n fp) x
  | n == 0 = return ()
  | otherwise = unsafePrimToPrim $
                case sizeOf x of
                  1 -> storableSetAsPrim n fp x (undefined :: Word8)
                  2 -> storableSetAsPrim n fp x (undefined :: Word16)
                  4 -> storableSetAsPrim n fp x (undefined :: Word32)
                  8 -> storableSetAsPrim n fp x (undefined :: Word64)
                  _ -> unsafeWithForeignPtr fp $ \p -> do
                       poke p x

                       let do_set i
                             | 2*i < n = do
                                 copyArray (p `advancePtr` i) p i
                                 do_set (2*i)
                             | otherwise = copyArray (p `advancePtr` i) p (n-i)

                       do_set 1

storableSetAsPrim
  :: forall a b . (Storable a, Prim b) => Int -> ForeignPtr a -> a -> b -> IO ()
{-# INLINE [0] storableSetAsPrim #-}
storableSetAsPrim n fp x _y = unsafeWithForeignPtr fp $ \ ptr  -> do
    poke ptr x
     -- we dont equate storable and prim reps, so we need to write to a slot
     -- in storable
     -- then read it back as a prim
    w<- peakPrimPtr_vector ((castPtr ptr) :: Ptr  b) 0
    memsetPrimPtr_vector ((castPtr ptr) `plusPtr` sizeOf x ) (n-1)  w



{-
AFTER primitive 0.7 is pretty old, move to using setPtr. which is really
a confusing misnomer for whats often called memset (intialize )
-}
-- Fill a memory block with the given value. The length is in
-- elements of type @a@ rather than in bytes.
memsetPrimPtr_vector :: forall a c m. (Prim c, PrimMonad m) => Ptr a -> Int -> c -> m ()
memsetPrimPtr_vector (Ptr addr#) (I# n#) x = primitive_ (DPT.setOffAddr# addr# 0# n# x)
{-# INLINE memsetPrimPtr_vector #-}


-- Read a value from a memory position given by an address and an offset.
-- The offset is in elements of type @a@ rather than in bytes.
peakPrimPtr_vector :: (Prim a, PrimMonad m) => Ptr a -> Int -> m a
peakPrimPtr_vector (Ptr addr#) (I# i#) = primitive (DPT.readOffAddr# addr# i#)
{-# INLINE peakPrimPtr_vector #-}

{-# INLINE mallocVector #-}
mallocVector :: Storable a => Int -> IO (ForeignPtr a)
mallocVector =
#if __GLASGOW_HASKELL__ >= 706
  doMalloc undefined
  where
    doMalloc :: Storable b => b -> Int -> IO (ForeignPtr b)
    doMalloc dummy size =
      mallocPlainForeignPtrAlignedBytes (size * sizeOf dummy) (alignment dummy)
#elif __GLASGOW_HASKELL__ >= 700
  doMalloc undefined
  where
    doMalloc :: Storable b => b -> Int -> IO (ForeignPtr b)
    doMalloc dummy size = do
      arr@(MutableByteArray arr#) <- newAlignedPinnedByteArray arrSize arrAlign
      newConcForeignPtr
        (Ptr (byteArrayContents# (unsafeCoerce# arr#)))
        -- Keep reference to mutable byte array until whole ForeignPtr goes out
        -- of scope.
        (touch arr)
      where
        arrSize  = size * sizeOf dummy
        arrAlign = alignment dummy
#else
    mallocForeignPtrArray
#endif

-- Length information
-- ------------------

-- | Length of the mutable vector.
length :: Storable a => MVector s a -> Int
{-# INLINE length #-}
length = G.length

-- | Check whether the vector is empty
null :: Storable a => MVector s a -> Bool
{-# INLINE null #-}
null = G.null

-- Extracting subvectors
-- ---------------------

-- | Yield a part of the mutable vector without copying it. The vector must
-- contain at least @i+n@ elements.
slice :: Storable a
      => Int  -- ^ @i@ starting index
      -> Int  -- ^ @n@ length
      -> MVector s a
      -> MVector s a
{-# INLINE slice #-}
slice = G.slice

take :: Storable a => Int -> MVector s a -> MVector s a
{-# INLINE take #-}
take = G.take

drop :: Storable a => Int -> MVector s a -> MVector s a
{-# INLINE drop #-}
drop = G.drop

splitAt :: Storable a => Int -> MVector s a -> (MVector s a, MVector s a)
{-# INLINE splitAt #-}
splitAt = G.splitAt

init :: Storable a => MVector s a -> MVector s a
{-# INLINE init #-}
init = G.init

tail :: Storable a => MVector s a -> MVector s a
{-# INLINE tail #-}
tail = G.tail

-- | Yield a part of the mutable vector without copying it. No bounds checks
-- are performed.
unsafeSlice :: Storable a
            => Int  -- ^ starting index
            -> Int  -- ^ length of the slice
            -> MVector s a
            -> MVector s a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

unsafeTake :: Storable a => Int -> MVector s a -> MVector s a
{-# INLINE unsafeTake #-}
unsafeTake = G.unsafeTake

unsafeDrop :: Storable a => Int -> MVector s a -> MVector s a
{-# INLINE unsafeDrop #-}
unsafeDrop = G.unsafeDrop

unsafeInit :: Storable a => MVector s a -> MVector s a
{-# INLINE unsafeInit #-}
unsafeInit = G.unsafeInit

unsafeTail :: Storable a => MVector s a -> MVector s a
{-# INLINE unsafeTail #-}
unsafeTail = G.unsafeTail

-- Overlapping
-- -----------

-- | Check whether two vectors overlap.
overlaps :: Storable a => MVector s a -> MVector s a -> Bool
{-# INLINE overlaps #-}
overlaps = G.overlaps

-- Initialisation
-- --------------

-- | Create a mutable vector of the given length.
new :: (PrimMonad m, Storable a) => Int -> m (MVector (PrimState m) a)
{-# INLINE new #-}
new = G.new

-- | Create a mutable vector of the given length. The vector content
--   is uninitialized, which means it is filled with whatever underlying memory
--   buffer happens to contain.
--
-- @since 0.5
unsafeNew :: (PrimMonad m, Storable a) => Int -> m (MVector (PrimState m) a)
{-# INLINE unsafeNew #-}
unsafeNew = G.unsafeNew

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with an initial value.
replicate :: (PrimMonad m, Storable a) => Int -> a -> m (MVector (PrimState m) a)
{-# INLINE replicate #-}
replicate = G.replicate

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with values produced by repeatedly executing the monadic action.
replicateM :: (PrimMonad m, Storable a) => Int -> m a -> m (MVector (PrimState m) a)
{-# INLINE replicateM #-}
replicateM = G.replicateM

-- | /O(n)/ Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with the results of applying the function to each index.
--
-- @since 0.12.3.0
generate :: (PrimMonad m, Storable a) => Int -> (Int -> a) -> m (MVector (PrimState m) a)
{-# INLINE generate #-}
generate = G.generate

-- | /O(n)/ Create a mutable vector of the given length (0 if the length is
-- negative) and fill it with the results of applying the monadic function to each
-- index. Iteration starts at index 0.
--
-- @since 0.12.3.0
generateM :: (PrimMonad m, Storable a) => Int -> (Int -> m a) -> m (MVector (PrimState m) a)
{-# INLINE generateM #-}
generateM = G.generateM

-- | Create a copy of a mutable vector.
clone :: (PrimMonad m, Storable a)
      => MVector (PrimState m) a -> m (MVector (PrimState m) a)
{-# INLINE clone #-}
clone = G.clone

-- Growing
-- -------

-- | Grow a storable vector by the given number of elements. The number must be
-- non-negative. Same semantics as in `G.grow` for generic vector.
--
-- ====__Examples__
--
-- >>> import qualified Data.Vector.Storable as VS
-- >>> import qualified Data.Vector.Storable.Mutable as MVS
-- >>> mv <- VS.thaw $ VS.fromList ([10, 20, 30] :: [Int])
-- >>> mv' <- MVS.grow mv 2
--
-- Extra memory at the end of the newly allocated vector is initialized to 0
-- bytes, which for `Storable` instance will usually correspond to some default
-- value for a particular type, eg. @0@ for @Int@, @False@ for @Bool@,
-- etc. However, if `unsafeGrow` was used instead this would not have been
-- guaranteed and some garbage would be there instead:
--
-- >>> VS.unsafeFreeze mv'
-- [10,20,30,0,0]
--
-- Having the extra space we can write new values in there:
--
-- >>> MVS.write mv' 3 999
-- >>> VS.unsafeFreeze mv'
-- [10,20,30,999,0]
--
-- It is important to note that the source mutable vector is not affected when
-- the newly allocated one is mutated.
--
-- >>> MVS.write mv' 2 888
-- >>> VS.unsafeFreeze mv'
-- [10,20,888,999,0]
-- >>> VS.unsafeFreeze mv
-- [10,20,30]
--
-- @since 0.5
grow :: (PrimMonad m, Storable a)
     => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
{-# INLINE grow #-}
grow = G.grow

-- | Grow a vector by the given number of elements. The number must be non-negative but
-- this is not checked. Same semantics as in `G.unsafeGrow` for generic vector.
--
-- @since 0.5
unsafeGrow :: (PrimMonad m, Storable a)
           => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
{-# INLINE unsafeGrow #-}
unsafeGrow = G.unsafeGrow

-- Restricting memory usage
-- ------------------------

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects. This is usually a noop for unboxed vectors.
clear :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> m ()
{-# INLINE clear #-}
clear = G.clear

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position.
read :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> m a
{-# INLINE read #-}
read = G.read

-- | Replace the element at the given position.
write
    :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> a -> m ()
{-# INLINE write #-}
write = G.write

-- | Modify the element at the given position.
modify :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> (a -> a) -> Int -> m ()
{-# INLINE modify #-}
modify = G.modify

-- | Modify the element at the given position using a monadic function.
--
-- @since 0.12.3.0
modifyM :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> (a -> m a) -> Int -> m ()
{-# INLINE modifyM #-}
modifyM = G.modifyM

-- | Swap the elements at the given positions.
swap
    :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> Int -> m ()
{-# INLINE swap #-}
swap = G.swap

-- | Replace the element at the given position and return the old element.
exchange :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> a -> m a
{-# INLINE exchange #-}
exchange = G.exchange

-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> m a
{-# INLINE unsafeRead #-}
unsafeRead = G.unsafeRead

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite
    :: (PrimMonad m, Storable a) =>  MVector (PrimState m) a -> Int -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite = G.unsafeWrite

-- | Modify the element at the given position. No bounds checks are performed.
unsafeModify :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> (a -> a) -> Int -> m ()
{-# INLINE unsafeModify #-}
unsafeModify = G.unsafeModify

-- | Modify the element at the given position using a monadic
-- function. No bounds checks are performed.
--
-- @since 0.12.3.0
unsafeModifyM :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> (a -> m a) -> Int -> m ()
{-# INLINE unsafeModifyM #-}
unsafeModifyM = G.unsafeModifyM

-- | Swap the elements at the given positions. No bounds checks are performed.
unsafeSwap
    :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> Int -> m ()
{-# INLINE unsafeSwap #-}
unsafeSwap = G.unsafeSwap

-- | Replace the element at the given position and return the old element. No
-- bounds checks are performed.
unsafeExchange :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> a -> m a
{-# INLINE unsafeExchange #-}
unsafeExchange = G.unsafeExchange

-- Filling and copying
-- -------------------

-- | Set all elements of the vector to the given value.
set :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> a -> m ()
{-# INLINE set #-}
set = G.set

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
copy :: (PrimMonad m, Storable a)
     => MVector (PrimState m) a   -- ^ target
     -> MVector (PrimState m) a   -- ^ source
     -> m ()
{-# INLINE copy #-}
copy = G.copy

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap. This is not checked.
unsafeCopy :: (PrimMonad m, Storable a)
           => MVector (PrimState m) a   -- ^ target
           -> MVector (PrimState m) a   -- ^ source
           -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy = G.unsafeCopy

-- | Move the contents of a vector. The two vectors must have the same
-- length.
--
-- If the vectors do not overlap, then this is equivalent to 'copy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
move :: (PrimMonad m, Storable a)
     => MVector (PrimState m) a   -- ^ target
     -> MVector (PrimState m) a   -- ^ source
     -> m ()
{-# INLINE move #-}
move = G.move

-- | Move the contents of a vector. The two vectors must have the same
-- length, but this is not checked.
--
-- If the vectors do not overlap, then this is equivalent to 'unsafeCopy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
unsafeMove :: (PrimMonad m, Storable a)
           => MVector (PrimState m) a   -- ^ target
           -> MVector (PrimState m) a   -- ^ source
           -> m ()
{-# INLINE unsafeMove #-}
unsafeMove = G.unsafeMove

-- | Compute the next (lexicographically) permutation of given vector in-place.
--   Returns False when input is the last permutation
nextPermutation :: (PrimMonad m, Storable e, Ord e) => MVector (PrimState m) e -> m Bool
{-# INLINE nextPermutation #-}
nextPermutation = G.nextPermutation


-- Folds
-- -----

-- | /O(n)/ Apply the monadic action to every element of the vector, discarding the results.
--
-- @since 0.12.3.0
mapM_ :: (PrimMonad m, Storable a) => (a -> m b) -> MVector (PrimState m) a -> m ()
{-# INLINE mapM_ #-}
mapM_ = G.mapM_

-- | /O(n)/ Apply the monadic action to every element of the vector and its index, discarding the results.
--
-- @since 0.12.3.0
imapM_ :: (PrimMonad m, Storable a) => (Int -> a -> m b) -> MVector (PrimState m) a -> m ()
{-# INLINE imapM_ #-}
imapM_ = G.imapM_

-- | /O(n)/ Apply the monadic action to every element of the vector,
-- discarding the results. It's same as the @flip mapM_@.
--
-- @since 0.12.3.0
forM_ :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ = G.forM_

-- | /O(n)/ Apply the monadic action to every element of the vector
-- and its index, discarding the results. It's same as the @flip imapM_@.
--
-- @since 0.12.3.0
iforM_ :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> (Int -> a -> m b) -> m ()
{-# INLINE iforM_ #-}
iforM_ = G.iforM_

-- | /O(n)/ Pure left fold.
--
-- @since 0.12.3.0
foldl :: (PrimMonad m, Storable a) => (b -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldl #-}
foldl = G.foldl

-- | /O(n)/ Pure left fold with strict accumulator.
--
-- @since 0.12.3.0
foldl' :: (PrimMonad m, Storable a) => (b -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldl' #-}
foldl' = G.foldl'

-- | /O(n)/ Pure left fold (function applied to each element and its index).
--
-- @since 0.12.3.0
ifoldl :: (PrimMonad m, Storable a) => (b -> Int -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldl #-}
ifoldl = G.ifoldl

-- | /O(n)/ Pure left fold with strict accumulator (function applied to each element and its index).
--
-- @since 0.12.3.0
ifoldl' :: (PrimMonad m, Storable a) => (b -> Int -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldl' #-}
ifoldl' = G.ifoldl'

-- | /O(n)/ Pure right fold.
--
-- @since 0.12.3.0
foldr :: (PrimMonad m, Storable a) => (a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldr #-}
foldr = G.foldr

-- | /O(n)/ Pure right fold with strict accumulator.
--
-- @since 0.12.3.0
foldr' :: (PrimMonad m, Storable a) => (a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldr' #-}
foldr' = G.foldr'

-- | /O(n)/ Pure right fold (function applied to each element and its index).
--
-- @since 0.12.3.0
ifoldr :: (PrimMonad m, Storable a) => (Int -> a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldr #-}
ifoldr = G.ifoldr

-- | /O(n)/ Pure right fold with strict accumulator (function applied
-- to each element and its index).
--
-- @since 0.12.3.0
ifoldr' :: (PrimMonad m, Storable a) => (Int -> a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldr' #-}
ifoldr' = G.ifoldr'

-- | /O(n)/ Monadic fold.
--
-- @since 0.12.3.0
foldM :: (PrimMonad m, Storable a) => (b -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldM #-}
foldM = G.foldM

-- | /O(n)/ Monadic fold with strict accumulator.
--
-- @since 0.12.3.0
foldM' :: (PrimMonad m, Storable a) => (b -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldM' #-}
foldM' = G.foldM'

-- | /O(n)/ Monadic fold (action applied to each element and its index).
--
-- @since 0.12.3.0
ifoldM :: (PrimMonad m, Storable a) => (b -> Int -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldM #-}
ifoldM = G.ifoldM

-- | /O(n)/ Monadic fold with strict accumulator (action applied to each element and its index).
--
-- @since 0.12.3.0
ifoldM' :: (PrimMonad m, Storable a) => (b -> Int -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldM' #-}
ifoldM' = G.ifoldM'

-- | /O(n)/ Monadic right fold.
--
-- @since 0.12.3.0
foldrM :: (PrimMonad m, Storable a) => (a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldrM #-}
foldrM = G.foldrM

-- | /O(n)/ Monadic right fold with strict accumulator.
--
-- @since 0.12.3.0
foldrM' :: (PrimMonad m, Storable a) => (a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldrM' #-}
foldrM' = G.foldrM'

-- | /O(n)/ Monadic right fold (action applied to each element and its index).
--
-- @since 0.12.3.0
ifoldrM :: (PrimMonad m, Storable a) => (Int -> a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldrM #-}
ifoldrM = G.ifoldrM

-- | /O(n)/ Monadic right fold with strict accumulator (action applied
-- to each element and its index).
--
-- @since 0.12.3.0
ifoldrM' :: (PrimMonad m, Storable a) => (Int -> a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldrM' #-}
ifoldrM' = G.ifoldrM'


-- Unsafe conversions
-- ------------------

-- | /O(1)/ Unsafely cast a mutable vector from one element type to another.
-- The operation just changes the type of the underlying pointer and does not
-- modify the elements.
--
-- The resulting vector contains as many elements as can fit into the
-- underlying memory block.
--
unsafeCast :: forall a b s.
              (Storable a, Storable b) => MVector s a -> MVector s b
{-# INLINE unsafeCast #-}
unsafeCast (MVector n fp)
  = MVector ((n * sizeOf (undefined :: a)) `div` sizeOf (undefined :: b))
            (castForeignPtr fp)

-- Raw pointers
-- ------------

-- | Create a mutable vector from a 'ForeignPtr' with an offset and a length.
--
-- Modifying data through the 'ForeignPtr' afterwards is unsafe if the vector
-- could have been frozen before the modification.
--
--  If your offset is 0 it is more efficient to use 'unsafeFromForeignPtr0'.
unsafeFromForeignPtr :: Storable a
                     => ForeignPtr a    -- ^ pointer
                     -> Int             -- ^ offset
                     -> Int             -- ^ length
                     -> MVector s a
{-# INLINE_FUSED unsafeFromForeignPtr #-}
unsafeFromForeignPtr fp i n = unsafeFromForeignPtr0 fp' n
    where
      fp' = updPtr (`advancePtr` i) fp

{-# RULES
"unsafeFromForeignPtr fp 0 n -> unsafeFromForeignPtr0 fp n " forall fp n.
  unsafeFromForeignPtr fp 0 n = unsafeFromForeignPtr0 fp n   #-}


-- | /O(1)/ Create a mutable vector from a 'ForeignPtr' and a length.
--
-- It is assumed the pointer points directly to the data (no offset).
-- Use `unsafeFromForeignPtr` if you need to specify an offset.
--
-- Modifying data through the 'ForeignPtr' afterwards is unsafe if the vector
-- could have been frozen before the modification.
unsafeFromForeignPtr0 :: Storable a
                      => ForeignPtr a    -- ^ pointer
                      -> Int             -- ^ length
                      -> MVector s a
{-# INLINE unsafeFromForeignPtr0 #-}
unsafeFromForeignPtr0 fp n = MVector n fp

-- | Yield the underlying 'ForeignPtr' together with the offset to the data
-- and its length. Modifying the data through the 'ForeignPtr' is
-- unsafe if the vector could have frozen before the modification.
unsafeToForeignPtr :: Storable a => MVector s a -> (ForeignPtr a, Int, Int)
{-# INLINE unsafeToForeignPtr #-}
unsafeToForeignPtr (MVector n fp) = (fp, 0, n)

-- | /O(1)/ Yield the underlying 'ForeignPtr' together with its length.
--
-- You can assume the pointer points directly to the data (no offset).
--
-- Modifying the data through the 'ForeignPtr' is unsafe if the vector could
-- have frozen before the modification.
unsafeToForeignPtr0 :: Storable a => MVector s a -> (ForeignPtr a, Int)
{-# INLINE unsafeToForeignPtr0 #-}
unsafeToForeignPtr0 (MVector n fp) = (fp, n)

-- | Pass a pointer to the vector's data to the IO action. Modifying data
-- through the pointer is unsafe if the vector could have been frozen before
-- the modification.
unsafeWith :: Storable a => IOVector a -> (Ptr a -> IO b) -> IO b
{-# INLINE unsafeWith #-}
unsafeWith (MVector _ fp) = withForeignPtr fp
