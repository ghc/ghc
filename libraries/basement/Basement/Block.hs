-- |
-- Module      : Basement.Block
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
--
-- A block of memory that contains elements of a type,
-- very similar to an unboxed array but with the key difference:
--
-- * It doesn't have slicing capability (no cheap take or drop)
-- * It consume less memory: 1 Offset, 1 CountOf
-- * It's unpackable in any constructor
-- * It uses unpinned memory by default
--
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Basement.Block
    ( Block(..)
    , MutableBlock(..)
    -- * Properties
    , length
    -- * Lowlevel functions
    , unsafeThaw
    , unsafeFreeze
    , unsafeIndex
    , thaw
    , freeze
    , copy
    , unsafeCast
    , cast
    -- * safer api
    , empty
    , create
    , isPinned
    , isMutablePinned
    , singleton
    , replicate
    , index
    , map
    , foldl'
    , foldr
    , foldl1'
    , foldr1
    , cons
    , snoc
    , uncons
    , unsnoc
    , sub
    , splitAt
    , revSplitAt
    , splitOn
    , break
    , breakEnd
    , span
    , elem
    , all
    , any
    , find
    , filter
    , reverse
    , sortBy
    , intersperse
    -- * Foreign interfaces
    , createFromPtr
    , unsafeCopyToPtr
    , withPtr
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.ST
import qualified Data.List
import           Basement.Compat.Base
import           Data.Proxy
import           Basement.Compat.Primitive
import           Basement.NonEmpty
import           Basement.Types.OffsetSize
import           Basement.Monad
import           Basement.Exception
import           Basement.PrimType
import qualified Basement.Block.Mutable as M
import           Basement.Block.Mutable (Block(..), MutableBlock(..), new, unsafeThaw, unsafeFreeze)
import           Basement.Block.Base
import           Basement.Numerical.Additive
import           Basement.Numerical.Subtractive
import           Basement.Numerical.Multiplicative
import qualified Basement.Alg.Mutable as MutAlg
import qualified Basement.Alg.Class as Alg
import qualified Basement.Alg.PrimArray as Alg

instance (PrimMonad prim, st ~ PrimState prim, PrimType ty) 
         => Alg.RandomAccess (MutableBlock ty st) prim ty where
    read (MutableBlock mba) = primMbaRead mba
    write (MutableBlock mba) = primMbaWrite mba

instance (PrimType ty) => Alg.Indexable (Block ty) ty where
    index (Block ba) = primBaIndex ba
    {-# INLINE index #-}

instance Alg.Indexable (Block Word8) Word64 where
    index (Block ba) = primBaIndex ba
    {-# INLINE index #-}

-- | Copy all the block content to the memory starting at the destination address
unsafeCopyToPtr :: forall ty prim . PrimMonad prim
                => Block ty -- ^ the source block to copy
                -> Ptr ty   -- ^ The destination address where the copy is going to start
                -> prim ()
unsafeCopyToPtr (Block blk) (Ptr p) = primitive $ \s1 ->
    (# copyByteArrayToAddr# blk 0# p (sizeofByteArray# blk) s1, () #)

-- | Create a new array of size @n by settings each cells through the
-- function @f.
create :: forall ty . PrimType ty
       => CountOf ty           -- ^ the size of the block (in element of ty)
       -> (Offset ty -> ty) -- ^ the function that set the value at the index
       -> Block ty          -- ^ the array created
create n initializer
    | n == 0    = mempty
    | otherwise = runST $ do
        mb <- new n
        M.iterSet initializer mb
        unsafeFreeze mb

-- | Freeze a chunk of memory pointed, of specific size into a new unboxed array
createFromPtr :: PrimType ty
              => Ptr ty
              -> CountOf ty
              -> IO (Block ty)
createFromPtr p sz = do
    mb <- new sz
    M.copyFromPtr p mb 0 sz
    unsafeFreeze mb

singleton :: PrimType ty => ty -> Block ty
singleton ty = create 1 (const ty)

replicate :: PrimType ty => CountOf ty -> ty -> Block ty
replicate sz ty = create sz (const ty)

-- | Thaw a Block into a MutableBlock
--
-- the Block is not modified, instead a new Mutable Block is created
-- and its content is copied to the mutable block
thaw :: (PrimMonad prim, PrimType ty) => Block ty -> prim (MutableBlock ty (PrimState prim))
thaw array = do
    ma <- M.unsafeNew Unpinned (lengthBytes array)
    M.unsafeCopyBytesRO ma 0 array 0 (lengthBytes array)
    pure ma
{-# INLINE thaw #-}

-- | Freeze a MutableBlock into a Block, copying all the data
--
-- If the data is modified in the mutable block after this call, then
-- the immutable Block resulting is not impacted.
freeze :: (PrimType ty, PrimMonad prim) => MutableBlock ty (PrimState prim) -> prim (Block ty)
freeze ma = do
    ma' <- unsafeNew Unpinned len
    M.unsafeCopyBytes ma' 0 ma 0 len
    --M.copyAt ma' (Offset 0) ma (Offset 0) len
    unsafeFreeze ma'
  where
    len = M.mutableLengthBytes ma

-- | Copy every cells of an existing Block to a new Block
copy :: PrimType ty => Block ty -> Block ty
copy array = runST (thaw array >>= unsafeFreeze)

-- | Return the element at a specific index from an array.
--
-- If the index @n is out of bounds, an error is raised.
index :: PrimType ty => Block ty -> Offset ty -> ty
index array n
    | isOutOfBound n len = outOfBound OOB_Index n len
    | otherwise          = unsafeIndex array n
  where
    !len = length array
{-# INLINE index #-}

-- | Map all element 'a' from a block to a new block of 'b'
map :: (PrimType a, PrimType b) => (a -> b) -> Block a -> Block b
map f a = create lenB (\i -> f $ unsafeIndex a (offsetCast Proxy i))
  where !lenB = sizeCast (Proxy :: Proxy (a -> b)) (length a)

foldr :: PrimType ty => (ty -> a -> a) -> a -> Block ty -> a
foldr f initialAcc vec = loop 0
  where
    !len = length vec
    loop !i
        | i .==# len = initialAcc
        | otherwise  = unsafeIndex vec i `f` loop (i+1)
{-# SPECIALIZE [2] foldr :: (Word8 -> a -> a) -> a -> Block Word8 -> a #-}

foldl' :: PrimType ty => (a -> ty -> a) -> a -> Block ty -> a
foldl' f initialAcc vec = loop 0 initialAcc
  where
    !len = length vec
    loop !i !acc
        | i .==# len = acc
        | otherwise  = loop (i+1) (f acc (unsafeIndex vec i))
{-# SPECIALIZE [2] foldl' :: (a -> Word8 -> a) -> a -> Block Word8 -> a #-}

foldl1' :: PrimType ty => (ty -> ty -> ty) -> NonEmpty (Block ty) -> ty
foldl1' f (NonEmpty arr) = loop 1 (unsafeIndex arr 0)
  where
    !len = length arr
    loop !i !acc
        | i .==# len = acc
        | otherwise  = loop (i+1) (f acc (unsafeIndex arr i))
{-# SPECIALIZE [3] foldl1' :: (Word8 -> Word8 -> Word8) -> NonEmpty (Block Word8) -> Word8 #-}

foldr1 :: PrimType ty => (ty -> ty -> ty) -> NonEmpty (Block ty) -> ty
foldr1 f arr = let (initialAcc, rest) = revSplitAt 1 $ getNonEmpty arr
               in foldr f (unsafeIndex initialAcc 0) rest

cons :: PrimType ty => ty -> Block ty -> Block ty
cons e vec
    | len == 0  = singleton e
    | otherwise = runST $ do
        muv <- new (len + 1)
        M.unsafeCopyElementsRO muv 1 vec 0 len
        M.unsafeWrite muv 0 e
        unsafeFreeze muv
  where
    !len = length vec

snoc :: PrimType ty => Block ty -> ty -> Block ty
snoc vec e
    | len == 0  = singleton e
    | otherwise = runST $ do
        muv <- new (len + 1)
        M.unsafeCopyElementsRO muv 0 vec 0 len
        M.unsafeWrite muv (0 `offsetPlusE` len) e
        unsafeFreeze muv
  where
     !len = length vec

sub :: PrimType ty => Block ty -> Offset ty -> Offset ty -> Block ty
sub blk start end
    | start >= end' = mempty
    | otherwise     = runST $ do
        dst <- new newLen
        M.unsafeCopyElementsRO dst 0 blk start newLen
        unsafeFreeze dst
  where
    newLen = end' - start
    end' = min (sizeAsOffset len) end
    !len = length blk

uncons :: PrimType ty => Block ty -> Maybe (ty, Block ty)
uncons vec
    | nbElems == 0 = Nothing
    | otherwise    = Just (unsafeIndex vec 0, sub vec 1 (0 `offsetPlusE` nbElems))
  where
    !nbElems = length vec

unsnoc :: PrimType ty => Block ty -> Maybe (Block ty, ty)
unsnoc vec = case length vec - 1 of
    Nothing -> Nothing
    Just offset -> Just (sub vec 0 lastElem, unsafeIndex vec lastElem)
                     where !lastElem = 0 `offsetPlusE` offset

splitAt :: PrimType ty => CountOf ty -> Block ty -> (Block ty, Block ty)
splitAt nbElems blk
    | nbElems <= 0 = (mempty, blk)
    | Just nbTails <- length blk - nbElems, nbTails > 0 = runST $ do
        left  <- new nbElems
        right <- new nbTails
        M.unsafeCopyElementsRO left  0 blk 0                      nbElems
        M.unsafeCopyElementsRO right 0 blk (sizeAsOffset nbElems) nbTails
        (,) <$> unsafeFreeze left <*> unsafeFreeze right
    | otherwise    = (blk, mempty)
{-# SPECIALIZE [2] splitAt :: CountOf Word8 -> Block Word8 -> (Block Word8, Block Word8) #-}

revSplitAt :: PrimType ty => CountOf ty -> Block ty -> (Block ty, Block ty)
revSplitAt n blk 
    | n <= 0                         = (mempty, blk)
    | Just nbElems <- length blk - n = let (x, y) = splitAt nbElems blk in (y, x)
    | otherwise                      = (blk, mempty)

break :: PrimType ty => (ty -> Bool) -> Block ty -> (Block ty, Block ty)
break predicate blk = findBreak 0
  where
    !len = length blk
    findBreak !i
        | i .==# len                    = (blk, mempty)
        | predicate (unsafeIndex blk i) = splitAt (offsetAsSize i) blk
        | otherwise                     = findBreak (i + 1)
    {-# INLINE findBreak #-}
{-# SPECIALIZE [2] break :: (Word8 -> Bool) -> Block Word8 -> (Block Word8, Block Word8) #-}

breakEnd :: PrimType ty => (ty -> Bool) -> Block ty -> (Block ty, Block ty)
breakEnd predicate blk
    | k == sentinel = (blk, mempty)
    | otherwise     = splitAt (offsetAsSize (k+1)) blk
  where
    !k = Alg.revFindIndexPredicate predicate blk 0 end
    !end = sizeAsOffset $ length blk
{-# SPECIALIZE [2] breakEnd :: (Word8 -> Bool) -> Block Word8 -> (Block Word8, Block Word8) #-}

span :: PrimType ty => (ty -> Bool) -> Block ty -> (Block ty, Block ty)
span p = break (not . p)

elem :: PrimType ty => ty -> Block ty -> Bool
elem v blk = loop 0
  where
    !len = length blk
    loop !i
        | i .==# len             = False
        | unsafeIndex blk i == v = True
        | otherwise              = loop (i+1)
{-# SPECIALIZE [2] elem :: Word8 -> Block Word8 -> Bool #-}

all :: PrimType ty => (ty -> Bool) -> Block ty -> Bool
all p blk = loop 0
  where
    !len = length blk
    loop !i
        | i .==# len            = True
        | p (unsafeIndex blk i) = loop (i+1)
        | otherwise             = False
{-# SPECIALIZE [2] all :: (Word8 -> Bool) -> Block Word8 -> Bool #-}

any :: PrimType ty => (ty -> Bool) -> Block ty -> Bool
any p blk = loop 0
  where
    !len = length blk
    loop !i
        | i .==# len            = False
        | p (unsafeIndex blk i) = True
        | otherwise             = loop (i+1)
{-# SPECIALIZE [2] any :: (Word8 -> Bool) -> Block Word8 -> Bool #-}

splitOn :: PrimType ty => (ty -> Bool) -> Block ty -> [Block ty]
splitOn predicate blk
    | len == 0  = [mempty]
    | otherwise = go 0 0
  where
    !len = length blk
    go !prevIdx !idx
        | idx .==# len = [sub blk prevIdx idx]
        | otherwise    =
            let e = unsafeIndex blk idx
                idx' = idx + 1
             in if predicate e
                    then sub blk prevIdx idx : go idx' idx'
                    else go prevIdx idx'

find :: PrimType ty => (ty -> Bool) -> Block ty -> Maybe ty
find predicate vec = loop 0
  where
    !len = length vec
    loop i
        | i .==# len = Nothing
        | otherwise  =
            let e = unsafeIndex vec i
             in if predicate e then Just e else loop (i+1)

filter :: PrimType ty => (ty -> Bool) -> Block ty -> Block ty
filter predicate vec = fromList $ Data.List.filter predicate $ toList vec

reverse :: forall ty . PrimType ty => Block ty -> Block ty
reverse blk
    | len == 0  = mempty
    | otherwise = runST $ do
        mb <- new len
        go mb
        unsafeFreeze mb
  where
    !len = length blk
    !endOfs = 0 `offsetPlusE` len

    go :: MutableBlock ty s -> ST s ()
    go mb = loop endOfs 0
      where
        loop o i
            | i .==# len = pure ()
            | otherwise  = unsafeWrite mb o' (unsafeIndex blk i) >> loop o' (i+1)
          where o' = pred o

sortBy :: PrimType ty => (ty -> ty -> Ordering) -> Block ty -> Block ty
sortBy ford vec
    | len == 0  = mempty
    | otherwise = runST $ do
        mblock <- thaw vec
        MutAlg.inplaceSortBy ford 0 len mblock
        unsafeFreeze mblock
  where len = length vec
{-# SPECIALIZE [2] sortBy :: (Word8 -> Word8 -> Ordering) -> Block Word8 -> Block Word8 #-}

intersperse :: forall ty . PrimType ty => ty -> Block ty -> Block ty
intersperse sep blk = case len - 1 of
    Nothing -> blk
    Just 0 -> blk
    Just size -> runST $ do
        mb <- new (len+size)
        go mb
        unsafeFreeze mb
  where
    !len = length blk

    go :: MutableBlock ty s -> ST s ()
    go mb = loop 0 0
      where
        loop !o !i
            | (i + 1) .==# len = unsafeWrite mb o (unsafeIndex blk i)
            | otherwise        = do
                unsafeWrite mb o     (unsafeIndex blk i)
                unsafeWrite mb (o+1) sep
                loop (o+2) (i+1)

-- | Unsafely recast an UArray containing 'a' to an UArray containing 'b'
--
-- The offset and size are converted from units of 'a' to units of 'b',
-- but no check are performed to make sure this is compatible.
--
-- use 'cast' if unsure.
unsafeCast :: PrimType b => Block a -> Block b
unsafeCast (Block ba) = Block ba

-- | Cast a Block of 'a' to a Block of 'b'
--
-- The requirement is that the size of type 'a' need to be a multiple or
-- dividend of the size of type 'b'.
--
-- If this requirement is not met, the InvalidRecast exception is thrown
cast :: forall a b . (PrimType a, PrimType b) => Block a -> Block b
cast blk@(Block ba)
    | aTypeSize == bTypeSize || bTypeSize == 1 = unsafeCast blk
    | missing   == 0                           = unsafeCast blk
    | otherwise                                =
        throw $ InvalidRecast (RecastSourceSize alen) (RecastDestinationSize $ alen + missing)
  where
    (CountOf alen) = lengthBytes blk

    aTypeSize = primSizeInBytes (Proxy :: Proxy a)
    bTypeSize@(CountOf bs) = primSizeInBytes (Proxy :: Proxy b)

    missing = alen `mod` bs
