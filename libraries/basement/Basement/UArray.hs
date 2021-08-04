-- |
-- Module      : Basement.UArray
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- An unboxed array of primitive types
--
-- All the cells in the array are in one chunk of contiguous
-- memory.
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Basement.UArray
    ( UArray(..)
    , PrimType(..)
    -- * methods
    , copy
    , unsafeCopyAtRO
    -- * internal methods
    -- , copyAddr
    , recast
    , unsafeRecast
    , length
    , freeze
    , unsafeFreeze
    , thaw
    , unsafeThaw
    -- * Creation
    , vFromListN
    , new
    , create
    , createFromIO
    , createFromPtr
    , sub
    , copyToPtr
    , withPtr
    , withMutablePtr
    , unsafeFreezeShrink
    , freezeShrink
    , fromBlock
    , toBlock
    -- * accessors
    , update
    , unsafeUpdate
    , unsafeIndex
    , unsafeIndexer
    , unsafeDewrap
    , unsafeRead
    , unsafeWrite
    -- * Functions
    , equalMemcmp
    , singleton
    , replicate
    , map
    , mapIndex
    , findIndex
    , revFindIndex
    , index
    , null
    , take
    , unsafeTake
    , drop
    , unsafeDrop
    , splitAt
    , revDrop
    , revTake
    , revSplitAt
    , splitOn
    , break
    , breakEnd
    , breakElem
    , breakLine
    , elem
    , indices
    , intersperse
    , span
    , spanEnd
    , cons
    , snoc
    , uncons
    , unsnoc
    , find
    , sortBy
    , filter
    , reverse
    , replace
    , foldr
    , foldl'
    , foldr1
    , foldl1'
    , all
    , any
    , isPrefixOf
    , isSuffixOf
    , foreignMem
    , fromForeignPtr
    , builderAppend
    , builderBuild
    , builderBuild_
    , toHexadecimal
    , toBase64Internal
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.Word
import           GHC.ST
import           GHC.Ptr
import           GHC.ForeignPtr (ForeignPtr)
import           Foreign.Marshal.Utils (copyBytes)
import           Basement.Compat.Base
import           Basement.Compat.Primitive
import           Data.Proxy
import           Basement.Types.OffsetSize
import           Basement.Compat.MonadTrans
import           Basement.NonEmpty
import           Basement.Monad
import           Basement.PrimType
import           Basement.FinalPtr
import           Basement.Exception
import           Basement.UArray.Base
import           Basement.Block (Block(..), MutableBlock(..))
import qualified Basement.Block as BLK
import qualified Basement.Block.Base as BLK (withPtr, unsafeWrite)
import           Basement.UArray.Mutable hiding (sub, copyToPtr)
import           Basement.Numerical.Additive
import           Basement.Numerical.Subtractive
import           Basement.Numerical.Multiplicative
import           Basement.MutableBuilder
import           Basement.Bindings.Memory (sysHsMemFindByteBa, sysHsMemFindByteAddr)
import qualified Basement.Compat.ExtList as List
import qualified Basement.Base16 as Base16
import qualified Basement.Alg.Mutable as Alg
import qualified Basement.Alg.Class as Alg
import qualified Basement.Alg.PrimArray as Alg

-- | Return the element at a specific index from an array.
--
-- If the index @n is out of bounds, an error is raised.
index :: PrimType ty => UArray ty -> Offset ty -> ty
index array n
    | isOutOfBound n len = outOfBound OOB_Index n len
    | otherwise          = unsafeIndex array n
  where
    !len = length array
{-# INLINE index #-}

foreignMem :: PrimType ty
           => FinalPtr ty -- ^ the start pointer with a finalizer
           -> CountOf ty  -- ^ the number of elements (in elements, not bytes)
           -> UArray ty
foreignMem fptr nb = UArray (Offset 0) nb (UArrayAddr fptr)

-- | Create a foreign UArray from foreign memory and given offset/size
--
-- No check are performed to make sure this is valid, so this is unsafe.
--
-- This is particularly useful when dealing with foreign memory and
-- 'ByteString'
fromForeignPtr :: PrimType ty
               => (ForeignPtr ty, Int, Int) -- ForeignPtr, an offset in prim elements, a size in prim elements
               -> UArray ty
fromForeignPtr (fptr, ofs, len) = UArray (Offset ofs) (CountOf len) (UArrayAddr $ toFinalPtrForeign fptr)


-- | Create a UArray from a Block
--
-- The block is still used by the uarray
fromBlock :: PrimType ty
          => Block ty
          -> UArray ty
fromBlock blk = UArray 0 (BLK.length blk) (UArrayBA blk)

-- | Allocate a new array with a fill function that has access to the elements of
--   the source array.
unsafeCopyFrom :: (PrimType a, PrimType b)
               => UArray a -- ^ Source array
               -> CountOf b -- ^ Length of the destination array
               -> (UArray a -> Offset a -> MUArray b s -> ST s ())
               -- ^ Function called for each element in the source array
               -> ST s (UArray b) -- ^ Returns the filled new array
unsafeCopyFrom v' newLen f = new newLen >>= fill 0 >>= unsafeFreeze
  where len = length v'
        fill i r'
            | i .==# len = pure r'
            | otherwise  = do f v' i r'
                              fill (i + 1) r'

-- | Freeze a MUArray into a UArray by copying all the content is a pristine new buffer
--
-- The MUArray in parameter can be still be used after the call without
-- changing the resulting frozen data.
freeze :: (PrimType ty, PrimMonad prim) => MUArray ty (PrimState prim) -> prim (UArray ty)
freeze ma = do
    ma' <- new len
    copyAt ma' (Offset 0) ma (Offset 0) len
    unsafeFreeze ma'
  where len = mutableLength ma

-- | Just like 'freeze' but copy only the first n bytes
--
-- The size requested need to be smaller or equal to the length
-- of the MUArray, otherwise a Out of Bounds exception is raised
freezeShrink :: (PrimType ty, PrimMonad prim) => MUArray ty (PrimState prim) -> CountOf ty -> prim (UArray ty)
freezeShrink ma n = do
    when (n > mutableLength ma) $ primOutOfBound OOB_MemCopy (sizeAsOffset n) (mutableLength ma)
    ma' <- new n
    copyAt ma' (Offset 0) ma (Offset 0) n
    unsafeFreeze ma'

-- | Create a new array of size @n by settings each cells through the
-- function @f.
create :: forall ty . PrimType ty
       => CountOf ty           -- ^ the size of the array
       -> (Offset ty -> ty) -- ^ the function that set the value at the index
       -> UArray ty         -- ^ the array created
create n initializer
    | n == 0    = mempty
    | otherwise = runST (new n >>= iter initializer)
  where
    iter :: (PrimType ty, PrimMonad prim) => (Offset ty -> ty) -> MUArray ty (PrimState prim) -> prim (UArray ty)
    iter f ma = loop 0
      where
        loop i
            | i .==# n  = unsafeFreeze ma
            | otherwise = unsafeWrite ma i (f i) >> loop (i+1)
        {-# INLINE loop #-}
    {-# INLINE iter #-}

-- | Create a pinned array that is filled by a 'filler' function (typically an IO call like hGetBuf)
createFromIO :: PrimType ty
             => CountOf ty                  -- ^ the size of the array
             -> (Ptr ty -> IO (CountOf ty)) -- ^ filling function that
             -> IO (UArray ty)
createFromIO size filler
    | size == 0 = pure mempty
    | otherwise = do
        mba <- newPinned size
        r   <- withMutablePtr mba $ \p -> filler p
        case r of
            0             -> pure mempty -- make sure we don't keep our array referenced by using empty
            _ | r < 0     -> error "filler returned negative number"
              | otherwise -> unsafeFreezeShrink mba r

-- | Freeze a chunk of memory pointed, of specific size into a new unboxed array
createFromPtr :: PrimType ty
              => Ptr ty
              -> CountOf ty
              -> IO (UArray ty)
createFromPtr p s = do
    ma <- new s
    copyFromPtr p s ma
    unsafeFreeze ma

-----------------------------------------------------------------------
-- higher level collection implementation
-----------------------------------------------------------------------

singleton :: PrimType ty => ty -> UArray ty
singleton ty = create 1 (const ty)

replicate :: PrimType ty => CountOf ty -> ty -> UArray ty
replicate sz ty = create sz (const ty)

-- | update an array by creating a new array with the updates.
--
-- the operation copy the previous array, modify it in place, then freeze it.
update :: PrimType ty
       => UArray ty
       -> [(Offset ty, ty)]
       -> UArray ty
update array modifiers = runST (thaw array >>= doUpdate modifiers)
  where doUpdate l ma = loop l
          where loop []         = unsafeFreeze ma
                loop ((i,v):xs) = write ma i v >> loop xs
                {-# INLINE loop #-}
        {-# INLINE doUpdate #-}

unsafeUpdate :: PrimType ty
             => UArray ty
             -> [(Offset ty, ty)]
             -> UArray ty
unsafeUpdate array modifiers = runST (thaw array >>= doUpdate modifiers)
  where doUpdate l ma = loop l
          where loop []         = unsafeFreeze ma
                loop ((i,v):xs) = unsafeWrite ma i v >> loop xs
                {-# INLINE loop #-}
        {-# INLINE doUpdate #-}

-- | Copy all the block content to the memory starting at the destination address
copyToPtr :: forall ty prim . (PrimType ty, PrimMonad prim)
          => UArray ty -- ^ the source array to copy
          -> Ptr ty    -- ^ The destination address where the copy is going to start
          -> prim ()
copyToPtr arr dst@(Ptr dst#) = onBackendPrim copyBa copyPtr arr
  where
    !(Offset os@(I# os#)) = offsetInBytes $ offset arr
    !(CountOf szBytes@(I# szBytes#)) = sizeInBytes $ length arr
    copyBa (Block ba) = primitive $ \s1 -> (# copyByteArrayToAddr# ba os# dst# szBytes# s1, () #)
    copyPtr fptr = unsafePrimFromIO $ withFinalPtr fptr $ \ptr -> copyBytes dst (ptr `plusPtr` os) szBytes

-- | Get a Ptr pointing to the data in the UArray.
--
-- Since a UArray is immutable, this Ptr shouldn't be
-- to use to modify the contents
--
-- If the UArray is pinned, then its address is returned as is,
-- however if it's unpinned, a pinned copy of the UArray is made
-- before getting the address.
withPtr :: forall ty prim a . (PrimMonad prim, PrimType ty)
        => UArray ty
        -> (Ptr ty -> prim a)
        -> prim a
withPtr a f =
    onBackendPrim (\blk  -> BLK.withPtr  blk  $ \ptr -> f (ptr `plusPtr` os))
                  (\fptr -> withFinalPtr fptr $ \ptr -> f (ptr `plusPtr` os))
                  a
  where
    !sz          = primSizeInBytes (Proxy :: Proxy ty)
    !(Offset os) = offsetOfE sz $ offset a
{-# INLINE withPtr #-}

-- | Recast an array of type a to an array of b
--
-- a and b need to have the same size otherwise this
-- raise an async exception
recast :: forall a b . (PrimType a, PrimType b) => UArray a -> UArray b
recast array
    | aTypeSize == bTypeSize = unsafeRecast array
    | missing   == 0         = unsafeRecast array
    | otherwise = throw $ InvalidRecast
                      (RecastSourceSize      alen)
                      (RecastDestinationSize $ alen + missing)
  where
    aTypeSize = primSizeInBytes (Proxy :: Proxy a)
    bTypeSize@(CountOf bs) = primSizeInBytes (Proxy :: Proxy b)
    (CountOf alen) = sizeInBytes (length array)
    missing = alen `mod` bs

-- | Unsafely recast an UArray containing 'a' to an UArray containing 'b'
--
-- The offset and size are converted from units of 'a' to units of 'b',
-- but no check are performed to make sure this is compatible.
--
-- use 'recast' if unsure.
unsafeRecast :: (PrimType a, PrimType b) => UArray a -> UArray b
unsafeRecast (UArray start len backend) = UArray (primOffsetRecast start) (sizeRecast len) $
    case backend of
        UArrayAddr fptr     -> UArrayAddr (castFinalPtr fptr)
        UArrayBA (Block ba) -> UArrayBA (Block ba)
{-# INLINE [1] unsafeRecast #-}
{-# SPECIALIZE [3] unsafeRecast :: PrimType a => UArray Word8 -> UArray a #-}

null :: UArray ty -> Bool
null arr = length arr == 0

-- | Take a count of elements from the array and create an array with just those elements
take :: CountOf ty -> UArray ty -> UArray ty
take n arr@(UArray start len backend)
    | n <= 0    = empty
    | n >= len  = arr
    | otherwise = UArray start n backend

unsafeTake :: CountOf ty -> UArray ty -> UArray ty
unsafeTake sz (UArray start _ ba) = UArray start sz ba

-- | Drop a count of elements from the array and return the new array minus those dropped elements
drop :: CountOf ty -> UArray ty -> UArray ty
drop n arr@(UArray start len backend)
    | n <= 0                             = arr
    | Just newLen <- len - n, newLen > 0 = UArray (start `offsetPlusE` n) newLen backend
    | otherwise                          = empty

unsafeDrop :: CountOf ty -> UArray ty -> UArray ty
unsafeDrop n (UArray start sz backend) = UArray (start `offsetPlusE` n) (sz `sizeSub` n) backend

-- | Split an array into two, with a count of at most N elements in the first one
-- and the remaining in the other.
splitAt :: CountOf ty -> UArray ty -> (UArray ty, UArray ty)
splitAt nbElems arr@(UArray start len backend)
    | nbElems <= 0                               = (empty, arr)
    | Just nbTails <- len - nbElems, nbTails > 0 = (UArray start                         nbElems backend
                                                   ,UArray (start `offsetPlusE` nbElems) nbTails backend)
    | otherwise                                  = (arr, empty)


breakElem :: PrimType ty => ty -> UArray ty -> (UArray ty, UArray ty)
breakElem !ty arr@(UArray start len backend)
    | k == sentinel = (arr, empty)
    | k == start    = (empty, arr)
    | otherwise     = (UArray start (offsetAsSize l1)       backend
                     , UArray k     (sizeAsOffset len - l1) backend)
  where
    !k = onBackendPure' arr $ Alg.findIndexElem ty
    l1 = k `offsetSub` start
{-# NOINLINE [3] breakElem #-}
{-# RULES "breakElem Word8" [4] breakElem = breakElemByte #-}
{-# SPECIALIZE [3] breakElem :: Word32 -> UArray Word32 -> (UArray Word32, UArray Word32) #-}

breakElemByte :: Word8 -> UArray Word8 -> (UArray Word8, UArray Word8)
breakElemByte !ty arr@(UArray start len backend)
    | k == end   = (arr, empty)
    | k == start = (empty, arr)
    | otherwise  = ( UArray start (offsetAsSize k `sizeSub` offsetAsSize start) backend
                   , UArray k     (len `sizeSub` (offsetAsSize k `sizeSub` offsetAsSize start)) backend)
  where
    !end = start `offsetPlusE` len
    !k = onBackendPure goBa goAddr arr
    goBa (Block ba) = sysHsMemFindByteBa ba start end ty
    goAddr (Ptr addr) = sysHsMemFindByteAddr addr start end ty

-- | Similar to breakElem specialized to split on linefeed
--
-- it either returns:
-- * Left. no line has been found, and whether the last character is a CR
-- * Right, a line has been found with an optional CR, and it returns
--   the array of bytes on the left of the CR/LF, and the
--   the array of bytes on the right of the LF.
--
breakLine :: UArray Word8 -> Either Bool (UArray Word8, UArray Word8)
breakLine arr@(UArray start len backend)
    | end == start = Left False
    | k2 == end    = Left (k1 /= k2)
    | otherwise    = let newArray start' len' = if len' == 0 then empty else UArray start' len' backend
                      in Right (newArray start (k1-start), newArray (k2+1) (end - (k2+1)))
  where
    !end = start `offsetPlusE` len
    -- return (offset of CR, offset of LF, whether the last element was a carriage return
    !(k1, k2) = onBackendPure goBa goAddr arr
    lineFeed = 0xa
    carriageReturn = 0xd
    goBa (Block ba) =
        let k = sysHsMemFindByteBa ba start end lineFeed
            cr = k > start && primBaIndex ba (k `offsetSub` 1) == carriageReturn
         in (if cr then k `offsetSub` 1 else k, k)
    goAddr (Ptr addr) =
        let k = sysHsMemFindByteAddr addr start end lineFeed
            cr = k > start && primAddrIndex addr (k `offsetSub` 1) == carriageReturn
         in (if cr then k `offsetSub` 1 else k, k)

-- inverse a CountOf that is specified from the end (e.g. take n elements from the end)
countFromStart :: UArray ty -> CountOf ty -> CountOf ty
countFromStart v sz@(CountOf sz')
    | sz >= len = CountOf 0
    | otherwise = CountOf (len' - sz')
  where len@(CountOf len') = length v

-- | Take the N elements from the end of the array
revTake :: CountOf ty -> UArray ty -> UArray ty
revTake n v = drop (countFromStart v n) v

-- | Drop the N elements from the end of the array
revDrop :: CountOf ty -> UArray ty -> UArray ty
revDrop n v = take (countFromStart v n) v

-- | Split an array at the N element from the end, and return
-- the last N elements in the first part of the tuple, and whatever first
-- elements remaining in the second
revSplitAt :: CountOf ty -> UArray ty -> (UArray ty, UArray ty)
revSplitAt n v = (drop sz v, take sz v) where sz = countFromStart v n

splitOn :: PrimType ty => (ty -> Bool) -> UArray ty -> [UArray ty]
splitOn xpredicate ivec
    | len == 0  = [mempty]
    | otherwise = runST $ unsafeIndexer ivec (pureST . go ivec xpredicate)
  where
    !len = length ivec
    go v predicate getIdx = loop 0 0
      where
        loop !prevIdx !idx
            | idx .==# len = [sub v prevIdx idx]
            | otherwise    =
                let e = getIdx idx
                    idx' = idx + 1
                 in if predicate e
                        then sub v prevIdx idx : loop idx' idx'
                        else loop prevIdx idx'
    {-# INLINE go #-}

sub :: PrimType ty => UArray ty -> Offset ty -> Offset ty -> UArray ty
sub (UArray start len backend) startIdx expectedEndIdx
    | startIdx >= endIdx = mempty
    | otherwise          = UArray (start + startIdx) newLen backend
  where
    newLen = endIdx - startIdx
    endIdx = min expectedEndIdx (0 `offsetPlusE` len)

findIndex :: PrimType ty => ty -> UArray ty -> Maybe (Offset ty)
findIndex ty arr
    | k == sentinel  = Nothing
    | otherwise      = Just (k `offsetSub` offset arr)
  where
    !k = onBackendPure' arr $ Alg.findIndexElem ty
{-# SPECIALIZE [3] findIndex :: Word8 -> UArray Word8 -> Maybe (Offset Word8) #-}

revFindIndex :: PrimType ty => ty -> UArray ty -> Maybe (Offset ty)
revFindIndex ty arr
    | k == sentinel = Nothing
    | otherwise     = Just (k `offsetSub` offset arr)
  where
    !k = onBackendPure' arr $ Alg.revFindIndexElem ty
{-# SPECIALIZE [3] revFindIndex :: Word8 -> UArray Word8 -> Maybe (Offset Word8) #-}

break :: forall ty . PrimType ty => (ty -> Bool) -> UArray ty -> (UArray ty, UArray ty)
break predicate arr
    | k == sentinel = (arr, mempty)
    | otherwise     = splitAt (k - offset arr) arr
  where
    !k = onBackendPure' arr $ Alg.findIndexPredicate predicate

{-
{-# SPECIALIZE [3] findIndex :: Word8 -> UArray Word8 -> Maybe (Offset Word8) #-}
    | len == 0  = (mempty, mempty)
    | otherwise = runST $ unsafeIndexer xv (go xv xpredicate)
  where
    !len = length xv
    go :: PrimType ty => UArray ty -> (ty -> Bool) -> (Offset ty -> ty) -> ST s (UArray ty, UArray ty)
    go v predicate getIdx = pure (findBreak $ Offset 0)
      where
        findBreak !i
            | i .==# len           = (v, mempty)
            | predicate (getIdx i) = splitAt (offsetAsSize i) v
            | otherwise            = findBreak (i + Offset 1)
        {-# INLINE findBreak #-}
    {-# INLINE go #-}
    -}
{-# NOINLINE [2] break #-}
{-# SPECIALIZE [2] break :: (Word8 -> Bool) -> UArray Word8 -> (UArray Word8, UArray Word8) #-}

{-
{-# RULES "break (== ty)" [3] forall (x :: forall ty . PrimType ty => ty) . break (== x) = breakElem x #-}
{-# RULES "break (ty ==)" [3] forall (x :: forall ty . PrimType ty => ty) . break (x ==) = breakElem x #-}
{-# RULES "break (== ty)" [3] forall (x :: Word8) . break (== x) = breakElem x #-}
-}

-- | Similar to break but start the search of the breakpoint from the end
--
-- > breakEnd (> 0) [1,2,3,0,0,0]
-- ([1,2,3], [0,0,0])
breakEnd :: forall ty . PrimType ty => (ty -> Bool) -> UArray ty -> (UArray ty, UArray ty)
breakEnd predicate arr
    | k == sentinel = (arr, mempty)
    | otherwise     = splitAt ((k+1) - offset arr) arr
  where
    !k = onBackendPure' arr $ Alg.revFindIndexPredicate predicate
{-# SPECIALIZE [3] breakEnd :: (Word8 -> Bool) -> UArray Word8 -> (UArray Word8, UArray Word8) #-}

elem :: PrimType ty => ty -> UArray ty -> Bool
elem !ty arr = onBackendPure' arr (Alg.findIndexElem ty) /= sentinel
{-# SPECIALIZE [2] elem :: Word8 -> UArray Word8 -> Bool #-}

intersperse :: forall ty . PrimType ty => ty -> UArray ty -> UArray ty
intersperse sep v = case len - 1 of
    Nothing -> v
    Just 0 -> v
    Just gaps -> runST $ unsafeCopyFrom v (len + gaps) go
  where
    len = length v

    go :: PrimType ty => UArray ty -> Offset ty -> MUArray ty s -> ST s ()
    go oldV oldI newV
        | (oldI + 1) .==# len = unsafeWrite newV newI e
        | otherwise           = do
            unsafeWrite newV newI e
            unsafeWrite newV (newI + 1) sep
      where
        e = unsafeIndex oldV oldI
        newI = scale (2 :: Word) oldI

span :: PrimType ty => (ty -> Bool) -> UArray ty -> (UArray ty, UArray ty)
span p = break (not . p)

spanEnd :: PrimType ty => (ty -> Bool) -> UArray ty -> (UArray ty, UArray ty)
spanEnd p = breakEnd (not . p)

map :: (PrimType a, PrimType b) => (a -> b) -> UArray a -> UArray b
map f a = create lenB (\i -> f $ unsafeIndex a (offsetCast Proxy i))
  where !lenB = sizeCast (Proxy :: Proxy (a -> b)) (length a)

mapIndex :: (PrimType a, PrimType b) => (Offset b -> a -> b) -> UArray a -> UArray b
mapIndex f a = create (sizeCast Proxy $ length a) (\i -> f i $ unsafeIndex a (offsetCast Proxy i))

cons :: PrimType ty => ty -> UArray ty -> UArray ty
cons e vec
    | len == CountOf 0 = singleton e
    | otherwise     = runST $ do
        muv <- new (len + 1)
        unsafeCopyAtRO muv 1 vec 0 len
        unsafeWrite muv 0 e
        unsafeFreeze muv
  where
    !len = length vec

snoc :: PrimType ty => UArray ty -> ty -> UArray ty
snoc vec e
    | len == CountOf 0 = singleton e
    | otherwise     = runST $ do
        muv <- new (len + CountOf 1)
        unsafeCopyAtRO muv (Offset 0) vec (Offset 0) len
        unsafeWrite muv (0 `offsetPlusE` length vec) e
        unsafeFreeze muv
  where
     !len = length vec

uncons :: PrimType ty => UArray ty -> Maybe (ty, UArray ty)
uncons vec
    | nbElems == 0 = Nothing
    | otherwise    = Just (unsafeIndex vec 0, sub vec 1 (0 `offsetPlusE` nbElems))
  where
    !nbElems = length vec

unsnoc :: PrimType ty => UArray ty -> Maybe (UArray ty, ty)
unsnoc vec = case length vec - 1 of
    Nothing -> Nothing
    Just newLen -> Just (sub vec 0 lastElem, unsafeIndex vec lastElem)
                     where !lastElem = 0 `offsetPlusE` newLen

find :: PrimType ty => (ty -> Bool) -> UArray ty -> Maybe ty
find predicate vec = loop 0
  where
    !len = length vec
    loop i
        | i .==# len = Nothing
        | otherwise  =
            let e = unsafeIndex vec i
             in if predicate e then Just e else loop (i+1)

sortBy :: forall ty . PrimType ty => (ty -> ty -> Ordering) -> UArray ty -> UArray ty
sortBy ford vec = runST $ do
    mvec <- thaw vec
    onMutableBackend goNative (\fptr -> withFinalPtr fptr goAddr) mvec
    unsafeFreeze mvec
  where
    !len = length vec
    !start = offset vec

    goNative :: MutableBlock ty s -> ST s ()
    goNative mb = Alg.inplaceSortBy ford start len mb
    goAddr :: Ptr ty -> ST s ()
    goAddr (Ptr addr) = Alg.inplaceSortBy ford start len (Ptr addr :: Ptr ty)
{-# SPECIALIZE [3] sortBy :: (Word8 -> Word8 -> Ordering) -> UArray Word8 -> UArray Word8 #-}

filter :: forall ty . PrimType ty => (ty -> Bool) -> UArray ty -> UArray ty
filter predicate arr = runST $ do
    (newLen, ma) <- newNative (length arr) $ \(MutableBlock mba) ->
            onBackendPrim (\block -> Alg.filter predicate mba block start end)
                          (\fptr -> withFinalPtr fptr $ \ptr@(Ptr !_) ->
                                        Alg.filter predicate mba ptr start end)
                          arr
    unsafeFreezeShrink ma newLen
  where
    !len   = length arr
    !start = offset arr
    !end   = start `offsetPlusE` len

reverse :: forall ty . PrimType ty => UArray ty -> UArray ty
reverse a
    | len == 0  = mempty
    | otherwise = runST $ do
        a <- newNative_ len $ \mba -> onBackendPrim (goNative mba)
                                                    (\fptr -> withFinalPtr fptr $ goAddr mba)
                                                    a
        unsafeFreeze a
  where
    !len = length a
    !end = 0 `offsetPlusE` len
    !start = offset a
    !endI = sizeAsOffset ((start + end) - Offset 1)

    goNative :: MutableBlock ty s -> Block ty -> ST s ()
    goNative !ma (Block !ba) = loop 0
      where
        loop !i
            | i == end  = pure ()
            | otherwise = BLK.unsafeWrite ma i (primBaIndex ba (sizeAsOffset (endI - i))) >> loop (i+1)
    goAddr :: MutableBlock ty s -> Ptr ty -> ST s ()
    goAddr !ma (Ptr addr) = loop 0
      where
        loop !i
            | i == end  = pure ()
            | otherwise = BLK.unsafeWrite ma i (primAddrIndex addr (sizeAsOffset (endI - i))) >> loop (i+1)
{-# SPECIALIZE [3] reverse :: UArray Word8 -> UArray Word8 #-}
{-# SPECIALIZE [3] reverse :: UArray Word32 -> UArray Word32 #-}
{-# SPECIALIZE [3] reverse :: UArray Char -> UArray Char #-}

-- Finds where are the insertion points when we search for a `needle`
-- within an `haystack`.
-- Throws an error in case `needle` is empty.
indices :: PrimType ty => UArray ty -> UArray ty -> [Offset ty]
indices needle hy
  | needleLen <= 0 = error "Basement.UArray.indices: needle is empty."
  | otherwise = case haystackLen < needleLen of
                  True  -> []
                  False -> go (Offset 0) []
  where
    !haystackLen = length hy

    !needleLen = length needle

    go currentOffset ipoints
      | (currentOffset `offsetPlusE` needleLen) > (sizeAsOffset haystackLen) = ipoints
      | otherwise =
        let matcher = take needleLen . drop (offsetAsSize currentOffset) $ hy
        in case matcher == needle of
             -- TODO: Move away from right-appending as it's gonna be slow.
             True  -> go (currentOffset `offsetPlusE` needleLen) (ipoints <> [currentOffset])
             False -> go (currentOffset + 1) ipoints

-- | Replace all the occurrencies of `needle` with `replacement` in
-- the `haystack` string.
replace :: PrimType ty => UArray ty -> UArray ty -> UArray ty -> UArray ty
replace (needle :: UArray ty) replacement haystack = runST $ do
    case null needle of
      True -> error "Basement.UArray.replace: empty needle"
      False -> do
        let insertionPoints = indices needle haystack
        let !(CountOf occs) = List.length insertionPoints
        let !newLen         = haystackLen `sizeSub` (multBy needleLen occs) + (multBy replacementLen occs)
        ms <- new newLen
        loop ms (Offset 0) (Offset 0) insertionPoints
  where

    multBy (CountOf x) y = CountOf (x * y)

    !needleLen = length needle

    !replacementLen = length replacement

    !haystackLen = length haystack

    -- Go through each insertion point and copy things over.
    -- We keep around the offset to the original string to
    -- be able to copy bytes which didn't change.
    loop :: PrimMonad prim
         => MUArray ty (PrimState prim)
         -> Offset ty
         -> Offset ty
         -> [Offset ty]
         -> prim (UArray ty)
    loop mba currentOffset offsetInOriginalString [] = do
      -- Finalise the string
      let !unchangedDataLen = sizeAsOffset haystackLen - offsetInOriginalString
      unsafeCopyAtRO mba currentOffset haystack offsetInOriginalString unchangedDataLen
      freeze mba
    loop mba currentOffset offsetInOriginalString (x:xs) = do
        -- 1. Copy from the old string.
        let !unchangedDataLen = (x - offsetInOriginalString)
        unsafeCopyAtRO mba currentOffset haystack offsetInOriginalString unchangedDataLen
        let !newOffset = currentOffset `offsetPlusE` unchangedDataLen
        -- 2. Copy the replacement.
        unsafeCopyAtRO mba newOffset replacement (Offset 0) replacementLen
        let !offsetInOriginalString' = offsetInOriginalString `offsetPlusE` unchangedDataLen `offsetPlusE` needleLen
        loop mba (newOffset `offsetPlusE` replacementLen) offsetInOriginalString' xs
{-# SPECIALIZE [3] replace :: UArray Word8 -> UArray Word8 -> UArray Word8 -> UArray Word8 #-}

foldr :: PrimType ty => (ty -> a -> a) -> a -> UArray ty -> a
foldr f initialAcc vec = loop 0
  where
    !len = length vec
    loop i
        | i .==# len = initialAcc
        | otherwise  = unsafeIndex vec i `f` loop (i+1)

foldl' :: PrimType ty => (a -> ty -> a) -> a -> UArray ty -> a
foldl' f initialAcc arr = onBackendPure' arr (Alg.foldl f initialAcc)
{-# SPECIALIZE [3] foldl' :: (a -> Word8 -> a) -> a -> UArray Word8 -> a #-}

foldl1' :: PrimType ty => (ty -> ty -> ty) -> NonEmpty (UArray ty) -> ty
foldl1' f (NonEmpty arr) = onBackendPure' arr (Alg.foldl1 f)
{-# SPECIALIZE [3] foldl1' :: (Word8 -> Word8 -> Word8) -> NonEmpty (UArray Word8) -> Word8 #-}

foldr1 :: PrimType ty => (ty -> ty -> ty) -> NonEmpty (UArray ty) -> ty
foldr1 f arr = let (initialAcc, rest) = revSplitAt 1 $ getNonEmpty arr
               in foldr f (unsafeIndex initialAcc 0) rest

all :: PrimType ty => (ty -> Bool) -> UArray ty -> Bool
all predicate arr = onBackendPure' arr $ Alg.all predicate
{-# SPECIALIZE [3] all :: (Word8 -> Bool) -> UArray Word8 -> Bool #-}

any :: PrimType ty => (ty -> Bool) -> UArray ty -> Bool
any predicate arr = onBackendPure' arr $ Alg.any predicate
{-# SPECIALIZE [3] any :: (Word8 -> Bool) -> UArray Word8 -> Bool #-}

builderAppend :: (PrimType ty, PrimMonad state) => ty -> Builder (UArray ty) (MUArray ty) ty state err ()
builderAppend v = Builder $ State $ \(i, st, e) ->
    if offsetAsSize i == chunkSize st
        then do
            cur      <- unsafeFreeze (curChunk st)
            newChunk <- new (chunkSize st)
            unsafeWrite newChunk 0 v
            pure ((), (Offset 1, st { prevChunks     = cur : prevChunks st
                                    , prevChunksSize = chunkSize st + prevChunksSize st
                                    , curChunk       = newChunk
                                    }, e))
        else do
            unsafeWrite (curChunk st) i v
            pure ((), (i + 1, st, e))

builderBuild :: (PrimType ty, PrimMonad m) => Int -> Builder (UArray ty) (MUArray ty) ty m err () -> m (Either err (UArray ty))
builderBuild sizeChunksI ab
    | sizeChunksI <= 0 = builderBuild 64 ab
    | otherwise        = do
        first      <- new sizeChunks
        (i, st, e) <- snd <$> runState (runBuilder ab) (Offset 0, BuildingState [] (CountOf 0) first sizeChunks, Nothing)
        case e of
          Just err -> pure (Left err)
          Nothing -> do
            cur <- unsafeFreezeShrink (curChunk st) (offsetAsSize i)
            -- Build final array
            let totalSize = prevChunksSize st + offsetAsSize i
            bytes <- new totalSize >>= fillFromEnd totalSize (cur : prevChunks st) >>= unsafeFreeze
            pure (Right bytes)
  where
      sizeChunks = CountOf sizeChunksI

      fillFromEnd _    []     mua = pure mua
      fillFromEnd !end (x:xs) mua = do
          let sz = length x
          let start = end `sizeSub` sz
          unsafeCopyAtRO mua (sizeAsOffset start) x (Offset 0) sz
          fillFromEnd start xs mua

builderBuild_ :: (PrimType ty, PrimMonad m) => Int -> Builder (UArray ty) (MUArray ty) ty m () () -> m (UArray ty)
builderBuild_ sizeChunksI ab = either (\() -> internalError "impossible output") id <$> builderBuild sizeChunksI ab

toHexadecimal :: PrimType ty => UArray ty -> UArray Word8
toHexadecimal ba
    | len == CountOf 0 = mempty
    | otherwise     = runST $ do
        ma <- new (len `scale` 2)
        unsafeIndexer b8 (go ma)
        unsafeFreeze ma
  where
    b8 = unsafeRecast ba
    !len = length b8
    !endOfs = Offset 0 `offsetPlusE` len

    go :: MUArray Word8 s -> (Offset Word8 -> Word8) -> ST s ()
    go !ma !getAt = loop 0 0
      where
        loop !dIdx !sIdx
            | sIdx == endOfs = pure ()
            | otherwise      = do
                let !(W8# !w)       = getAt sIdx
                    !(# wHi, wLo #) = Base16.unsafeConvertByte w
                unsafeWrite ma dIdx     (W8# wHi)
                unsafeWrite ma (dIdx+1) (W8# wLo)
                loop (dIdx + 2) (sIdx+1)

toBase64Internal :: PrimType ty => Addr# -> UArray ty -> Bool -> UArray Word8
toBase64Internal table src padded
    | len == CountOf 0 = mempty
    | otherwise = runST $ do
        ma <- new dstLen
        unsafeIndexer b8 (go ma)
        unsafeFreeze ma
  where
    b8 = unsafeRecast src
    !len = length b8
    !dstLen = outputLengthBase64 padded len
    !endOfs = Offset 0 `offsetPlusE` len
    !dstEndOfs = Offset 0 `offsetPlusE` dstLen

    go :: MUArray Word8 s -> (Offset Word8 -> Word8) -> ST s ()
    go !ma !getAt = loop 0 0
      where
        eqChar = 0x3d :: Word8

        loop !sIdx !dIdx
            | sIdx == endOfs = when padded $ do
                when (dIdx `offsetPlusE` CountOf 1 <= dstEndOfs) $ unsafeWrite ma dIdx eqChar
                when (dIdx `offsetPlusE` CountOf 2 == dstEndOfs) $ unsafeWrite ma (dIdx `offsetPlusE` CountOf 1) eqChar
            | otherwise = do
                let !b2Idx = sIdx `offsetPlusE` CountOf 1
                    !b3Idx = sIdx `offsetPlusE` CountOf 2

                    !b2Available = b2Idx < endOfs
                    !b3Available = b3Idx < endOfs

                    !b1 = getAt sIdx
                    !b2 = if b2Available then getAt b2Idx else 0
                    !b3 = if b3Available then getAt b3Idx else 0

                    (w,x,y,z) = convert3 table b1 b2 b3

                    sNextIncr = 1 + fromEnum b2Available + fromEnum b3Available
                    dNextIncr = 1 + sNextIncr

                unsafeWrite ma dIdx w
                unsafeWrite ma (dIdx `offsetPlusE` CountOf 1) x

                when b2Available $ unsafeWrite ma (dIdx `offsetPlusE` CountOf 2) y
                when b3Available $ unsafeWrite ma (dIdx `offsetPlusE` CountOf 3) z

                loop (sIdx `offsetPlusE` CountOf sNextIncr) (dIdx `offsetPlusE` CountOf dNextIncr)

outputLengthBase64 :: Bool -> CountOf Word8 -> CountOf Word8
outputLengthBase64 padding (CountOf inputLenInt) = outputLength
  where
    outputLength = if padding then CountOf lenWithPadding else CountOf lenWithoutPadding
    lenWithPadding
        | m == 0    = 4 * d
        | otherwise = 4 * (d + 1)
    lenWithoutPadding
        | m == 0    = 4 * d
        | otherwise = 4 * d + m + 1
    (d,m) = inputLenInt `divMod` 3

convert3 :: Addr# -> Word8 -> Word8 -> Word8 -> (Word8, Word8, Word8, Word8)
convert3 table (W8# a) (W8# b) (W8# c) =
    let !w = narrow8Word# (uncheckedShiftRL# a 2#)
        !x = or# (and# (uncheckedShiftL# a 4#) 0x30##) (uncheckedShiftRL# b 4#)
        !y = or# (and# (uncheckedShiftL# b 2#) 0x3c##) (uncheckedShiftRL# c 6#)
        !z = and# c 0x3f##
     in (idx w, idx x, idx y, idx z)
  where
    idx :: Word# -> Word8
    idx i = W8# (indexWord8OffAddr# table (word2Int# i))

isPrefixOf :: PrimType ty => UArray ty -> UArray ty -> Bool
isPrefixOf pre arr
    | pLen > pArr = False
    | otherwise   = pre == unsafeTake pLen arr
  where
    !pLen = length pre
    !pArr = length arr
{-# SPECIALIZE [3] isPrefixOf :: UArray Word8 -> UArray Word8 -> Bool #-}

isSuffixOf :: PrimType ty => UArray ty -> UArray ty -> Bool
isSuffixOf suffix arr
    | pLen > pArr = False
    | otherwise   = suffix == revTake pLen arr
  where
    !pLen = length suffix
    !pArr = length arr
{-# SPECIALIZE [3] isSuffixOf :: UArray Word8 -> UArray Word8 -> Bool #-}
