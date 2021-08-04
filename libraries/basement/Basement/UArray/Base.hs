{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Basement.UArray.Base
    ( MUArray(..)
    , UArray(..)
    , MUArrayBackend(..)
    , UArrayBackend(..)
    -- * New mutable array creation
    , newUnpinned
    , newPinned
    , newNative
    , newNative_
    , new
    -- * Pinning status
    , isPinned
    , isMutablePinned
    -- * Mutable array accessor
    , unsafeRead
    , unsafeWrite
    -- * Freezing routines
    , unsafeFreezeShrink
    , unsafeFreeze
    , unsafeThaw
    , thaw
    , copy
    -- * Array accessor
    , unsafeIndex
    , unsafeIndexer
    , onBackend
    , onBackendPure
    , onBackendPure'
    , onBackendPrim
    , onMutableBackend
    , unsafeDewrap
    , unsafeDewrap2
    -- * Basic lowlevel functions
    , vFromListN
    , empty
    , length
    , offset
    , ValidRange(..)
    , offsetsValidRange
    , equal
    , equalMemcmp
    , compare
    , copyAt
    , unsafeCopyAtRO
    , toBlock
    -- * temporary
    , pureST
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.Ptr
import           GHC.ST
import           Basement.Compat.Primitive
import           Basement.Monad
import           Basement.PrimType
import           Basement.Compat.Base
import           Basement.Compat.C.Types
import           Basement.Compat.Semigroup
import qualified Basement.Runtime as Runtime
import           Data.Proxy
import qualified Basement.Compat.ExtList as List
import qualified Basement.Alg.Class as Alg
import           Basement.Types.OffsetSize
import           Basement.FinalPtr
import           Basement.NormalForm
import           Basement.Block (MutableBlock(..), Block(..))
import qualified Basement.Block as BLK
import qualified Basement.Block.Mutable as MBLK
import           Basement.Numerical.Additive
import           Basement.Bindings.Memory
import           System.IO.Unsafe (unsafeDupablePerformIO)

-- | A Mutable array of types built on top of GHC primitive.
--
-- Element in this array can be modified in place.
data MUArray ty st = MUArray {-# UNPACK #-} !(Offset ty)
                             {-# UNPACK #-} !(CountOf ty)
                                            !(MUArrayBackend ty st)

data MUArrayBackend ty st = MUArrayMBA (MutableBlock ty st) | MUArrayAddr (FinalPtr ty)


instance PrimType ty => Alg.Indexable (Ptr ty) ty where
    index (Ptr addr) = primAddrIndex addr

instance Alg.Indexable (Ptr Word8) Word64 where
    index (Ptr addr) = primAddrIndex addr

instance (PrimMonad prim, PrimType ty) => Alg.RandomAccess (Ptr ty) prim ty where
    read (Ptr addr) = primAddrRead addr
    write (Ptr addr) = primAddrWrite addr

-- | An array of type built on top of GHC primitive.
--
-- The elements need to have fixed sized and the representation is a
-- packed contiguous array in memory that can easily be passed
-- to foreign interface
data UArray ty = UArray {-# UNPACK #-} !(Offset ty)
                        {-# UNPACK #-} !(CountOf ty)
                                       !(UArrayBackend ty)
    deriving (Typeable)

data UArrayBackend ty = UArrayBA !(Block ty) | UArrayAddr !(FinalPtr ty)
    deriving (Typeable)

instance Data ty => Data (UArray ty) where
    dataTypeOf _ = arrayType
    toConstr _   = error "toConstr"
    gunfold _ _  = error "gunfold"

arrayType :: DataType
arrayType = mkNoRepType "Basement.UArray"

instance NormalForm (UArray ty) where
    toNormalForm (UArray _ _ !_) = ()
instance (PrimType ty, Show ty) => Show (UArray ty) where
    show v = show (toList v)
instance (PrimType ty, Eq ty) => Eq (UArray ty) where
    (==) = equal
instance (PrimType ty, Ord ty) => Ord (UArray ty) where
    {-# SPECIALIZE instance Ord (UArray Word8) #-}
    compare = vCompare

instance PrimType ty => Semigroup (UArray ty) where
    (<>) = append
instance PrimType ty => Monoid (UArray ty) where
    mempty  = empty
    mappend = append
    mconcat = concat

instance PrimType ty => IsList (UArray ty) where
    type Item (UArray ty) = ty
    fromList = vFromList
    fromListN len = vFromListN (CountOf len)
    toList = vToList

length :: UArray ty -> CountOf ty
length (UArray _ len _) = len
{-# INLINE[1] length #-}

offset :: UArray ty -> Offset ty
offset (UArray ofs _ _) = ofs
{-# INLINE[1] offset #-}

data ValidRange ty = ValidRange {-# UNPACK #-} !(Offset ty) {-# UNPACK #-} !(Offset ty)

offsetsValidRange :: UArray ty -> ValidRange ty
offsetsValidRange (UArray ofs len _) = ValidRange ofs (ofs `offsetPlusE` len)

-- | Return if the array is pinned in memory
--
-- note that Foreign array are considered pinned
isPinned :: UArray ty -> PinnedStatus
isPinned (UArray _ _ (UArrayAddr {})) = Pinned
isPinned (UArray _ _ (UArrayBA blk))  = BLK.isPinned blk

-- | Return if a mutable array is pinned in memory
isMutablePinned :: MUArray ty st -> PinnedStatus
isMutablePinned (MUArray _ _ (MUArrayAddr {})) = Pinned
isMutablePinned (MUArray _ _ (MUArrayMBA mb))  = BLK.isMutablePinned mb

-- | Create a new pinned mutable array of size @n.
--
-- all the cells are uninitialized and could contains invalid values.
--
-- All mutable arrays are allocated on a 64 bits aligned addresses
newPinned :: forall prim ty . (PrimMonad prim, PrimType ty) => CountOf ty -> prim (MUArray ty (PrimState prim))
newPinned n = MUArray 0 n . MUArrayMBA <$> MBLK.newPinned n

-- | Create a new unpinned mutable array of size @n elements.
--
-- If the size exceeds a GHC-defined threshold, then the memory will be
-- pinned. To be certain about pinning status with small size, use 'newPinned'
newUnpinned :: forall prim ty . (PrimMonad prim, PrimType ty) => CountOf ty -> prim (MUArray ty (PrimState prim))
newUnpinned n = MUArray 0 n . MUArrayMBA <$> MBLK.new n

newNative :: (PrimMonad prim, PrimType ty)
          => CountOf ty
          -> (MutableBlock ty (PrimState prim) -> prim a)
          -> prim (a, MUArray ty (PrimState prim))
newNative n f = do
    mb <- MBLK.new n
    a  <- f mb
    pure (a, MUArray 0 n (MUArrayMBA mb))

-- | Same as newNative but expect no extra return value from f
newNative_ :: (PrimMonad prim, PrimType ty)
           => CountOf ty
           -> (MutableBlock ty (PrimState prim) -> prim ())
           -> prim (MUArray ty (PrimState prim))
newNative_ n f = do
    mb <- MBLK.new n
    f mb
    pure (MUArray 0 n (MUArrayMBA mb))

-- | Create a new mutable array of size @n.
--
-- When memory for a new array is allocated, we decide if that memory region
-- should be pinned (will not be copied around by GC) or unpinned (can be
-- moved around by GC) depending on its size.
--
-- You can change the threshold value used by setting the environment variable
-- @HS_FOUNDATION_UARRAY_UNPINNED_MAX@.
new :: (PrimMonad prim, PrimType ty) => CountOf ty -> prim (MUArray ty (PrimState prim))
new sz
    | sizeRecast sz <= maxSizeUnpinned = newUnpinned sz
    | otherwise                        = newPinned sz
  where
    -- Safe to use here: If the value changes during runtime, this will only
    -- have an impact on newly created arrays.
    maxSizeUnpinned = Runtime.unsafeUArrayUnpinnedMaxSize
{-# INLINE new #-}

-- | read from a cell in a mutable array without bounds checking.
--
-- Reading from invalid memory can return unpredictable and invalid values.
-- use 'read' if unsure.
unsafeRead :: (PrimMonad prim, PrimType ty) => MUArray ty (PrimState prim) -> Offset ty -> prim ty
unsafeRead (MUArray start _ (MUArrayMBA (MutableBlock mba))) i = primMbaRead mba (start + i)
unsafeRead (MUArray start _ (MUArrayAddr fptr)) i = withFinalPtr fptr $ \(Ptr addr) -> primAddrRead addr (start + i)
{-# INLINE unsafeRead #-}


-- | write to a cell in a mutable array without bounds checking.
--
-- Writing with invalid bounds will corrupt memory and your program will
-- become unreliable. use 'write' if unsure.
unsafeWrite :: (PrimMonad prim, PrimType ty) => MUArray ty (PrimState prim) -> Offset ty -> ty -> prim ()
unsafeWrite (MUArray start _ (MUArrayMBA mb)) i v = MBLK.unsafeWrite mb (start+i) v
unsafeWrite (MUArray start _ (MUArrayAddr fptr)) i v = withFinalPtr fptr $ \(Ptr addr) -> primAddrWrite addr (start+i) v
{-# INLINE unsafeWrite #-}

-- | Return the element at a specific index from an array without bounds checking.
--
-- Reading from invalid memory can return unpredictable and invalid values.
-- use 'index' if unsure.
unsafeIndex :: forall ty . PrimType ty => UArray ty -> Offset ty -> ty
unsafeIndex (UArray start _ (UArrayBA ba)) n = BLK.unsafeIndex ba (start + n)
unsafeIndex (UArray start _ (UArrayAddr fptr)) n = withUnsafeFinalPtr fptr (\(Ptr addr) -> return (primAddrIndex addr (start+n)) :: IO ty)
{-# INLINE unsafeIndex #-}

unsafeIndexer :: (PrimMonad prim, PrimType ty) => UArray ty -> ((Offset ty -> ty) -> prim a) -> prim a
unsafeIndexer (UArray start _ (UArrayBA ba)) f = f (\n -> BLK.unsafeIndex ba (start + n))
unsafeIndexer (UArray start _ (UArrayAddr fptr)) f = withFinalPtr fptr $ \(Ptr addr) -> f (\n -> primAddrIndex addr (start + n))
{-# INLINE unsafeIndexer #-}

-- | Freeze a mutable array into an array.
--
-- the MUArray must not be changed after freezing.
unsafeFreeze :: PrimMonad prim => MUArray ty (PrimState prim) -> prim (UArray ty)
unsafeFreeze (MUArray start len (MUArrayMBA mba)) =
    UArray start len . UArrayBA <$> MBLK.unsafeFreeze mba
unsafeFreeze (MUArray start len (MUArrayAddr fptr)) =
    pure $ UArray start len (UArrayAddr fptr)
{-# INLINE unsafeFreeze #-}

unsafeFreezeShrink :: (PrimType ty, PrimMonad prim) => MUArray ty (PrimState prim) -> CountOf ty -> prim (UArray ty)
unsafeFreezeShrink (MUArray start _ backend) n = unsafeFreeze (MUArray start n backend)
{-# INLINE unsafeFreezeShrink #-}

-- | Thaw an immutable array.
--
-- The UArray must not be used after thawing.
unsafeThaw :: (PrimType ty, PrimMonad prim) => UArray ty -> prim (MUArray ty (PrimState prim))
unsafeThaw (UArray start len (UArrayBA blk)) = MUArray start len . MUArrayMBA <$> BLK.unsafeThaw blk
unsafeThaw (UArray start len (UArrayAddr fptr)) = pure $ MUArray start len (MUArrayAddr fptr)
{-# INLINE unsafeThaw #-}

-- | Thaw an array to a mutable array.
--
-- the array is not modified, instead a new mutable array is created
-- and every values is copied, before returning the mutable array.
thaw :: (PrimMonad prim, PrimType ty) => UArray ty -> prim (MUArray ty (PrimState prim))
thaw array = do
    ma <- new (length array)
    unsafeCopyAtRO ma azero array (Offset 0) (length array)
    pure ma
{-# INLINE thaw #-}

-- | Copy every cells of an existing array to a new array
copy :: PrimType ty => UArray ty -> UArray ty
copy array = runST (thaw array >>= unsafeFreeze)


onBackend :: (Block ty -> a)
          -> (FinalPtr ty -> Ptr ty -> ST s a)
          -> UArray ty
          -> a
onBackend onBa _      (UArray _ _ (UArrayBA ba))     = onBa ba
onBackend _    onAddr (UArray _ _ (UArrayAddr fptr)) = withUnsafeFinalPtr fptr $ \ptr@(Ptr !_) -> 
                                                           onAddr fptr ptr
{-# INLINE onBackend #-}

onBackendPure :: (Block ty -> a)
              -> (Ptr ty -> a)
              -> UArray ty
              -> a
onBackendPure goBA goAddr arr = onBackend goBA (\_ -> pureST . goAddr) arr
{-# INLINE onBackendPure #-}

onBackendPure' :: forall ty a . PrimType  ty
               => UArray ty
               -> (forall container. Alg.Indexable container ty 
                   => container -> Offset ty -> Offset ty -> a)
               -> a
onBackendPure' arr f = onBackendPure f' f' arr
  where f' :: Alg.Indexable container ty => container -> a
        f' c = f c start end
          where (ValidRange !start !end) = offsetsValidRange arr
{-# INLINE onBackendPure' #-}

onBackendPrim :: PrimMonad prim
              => (Block ty -> prim a)
              -> (FinalPtr ty -> prim a)
              -> UArray ty
              -> prim a
onBackendPrim onBa _      (UArray _ _ (UArrayBA ba))     = onBa ba
onBackendPrim _    onAddr (UArray _ _ (UArrayAddr fptr)) = onAddr fptr
{-# INLINE onBackendPrim #-}

onMutableBackend :: PrimMonad prim
                 => (MutableBlock ty (PrimState prim) -> prim a)
                 -> (FinalPtr ty -> prim a)
                 -> MUArray ty (PrimState prim)
                 -> prim a
onMutableBackend onMba _      (MUArray _ _ (MUArrayMBA mba))   = onMba mba
onMutableBackend _     onAddr (MUArray _ _ (MUArrayAddr fptr)) = onAddr fptr
{-# INLINE onMutableBackend #-}


unsafeDewrap :: (Block ty -> Offset ty -> a)
             -> (Ptr ty -> Offset ty -> ST s a)
             -> UArray ty
             -> a
unsafeDewrap _ g (UArray start _ (UArrayAddr fptr))     = withUnsafeFinalPtr fptr $ \ptr -> g ptr start
unsafeDewrap f _ (UArray start _ (UArrayBA ba)) = f ba start
{-# INLINE unsafeDewrap #-}

unsafeDewrap2 :: (ByteArray# -> ByteArray# -> a)
              -> (Ptr ty -> Ptr ty -> ST s a)
              -> (ByteArray# -> Ptr ty -> ST s a)
              -> (Ptr ty -> ByteArray# -> ST s a)
              -> UArray ty
              -> UArray ty
              -> a
unsafeDewrap2 f g h i (UArray _ _ back1) (UArray _ _ back2) =
    case (back1, back2) of
        (UArrayBA (Block ba1), UArrayBA (Block ba2)) -> f ba1 ba2
        (UArrayAddr fptr1, UArrayAddr fptr2)         -> withUnsafeFinalPtr fptr1 $ \ptr1 -> withFinalPtr fptr2 $ \ptr2 -> g ptr1 ptr2
        (UArrayBA (Block ba1), UArrayAddr fptr2)     -> withUnsafeFinalPtr fptr2 $ \ptr2 -> h ba1 ptr2
        (UArrayAddr fptr1, UArrayBA (Block ba2))     -> withUnsafeFinalPtr fptr1 $ \ptr1 -> i ptr1 ba2
{-# INLINE [2] unsafeDewrap2 #-}

pureST :: a -> ST s a
pureST = pure

-- | make an array from a list of elements.
vFromList :: forall ty . PrimType ty => [ty] -> UArray ty
vFromList l = runST $ do
    a <- newNative_ len copyList
    unsafeFreeze a
  where
    len = List.length l
    copyList :: MutableBlock ty s -> ST s ()
    copyList mb = loop 0 l
      where
        loop _  []     = pure ()
        loop !i (x:xs) = MBLK.unsafeWrite mb i x >> loop (i+1) xs

-- | Make an array from a list of elements with a size hint.
--
-- The list should be of the same size as the hint, as otherwise:
--
-- * The length of the list is smaller than the hint:
--    the array allocated is of the size of the hint, but is sliced
--    to only represent the valid bits
-- * The length of the list is bigger than the hint:
--    The allocated array is the size of the hint, and the list is truncated to
--    fit.
vFromListN :: forall ty . PrimType ty => CountOf ty -> [ty] -> UArray ty
vFromListN len l = runST $ do
    (sz, ma) <- newNative len copyList
    unsafeFreezeShrink ma sz
  where
    copyList :: MutableBlock ty s -> ST s (CountOf ty)
    copyList mb = loop 0 l
      where
        loop !i  []     = pure (offsetAsSize i)
        loop !i (x:xs)
            | i .==# len = pure (offsetAsSize i)
            | otherwise  = MBLK.unsafeWrite mb i x >> loop (i+1) xs

-- | transform an array to a list.
vToList :: forall ty . PrimType ty => UArray ty -> [ty]
vToList a
    | len == 0  = []
    | otherwise = unsafeDewrap goBa goPtr a
  where
    !len = length a
    goBa (Block ba) start = loop start
      where
        !end = start `offsetPlusE` len
        loop !i | i == end  = []
                | otherwise = primBaIndex ba i : loop (i+1)
    goPtr (Ptr addr) start = pureST (loop start)
      where
        !end = start `offsetPlusE` len
        loop !i | i == end  = []
                | otherwise = primAddrIndex addr i : loop (i+1)

-- | Check if two vectors are identical
equal :: (PrimType ty, Eq ty) => UArray ty -> UArray ty -> Bool
equal a b
    | la /= lb  = False
    | otherwise = unsafeDewrap2 goBaBa goPtrPtr goBaPtr goPtrBa a b
  where
    !start1 = offset a
    !start2 = offset b
    !end = start1 `offsetPlusE` la
    !la = length a
    !lb = length b
    goBaBa ba1 ba2 = loop start1 start2
      where
        loop !i !o | i == end  = True
                   | otherwise = primBaIndex ba1 i == primBaIndex ba2 o && loop (i+o1) (o+o1)
    goPtrPtr (Ptr addr1) (Ptr addr2) = pureST (loop start1 start2)
      where
        loop !i !o | i == end  = True
                   | otherwise = primAddrIndex addr1 i == primAddrIndex addr2 o && loop (i+o1) (o+o1)
    goBaPtr ba1 (Ptr addr2) = pureST (loop start1 start2)
      where
        loop !i !o | i == end  = True
                   | otherwise = primBaIndex ba1 i == primAddrIndex addr2 o && loop (i+o1) (o+o1)
    goPtrBa (Ptr addr1) ba2 = pureST (loop start1 start2)
      where
        loop !i !o | i == end  = True
                   | otherwise = primAddrIndex addr1 i == primBaIndex ba2 o && loop (i+o1) (o+o1)

    o1 = Offset (I# 1#)
{-# RULES "UArray/Eq/Word8" [3] equal = equalBytes #-}
{-# INLINEABLE [2] equal #-}

equalBytes :: UArray Word8 -> UArray Word8 -> Bool
equalBytes a b
    | la /= lb  = False
    | otherwise = memcmp a b (sizeInBytes la) == 0
  where
    !la = length a
    !lb = length b

equalMemcmp :: PrimType ty => UArray ty -> UArray ty -> Bool
equalMemcmp a b
    | la /= lb  = False
    | otherwise = memcmp a b (sizeInBytes la) == 0
  where
    !la = length a
    !lb = length b

-- | Compare 2 vectors
vCompare :: (Ord ty, PrimType ty) => UArray ty -> UArray ty -> Ordering
vCompare a@(UArray start1 la _) b@(UArray start2 lb _) = unsafeDewrap2 goBaBa goPtrPtr goBaPtr goPtrBa a b
  where
    !end = start1 `offsetPlusE` min la lb
    o1 = Offset (I# 1#)
    goBaBa ba1 ba2 = loop start1 start2
      where
        loop !i !o | i == end   = la `compare` lb
                   | v1 == v2   = loop (i + o1) (o + o1)
                   | otherwise  = v1 `compare` v2
          where v1 = primBaIndex ba1 i
                v2 = primBaIndex ba2 o
    goPtrPtr (Ptr addr1) (Ptr addr2) = pureST (loop start1 start2)
      where
        loop !i !o | i == end   = la `compare` lb
                   | v1 == v2   = loop (i + o1) (o + o1)
                   | otherwise  = v1 `compare` v2
          where v1 = primAddrIndex addr1 i
                v2 = primAddrIndex addr2 o
    goBaPtr ba1 (Ptr addr2) = pureST (loop start1 start2)
      where
        loop !i !o | i == end   = la `compare` lb
                   | v1 == v2   = loop (i + o1) (o + o1)
                   | otherwise  = v1 `compare` v2
          where v1 = primBaIndex ba1 i
                v2 = primAddrIndex addr2 o
    goPtrBa (Ptr addr1) ba2 = pureST (loop start1 start2)
      where
        loop !i !o | i == end   = la `compare` lb
                   | v1 == v2   = loop (i + o1) (o + o1)
                   | otherwise  = v1 `compare` v2
          where v1 = primAddrIndex addr1 i
                v2 = primBaIndex ba2 o
-- {-# SPECIALIZE [3] vCompare :: UArray Word8 -> UArray Word8 -> Ordering = vCompareBytes #-}
{-# RULES "UArray/Ord/Word8" [3] vCompare = vCompareBytes #-}
{-# INLINEABLE [2] vCompare #-}

vCompareBytes :: UArray Word8 -> UArray Word8 -> Ordering
vCompareBytes = vCompareMemcmp

vCompareMemcmp :: (Ord ty, PrimType ty) => UArray ty -> UArray ty -> Ordering
vCompareMemcmp a b = cintToOrdering $ memcmp a b sz
  where
    la = length a
    lb = length b
    sz = sizeInBytes $ min la lb
    cintToOrdering :: CInt -> Ordering
    cintToOrdering 0 = la `compare` lb
    cintToOrdering r | r < 0     = LT
                     | otherwise = GT
{-# SPECIALIZE [3] vCompareMemcmp :: UArray Word8 -> UArray Word8 -> Ordering #-}

memcmp :: PrimType ty => UArray ty -> UArray ty -> CountOf Word8 -> CInt
memcmp a@(UArray (offsetInBytes -> o1) _ _) b@(UArray (offsetInBytes -> o2) _ _) sz = unsafeDewrap2
    (\s1 s2 -> unsafeDupablePerformIO $ sysHsMemcmpBaBa s1 o1 s2 o2 sz)
    (\s1 s2 -> unsafePrimToST $ sysHsMemcmpPtrPtr s1 o1 s2 o2 sz)
    (\s1 s2 -> unsafePrimToST $ sysHsMemcmpBaPtr s1 o1 s2 o2 sz)
    (\s1 s2 -> unsafePrimToST $ sysHsMemcmpPtrBa s1 o1 s2 o2 sz)
    a b
{-# SPECIALIZE [3] memcmp :: UArray Word8 -> UArray Word8 -> CountOf Word8 -> CInt #-}

-- | Copy a number of elements from an array to another array with offsets
copyAt :: forall prim ty . (PrimMonad prim, PrimType ty)
       => MUArray ty (PrimState prim) -- ^ destination array
       -> Offset ty                  -- ^ offset at destination
       -> MUArray ty (PrimState prim) -- ^ source array
       -> Offset ty                  -- ^ offset at source
       -> CountOf ty                    -- ^ number of elements to copy
       -> prim ()
copyAt (MUArray dstStart _ (MUArrayMBA (MutableBlock dstMba))) ed (MUArray srcStart _ (MUArrayMBA (MutableBlock srcBa))) es n =
    primitive $ \st -> (# copyMutableByteArray# srcBa os dstMba od nBytes st, () #)
  where
    !sz                 = primSizeInBytes (Proxy :: Proxy ty)
    !(Offset (I# os))   = offsetOfE sz (srcStart + es)
    !(Offset (I# od))   = offsetOfE sz (dstStart + ed)
    !(CountOf (I# nBytes)) = sizeOfE sz n
copyAt (MUArray dstStart _ (MUArrayMBA (MutableBlock dstMba))) ed (MUArray srcStart _ (MUArrayAddr srcFptr)) es n =
    withFinalPtr srcFptr $ \srcPtr ->
        let !(Ptr srcAddr) = srcPtr `plusPtr` os
         in primitive $ \s -> (# copyAddrToByteArray# srcAddr dstMba od nBytes s, () #)
  where
    !sz                 = primSizeInBytes (Proxy :: Proxy ty)
    !(Offset os)        = offsetOfE sz (srcStart + es)
    !(Offset (I# od))   = offsetOfE sz (dstStart + ed)
    !(CountOf (I# nBytes)) = sizeOfE sz n
copyAt dst od src os n = loop od os
  where
    !endIndex = os `offsetPlusE` n
    loop !d !i
        | i == endIndex = return ()
        | otherwise     = unsafeRead src i >>= unsafeWrite dst d >> loop (d+1) (i+1)

-- TODO Optimise with copyByteArray#
-- | Copy @n@ sequential elements from the specified offset in a source array
--   to the specified position in a destination array.
--
--   This function does not check bounds. Accessing invalid memory can return
--   unpredictable and invalid values.
unsafeCopyAtRO :: forall prim ty . (PrimMonad prim, PrimType ty)
               => MUArray ty (PrimState prim) -- ^ destination array
               -> Offset ty                   -- ^ offset at destination
               -> UArray ty                   -- ^ source array
               -> Offset ty                   -- ^ offset at source
               -> CountOf ty                     -- ^ number of elements to copy
               -> prim ()
unsafeCopyAtRO (MUArray dstStart _ (MUArrayMBA (MutableBlock dstMba))) ed (UArray srcStart _ (UArrayBA (Block srcBa))) es n =
    primitive $ \st -> (# copyByteArray# srcBa os dstMba od nBytes st, () #)
  where
    sz = primSizeInBytes (Proxy :: Proxy ty)
    !(Offset (I# os))   = offsetOfE sz (srcStart+es)
    !(Offset (I# od))   = offsetOfE sz (dstStart+ed)
    !(CountOf (I# nBytes)) = sizeOfE sz n
unsafeCopyAtRO (MUArray dstStart _ (MUArrayMBA (MutableBlock dstMba))) ed (UArray srcStart _ (UArrayAddr srcFptr)) es n =
    withFinalPtr srcFptr $ \srcPtr ->
        let !(Ptr srcAddr) = srcPtr `plusPtr` os
         in primitive $ \s -> (# copyAddrToByteArray# srcAddr dstMba od nBytes s, () #)
  where
    sz  = primSizeInBytes (Proxy :: Proxy ty)
    !(Offset os)        = offsetOfE sz (srcStart+es)
    !(Offset (I# od))   = offsetOfE sz (dstStart+ed)
    !(CountOf (I# nBytes)) = sizeOfE sz n
unsafeCopyAtRO dst od src os n = loop od os
  where
    !endIndex = os `offsetPlusE` n
    loop d i
        | i == endIndex = return ()
        | otherwise     = unsafeWrite dst d (unsafeIndex src i) >> loop (d+1) (i+1)

empty_ :: Block ()
empty_ = runST $ primitive $ \s1 ->
    case newByteArray# 0# s1           of { (# s2, mba #) ->
    case unsafeFreezeByteArray# mba s2 of { (# s3, ba  #) ->
        (# s3, Block ba #) }}

empty :: UArray ty
empty = UArray 0 0 (UArrayBA $ Block ba) where !(Block ba) = empty_

-- | Append 2 arrays together by creating a new bigger array
append :: PrimType ty => UArray ty -> UArray ty -> UArray ty
append a b
    | la == azero = b
    | lb == azero = a
    | otherwise = runST $ do
        r  <- new (la+lb)
        ma <- unsafeThaw a
        mb <- unsafeThaw b
        copyAt r (Offset 0) ma (Offset 0) la
        copyAt r (sizeAsOffset la) mb (Offset 0) lb
        unsafeFreeze r
  where
    !la = length a
    !lb = length b

concat :: forall ty . PrimType ty => [UArray ty] -> UArray ty
concat original = runST $ do
    r <- new total
    goCopy r 0 original
    unsafeFreeze r
  where
    !total = size 0 original
    -- size
    size !sz []     = sz
    size !sz (x:xs) = size (length x + sz) xs

    zero = Offset 0

    goCopy r = loop
      where
        loop _  []      = pure ()
        loop !i (x:xs) = do
            unsafeCopyAtRO r i x zero lx
            loop (i `offsetPlusE` lx) xs
          where !lx = length x

-- | Create a Block from a UArray.
--
-- Note that because of the slice, the destination block
-- is re-allocated and copied, unless the slice point
-- at the whole array
toBlock :: PrimType ty => UArray ty -> Block ty
toBlock arr@(UArray start len (UArrayBA blk))
    | start == 0 && BLK.length blk == len = blk
    | otherwise                           = toBlock $ copy arr
toBlock arr = toBlock $ copy arr
