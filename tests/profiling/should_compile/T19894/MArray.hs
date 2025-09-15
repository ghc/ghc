{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
module MArray
    (
      Array (..)
    , writeNUnsafe
    , length
    , fromList
    , fromListN
    , defaultChunkSize
    , read
    , shrinkToFit
    , mutableArray
    , unsafeInlineIO
    , memcmp
    , foldl'
    , unsafeIndexIO
    )

where

import Control.Exception (assert)
import Control.Monad (when, void)
import Data.Functor.Identity (runIdentity)
import Data.Word (Word8)
import Foreign.C.Types (CSize(..), CInt(..))
import Fold (Fold(..))
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr (withForeignPtr, touchForeignPtr)
import Foreign.Ptr (plusPtr, minusPtr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.Base (realWorld#)
import GHC.ForeignPtr (ForeignPtr(..))
import GHC.IO (IO(IO), unsafePerformIO)
import GHC.Ptr (Ptr(..))
import Step (Step(..))
import Unfold (Unfold(..))
import qualified GHC.ForeignPtr as GHC
import qualified Fold as FL
import qualified StreamD as D
import qualified StreamK as K
import Prelude hiding (Foldable(..), read)

data Array a =
    Array
    { aStart :: {-# UNPACK #-} !(ForeignPtr a) -- ^ first address
    , aEnd   :: {-# UNPACK #-} !(Ptr a)        -- ^ first unused address
    , aBound :: {-# UNPACK #-} !(Ptr a)        -- ^ first address beyond allocated memory
    }

{-# INLINE mutableArray #-}
mutableArray ::
    ForeignPtr a -> Ptr a -> Ptr a -> Array a
mutableArray = Array

data ArrayUnsafe a = ArrayUnsafe
    {-# UNPACK #-} !(ForeignPtr a) -- first address
    {-# UNPACK #-} !(Ptr a)        -- first unused address

-- | allocate a new array using the provided allocator function.
{-# INLINE newArrayAlignedAllocWith #-}
newArrayAlignedAllocWith :: forall a. Storable a
    => (Int -> Int -> IO (ForeignPtr a)) -> Int -> Int -> IO (Array a)
newArrayAlignedAllocWith alloc alignSize count = do
    let size = count * sizeOf (undefined :: a)
    fptr <- alloc size alignSize
    let p = unsafeForeignPtrToPtr fptr
    return $ Array
        { aStart = fptr
        , aEnd   = p
        , aBound = p `plusPtr` size
        }

{-# INLINE mallocForeignPtrAlignedBytes #-}
mallocForeignPtrAlignedBytes :: Int -> Int -> IO (GHC.ForeignPtr a)
mallocForeignPtrAlignedBytes =
    GHC.mallocPlainForeignPtrAlignedBytes

{-# INLINE newArrayAligned #-}
newArrayAligned :: forall a. Storable a => Int -> Int -> IO (Array a)
newArrayAligned = newArrayAlignedAllocWith mallocForeignPtrAlignedBytes

{-# INLINE newArray #-}
newArray :: forall a. Storable a => Int -> IO (Array a)
newArray = newArrayAligned (alignment (undefined :: a))

-- | Like 'writeN' but does not check the array bounds when writing. The fold
-- driver must not call the step function more than 'n' times otherwise it will
-- corrupt the memory and crash. This function exists mainly because any
-- conditional in the step function blocks fusion causing 10x performance
-- slowdown.
--
-- @since 0.7.0
{-# INLINE [1] writeNUnsafe #-}
writeNUnsafe :: forall m a. (MonadIO m, Storable a)
    => Int -> Fold m a (Array a)
writeNUnsafe n = Fold step initial extract

    where

    initial = do
        (Array start end _) <- liftIO $ newArray (max n 0)
        return $ FL.Partial $ ArrayUnsafe start end

    step (ArrayUnsafe start end) x = do
        liftIO $ poke end x
        return
          $ FL.Partial
          $ ArrayUnsafe start (end `plusPtr` sizeOf (undefined :: a))

    extract (ArrayUnsafe start end) = return $ Array start end end -- liftIO . shrinkToFit

{-# INLINE byteLength #-}
byteLength :: Array a -> Int
byteLength Array{..} =
    let p = unsafeForeignPtrToPtr aStart
        len = aEnd `minusPtr` p
    in assert (len >= 0) len

-- | /O(1)/ Get the length of the array i.e. the number of elements in the
-- array.
--
-- @since 0.7.0
{-# INLINE length #-}
length :: forall a. Storable a => Array a -> Int
length arr = byteLength arr `div` sizeOf (undefined :: a)

{-# INLINE [1] fromStreamDN #-}
fromStreamDN :: forall m a. (MonadIO m, Storable a)
    => Int -> D.Stream m a -> m (Array a)
fromStreamDN limit str = do
    arr <- liftIO $ newArray limit
    end <- D.foldlM' fwrite (return $ aEnd arr) $ D.take limit str
    return $ arr {aEnd = end}

    where

    fwrite ptr x = do
        liftIO $ poke ptr x
        return $ ptr `plusPtr` sizeOf (undefined :: a)

{-# INLINABLE fromListN #-}
fromListN :: Storable a => Int -> [a] -> Array a
fromListN n xs = unsafePerformIO $ fromStreamDN n $ D.fromList xs

data GroupState s start end bound
    = GroupStart s
    | GroupBuffer s start end bound
    | GroupYield start end bound (GroupState s start end bound)
    | GroupFinish

-- | @arraysOf n stream@ groups the input stream into a stream of
-- arrays of size n.
--
-- @arraysOf n = StreamD.foldMany (Array.writeN n)@
--
-- /Pre-release/
{-# INLINE [1] arraysOf #-}
arraysOf :: forall m a. (MonadIO m, Storable a)
    => Int -> D.Stream m a -> D.Stream m (Array a)
-- XXX the idiomatic implementation leads to large regression in the D.reverse'
-- benchmark. It seems it has difficulty producing optimized code when
-- converting to StreamK. Investigate GHC optimizations.
-- arraysOf n = D.foldMany (writeN n)
arraysOf n (D.Stream step state) =
    D.Stream step' (GroupStart state)

    where

    {-# INLINE [0] step' #-}
    step' _ (GroupStart st) = do
        when (n <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ "Streamly.Internal.Data.Array.Foreign.Mut.Type.fromStreamDArraysOf: the size of "
                 ++ "arrays [" ++ show n ++ "] must be a natural number"
        Array start end bound <- liftIO $ newArray n
        return $ D.Skip (GroupBuffer st start end bound)

    step' gst (GroupBuffer st start end bound) = do
        r <- step (K.adaptState gst) st
        case r of
            D.Yield x s -> do
                liftIO $ poke end x
                let end' = end `plusPtr` sizeOf (undefined :: a)
                return $
                    if end' >= bound
                    then D.Skip (GroupYield start end' bound (GroupStart s))
                    else D.Skip (GroupBuffer s start end' bound)
            D.Skip s -> return $ D.Skip (GroupBuffer s start end bound)
            D.Stop -> return $ D.Skip (GroupYield start end bound GroupFinish)

    step' _ (GroupYield start end bound next) =
        return $ D.Yield (Array start end bound) next

    step' _ GroupFinish = return D.Stop

allocOverhead :: Int
allocOverhead = 2 * sizeOf (undefined :: Int)

mkChunkSize :: Int -> Int
mkChunkSize n = let size = n - allocOverhead in max size 0

mkChunkSizeKB :: Int -> Int
mkChunkSizeKB n = mkChunkSize (n * k)
   where k = 1024

defaultChunkSize :: Int
defaultChunkSize = mkChunkSizeKB 32

{-# INLINE bufferChunks #-}
bufferChunks :: (MonadIO m, Storable a) =>
    D.Stream m a -> m (K.Stream m (Array a))
bufferChunks m = D.foldr K.cons K.nil $ arraysOf defaultChunkSize m

data Producer m a b =
    -- | @Producer step inject extract@
    forall s. Producer (s -> m (Step s b)) (a -> m s) (s -> m a)

{-# INLINE unsafeInlineIO #-}
unsafeInlineIO :: IO a -> a
unsafeInlineIO (IO m) = case m realWorld# of (# _, r #) -> r

data ReadUState a = ReadUState
    {-# UNPACK #-} !(ForeignPtr a)  -- foreign ptr with end of array pointer
    {-# UNPACK #-} !(Ptr a)         -- current pointer

-- | Resumable unfold of an array.
--
{-# INLINE [1] producer #-}
producer :: forall m a. (Monad m, Storable a) => Producer m (Array a) a
producer = Producer step inject extract
    where

    inject (Array (ForeignPtr start contents) (Ptr end) _) =
        return $ ReadUState (ForeignPtr end contents) (Ptr start)

    {-# INLINE [0] step #-}
    step (ReadUState fp@(ForeignPtr end _) p) | p == Ptr end =
        let x = unsafeInlineIO $ touchForeignPtr fp
        in x `seq` return D.Stop
    step (ReadUState fp p) = do
            -- unsafeInlineIO allows us to run this in Identity monad for pure
            -- toList/foldr case which makes them much faster due to not
            -- accumulating the list and fusing better with the pure consumers.
            --
            -- This should be safe as the array contents are guaranteed to be
            -- evaluated/written to before we peek at them.
            let !x = unsafeInlineIO $ peek p
            return $ D.Yield x
                (ReadUState fp (p `plusPtr` sizeOf (undefined :: a)))

    extract (ReadUState (ForeignPtr end contents) (Ptr p)) =
        return $ Array (ForeignPtr p contents) (Ptr end) (Ptr end)

{-# INLINE simplify #-}
simplify :: Producer m a b -> Unfold m a b
simplify (Producer step inject _) = Unfold step inject

-- | Unfold an array into a stream.
--
-- @since 0.7.0
{-# INLINE [1] read #-}
read :: forall m a. (Monad m, Storable a) => Unfold m (Array a) a
read = simplify producer

{-# INLINE fromStreamD #-}
fromStreamD :: (MonadIO m, Storable a) => D.Stream m a -> m (Array a)
fromStreamD m = do
    buffered <- bufferChunks m
    len <- K.foldl' (+) 0 (K.map length buffered)
    fromStreamDN len $ D.unfoldMany read $ D.fromStreamK buffered

-- | Create an 'Array' from a list. The list must be of finite size.
--
-- @since 0.7.0
{-# INLINABLE fromList #-}
fromList :: Storable a => [a] -> Array a
fromList xs = unsafePerformIO $ fromStreamD $ D.fromList xs

foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

-- XXX we are converting Int to CSize
memcpy :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
memcpy dst src len = void (c_memcpy dst src (fromIntegral len))

foreign import ccall unsafe "string.h memcmp" c_memcmp
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

-- XXX we are converting Int to CSize
-- return True if the memory locations have identical contents
{-# INLINE memcmp #-}
memcmp :: Ptr Word8 -> Ptr Word8 -> Int -> IO Bool
memcmp p1 p2 len = do
    r <- c_memcmp p1 p2 (fromIntegral len)
    return $ r == 0

{-# NOINLINE reallocAligned #-}
reallocAligned :: Int -> Int -> Array a -> IO (Array a)
reallocAligned alignSize newSize Array{..} = do
    assert (aEnd <= aBound) (return ())
    let oldStart = unsafeForeignPtrToPtr aStart
    let size = aEnd `minusPtr` oldStart
    newPtr <- mallocForeignPtrAlignedBytes newSize alignSize
    withForeignPtr newPtr $ \pNew -> do
        memcpy (castPtr pNew) (castPtr oldStart) size
        touchForeignPtr aStart
        return $ Array
            { aStart = newPtr
            , aEnd   = pNew `plusPtr` size
            , aBound = pNew `plusPtr` newSize
            }

-- XXX can unaligned allocation be more efficient when alignment is not needed?
{-# INLINABLE realloc #-}
realloc :: forall a. Storable a => Int -> Array a -> IO (Array a)
realloc = reallocAligned (alignment (undefined :: a))

shrinkToFit :: forall a. Storable a => Array a -> IO (Array a)
shrinkToFit arr@Array{..} = do
    assert (aEnd <= aBound) (return ())
    let start = unsafeForeignPtrToPtr aStart
    let used = aEnd `minusPtr` start
        waste = aBound `minusPtr` aEnd
    -- if used == waste == 0 then do not realloc
    -- if the wastage is more than 25% of the array then realloc
    if used < 3 * waste
    then realloc used arr
    else return arr

{-# INLINE [1] toStreamD #-}
toStreamD :: forall m a. (Monad m, Storable a) => Array a -> D.Stream m a
toStreamD Array{..} =
    let p = unsafeForeignPtrToPtr aStart
    in D.Stream step p

    where

    {-# INLINE [0] step #-}
    step _ p | p == aEnd = return D.Stop
    step _ p = do
        -- unsafeInlineIO allows us to run this in Identity monad for pure
        -- toList/foldr case which makes them much faster due to not
        -- accumulating the list and fusing better with the pure consumers.
        --
        -- This should be safe as the array contents are guaranteed to be
        -- evaluated/written to before we peek at them.
        let !x = unsafeInlineIO $ do
                    r <- peek p
                    touchForeignPtr aStart
                    return r
        return $ D.Yield x (p `plusPtr` sizeOf (undefined :: a))

{-# INLINE [1] foldl' #-}
foldl' :: forall a b. Storable a => (b -> a -> b) -> b -> Array a -> b
foldl' f z arr = runIdentity $ D.foldl' f z $ toStreamD arr

{-# INLINE [1] unsafeIndexIO #-}
unsafeIndexIO :: forall a. Storable a => Array a -> Int -> IO a
unsafeIndexIO Array {..} i =
     withForeignPtr aStart $ \p -> do
        let elemSize = sizeOf (undefined :: a)
            elemOff = p `plusPtr` (elemSize * i)
        assert (i >= 0 && elemOff `plusPtr` elemSize <= aEnd)
               (return ())
        peek elemOff
