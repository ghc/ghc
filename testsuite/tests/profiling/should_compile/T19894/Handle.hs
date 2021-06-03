{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Handle
    (
      write
    , read
    )

where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Foreign.Storable (Storable(..))
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr, minusPtr)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import System.IO (Handle, hGetBufSome, hPutBuf)
import Unfold (Unfold(..))
import Fold (Fold(..))
import Array (Array(..))
import qualified MArray as MA
import qualified Fold as FL
import qualified Unfold as UF
import qualified StreamD as D
import qualified Array as A
import Prelude hiding (length, read)

{-# INLINABLE writeArray #-}
writeArray :: Storable a => Handle -> Array a -> IO ()
writeArray _ arr | A.length arr == 0 = return ()
writeArray h Array{..} = withForeignPtr aStart $ \p -> hPutBuf h p aLen
    where
    aLen =
        let p = unsafeForeignPtrToPtr aStart
        in aEnd `minusPtr` p

{-# INLINE writeChunks #-}
writeChunks :: (MonadIO m, Storable a) => Handle -> Fold m (Array a) ()
writeChunks h = FL.drainBy (liftIO . writeArray h)

{-# INLINE writeWithBufferOf #-}
writeWithBufferOf :: MonadIO m => Int -> Handle -> Fold m Word8 ()
writeWithBufferOf n h = FL.chunksOf n (A.writeNUnsafe n) (writeChunks h)

{-# INLINE write #-}
write :: MonadIO m => Handle -> Fold m Word8 ()
write = writeWithBufferOf MA.defaultChunkSize

{-# INLINABLE readArrayUpto #-}
readArrayUpto :: Int -> Handle -> IO (Array Word8)
readArrayUpto size h = do
    ptr <- mallocPlainForeignPtrBytes size
    -- ptr <- mallocPlainForeignPtrAlignedBytes size (alignment (undefined :: Word8))
    withForeignPtr ptr $ \p -> do
        n <- hGetBufSome h p size
        -- XXX shrink only if the diff is significant
        return $
            A.unsafeFreezeWithShrink $
            MA.mutableArray ptr (p `plusPtr` n) (p `plusPtr` size)

{-# INLINE [1] readChunksWithBufferOf #-}
readChunksWithBufferOf :: MonadIO m => Unfold m (Int, Handle) (Array Word8)
readChunksWithBufferOf = Unfold step return
    where
    {-# INLINE [0] step #-}
    step (size, h) = do
        arr <- liftIO $ readArrayUpto size h
        return $
            case A.length arr of
                0 -> D.Stop
                _ -> D.Yield arr (size, h)

{-# INLINE readWithBufferOf #-}
readWithBufferOf :: MonadIO m => Unfold m (Int, Handle) Word8
readWithBufferOf = UF.many readChunksWithBufferOf A.read

{-# INLINE read #-}
read :: MonadIO m => Unfold m Handle Word8
read = UF.supplyFirst MA.defaultChunkSize readWithBufferOf
