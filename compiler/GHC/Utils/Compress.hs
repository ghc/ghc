{-# LANGUAGE CPP #-}
module GHC.Utils.Compress where

import GHC.Prelude
import qualified Data.ByteString as BS
import GHC.Ptr(Ptr)
import Foreign.ForeignPtr (ForeignPtr)

#if defined(HAVE_LIBZSTD)
import Foreign.C.Types
import qualified Data.ByteString.Internal as BSI
import GHC.IO (unsafePerformIO)
import GHC.Ptr (plusPtr, castPtr)
import Foreign.ForeignPtr (plusForeignPtr, withForeignPtr)
import Foreign.Marshal.Utils
import Data.Word
import Foreign.Storable
#endif

do_compress :: Int
compress    :: Int -> BS.ByteString -> BS.ByteString
compressPtr :: Int -> Ptr src -> Int -> (Int -> Ptr src -> IO b) -> IO b

decompress :: BS.ByteString -> BS.ByteString
decompressPtr :: ForeignPtr src -> Int -> IO (ForeignPtr src, Int)
#if !defined(HAVE_LIBZSTD)
do_compress   = 0
compress _ bs = bs
compressPtr _ srcPtr len k = k len srcPtr
decompress bs = bs
decompressPtr p len = return (p, len)

#else
do_compress = 1

compress clvl (BSI.PS srcForeignPtr off len) = unsafePerformIO $
    withForeignPtr srcForeignPtr $ \srcPtr -> do
      compressPtr clvl (srcPtr `plusPtr` off) len $ \compressedSize dstPtr ->
        BSI.create compressedSize $ \p -> copyBytes p dstPtr compressedSize

compressPtr clvl srcPtr len k = do
  maxCompressedSize <- zstd_compress_bound $ fromIntegral len
  dstForeignPtr <- BSI.mallocByteString (fromIntegral maxCompressedSize)
  withForeignPtr dstForeignPtr $ \dstPtr -> do
    compressedSize <- fromIntegral @_ @Int <$>
      zstd_compress
        dstPtr
        maxCompressedSize
        srcPtr
        (fromIntegral @Int len)
        (fromIntegral @Int clvl)
    k compressedSize dstPtr

decompress (BSI.PS srcForeignPtr off len) = unsafePerformIO $ do
      (fptr, actualSize) <- decompressPtr (srcForeignPtr `plusForeignPtr` off) (fromIntegral len)
      withForeignPtr fptr $ \dstPtr ->
        BSI.create actualSize (\p -> copyBytes p dstPtr actualSize)


decompressPtr srcForeignPtr srcSize = do
  withForeignPtr srcForeignPtr $ \srcPtr -> do
    decompressedSizeM <- getDecompressedSize srcPtr (fromIntegral srcSize)
    print decompressedSizeM
    printFirstBytes srcPtr
    case decompressedSizeM of
      Nothing -> error "Decompression failed"
      Just decompressedSize -> do
          dstForeignPtr <- BSI.mallocByteString (fromIntegral decompressedSize)
          withForeignPtr dstForeignPtr $ \dstPtr -> do
            decompressedSize <- fromIntegral <$> zstd_decompress dstPtr (fromIntegral decompressedSize) srcPtr (fromIntegral srcSize)
            if decompressedSize == (-1)
              then error "Decompression failed"
              else return (dstForeignPtr, decompressedSize)

printFirstBytes :: Ptr src -> IO ()
printFirstBytes ptr = do
    bytes <- peekBytes ptr 4  -- Read the first 4 bytes
    putStrLn $ "First bytes: " ++ show bytes

peekBytes :: Ptr src -> Int -> IO [Word8]
peekBytes ptr n = mapM (peekElemOff (castPtr ptr)) [0..(n-1)]


getDecompressedSize :: Ptr src -> CSize -> IO (Maybe CSize)
getDecompressedSize input inputSize = do
    size <- zstd_getFrameContentSize input inputSize
    if size == fromIntegral (maxBound :: Word64) -- ZSTD_CONTENTSIZE_UNKNOWN
        then pure Nothing
        else pure (Just size)

foreign import ccall unsafe "ZSTD_compress"
    zstd_compress ::
         Ptr dst -- ^ Destination buffer
      -> CSize   -- ^ Capacity of destination buffer
      -> Ptr src -- ^ Source buffer
      -> CSize   -- ^ Size of source buffer
      -> CInt    -- ^ Compression level
      -> IO CSize

-- | Compute the maximum compressed size for a given source buffer size
foreign import ccall unsafe "ZSTD_compressBound"
    zstd_compress_bound ::
         CSize -- ^ Size of source buffer
      -> IO CSize

foreign import ccall unsafe "ZSTD_decompress"
  zstd_decompress :: Ptr src -> CSize -> Ptr dst -> CSize -> IO CSize

foreign import ccall unsafe "ZSTD_getFrameContentSize"
  zstd_getFrameContentSize :: Ptr src -> CSize -> IO CSize
#endif

defaultCompressionLevel :: Int
defaultCompressionLevel = 3
