{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : LZ4
-- Copyright   : (c) Mark Wotton, Austin Seipp 2012-2015
-- License     : BSD3
--
-- Compression utilities (currently utilizing @LZ4 r127@).
--
module LZ4
       ( compress            -- :: S.ByteString -> S.ByteString
       , decompress          -- :: S.ByteString -> Maybe S.ByteString
       ) where

import Prelude hiding (max)
import Data.Word
import Data.Bits
import Foreign.Ptr
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
--import Control.Monad
import Data.Monoid ((<>))

import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as SI
import qualified Data.ByteString.Unsafe as U

--------------------------------------------------------------------------------
-- Compression

-- | Compresses the input 'ByteString'.
--
-- Will return 'Nothing' if the compression fails. Otherwise, returns
-- @Just xs@ with the compressed string.
compress :: S.ByteString -> Maybe S.ByteString
compress xs | S.null xs = Just S.empty
compress xs = unsafePerformIO $
  U.unsafeUseAsCStringLen xs $ \(cstr,len) -> do
    let len' = fromIntegral len :: CInt
    let max = c_LZ4_compressBound len'
    bs <- SI.createAndTrim (fromIntegral max) $ \output ->
            fromIntegral <$> c_LZ4_compress cstr output len'
    case (S.null bs) of
      True -> return Nothing
      -- Prefix the compressed string with the uncompressed length
      False -> return $ Just (format (fromIntegral len) bs)
{-# INLINEABLE compress #-}

--------------------------------------------------------------------------------
-- Decompression

-- | Decompress the input 'ByteString'.
decompress :: S.ByteString -> Maybe S.ByteString
decompress xs | S.null xs = Just S.empty
-- Get the length of the uncompressed buffer and do our thing
decompress xs = maybe Nothing (unsafePerformIO . go) (unformat xs)
  where
    go (l, str) =
      U.unsafeUseAsCString str $ \cstr -> do
        out <- SI.createAndTrim l $ \p -> do
          r :: Int <- fromIntegral <$> c_LZ4_uncompress cstr p (fromIntegral l)
          --- NOTE: r is the count of bytes c_LZ4_uncompress read from
          --- input buffer, and NOT the count of bytes used in result
          --- buffer
          return $! if (r <= 0) then 0 else l
        return $! if (S.null out) then Nothing else (Just out)
{-# INLINEABLE decompress #-}

--------------------------------------------------------------------------------
-- Utilities

-- | Pushes a Word32 and a ByteString into the format we use to correctly
-- encode/decode.
format :: Word32 -> S.ByteString -> S.ByteString
format l xs = write32LE l <> write32LE (fromIntegral $ S.length xs) <> xs

write32LE :: Word32 -> S.ByteString
write32LE w
  = S.pack [ fromIntegral (w `shiftR` 0)  :: Word8
           , fromIntegral (w `shiftR` 8)  :: Word8
           , fromIntegral (w `shiftR` 16) :: Word8
           , fromIntegral (w `shiftR` 24) :: Word8
           ]

-- | Gets a ByteString and it's length from the compressed format.
unformat :: S.ByteString -> Maybe (Int, S.ByteString)
unformat xs
  | S.length xs < 8        = Nothing -- Need at least 8 bytes
  | bsLen /= S.length rest = Nothing -- Header doesn't match real size
  | otherwise              = Just (fromIntegral origLen, rest)
  where
    origLen = fromIntegral (read32LE l0 l1 l2 l3) :: Int
    bsLen   = fromIntegral (read32LE s0 s1 s2 s3) :: Int

    [l0,l1,l2,l3] = S.unpack (S.take 4 xs)
    [s0,s1,s2,s3] = S.unpack (S.take 4 $ S.drop 4 xs)
    rest = S.drop 8 xs

read32LE :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
read32LE x0 x1 x2 x3
  = fi x0 + (fi x1 `shiftL` 8) + (fi x2 `shiftL` 16) + (fi x3 `shiftL` 24)
  where fi = fromIntegral :: Word8 -> Word32

--------------------------------------------------------------------------------
-- FFI Bindings

-- | Worst case compression bounds on an input string.
foreign import ccall unsafe "LZ4_compressBound"
  c_LZ4_compressBound :: CInt -> CInt

-- | Compresses a string.
foreign import ccall unsafe "LZ4_compress"
  c_LZ4_compress :: Ptr a   -- ^ Source
                 -> Ptr b   -- ^ Dest
                 -> CInt    -- ^ Input size
                 -> IO CInt -- ^ Result

-- | Decompresses a string.
foreign import ccall unsafe "LZ4_decompress_fast"
  c_LZ4_uncompress :: Ptr a   -- ^ Source
                   -> Ptr b   -- ^ Dest
                   -> CInt    -- ^ Size of ORIGINAL INPUT
                   -> IO CInt -- ^ Result
