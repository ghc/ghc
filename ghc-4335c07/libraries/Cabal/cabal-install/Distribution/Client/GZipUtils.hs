{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.GZipUtils
-- Copyright   :  (c) Dmitry Astapov 2010
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Provides a convenience functions for working with files that may or may not
-- be zipped.
-----------------------------------------------------------------------------
module Distribution.Client.GZipUtils (
    maybeDecompress,
  ) where

import Codec.Compression.Zlib.Internal
import Data.ByteString.Lazy.Internal as BS (ByteString(Empty, Chunk))

#if MIN_VERSION_zlib(0,6,0)
import Control.Exception (throw)
import Control.Monad (liftM)
import Control.Monad.ST.Lazy (ST, runST)
import qualified Data.ByteString as Strict
#endif

-- | Attempts to decompress the `bytes' under the assumption that
-- "data format" error at the very beginning of the stream means
-- that it is already decompressed. Caller should make sanity checks
-- to verify that it is not, in fact, garbage.
--
-- This is to deal with http proxies that lie to us and transparently
-- decompress without removing the content-encoding header. See:
-- <https://github.com/haskell/cabal/issues/678>
--
maybeDecompress :: ByteString -> ByteString
#if MIN_VERSION_zlib(0,6,0)
maybeDecompress bytes = runST (go bytes decompressor)
  where
    decompressor :: DecompressStream (ST s)
    decompressor = decompressST gzipOrZlibFormat defaultDecompressParams

    -- DataError at the beginning of the stream probably means that stream is
    -- not compressed, so we return it as-is.
    -- TODO: alternatively, we might consider looking for the two magic bytes
    -- at the beginning of the gzip header.  (not an option for zlib, though.)
    go :: Monad m => ByteString -> DecompressStream m -> m ByteString
    go cs (DecompressOutputAvailable bs k) = liftM (Chunk bs) $ go' cs =<< k
    go _  (DecompressStreamEnd       _bs ) = return Empty
    go _  (DecompressStreamError _err    ) = return bytes
    go cs (DecompressInputRequired      k) = go cs' =<< k c
      where
        (c, cs') = uncons cs

    -- Once we have received any output though we regard errors as actual errors
    -- and we throw them (as pure exceptions).
    -- TODO: We could (and should) avoid these pure exceptions.
    go' :: Monad m => ByteString -> DecompressStream m -> m ByteString
    go' cs (DecompressOutputAvailable bs k) = liftM (Chunk bs) $ go' cs =<< k
    go' _  (DecompressStreamEnd       _bs ) = return Empty
    go' _  (DecompressStreamError err     ) = throw err
    go' cs (DecompressInputRequired      k) = go' cs' =<< k c
      where
        (c, cs') = uncons cs

    uncons :: ByteString -> (Strict.ByteString, ByteString)
    uncons Empty        = (Strict.empty, Empty)
    uncons (Chunk c cs) = (c, cs)
#else
maybeDecompress bytes = foldStream $ decompressWithErrors gzipOrZlibFormat defaultDecompressParams bytes
  where
    -- DataError at the beginning of the stream probably means that stream is not compressed.
    -- Returning it as-is.
    -- TODO: alternatively, we might consider looking for the two magic bytes
    -- at the beginning of the gzip header.
    foldStream (StreamError _ _) = bytes
    foldStream somethingElse = doFold somethingElse

    doFold StreamEnd               = BS.Empty
    doFold (StreamChunk bs stream) = BS.Chunk bs (doFold stream)
    doFold (StreamError _ msg)  = error $ "Codec.Compression.Zlib: " ++ msg
#endif
