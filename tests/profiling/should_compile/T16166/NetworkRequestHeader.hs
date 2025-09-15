{-# LANGUAGE BangPatterns #-}
-- NetworkRequestHeader.hs
module NetworkRequestHeader (parseHeaderLines, parseRequestLine) where

import Control.Exception
import Control.Monad
import Data.ByteString.Internal (ByteString(..), memchr)
import Data.Word
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr, minusPtr, nullPtr)
import Foreign.Storable (peek)

-- | Error types for bad 'Request'.
data InvalidRequest = NonHttp

instance Show InvalidRequest where show _ = ""
instance Exception InvalidRequest

parseHeaderLines :: [ByteString]
                 -> IO (ByteString
                       ,ByteString  --  Path
                       ,ByteString  --  Path, parsed
                       )
parseHeaderLines [] = throwIO $ NonHttp
parseHeaderLines (firstLine:_) = do
    (method, path') <- parseRequestLine firstLine
    let path = path'
    return (method, path', path)

parseRequestLine :: ByteString
                 -> IO (ByteString
                       ,ByteString)
parseRequestLine (PS fptr off len) = withForeignPtr fptr $ \ptr -> do
    when (len < 14) $ throwIO NonHttp
    let methodptr = ptr `plusPtr` off
        limptr = methodptr `plusPtr` len
        lim0 = fromIntegral len

    pathptr0 <- memchr methodptr 32 lim0 -- ' '
    when (pathptr0 == nullPtr || (limptr `minusPtr` pathptr0) < 11) $
        throwIO NonHttp
    let pathptr = pathptr0 `plusPtr` 1
        lim1 = fromIntegral (limptr `minusPtr` pathptr0)

    httpptr0 <- memchr pathptr 32 lim1 -- ' '
    when (httpptr0 == nullPtr || (limptr `minusPtr` httpptr0) < 9) $
        throwIO NonHttp
    let httpptr = httpptr0 `plusPtr` 1
        lim2 = fromIntegral (httpptr0 `minusPtr` pathptr)

    checkHTTP httpptr
    queryptr <- memchr pathptr 63 lim2 -- '?'

    let !method = bs ptr methodptr pathptr0
        !path
          | queryptr == nullPtr = bs ptr pathptr httpptr0
          | otherwise           = bs ptr pathptr queryptr

    return (method,path)
  where
    check :: Ptr Word8 -> Int -> Word8 -> IO ()
    check p n w = do
        w0 <- peek $ p `plusPtr` n
        when (w0 /= w) $ throwIO NonHttp
    checkHTTP httpptr = do
        check httpptr 0 72 -- 'H'
        check httpptr 1 84 -- 'T'
        check httpptr 2 84 -- 'T'
        check httpptr 3 80 -- 'P'
        check httpptr 4 47 -- '/'
        check httpptr 6 46 -- '.'
    bs ptr p0 p1 = PS fptr o l
      where
        o = p0 `minusPtr` ptr
        l = p1 `minusPtr` p0

