{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.Marshal.Utils.Compat (
  module Base
, fillBytes
) where

import Foreign.Marshal.Utils as Base

#if !(MIN_VERSION_base(4,8,0))
import Data.Word (Word8)
import Foreign.C.Types
import Foreign.Ptr
import Prelude

-- |Fill a given number of bytes in memory area with a byte value.
--
-- /Since: 4.8.0.0/
fillBytes :: Ptr a -> Word8 -> Int -> IO ()
fillBytes dest char size = do
  _ <- memset dest (fromIntegral char) (fromIntegral size)
  return ()

foreign import ccall unsafe "string.h" memset :: Ptr a -> CInt -> CSize -> IO (Ptr a)
#endif
