{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
module Data.Attoparsec.Internal.Compat where

import Data.ByteString.Internal (ByteString(..))
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)

#if MIN_VERSION_bytestring(0,11,0)
import Data.ByteString.Internal (plusForeignPtr)
#endif

withPS :: ByteString -> (ForeignPtr Word8 -> Int -> Int -> r) -> r
#if MIN_VERSION_bytestring(0,11,0)
withPS (BS fp len)     kont = kont fp 0   len
#else
withPS (PS fp off len) kont = kont fp off len
#endif
{-# INLINE withPS #-}

mkPS :: ForeignPtr Word8 -> Int -> Int -> ByteString
#if MIN_VERSION_bytestring(0,11,0)
mkPS fp off len = BS (plusForeignPtr fp off) len
#else
mkPS fp off len = PS fp off len
#endif
{-# INLINE mkPS #-}
