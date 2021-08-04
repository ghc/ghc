{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Basement.Bindings.Memory
    where

import GHC.IO
import GHC.Prim
import GHC.Word
import Basement.Compat.C.Types
import Foreign.Ptr
import Basement.Types.OffsetSize

foreign import ccall unsafe "_foundation_memcmp" sysHsMemcmpBaBa ::
    ByteArray# -> Offset Word8 -> ByteArray# -> Offset Word8 -> CountOf Word8 -> IO CInt

foreign import ccall unsafe "_foundation_memcmp" sysHsMemcmpBaPtr ::
    ByteArray# -> Offset Word8 -> Ptr a -> Offset Word8 -> CountOf Word8 -> IO CInt

foreign import ccall unsafe "_foundation_memcmp" sysHsMemcmpPtrBa ::
    Ptr a -> Offset Word8 -> ByteArray# -> Offset Word8 -> CountOf Word8 -> IO CInt

foreign import ccall unsafe "_foundation_memcmp" sysHsMemcmpPtrPtr ::
    Ptr a -> Offset Word8 -> Ptr b -> Offset Word8 -> CountOf Word8 -> IO CInt

foreign import ccall unsafe "_foundation_mem_findbyte" sysHsMemFindByteBa ::
    ByteArray# -> Offset Word8 -> Offset Word8 -> Word8 -> Offset Word8

foreign import ccall unsafe "_foundation_mem_findbyte" sysHsMemFindByteAddr ::
    Addr# -> Offset Word8 -> Offset Word8 -> Word8 -> Offset Word8
