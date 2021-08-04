{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Foreign.Marshal.Array.Compat (
  module Base
, callocArray
, callocArray0
) where
import Foreign.Marshal.Array as Base

#if !(MIN_VERSION_base(4,8,0))
import Foreign.Marshal.Alloc.Compat
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import Prelude.Compat

-- |Like 'mallocArray', but allocated memory is filled with bytes of value zero.
--
callocArray :: Storable a => Int -> IO (Ptr a)
callocArray  = doCalloc undefined
  where
    doCalloc :: Storable a' => a' -> Int -> IO (Ptr a')
    doCalloc dummy size  = callocBytes (size * sizeOf dummy)

-- |Like 'callocArray0', but allocated memory is filled with bytes of value
-- zero.
--
callocArray0 :: Storable a => Int -> IO (Ptr a)
callocArray0 size  = callocArray (size + 1)
#endif
