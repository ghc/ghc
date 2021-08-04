{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Foreign.ForeignPtr.Unsafe.Compat (
  -- ** Unsafe low-level operations
  unsafeForeignPtrToPtr
) where

#if MIN_VERSION_base(4,6,0)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
#else
import Foreign.ForeignPtr (unsafeForeignPtrToPtr)
#endif
