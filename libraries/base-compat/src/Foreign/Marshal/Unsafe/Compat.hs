{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Foreign.Marshal.Unsafe.Compat (
  -- * Unsafe functions
  unsafeLocalState
) where

#if MIN_VERSION_base(4,6,0)
import Foreign.Marshal.Unsafe (unsafeLocalState)
#else
import Foreign.Marshal (unsafeLocalState)
#endif
