{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Control.Monad.ST.Lazy.Unsafe.Compat (
  -- * Unsafe operations
  unsafeInterleaveST
, unsafeIOToST
) where

#if MIN_VERSION_base(4,6,0)
import Control.Monad.ST.Lazy.Unsafe (unsafeInterleaveST, unsafeIOToST)
#else
import Control.Monad.ST.Lazy (unsafeInterleaveST, unsafeIOToST)
#endif
