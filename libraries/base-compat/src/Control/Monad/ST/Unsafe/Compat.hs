{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Control.Monad.ST.Unsafe.Compat (
  -- * Unsafe operations
  unsafeInterleaveST
, unsafeIOToST
, unsafeSTToIO
) where

#if MIN_VERSION_base(4,6,0)
import Control.Monad.ST.Unsafe (unsafeInterleaveST, unsafeIOToST, unsafeSTToIO)
#else
import Control.Monad.ST (unsafeInterleaveST, unsafeIOToST, unsafeSTToIO)
#endif
