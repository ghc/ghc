{-# LANGUAGE CPP #-}

-- |
-- Module      : Data.Vector.Storable.Internal
-- Copyright   : (c) Roman Leshchinskiy 2009-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Ugly internal utility functions for implementing 'Storable'-based vectors.
--

module Data.Vector.Storable.Internal (
  getPtr, setPtr, updPtr, unsafeWithForeignPtr
) where

import Foreign.ForeignPtr ()
import Foreign.Ptr        ()
import GHC.ForeignPtr   ( ForeignPtr(..) )
#if MIN_VERSION_base(4,15,0)
import GHC.ForeignPtr       ( unsafeWithForeignPtr )
#else
import Foreign.ForeignPtr   ( withForeignPtr )
#endif
import GHC.Ptr          ( Ptr(..) )

getPtr :: ForeignPtr a -> Ptr a
{-# INLINE getPtr #-}
getPtr (ForeignPtr addr _) = Ptr addr

setPtr :: ForeignPtr a -> Ptr a -> ForeignPtr a
{-# INLINE setPtr #-}
setPtr (ForeignPtr _ c) (Ptr addr) = ForeignPtr addr c

updPtr :: (Ptr a -> Ptr a) -> ForeignPtr a -> ForeignPtr a
{-# INLINE updPtr #-}
updPtr f (ForeignPtr p c) = case f (Ptr p) of { Ptr q -> ForeignPtr q c }

#if !MIN_VERSION_base(4,15,0)
-- | A compatibility wrapper for 'GHC.ForeignPtr.unsafeWithForeignPtr' provided
-- by GHC 9.0.1 and later.
--
-- Only to be used when the continuation is known not to
-- unconditionally diverge lest unsoundness can result.
unsafeWithForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
unsafeWithForeignPtr = withForeignPtr
#endif
