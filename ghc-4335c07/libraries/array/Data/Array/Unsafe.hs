-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Unsafe
-- Copyright   :  (c) The University of Glasgow 2011
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Array.MArray)
--
-- Contains the various unsafe operations that can be performed
-- on arrays.
--
-- @since 0.4.0.0
-----------------------------------------------------------------------------

module Data.Array.Unsafe (
    -- * Unsafe operations
    castSTUArray,  -- :: STUArray s i a -> ST s (STUArray s i b)
    castIOUArray,  -- :: IOUArray i a -> IO (IOUArray i b)

    unsafeFreeze,  -- :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
    unsafeThaw,    -- :: (Ix i, IArray a e, MArray b e m) => a i e -> m (b i e)

    unsafeForeignPtrToStorableArray -- :: Ix i => ForeignPtr e -> (i,i)
                                    --         -> IO (StorableArray i e)
  ) where


import Data.Array.Base ( castSTUArray, unsafeFreeze, unsafeThaw )
import Data.Array.IO.Internals ( castIOUArray )
import Data.Array.Storable.Internals ( unsafeForeignPtrToStorableArray )

