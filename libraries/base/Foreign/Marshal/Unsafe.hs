{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Marshal.Unsafe
-- Copyright   :  (c) The FFI task force 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Marshalling support. Unsafe API.
--
-----------------------------------------------------------------------------

module Foreign.Marshal.Unsafe (
        -- * Unsafe functions
        unsafeLocalState
    ) where

import GHC.IO

{- |
Sometimes an external entity is a pure function, except that it passes
arguments and/or results via pointers.  The function
@unsafeLocalState@ permits the packaging of such entities as pure
functions.  

The only IO operations allowed in the IO action passed to
@unsafeLocalState@ are (a) local allocation (@alloca@, @allocaBytes@
and derived operations such as @withArray@ and @withCString@), and (b)
pointer operations (@Foreign.Storable@ and @Foreign.Ptr@) on the
pointers to local storage, and (c) foreign functions whose only
observable effect is to read and/or write the locally allocated
memory.  Passing an IO operation that does not obey these rules
results in undefined behaviour.

It is expected that this operation will be
replaced in a future revision of Haskell.
-}
unsafeLocalState :: IO a -> a
unsafeLocalState = unsafeDupablePerformIO

