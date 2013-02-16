{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP, NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Marshal
-- Copyright   :  (c) The FFI task force 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Marshalling support
--
-----------------------------------------------------------------------------

module Foreign.Marshal
        (
         -- | The module "Foreign.Marshal" re-exports the safe content in the
         -- @Foreign.Marshal@ hierarchy:
          module Foreign.Marshal.Safe
         -- | and provides one function:
        , unsafeLocalState
        ) where

import Foreign.Marshal.Safe

#ifdef __GLASGOW_HASKELL__
import GHC.IO
#else
import System.IO.Unsafe
#endif

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
{-# DEPRECATED unsafeLocalState "Please import from Foreign.Marshall.Unsafe instead; This will be removed in the next release" #-} -- deprecated in 7.2
unsafeLocalState :: IO a -> a
unsafeLocalState = unsafePerformIO

