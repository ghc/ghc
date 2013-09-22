{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.IO.Unsafe
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- \"Unsafe\" IO operations.
--
-----------------------------------------------------------------------------

module System.IO.Unsafe (
   -- * Unsafe 'System.IO.IO' operations
   unsafePerformIO,
   unsafeDupablePerformIO,
   unsafeInterleaveIO,
   unsafeFixIO,
  ) where

import GHC.Base
import GHC.IO
import GHC.IORef
import GHC.Exception
import Control.Exception

-- | A slightly faster version of `System.IO.fixIO` that may not be
-- safe to use with multiple threads.  The unsafety arises when used
-- like this:
--
-- >  unsafeFixIO $ \r ->
-- >     forkIO (print r)
-- >     return (...)
--
-- In this case, the child thread will receive a @NonTermination@
-- exception instead of waiting for the value of @r@ to be computed.
--
-- /Since: 4.5.0.0/
unsafeFixIO :: (a -> IO a) -> IO a
unsafeFixIO k = do
  ref <- newIORef (throw NonTermination)
  ans <- unsafeDupableInterleaveIO (readIORef ref)
  result <- k ans
  writeIORef ref result
  return result
