{-# LANGUAGE CPP, NoImplicitPrelude #-}
module System.IO.Unsafe.Compat (
  module Base
, unsafeFixIO
, unsafeDupablePerformIO
) where

import System.IO.Unsafe as Base

#if !(MIN_VERSION_base(4,5,0))
import Control.Exception
import Data.IORef
import GHC.Base
import GHC.IO

-- | A slightly faster version of `System.IO.fixIO` that may not be
-- safe to use with multiple threads.  The unsafety arises when used
-- like this:
--
-- >  unsafeFixIO $ \r -> do
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
#endif
