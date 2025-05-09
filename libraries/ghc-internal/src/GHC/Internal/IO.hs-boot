{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.IO where

import GHC.Internal.Stack.Types (HasCallStack)
import GHC.Internal.Types
import {-# SOURCE #-} GHC.Internal.Exception.Type (Exception, SomeException)

mplusIO :: IO a -> IO a -> IO a
mkUserError :: [Char] -> SomeException
throwIO :: (HasCallStack, Exception e) => e -> IO a
