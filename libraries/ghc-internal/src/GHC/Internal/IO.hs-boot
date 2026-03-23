{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -fdefines-known-key-names #-}
module GHC.Internal.IO where

import GHC.Internal.Stack.Types
import GHC.Internal.Types
import {-# SOURCE #-} GHC.Internal.Exception.Type (Exception, SomeException)

mplusIO :: IO a -> IO a -> IO a
mkUserError :: [Char] -> SomeException
throwIO :: (HasCallStack, Exception e) => e -> IO a
