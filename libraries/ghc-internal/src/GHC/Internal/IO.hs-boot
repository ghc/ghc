{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.IO where

import GHC.Types
import {-# SOURCE #-} GHC.Internal.Exception.Type (SomeException)

mplusIO :: IO a -> IO a -> IO a
mkUserError :: [Char] -> SomeException
