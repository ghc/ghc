{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.IO where

import GHC.Types
import {-# SOURCE #-} GHC.Exception.Type (SomeExceptionWithLocation)

mplusIO :: IO a -> IO a -> IO a
mkUserError :: [Char] -> SomeExceptionWithLocation
