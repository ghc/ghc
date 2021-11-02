{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.IO(
    IO(..),
    mplusIO,
    throwIOUserError
) where

import GHC.Types
import {-# SOURCE #-} GHC.Base (String)

mplusIO :: IO a -> IO a -> IO a
throwIOUserError :: String -> IO a
