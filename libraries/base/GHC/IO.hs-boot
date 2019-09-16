{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.IO where

import GHC.Types
import GHC.Integer () -- See Note [Depend on GHC.Integer] in GHC.Base

mplusIO :: IO a -> IO a -> IO a
