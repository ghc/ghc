{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.IO where

import GHC.Types
import GHC.Integer () -- see Note [Depend upon GHC.Integer] in libraries/base/GHC/Base.hs

failIO :: [Char] -> IO a
mplusIO :: IO a -> IO a -> IO a
