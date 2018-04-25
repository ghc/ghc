{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.IO where

import GHC.Types

failIO :: [Char] -> IO a
mplusIO :: IO a -> IO a -> IO a
