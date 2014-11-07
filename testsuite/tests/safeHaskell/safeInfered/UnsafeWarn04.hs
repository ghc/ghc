{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fwarn-trustworthy-safe #-}
{-# OPTIONS_GHC -fwarn-unsafe #-}

-- | Trivial Unsafe Module
module UnsafeWarn04 where

import System.IO.Unsafe

f :: IO a -> a
f = unsafePerformIO

