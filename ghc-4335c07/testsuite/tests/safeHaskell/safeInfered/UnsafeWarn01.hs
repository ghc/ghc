{-# OPTIONS_GHC -fwarn-safe #-}
{-# OPTIONS_GHC -fwarn-unsafe #-}

-- | Trivial Unsafe Module
module UnsafeWarn01 where

import System.IO.Unsafe

f :: IO a -> a
f = unsafePerformIO

