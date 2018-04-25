{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -fwarn-trustworthy-safe #-}
{-# OPTIONS_GHC -fwarn-safe #-}
{-# OPTIONS_GHC -fwarn-unsafe #-}
{-# OPTIONS_GHC -fenable-rewrite-rules #-}

-- | Trivial Unsafe Module
module UnsafeWarn05 where

import System.IO.Unsafe

f :: IO a -> a
f = unsafePerformIO

{-# RULES "g" g = undefined #-}
{-# NOINLINE [1] g #-}
g :: Int
g = 1

