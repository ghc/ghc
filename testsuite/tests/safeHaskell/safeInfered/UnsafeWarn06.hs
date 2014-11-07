{-# OPTIONS_GHC -fenable-rewrite-rules #-}
{-# OPTIONS_GHC -fwarn-safe #-}
{-# OPTIONS_GHC -fwarn-unsafe #-}

-- | Unsafe as uses RULES
module UnsafeWarn06 where

{-# RULES "f" f = undefined #-}
{-# NOINLINE [1] f #-}
f :: Int
f = 1

