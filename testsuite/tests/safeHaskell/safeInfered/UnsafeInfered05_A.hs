{-# OPTIONS_GHC -fenable-rewrite-rules #-}
-- | Unsafe as uses RULES
-- Although only turns on the flag doesn't define? So mark safe
-- maybe?
module UnsafeInfered05_A where

{-# RULES "f" f = undefined #-}
{-# NOINLINE [1] f #-}
f :: Int
f = 1

