-- Trac #1888 
-- Pretty printing for pragmas

module ShouldFail where

{-# NOINLINE[1] foo #-}
{-# NOINLINE[~2] foo #-}
{-# NOINLINE foo #-}
{-# INLINE[1] foo #-}
{-# INLINE[~2] foo #-}
{-# INLINE foo #-}

foo n = foo (n+1)
