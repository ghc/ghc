module T22629 where

-- This should work.
{-# INLINABLE foo #-}
{-# NOINLINE[1] foo #-}
foo = case error "wombat" of { True -> "fred"; False -> "bill" }
